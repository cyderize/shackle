//! Shackle runtime
pub mod builtin;
pub mod comprehension;
mod error;
pub mod nanozinc;
pub mod stack;
pub mod value;
use std::num::NonZeroU32;

use shackle_ir::{FunctionId, Instruction, InstructionId, Program};
pub use value::{AnnotationView, DataView, FloatVal, IntVal, Value};

use crate::{
	comprehension::ComprehensionState,
	nanozinc::NanoZinc,
	stack::{SlotIndex, Stack},
	value::{FLOAT_SET_INF, INT_SET_INF},
};

/// The address of an instruction
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Address<'db> {
	/// The function that the address is in
	pub function: FunctionId<'db>,
	/// The instruction that the address points to
	pub instruction: InstructionId<'db>,
}

impl<'db> Address<'db> {
	/// Get the next instruction after this one
	pub fn next(&self) -> Self {
		Self {
			function: self.function,
			instruction: self.instruction.next(),
		}
	}
}

/// The interpreter for the Shackle IR
#[derive(Debug)]
pub struct Interpreter<'db> {
	db: &'db dyn shackle_hir::Db,
	pc: Address<'db>,
	stack: Stack<'db>,
	comprehensions: ComprehensionState,
	nanozinc: NanoZinc<'db>,
}

impl<'db> Interpreter<'db> {
	/// Evaluate a function
	pub fn eval(
		&mut self,
		program: &Program<'db>,
		function: FunctionId<'db>,
		arguments: &[Value],
	) -> Value {
		let old_pc = self.pc;
		let return_slot = SlotIndex::first();
		self.stack
			.push_call_frame(old_pc, return_slot, program.functions[function].slot_count);
		for (i, arg) in arguments.iter().enumerate() {
			let slot = shackle_ir::SlotId::Relative(NonZeroU32::new((i + 1) as u32).unwrap());
			self.stack[slot] = arg.clone();
		}
		self.pc = Address {
			function,
			instruction: program.functions[function].body.keys().next().unwrap(),
		};
		while self.pc != old_pc {
			self.step(program);
		}
		self.stack[return_slot].clone()
	}

	/// Step the interpreter forward one instruction
	pub fn step(&mut self, program: &Program<'db>) {
		let instruction = &program.functions[self.pc.function].body[self.pc.instruction];
		self.pc = self.pc.next();
		match instruction {
			Instruction::Assign {
				slot,
				expression: shackle_ir::Expression::AnnotationCall(c),
			} => {
				let ident = c.annotation.into();
				let value = Value::new_annotation(
					ident,
					c.arguments.iter().map(|arg| self.eval_value(arg)),
				);
				self.stack[*slot] = value;
			}
			Instruction::Assign {
				slot,
				expression: shackle_ir::Expression::Builtin(b),
			} => self.stack[*slot] = self.eval_builtin(b),
			Instruction::Assign {
				slot,
				expression: shackle_ir::Expression::Call(c),
			} => {
				assert!(
					!program.functions[c.function].body.is_empty(),
					"Call assigned to slot must have a body"
				);
				self.stack.push_call_frame(
					self.pc,
					*slot,
					program.functions[c.function].slot_count,
				);
				for (i, arg) in c.arguments.iter().enumerate() {
					let slot =
						shackle_ir::SlotId::Relative(NonZeroU32::new((i + 1) as u32).unwrap());
					let value = self.eval_value(arg);
					self.stack[slot] = value;
				}
				self.pc = Address {
					function: c.function,
					instruction: program.functions[c.function].body.keys().next().unwrap(),
				};
			}
			Instruction::Assign {
				slot,
				expression: shackle_ir::Expression::Value(value),
			} => {
				self.stack[*slot] = self.eval_value(value);
			}
			Instruction::Constraint {
				expression: shackle_ir::Expression::Call(c),
			} if program.functions[c.function].body.is_empty() => {
				// TODO: Add to nanozinc
				todo!()
			}
			Instruction::Constraint {
				expression: shackle_ir::Expression::Call(c),
			} => {
				self.stack.push_call_frame(
					self.pc,
					SlotIndex::first(),
					program.functions[c.function].slot_count,
				);
				for (i, arg) in c.arguments.iter().enumerate() {
					let slot =
						shackle_ir::SlotId::Relative(NonZeroU32::new((i + 1) as u32).unwrap());
					let value = self.eval_value(arg);
					self.stack[slot] = value;
				}
				self.pc = Address {
					function: c.function,
					instruction: program.functions[c.function].body.keys().next().unwrap(),
				};
			}
			Instruction::Constraint {
				expression: shackle_ir::Expression::Builtin(b),
			} => {
				let _ = self.eval_builtin(b);
			}
			Instruction::Constraint {
				expression: shackle_ir::Expression::Value(_),
			}
			| Instruction::Constraint {
				expression: shackle_ir::Expression::AnnotationCall(_),
			} => {
				unreachable!("Invalid constraint expression");
			}
			Instruction::Jump { target } => {
				self.pc.instruction = *target;
			}
			Instruction::JumpIfFalse { condition, target } => {
				let value = self.eval_value(condition);
				if value.as_int() == 0.into() {
					self.pc.instruction = *target;
				}
			}
			Instruction::IteratorStart { value } => {
				let value = self.eval_value(value);
				self.comprehensions.start_generator(value);
			}
			Instruction::IteratorNext { slot, done } => {
				let next = self.comprehensions.next_generator_value();
				if let Some(value) = next {
					self.stack[*slot] = value;
				} else {
					self.pc.instruction = *done;
				}
			}
			Instruction::ComprehensionStart => {
				self.comprehensions.start_comprehension();
			}
			Instruction::ComprehensionAppend { value } => {
				let value = self.eval_value(value);
				self.comprehensions.append_comprehension_value(value);
			}
			Instruction::ComprehensionEnd { slot } => {
				let value = self.comprehensions.end_comprehension();
				self.stack[*slot] = value;
			}
			Instruction::Return {
				expression: shackle_ir::Expression::AnnotationCall(c),
			} => {
				let ident = c.annotation.into();
				let value = Value::new_annotation(
					ident,
					c.arguments.iter().map(|arg| self.eval_value(arg)),
				);
				let frame = self.stack.pop_call_frame();
				self.pc = frame.return_address;
				self.stack[frame.return_slot] = value;
			}
			Instruction::Return {
				expression: shackle_ir::Expression::Builtin(b),
			} => {
				let value = self.eval_builtin(b);
				let frame = self.stack.pop_call_frame();
				self.pc = frame.return_address;
				self.stack[frame.return_slot] = value;
			}
			Instruction::Return {
				expression: shackle_ir::Expression::Call(c),
			} => {
				self.stack
					.resize_call_frame(program.functions[c.function].slot_count);
				for (i, arg) in c.arguments.iter().enumerate() {
					let slot =
						shackle_ir::SlotId::Relative(NonZeroU32::new((i + 1) as u32).unwrap());
					let value = self.eval_value(arg);
					self.stack[slot] = value;
				}
				self.pc = Address {
					function: c.function,
					instruction: program.functions[c.function].body.keys().next().unwrap(),
				};
			}
			Instruction::Return {
				expression: shackle_ir::Expression::Value(value),
			} => {
				let value = self.eval_value(value);
				let frame = self.stack.pop_call_frame();
				self.pc = frame.return_address;
				self.stack[frame.return_slot] = value;
			}
			Instruction::NoOp => (),
		}
	}

	fn eval_value(&self, value: &shackle_ir::Value<'db>) -> Value {
		match value {
			shackle_ir::Value::Literal(literal) => self.eval_literal(literal),
			shackle_ir::Value::Tuple(tuple) => {
				Value::from_iter(tuple.members.iter().map(|l| self.eval_literal(l)))
			}
			shackle_ir::Value::Set(set) => {
				Value::from_iter(set.members.iter().map(|l| self.eval_literal(l)))
			}
			shackle_ir::Value::Array(array) => {
				Value::from_iter(array.members.iter().map(|l| self.eval_literal(l)))
			}
			shackle_ir::Value::TupleAccess(tuple_access) => {
				let shackle_ir::Identifier::Slot(s) = &tuple_access.tuple else {
					unreachable!()
				};
				let tuple = &self.stack[*s];
				let DataView::Seq(view) = tuple.deref() else {
					unreachable!()
				};
				view[&[tuple_access.field.0 - 1]].clone()
			}
		}
	}

	fn eval_literal(&self, literal: &shackle_ir::Literal<'db>) -> Value {
		match literal {
			shackle_ir::Literal::Boolean(boolean_literal) => Value::from(boolean_literal.0),
			shackle_ir::Literal::Integer(integer_literal) => Value::from(integer_literal.0),
			shackle_ir::Literal::Float(float_literal) => Value::from(float_literal.value()),
			shackle_ir::Literal::String(string_literal) => {
				Value::from(string_literal.value(self.db))
			}
			shackle_ir::Literal::Infinity => Value::from(IntVal::InfPos),
			shackle_ir::Literal::InfiniteIntSet => INT_SET_INF.clone(),
			shackle_ir::Literal::InfiniteFloatSet => FLOAT_SET_INF.clone(),
			shackle_ir::Literal::Identifier(shackle_ir::Identifier::Slot(s)) => {
				self.stack[*s].clone()
			}
			shackle_ir::Literal::Identifier(shackle_ir::Identifier::Annotation(a)) => {
				let ident = (*a).into();
				Value::new_annotation(ident, [])
			}
		}
	}

	fn eval_builtin(&mut self, builtin: &shackle_ir::Builtin<'db>) -> Value {
		match builtin {
			shackle_ir::Builtin::SetUnionInt { left, right } => {
				let a = self.eval_value(left);
				let b = self.eval_value(right);
				builtin::int_set_union(a, b)
			}
			shackle_ir::Builtin::VarBool => self.nanozinc.add_var_bool(),
			shackle_ir::Builtin::VarInt { domain } => {
				let domain = self.eval_value(domain);
				self.nanozinc.add_var_int(domain)
			}
			shackle_ir::Builtin::VarFloat { domain } => {
				let domain = self.eval_value(domain);
				self.nanozinc.add_var_float(domain)
			}
			shackle_ir::Builtin::VarSetOfInt { domain } => {
				let domain = self.eval_value(domain);
				self.nanozinc.add_var_set_of_int(domain)
			}
			_ => todo!(),
		}
	}
}
