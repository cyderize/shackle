//! Shackle runtime
pub mod builtin;
pub mod comprehension;
pub mod error;
pub mod nanozinc;
pub mod stack;
pub mod value;
use std::{
	io::{self, Write},
	num::NonZeroU32,
};

pub use error::{RuntimeError, RuntimeErrorKind, RuntimeLocation};
use shackle_ir::{FreshVariable, FunctionId, Instruction, InstructionId, Program};
pub use value::{AnnotationView, DataView, FloatVal, IntVal, Value};

use crate::{
	comprehension::ComprehensionState,
	nanozinc::NanoZinc,
	stack::{CallFrame, SlotIndex, Stack},
	value::{FLOAT_SET_INF, INT_SET_INF},
};

/// The state of the interpreter
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InterpreterState {
	/// Running state
	Running,
	/// Paused state
	Paused,
	/// Aborted state
	Aborted,
}

/// The address of an instruction
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct Address {
	/// The function that the address is in
	pub function: FunctionId,
	/// The instruction that the address points to
	pub instruction: InstructionId,
}

impl Address {
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
pub struct Interpreter<'a> {
	program: &'a Program,
	pc: Address,
	state: InterpreterState,
	stack: Stack,
	comprehensions: ComprehensionState,
	nanozinc: NanoZinc,
	output: Box<dyn IoStreams + 'a>,
}

/// Trait for IO streams
pub trait IoStreams: std::fmt::Debug {
	/// Get a mutable reference to the standard output stream
	fn stdout(&mut self) -> &mut dyn Write;
	/// Get a mutable reference to the standard error stream
	fn stderr(&mut self) -> &mut dyn Write;
}

#[derive(Debug)]
struct StdIoStreams {
	stdout: io::Stdout,
	stderr: io::Stderr,
}

impl IoStreams for StdIoStreams {
	fn stdout(&mut self) -> &mut dyn Write {
		&mut self.stdout
	}

	fn stderr(&mut self) -> &mut dyn Write {
		&mut self.stderr
	}
}

impl Default for StdIoStreams {
	fn default() -> Self {
		Self {
			stdout: io::stdout(),
			stderr: io::stderr(),
		}
	}
}

impl<'a> Interpreter<'a> {
	/// Create a new interpreter for the given program
	pub fn new(program: &'a Program) -> Self {
		Self::with_output(program, StdIoStreams::default())
	}

	/// Create an interpreter with custom output streams.
	///
	/// The streams can be borrowed buffers, allowing callers to inspect output
	/// after the interpreter is dropped. Write failures panic, as with `println!`.
	pub fn with_output(program: &'a Program, outputs: impl IoStreams + 'a) -> Self {
		Self {
			program,
			pc: Address {
				function: program.main_entry_point,
				instruction: program.functions[program.main_entry_point]
					.body
					.keys()
					.next()
					.unwrap(),
			},
			state: InterpreterState::Paused,
			stack: Stack::new(program.global_slot_count),
			comprehensions: ComprehensionState::default(),
			nanozinc: NanoZinc::default(),
			output: Box::new(outputs),
		}
	}

	/// Get a reference to the NanoZinc model
	pub fn nanozinc(&self) -> &NanoZinc {
		&self.nanozinc
	}

	/// Get a mutable reference to the NanoZinc model
	pub fn nanozinc_mut(&mut self) -> &mut NanoZinc {
		&mut self.nanozinc
	}

	/// Get the number of call frames on the call stack
	pub fn call_frame_count(&self) -> usize {
		self.stack.call_frame_count()
	}

	/// Whether the current call frame an entry point of the program
	pub fn is_entry_point(&self) -> bool {
		self.pc.function == self.program.main_entry_point
			|| self.pc.function == self.program.output_entry_point
	}

	/// Run the interpreter until it halts. An error permanently aborts this interpreter.
	/// Side effects completed before the error are not rolled back.
	pub fn run(&mut self) -> Result<(), RuntimeError> {
		if self.state == InterpreterState::Aborted {
			return Err(RuntimeError::already_aborted());
		}
		if self.stack.call_frame_count() == 0 {
			self.stack.push_call_frame(
				CallFrame {
					return_address: self.pc,
					return_slot: SlotIndex::first(),
					halt_on_return: true,
					annotations: Vec::new(),
					nanozinc_pending_count: 0,
				},
				self.program.functions[self.pc.function].slot_count,
			);
		}
		if self.state == InterpreterState::Paused {
			self.state = InterpreterState::Running;
		}
		while self.state == InterpreterState::Running {
			self.step()?;
		}
		Ok(())
	}

	/// Evaluate a function. An error permanently aborts this interpreter.
	/// Side effects completed before the error are not rolled back.
	pub fn eval(
		&mut self,
		function: FunctionId,
		arguments: &[Value],
	) -> Result<Value, RuntimeError> {
		if self.state == InterpreterState::Aborted {
			return Err(RuntimeError::already_aborted());
		}
		let old_state = self.state;
		self.state = InterpreterState::Running;
		let old_pc = self.pc;
		let return_slot = SlotIndex::first();
		self.stack.push_call_frame(
			CallFrame {
				return_address: old_pc,
				return_slot,
				halt_on_return: true,
				annotations: Vec::new(),
				nanozinc_pending_count: 0,
			},
			self.program.functions[function].slot_count,
		);
		for (i, arg) in arguments.iter().enumerate() {
			let slot = shackle_ir::SlotId::Relative(NonZeroU32::new((i + 1) as u32).unwrap());
			self.stack[slot] = arg.clone();
		}
		self.pc = Address {
			function,
			instruction: self.program.functions[function].body.keys().next().unwrap(),
		};
		self.state = InterpreterState::Running;
		while self.state == InterpreterState::Running {
			self.step()?;
		}
		assert_eq!(self.pc, old_pc);
		self.state = old_state;
		Ok(self.stack[return_slot].clone())
	}

	/// Step the interpreter forward one instruction. An error permanently aborts this interpreter.
	pub fn step(&mut self) -> Result<(), RuntimeError> {
		if self.state == InterpreterState::Aborted {
			return Err(RuntimeError::already_aborted());
		}
		let address = self.pc;
		match self.execute_step() {
			Ok(()) => Ok(()),
			Err(kind) => {
				let stack_trace = self.stack.trace(self.program, address);
				self.state = InterpreterState::Aborted;
				Err(RuntimeError { kind, stack_trace })
			}
		}
	}

	fn execute_step(&mut self) -> Result<(), RuntimeErrorKind> {
		let instruction = &self.program.functions[self.pc.function].body[self.pc.instruction];
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
			} => self.stack[*slot] = self.eval_builtin(b)?,
			Instruction::Assign {
				slot,
				expression: shackle_ir::Expression::FreshVariable(fv),
			} => self.stack[*slot] = self.eval_fresh_variable(fv),
			Instruction::Assign {
				slot,
				expression: shackle_ir::Expression::Call(c),
			} => {
				assert!(
					!self.program.functions[c.function].body.is_empty(),
					"Call assigned to slot must have a body"
				);
				let mut annotations = c
					.annotations
					.iter()
					.map(|ann| match ann {
						shackle_ir::Identifier::Annotation(idx) => {
							Value::new_annotation((*idx).into(), Vec::new())
						}
						shackle_ir::Identifier::Slot(slot) => self.stack[*slot].clone(),
					})
					.collect::<Vec<_>>();
				if c.forward_annotations {
					annotations.extend(self.stack.annotations().iter().cloned());
				}
				let promise_total = self.program.functions[c.function].promise_total;
				if promise_total {
					self.nanozinc_mut().push_pending();
				}
				self.stack.push_call_frame(
					CallFrame {
						return_address: self.pc,
						return_slot: *slot,
						halt_on_return: false,
						annotations,
						nanozinc_pending_count: promise_total.into(),
					},
					self.program.functions[c.function].slot_count,
				);
				for (i, arg) in c.arguments.iter().enumerate() {
					let slot =
						shackle_ir::SlotId::Relative(NonZeroU32::new((i + 1) as u32).unwrap());
					let value = self.eval_value(arg);
					self.stack[slot] = value;
				}
				self.pc = Address {
					function: c.function,
					instruction: self.program.functions[c.function]
						.body
						.keys()
						.next()
						.unwrap(),
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
			} => {
				let mut annotations = c
					.annotations
					.iter()
					.map(|ann| match ann {
						shackle_ir::Identifier::Annotation(idx) => {
							Value::new_annotation((*idx).into(), Vec::new())
						}
						shackle_ir::Identifier::Slot(slot) => self.stack[*slot].clone(),
					})
					.collect::<Vec<_>>();
				if c.forward_annotations {
					annotations.extend(self.stack.annotations().iter().cloned());
				}
				match c.kind {
					shackle_ir::CallType::Predicate | shackle_ir::CallType::Delayed => {
						let args = c
							.arguments
							.iter()
							.map(|arg| self.eval_value(arg))
							.collect::<Vec<_>>();
						self.nanozinc_mut()
							.add_constraint(c.function, args, annotations);
					}
					shackle_ir::CallType::Function => {
						self.stack.push_call_frame(
							CallFrame {
								return_address: self.pc,
								return_slot: SlotIndex::first(),
								halt_on_return: false,
								annotations,
								nanozinc_pending_count: 0,
							},
							self.program.functions[c.function].slot_count,
						);
						for (i, arg) in c.arguments.iter().enumerate() {
							let slot = shackle_ir::SlotId::Relative(
								NonZeroU32::new((i + 1) as u32).unwrap(),
							);
							let value = self.eval_value(arg);
							self.stack[slot] = value;
						}
						self.pc = Address {
							function: c.function,
							instruction: self.program.functions[c.function]
								.body
								.keys()
								.next()
								.unwrap(),
						};
					}
				}
			}
			Instruction::Constraint {
				expression: shackle_ir::Expression::Builtin(b),
			} => {
				let result = self.eval_builtin(b)?;
				if result != 1.into() {
					return Err(RuntimeErrorKind::ConstraintFalse);
				}
			}
			Instruction::Constraint {
				expression: shackle_ir::Expression::Value(_),
			}
			| Instruction::Constraint {
				expression: shackle_ir::Expression::AnnotationCall(_),
			}
			| Instruction::Constraint {
				expression: shackle_ir::Expression::FreshVariable(_),
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
				for _ in 0..frame.nanozinc_pending_count {
					self.nanozinc_mut().pop_pending();
				}
				if frame.halt_on_return {
					self.state = InterpreterState::Paused;
				}
			}
			Instruction::Return {
				expression: shackle_ir::Expression::Builtin(b),
			} => {
				let value = self.eval_builtin(b)?;
				let frame = self.stack.pop_call_frame();
				self.pc = frame.return_address;
				self.stack[frame.return_slot] = value;
				for _ in 0..frame.nanozinc_pending_count {
					self.nanozinc_mut().pop_pending();
				}
				if frame.halt_on_return {
					self.state = InterpreterState::Paused;
				}
			}
			Instruction::Return {
				expression: shackle_ir::Expression::FreshVariable(fv),
			} => {
				let value = self.eval_fresh_variable(fv);
				let frame = self.stack.pop_call_frame();
				self.pc = frame.return_address;
				self.stack[frame.return_slot] = value;
				for _ in 0..frame.nanozinc_pending_count {
					self.nanozinc_mut().pop_pending();
				}
				if frame.halt_on_return {
					self.state = InterpreterState::Paused;
				}
			}
			Instruction::Return {
				expression: shackle_ir::Expression::Call(c),
			} => {
				let annotations = c
					.annotations
					.iter()
					.map(|ann| match ann {
						shackle_ir::Identifier::Annotation(idx) => {
							Value::new_annotation((*idx).into(), Vec::new())
						}
						shackle_ir::Identifier::Slot(slot) => self.stack[*slot].clone(),
					})
					.collect::<Vec<_>>();
				self.stack.tail_call(
					self.program.functions[c.function].slot_count,
					self.program.functions[c.function].promise_total,
					!c.forward_annotations,
					annotations,
				);
				for (i, arg) in c.arguments.iter().enumerate() {
					let slot =
						shackle_ir::SlotId::Relative(NonZeroU32::new((i + 1) as u32).unwrap());
					let value = self.eval_value(arg);
					self.stack[slot] = value;
				}
				self.pc = Address {
					function: c.function,
					instruction: self.program.functions[c.function]
						.body
						.keys()
						.next()
						.unwrap(),
				};
			}
			Instruction::Return {
				expression: shackle_ir::Expression::Value(value),
			} => {
				let value = self.eval_value(value);
				let frame = self.stack.pop_call_frame();
				self.pc = frame.return_address;
				self.stack[frame.return_slot] = value;
				for _ in 0..frame.nanozinc_pending_count {
					self.nanozinc_mut().pop_pending();
				}
				if frame.halt_on_return {
					self.state = InterpreterState::Paused;
				}
			}
			Instruction::ReturnRoot => {
				let frame = self.stack.pop_call_frame();
				self.pc = frame.return_address;
				if frame.halt_on_return {
					self.state = InterpreterState::Paused;
				}
			}
			Instruction::NoOp => (),
		}
		Ok(())
	}

	fn eval_value(&self, value: &shackle_ir::Value) -> Value {
		match value {
			shackle_ir::Value::Literal(literal) => self.eval_literal(literal),
			shackle_ir::Value::Tuple(tuple) => {
				Value::from_iter(tuple.members.iter().map(|l| self.eval_literal(l)))
			}
			shackle_ir::Value::Set(set) => match set.kind {
				shackle_ir::SetType::Integer => Value::from_iter(set.members.iter().map(|l| {
					let v = self.eval_literal(l).as_int();
					v..=v
				})),
				shackle_ir::SetType::Float => Value::from_iter(set.members.iter().map(|l| {
					let v = self.eval_literal(l).as_float();
					v..=v
				})),
				shackle_ir::SetType::Discrete => {
					Value::new_discrete_set(set.members.iter().map(|l| self.eval_literal(l)))
				}
			},
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
				view[&[tuple_access.field.0]].clone()
			}
		}
	}

	fn eval_literal(&self, literal: &shackle_ir::Literal) -> Value {
		match literal {
			shackle_ir::Literal::Boolean(boolean_literal) => Value::from(boolean_literal.0),
			shackle_ir::Literal::Integer(integer_literal) => Value::from(integer_literal.0),
			shackle_ir::Literal::Float(float_literal) => Value::from(float_literal.value()),
			shackle_ir::Literal::String(s) => Value::from(&self.program.strings[*s][..]),
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

	fn eval_builtin(&mut self, builtin: &shackle_ir::Builtin) -> Result<Value, RuntimeErrorKind> {
		builtin::eval_builtin(self, builtin.map(|value| self.eval_value(value)))
	}

	fn eval_fresh_variable(&mut self, fv: &FreshVariable<shackle_ir::Value>) -> Value {
		match fv {
			FreshVariable::VarBool { is_defined } => self.nanozinc_mut().add_var_bool(*is_defined),
			FreshVariable::VarInt { domain, is_defined } => {
				let domain = self.eval_value(domain);
				self.nanozinc_mut().add_var_int(domain, *is_defined)
			}
			FreshVariable::VarFloat { domain, is_defined } => {
				let domain = self.eval_value(domain);
				self.nanozinc_mut().add_var_float(domain, *is_defined)
			}
			FreshVariable::VarSetOfInt { domain, is_defined } => {
				let domain = self.eval_value(domain);
				self.nanozinc_mut().add_var_set_of_int(domain, *is_defined)
			}
		}
	}

	/// Get the output streams
	pub fn output(&mut self) -> &mut dyn IoStreams {
		self.output.as_mut()
	}
}

#[cfg(test)]
mod tests {
	use expect_test::expect;
	use shackle_hir::{
		CompilerDatabase,
		db::Setter,
		input::{CompilerSettings, InlineModelFile, InputFiles, InputLang},
	};

	use crate::{DataView, Interpreter, InterpreterState, IoStreams, RuntimeErrorKind};

	#[derive(Default)]
	struct MemoryStreams {
		stdout: Vec<u8>,
		stderr: Vec<u8>,
	}

	impl IoStreams for MemoryStreams {
		fn stdout(&mut self) -> &mut dyn std::io::prelude::Write {
			&mut self.stdout
		}

		fn stderr(&mut self) -> &mut dyn std::io::prelude::Write {
			&mut self.stderr
		}
	}

	impl std::fmt::Debug for MemoryStreams {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			f.debug_struct("MemoryStreams")
				.field("stdout", &String::from_utf8(self.stdout.clone()).unwrap())
				.field("stderr", &String::from_utf8(self.stderr.clone()).unwrap())
				.finish()
		}
	}

	fn value_test_program() -> shackle_ir::Program {
		use shackle_utils::arena::Arena;

		let mut strings = Arena::default();
		let name = strings.insert(String::from("main"));
		let mut body = Arena::default();
		let _ = body.insert(shackle_ir::Instruction::ReturnRoot);
		let mut functions = Arena::default();
		let main = functions.insert(shackle_ir::Function {
			name,
			slot_count: 0,
			promise_total: false,
			body,
			origins: Default::default(),
		});
		shackle_ir::Program {
			global_slot_count: 2,
			functions,
			annotations: Default::default(),
			main_entry_point: main,
			output_entry_point: main,
			strings,
		}
	}

	#[test]
	fn eval_value_literals_and_identifiers() {
		use std::num::NonZeroU32;

		use shackle_hir::{BooleanLiteral, FloatLiteral, IntegerLiteral};
		use shackle_ir::{Identifier, Literal, SlotId};

		let mut program = value_test_program();
		let string = program.strings.insert(String::from("hello"));
		let annotation = program
			.annotations
			.insert(shackle_ir::Annotation { name: string });
		let mut interpreter = Interpreter::new(&program);
		let slot = SlotId::Absolute(NonZeroU32::new(1).unwrap());
		interpreter.stack[slot] = crate::Value::from(37);
		let cases = [
			(
				Literal::Boolean(BooleanLiteral(true)),
				crate::Value::from(true),
			),
			(
				Literal::Integer(IntegerLiteral(-12)),
				crate::Value::from(-12),
			),
			(
				Literal::Float(FloatLiteral::new(1.5)),
				crate::Value::from(1.5),
			),
			(Literal::String(string), crate::Value::from("hello")),
			(Literal::Infinity, crate::Value::from(crate::IntVal::InfPos)),
			(Literal::InfiniteIntSet, crate::value::INT_SET_INF.clone()),
			(
				Literal::InfiniteFloatSet,
				crate::value::FLOAT_SET_INF.clone(),
			),
			(
				Literal::Identifier(Identifier::Slot(slot)),
				crate::Value::from(37),
			),
			(
				Literal::Identifier(Identifier::Annotation(annotation)),
				crate::Value::new_annotation(annotation.into(), []),
			),
		];
		for (literal, expected) in cases {
			assert_eq!(
				interpreter.eval_value(&literal.clone().into()),
				expected,
				"{literal:?}"
			);
		}
	}

	#[test]
	fn eval_value_collections_and_tuple_access() {
		use std::num::NonZeroU32;

		use shackle_hir::{FloatLiteral, IntegerLiteral};
		use shackle_ir::{Identifier, Literal, SetType, SlotId};

		let program = value_test_program();
		let mut interpreter = Interpreter::new(&program);
		let slot = SlotId::Absolute(NonZeroU32::new(1).unwrap());
		interpreter.stack[slot] = crate::Value::from(7);
		let int = |n| Literal::Integer(IntegerLiteral(n));
		let slot_literal = Literal::Identifier(Identifier::Slot(slot));
		let members = vec![int(3), slot_literal.clone(), int(3)];
		let sequence = crate::Value::from_iter([3, 7, 3].map(crate::Value::from));
		for value in [
			shackle_ir::Value::Tuple(shackle_ir::Tuple {
				members: members.clone(),
			}),
			shackle_ir::Value::Array(shackle_ir::Array {
				members: members.clone(),
			}),
		] {
			assert_eq!(interpreter.eval_value(&value), sequence);
		}
		let set = shackle_ir::Value::Set(shackle_ir::Set {
			kind: SetType::Integer,
			members: vec![int(3), slot_literal.clone()],
		});
		let expected = crate::Value::from_iter([
			crate::IntVal::from(3)..=crate::IntVal::from(3),
			crate::IntVal::from(7)..=crate::IntVal::from(7),
		]);
		assert_eq!(interpreter.eval_value(&set), expected);
		let set = shackle_ir::Value::Set(shackle_ir::Set {
			kind: SetType::Float,
			members: vec![
				Literal::Float(FloatLiteral::new(-1.0)),
				Literal::Float(FloatLiteral::new(2.5)),
			],
		});
		let expected = crate::Value::from_iter([
			crate::FloatVal::from(-1.0)..=crate::FloatVal::from(-1.0),
			crate::FloatVal::from(2.5)..=crate::FloatVal::from(2.5),
		]);
		assert_eq!(interpreter.eval_value(&set), expected);
		let set = shackle_ir::Value::Set(shackle_ir::Set {
			kind: SetType::Discrete,
			members: vec![int(3), slot_literal, int(3)],
		});
		assert_eq!(
			interpreter.eval_value(&set),
			crate::Value::new_discrete_set([crate::Value::from(3), crate::Value::from(7)])
		);
		interpreter.stack[slot] = sequence;
		let access = shackle_ir::Value::TupleAccess(shackle_ir::TupleAccess {
			tuple: Identifier::Slot(slot),
			field: IntegerLiteral(2),
		});
		assert_eq!(interpreter.eval_value(&access), crate::Value::from(7));
	}

	#[test]
	fn test_interpreter_1() {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let source = r#"
			annotation mzn_forward_annotations;
			annotation output;
			function int: '+'(int: a, int: b);
			function int: foo() = 123 + 456;
			int: x :: output = foo();
			"#;
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let program = shackle_ir::Program::lower_from_mir(&db);
		let function = program.functions.keys().next().unwrap();
		let mut interpreter = Interpreter::new(&program);
		let result = interpreter.eval(function, &[]).unwrap();
		let DataView::Int(x) = result.deref() else {
			panic!("expected result to be an integer");
		};
		assert_eq!(x, 579.into());
	}

	#[test]
	fn test_interpreter_2() {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let source = r#"
			predicate foo(var int: x);
			test abort(string);
			annotation output;
			var {1, 3, 5}: x;
			constraint foo(x);
			"#;
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let program = shackle_ir::Program::lower_from_mir(&db);
		let mut interpreter = Interpreter::new(&program);
		interpreter.run().unwrap();
		expect![[r#"
    var 1..1 ∪ 3..3 ∪ 5..5: _X_INTRODUCED_0;

    constraint foo(_X_INTRODUCED_0);

    solve satisfy;
"#]]
		.assert_eq(&interpreter.nanozinc_mut().print(&program));
	}

	#[test]
	fn test_interpreter_3() {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let source = r#"
			annotation mzn_forward_annotations;
			predicate foo(var int: x, var int: y);
			function var int: foo_t(var int: x) :: promise_total = let {
				var int: y;
				constraint foo(x, y);
			} in y;
			test abort(string);
			annotation promise_total;
			annotation output;
			annotation my_ann;
			var {1, 3, 5}: x;
			var int: y :: output = foo_t(x) :: my_ann;
			"#;
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let program = shackle_ir::Program::lower_from_mir(&db);
		let mut interpreter = Interpreter::new(&program);
		interpreter.run().unwrap();
		expect![[r#"
    var 1..1 ∪ 3..3 ∪ 5..5: _X_INTRODUCED_0;
    var int: _X_INTRODUCED_1; % defined by group 1

    % defining group 1
    constraint foo(_X_INTRODUCED_0, _X_INTRODUCED_1) :: my_ann;

    solve satisfy;
"#]]
		.assert_eq(&interpreter.nanozinc_mut().print(&program));
	}

	#[test]
	fn test_interpreter_4() {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let source = r#"
			annotation mzn_forward_annotations;
			predicate foo(var int: x, array [int] of var int: y);
			function array [int] of var int: mzn_array_kd(tuple(set of int): d, array [int] of var int: x);
			function array [int] of var int: foo_t(var int: x) :: promise_total = let {
				array [{1, 2, 3}] of var int: y;
				constraint foo(x, y);
			} in y;
			test abort(string);
			annotation promise_total;
			annotation output;
			var {1, 3, 5}: x;
			any: y :: output = foo_t(x);
			"#;
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let program = shackle_ir::Program::lower_from_mir(&db);
		let mut interpreter = Interpreter::new(&program);
		interpreter.run().unwrap();
		expect![[r#"
    var 1..1 ∪ 3..3 ∪ 5..5: _X_INTRODUCED_0;
    var int: _X_INTRODUCED_1; % defined by group 1
    var int: _X_INTRODUCED_2; % defined by group 1
    var int: _X_INTRODUCED_3; % defined by group 1

    % defining group 1
    constraint foo(_X_INTRODUCED_0, [_X_INTRODUCED_1, _X_INTRODUCED_2, _X_INTRODUCED_3]);

    solve satisfy;
"#]]
		.assert_eq(&interpreter.nanozinc_mut().print(&program));
	}

	#[test]
	fn test_abort() {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let source = r#"annotation mzn_forward_annotations;
test abort(string: msg);
function int: foo() = let {
  constraint abort("This is an abort message");
} in 1;
any: x = foo();
			"#;
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let program = shackle_ir::Program::lower_from_mir(&db);
		let mut interpreter = Interpreter::with_output(&program, MemoryStreams::default());
		let error = interpreter.run().unwrap_err();
		assert_eq!(
			error.kind,
			RuntimeErrorKind::Abort("This is an abort message".into())
		);
		assert_eq!(interpreter.state, InterpreterState::Aborted);
		assert_eq!(
			error
				.stack_trace
				.iter()
				.map(ToString::to_string)
				.collect::<Vec<_>>(),
			vec!["<unnamed file>:4.14-47", "<unnamed file>:6.10-15"]
		);
		assert_eq!(
			interpreter.run().unwrap_err().kind,
			RuntimeErrorKind::AlreadyAborted
		);
		assert_eq!(
			interpreter.step().unwrap_err().kind,
			RuntimeErrorKind::AlreadyAborted
		);
		assert_eq!(
			interpreter
				.eval(program.main_entry_point, &[])
				.unwrap_err()
				.kind,
			RuntimeErrorKind::AlreadyAborted
		);
		expect![[r#"
    MemoryStreams {
        stdout: "",
        stderr: "",
    }
"#]]
		.assert_debug_eq(&interpreter.output());
	}

	#[test]
	fn eval_failure_aborts_interpreter() {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let source = r#"annotation mzn_forward_annotations;
 test abort(string: msg);
 function int: foo() = let { constraint abort("failed"); } in 1;
 any: x = foo();"#;
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let program = shackle_ir::Program::lower_from_mir(&db);
		let function = program
			.functions
			.iter()
			.find(|(_, f)| program.strings[f.name] == "foo")
			.unwrap()
			.0;
		let mut interpreter = Interpreter::with_output(&program, MemoryStreams::default());
		let error = interpreter.eval(function, &[]).unwrap_err();
		assert_eq!(error.kind, RuntimeErrorKind::Abort("failed".into()));
		assert_eq!(interpreter.state, InterpreterState::Aborted);
		assert_eq!(
			interpreter.eval(function, &[]).unwrap_err().kind,
			RuntimeErrorKind::AlreadyAborted
		);
	}

	#[test]
	fn builtin_errors_preserve_kind() {
		let program = value_test_program();
		let mut interpreter = Interpreter::new(&program);
		let error = crate::builtin::eval_builtin(
			&mut interpreter,
			shackle_ir::Builtin::DivInt {
				dividend: 1.into(),
				divisor: 0.into(),
			},
		)
		.unwrap_err();
		assert!(matches!(error, RuntimeErrorKind::Arithmetic(_)));
		let error = crate::builtin::eval_builtin(
			&mut interpreter,
			shackle_ir::Builtin::UniformInt {
				lower: 2.into(),
				upper: 1.into(),
			},
		)
		.unwrap_err();
		assert!(matches!(error, RuntimeErrorKind::InvalidDistribution(_)));
	}

	#[test]
	fn warning_uses_diagnostic_stream() {
		let program = value_test_program();
		let mut interpreter = Interpreter::with_output(&program, MemoryStreams::default());
		let result = crate::builtin::eval_builtin(
			&mut interpreter,
			shackle_ir::Builtin::Warn(crate::Value::from("careful")),
		);
		assert_eq!(result.unwrap(), true.into());
		expect![[r#"
    MemoryStreams {
        stdout: "",
        stderr: "Warning: careful\n",
    }
"#]]
		.assert_debug_eq(&interpreter.output());
	}

	#[test]
	fn test_interpreter_solve_item() {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let source = r#"
			annotation 'output';
			annotation foo(var int: x);
			var int: x;
			solve :: foo(x) minimize x;
			"#;
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let program = shackle_ir::Program::lower_from_mir(&db);
		let mut interpreter = Interpreter::new(&program);
		interpreter.run().unwrap();
		expect![[r#"
    var int: _X_INTRODUCED_0;

    solve :: foo(_X_INTRODUCED_0) minimize _X_INTRODUCED_0;
"#]]
		.assert_eq(&interpreter.nanozinc_mut().print(&program));
	}
}
