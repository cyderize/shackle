//! Shackle runtime
pub mod builtin;
pub mod comprehension;
pub mod error;
pub mod nanozinc;
pub mod stack;
pub mod value;
use std::num::NonZeroU32;

use shackle_hir::Db;
use shackle_ir::{FunctionId, Instruction, InstructionId, Program};
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
pub struct Interpreter<'a, 'db> {
	db: &'db dyn Db,
	program: &'a Program<'db>,
	pc: Address<'db>,
	state: InterpreterState,
	stack: Stack<'db>,
	comprehensions: ComprehensionState,
	nanozinc: NanoZinc<'db>,
}

impl<'a, 'db> Interpreter<'a, 'db> {
	/// Create a new interpreter for the given program
	pub fn new(db: &'db dyn Db, program: &'a Program<'db>) -> Self {
		Self {
			db,
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
		}
	}

	/// Get a reference to the NanoZinc model
	pub fn nanozinc(&self) -> &NanoZinc<'db> {
		&self.nanozinc
	}

	/// Get a mutable reference to the NanoZinc model
	pub fn nanozinc_mut(&mut self) -> &mut NanoZinc<'db> {
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

	/// Get the values defined in the current call frame
	pub fn values_defined(&self) -> &[Value] {
		self.stack.values_defined()
	}

	/// Add a value to the list of values defined in the current call frame
	pub fn add_value_defined(&mut self, value: Value) {
		self.stack.add_value_defined(value);
	}

	/// Create a new boolean variable
	pub fn new_var_bool(&mut self) -> Value {
		let var = self.nanozinc_mut().add_var_bool();
		self.stack.add_value_defined(var.clone());
		var
	}

	/// Create a new integer variable with the given domain
	pub fn new_var_int(&mut self, domain: Value) -> Value {
		let var = self.nanozinc_mut().add_var_int(domain);
		self.stack.add_value_defined(var.clone());
		var
	}

	/// Create a new float variable with the given domain
	pub fn new_var_float(&mut self, domain: Value) -> Value {
		let var = self.nanozinc_mut().add_var_float(domain);
		self.stack.add_value_defined(var.clone());
		var
	}

	/// Create a new set of integer variable with the given domain
	pub fn new_var_set_of_int(&mut self, domain: Value) -> Value {
		let var = self.nanozinc_mut().add_var_set_of_int(domain);
		self.stack.add_value_defined(var.clone());
		var
	}

	/// Run the interpreter until it halts
	pub fn run(&mut self) {
		assert_ne!(
			self.state,
			InterpreterState::Aborted,
			"Cannot run an aborted interpreter"
		);
		if self.stack.call_frame_count() == 0 {
			self.stack.push_call_frame(
				CallFrame {
					return_address: self.pc,
					return_slot: SlotIndex::first(),
					halt_on_return: true,
				},
				self.program.functions[self.pc.function].slot_count,
			);
		}
		if self.state == InterpreterState::Paused {
			self.state = InterpreterState::Running;
		}
		while self.state == InterpreterState::Running {
			self.step();
		}
	}

	/// Evaluate a function
	pub fn eval(&mut self, function: FunctionId<'db>, arguments: &[Value]) -> Value {
		let old_state = self.state;
		self.state = InterpreterState::Running;
		let old_pc = self.pc;
		let return_slot = SlotIndex::first();
		self.stack.push_call_frame(
			CallFrame {
				return_address: old_pc,
				return_slot,
				halt_on_return: true,
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
			self.step();
		}
		assert_eq!(self.pc, old_pc);
		self.state = old_state;
		self.stack[return_slot].clone()
	}

	/// Step the interpreter forward one instruction
	pub fn step(&mut self) {
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
			} => self.stack[*slot] = self.eval_builtin(b),
			Instruction::Assign {
				slot,
				expression: shackle_ir::Expression::Call(c),
			} => {
				assert!(
					!self.program.functions[c.function].body.is_empty(),
					"Call assigned to slot must have a body"
				);
				self.stack.push_call_frame(
					CallFrame {
						return_address: self.pc,
						return_slot: *slot,
						halt_on_return: false,
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
			} if self.program.functions[c.function].body.is_empty() => {
				let args = c
					.arguments
					.iter()
					.map(|arg| self.eval_value(arg))
					.collect::<Vec<_>>();
				if self.is_entry_point() {
					self.nanozinc_mut().add_constraint(c.function, args);
				} else {
					let defined_vars = self.values_defined().to_vec();
					for variable in defined_vars {
						self.nanozinc_mut().add_defining_constraint(
							c.function,
							args.iter().cloned(),
							variable.clone(),
						);
					}
				}
			}
			Instruction::Constraint {
				expression: shackle_ir::Expression::Call(c),
			} => {
				self.stack.push_call_frame(
					CallFrame {
						return_address: self.pc,
						return_slot: SlotIndex::first(),
						halt_on_return: false,
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
				if frame.halt_on_return {
					self.state = InterpreterState::Paused;
				}
			}
			Instruction::Return {
				expression: shackle_ir::Expression::Builtin(b),
			} => {
				let value = self.eval_builtin(b);
				let frame = self.stack.pop_call_frame();
				self.pc = frame.return_address;
				self.stack[frame.return_slot] = value;
				if frame.halt_on_return {
					self.state = InterpreterState::Paused;
				}
			}
			Instruction::Return {
				expression: shackle_ir::Expression::Call(c),
			} => {
				self.stack
					.tail_call(self.program.functions[c.function].slot_count);
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
	}

	pub(crate) fn abort(&mut self, reason: &str) {
		println!("{}", reason);
		println!("{}", self.stack.dump(self.db, self.program));
		self.state = InterpreterState::Aborted;
	}

	fn eval_value(&self, value: &shackle_ir::Value<'db>) -> Value {
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
		builtin::eval_builtin(self, builtin.map(|value| self.eval_value(value)))
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

	use crate::{DataView, Interpreter};

	#[test]
	fn test_interpreter_1() {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let source = r#"
			annotation output;
			function int: '+'(int: a, int: b);
			function int: foo() = 123 + 456;
			int: x :: output = foo();
			"#;
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let program = shackle_ir::Program::lower_from_mir(&db);
		let function = program.functions.keys().next().unwrap();
		let mut interpreter = Interpreter::new(&db, &program);
		let result = interpreter.eval(function, &[]);
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
		let mut interpreter = Interpreter::new(&db, &program);
		interpreter.run();
		expect![[r#"
    var 1..1 ∪ 3..3 ∪ 5..5: _X_INTRODUCED_0;
    constraint foo(_X_INTRODUCED_0);
"#]]
		.assert_eq(&interpreter.nanozinc_mut().print(&db, &program));
	}

	#[test]
	fn test_interpreter_3() {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let source = r#"
			predicate foo(var int: x, var int: y);
			function var int: foo_t(var int: x) :: promise_total = let {
				var int: y;
				constraint foo(x, y);
			} in y;
			test abort(string);
			annotation promise_total;
			annotation output;
			var {1, 3, 5}: x;
			var int: y :: output = foo_t(x);
			"#;
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let program = shackle_ir::Program::lower_from_mir(&db);
		let mut interpreter = Interpreter::new(&db, &program);
		interpreter.run();
		expect![[r#"
    var 1..1 ∪ 3..3 ∪ 5..5: _X_INTRODUCED_0;
    var int: _X_INTRODUCED_1;
    constraint foo(_X_INTRODUCED_0, _X_INTRODUCED_1);
"#]]
		.assert_eq(&interpreter.nanozinc_mut().print(&db, &program));
	}
}
