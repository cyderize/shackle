//! Lowering of MIR to IR

use std::num::NonZeroU32;

use shackle_mir::liveness::{Liveness, SlotAllocator};
use shackle_utils::arena::{Arena, ArenaMap};

use crate::{
	Annotation, AnnotationCall, AnnotationId, Array, Call, Expression, Function, FunctionId,
	Identifier, Instruction, InstructionId, Literal, Program, Set, SlotId, Tuple, TupleAccess,
	Value,
};

/// Lower a model to IR
pub fn lower_model<'db>(model: &shackle_mir::Model<'db>) -> Program<'db> {
	let mut ctx = LoweringContext {
		functions: Arena::new(),
		function_map: ArenaMap::new(),
		annotations: Arena::new(),
		annotation_map: ArenaMap::new(),
		global_slots: ArenaMap::new(),
	};

	for (i, function) in model.functions.iter() {
		let idx = ctx.functions.insert(Function {
			name: function.name.clone(),
			origins: ArenaMap::new(),
			slot_count: 0,
			body: Arena::new(),
		});
		ctx.function_map.insert(i, idx);
	}

	for (i, annotation) in model.annotations.iter() {
		let idx = ctx.annotations.insert(Annotation {
			name: annotation.name,
		});
		ctx.annotation_map.insert(i, idx);
	}

	let global_slot_count = model.global_vars.len();
	for (i, _) in model.global_vars.iter() {
		ctx.global_slots
			.insert(i, SlotId::Absolute(NonZeroU32::new(i.into()).unwrap()));
	}

	for (idx, function) in model.functions.iter() {
		if let Some(body) = function.body.as_ref() {
			let interferences = Liveness::new(function).analyse();
			let slot_assignments = SlotAllocator::new(function, &interferences).allocate_slots();
			let mut slot_count = function.parameters.len() as u32;
			let slots = slot_assignments
				.into_iter()
				.map(|(d, s)| {
					slot_count = slot_count.max(s.into_raw());
					(d, SlotId::Relative(NonZeroU32::new(s.into_raw()).unwrap()))
				})
				.collect();
			let parameter_slots = function
				.parameters
				.iter()
				.map(|p| (*p, SlotId::Relative(NonZeroU32::new((*p).into()).unwrap())))
				.collect();
			let mut collector = FunctionLowerer {
				function,
				parent: &mut ctx,
				instructions: Arena::new(),
				origins: ArenaMap::new(),
				slots,
				parameter_slots,
			};
			collector.lower_let(Destination::Return, body);
			let f = collector.parent.function_map[idx];
			collector.parent.functions[f].body = collector.instructions;
			collector.parent.functions[f].origins = collector.origins;
			collector.parent.functions[f].slot_count = slot_count as usize;
		}
	}

	Program {
		global_slot_count: global_slot_count as usize,
		functions: ctx.functions,
		annotations: ctx.annotations,
		main_entry_point: ctx.function_map[model.main_entrypoint],
		output_entry_point: ctx.function_map[model.output_entrypoint],
	}
}

struct LoweringContext<'db> {
	functions: Arena<Function<'db>>,
	function_map: ArenaMap<shackle_mir::Function<'db>, FunctionId<'db>>,
	annotations: Arena<Annotation<'db>>,
	annotation_map: ArenaMap<shackle_mir::Annotation<'db>, AnnotationId<'db>>,
	global_slots: ArenaMap<shackle_mir::GlobalVar<'db>, SlotId>,
}

struct FunctionLowerer<'a, 'db> {
	parent: &'a mut LoweringContext<'db>,
	instructions: Arena<Instruction<'db>>,
	origins: ArenaMap<Instruction<'db>, shackle_thir::source::Origin<'db>>,
	function: &'a shackle_mir::Function<'db>,
	slots: ArenaMap<shackle_mir::Declaration<'db>, SlotId>,
	parameter_slots: ArenaMap<shackle_mir::Parameter<'db>, SlotId>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Destination {
	Slot(SlotId),
	Constraint,
	Return,
}

impl<'a, 'db> FunctionLowerer<'a, 'db> {
	fn lower_let(&mut self, destination: Destination, l: &shackle_mir::Let<'db>) {
		for item in l.items.iter() {
			match item {
				shackle_mir::LetItem::Constraint(constraint) => {
					self.lower_constraint(constraint);
				}
				shackle_mir::LetItem::Declaration(d) => {
					self.lower_declaration(*d);
				}
				shackle_mir::LetItem::GlobalAssignment(global_assignment) => {
					self.lower_expression(
						Destination::Slot(self.parent.global_slots[global_assignment.global]),
						&global_assignment.definition,
					);
				}
			}
		}
		if let shackle_mir::LetResult::Expression(e) = &l.result {
			self.lower_expression(destination, e);
		} else if destination == Destination::Return {
			let _ = self.add_instruction(Instruction::ReturnRoot, l.origin);
		}
	}

	fn lower_declaration(&mut self, d: shackle_mir::DeclarationId<'db>) {
		self.lower_expression(
			Destination::Slot(self.slots[d]),
			&self.function.data[d].definition,
		);
	}

	fn lower_constraint(&mut self, constraint: &shackle_mir::Constraint<'db>) {
		self.lower_expression(Destination::Constraint, &constraint.expression);
	}

	fn lower_expression(&mut self, destination: Destination, expr: &shackle_mir::Expression<'db>) {
		let expression = match &expr.data {
			shackle_mir::ExpressionData::Call(call) => Expression::Call(Call {
				function: self.parent.function_map[call.function],
				arguments: call
					.arguments
					.iter()
					.map(|arg| self.lower_value(&arg.data))
					.collect(),
			}),
			shackle_mir::ExpressionData::AnnotationCall(call) => {
				Expression::AnnotationCall(AnnotationCall {
					annotation: self.parent.annotation_map[call.annotation],
					arguments: call
						.arguments
						.iter()
						.map(|arg| self.lower_value(&arg.data))
						.collect(),
				})
			}
			shackle_mir::ExpressionData::Builtin(builtin) => {
				Expression::Builtin(builtin.map(|arg| Box::new(self.lower_value(&arg.data))))
			}
			shackle_mir::ExpressionData::IfThenElse(if_then_else) => {
				let condition = self.lower_value(&if_then_else.condition.data);
				let condition_idx =
					self.add_instruction(Instruction::NoOp, if_then_else.condition.origin);
				self.lower_let(destination, &if_then_else.then);
				let then_end_idx =
					self.add_instruction(Instruction::NoOp, if_then_else.then.origin);
				let else_start_idx = self.instructions.next_index();
				self.lower_let(destination, &if_then_else.else_expression);
				self.instructions[condition_idx] = Instruction::JumpIfFalse {
					condition,
					target: else_start_idx,
				};
				self.instructions[then_end_idx] = Instruction::Jump {
					target: self.instructions.next_index(),
				};
				return;
			}
			shackle_mir::ExpressionData::Comprehension(comprehension) => {
				let _ = self.add_instruction(Instruction::ComprehensionStart, expr.origin);
				let mut first_iteration = None;
				let mut last_iteration_idx = self.instructions.next_index();
				for g in comprehension.generators.iter() {
					match g {
						shackle_mir::Generator::Iterator { element } => {
							let shackle_mir::ExpressionData::Iterator(iter) =
								&self.function.data[*element].definition.data
							else {
								unreachable!()
							};
							let iterator_value = self.lower_value(&iter);
							let _ = self.add_instruction(
								Instruction::IteratorStart {
									value: iterator_value,
								},
								self.function.data[*element].definition.origin,
							);
							if first_iteration.is_some() {
								last_iteration_idx = self.add_instruction(
									Instruction::IteratorNext {
										slot: self.slots[*element],
										done: last_iteration_idx,
									},
									self.function.data[*element].definition.origin,
								);
							} else {
								first_iteration = Some((
									self.add_instruction(
										Instruction::NoOp,
										self.function.data[*element].definition.origin,
									),
									self.slots[*element],
								));
							}
						}
						shackle_mir::Generator::Assignment { declaration } => {
							self.lower_declaration(*declaration);
						}
						shackle_mir::Generator::Constraint(constraint) => {
							self.lower_constraint(constraint);
						}
						shackle_mir::Generator::Where(w) => {
							let condition = self.lower_value(&w.data);
							let _ = self.add_instruction(
								Instruction::JumpIfFalse {
									condition,
									target: last_iteration_idx,
								},
								w.origin,
							);
						}
					}
				}
				let template = self.lower_value(&comprehension.template.data);
				let _ = self.add_instruction(
					Instruction::ComprehensionAppend { value: template },
					comprehension.template.origin,
				);
				let (first_idx, first_slot) = first_iteration.unwrap();
				let _ = self.add_instruction(Instruction::Jump { target: first_idx }, expr.origin);
				let Destination::Slot(dest) = destination else {
					unreachable!()
				};
				let done =
					self.add_instruction(Instruction::ComprehensionEnd { slot: dest }, expr.origin);
				self.instructions[first_idx] = Instruction::IteratorNext {
					slot: first_slot,
					done,
				};
				return;
			}
			shackle_mir::ExpressionData::Iterator(_) => unreachable!(),
			shackle_mir::ExpressionData::Value(value_data) => {
				Expression::Value(self.lower_value(value_data))
			}
		};

		let _ = self.add_instruction(
			match destination {
				Destination::Slot(slot) => Instruction::Assign { slot, expression },
				Destination::Constraint => Instruction::Constraint { expression },
				Destination::Return => Instruction::Return { expression },
			},
			expr.origin,
		);
	}

	fn lower_value(&mut self, value: &shackle_mir::ValueData<'db>) -> Value<'db> {
		match value {
			shackle_mir::ValueData::Literal(literal_data) => {
				Value::Literal(self.lower_literal(literal_data))
			}
			shackle_mir::ValueData::Tuple(tuple) => Value::Tuple(Tuple {
				members: tuple
					.members
					.iter()
					.map(|l| self.lower_literal(&l.data))
					.collect(),
			}),
			shackle_mir::ValueData::Set(set) => Value::Set(Set {
				kind: set.kind,
				members: set
					.members
					.iter()
					.map(|l| self.lower_literal(&l.data))
					.collect(),
			}),
			shackle_mir::ValueData::Array(array) => Value::Array(Array {
				members: array
					.members
					.iter()
					.map(|l| self.lower_literal(&l.data))
					.collect(),
			}),
			shackle_mir::ValueData::TupleAccess(ta) => Value::TupleAccess(TupleAccess {
				tuple: match ta.tuple {
					shackle_mir::Identifier::Declaration(d) => Identifier::Slot(self.slots[d]),
					shackle_mir::Identifier::Parameter(p) => {
						Identifier::Slot(self.parameter_slots[p])
					}
					shackle_mir::Identifier::GlobalVar(g) => {
						Identifier::Slot(self.parent.global_slots[g])
					}
					shackle_mir::Identifier::Annotation(_) => unreachable!(),
				},
				field: ta.field,
			}),
		}
	}

	fn lower_literal(&mut self, literal: &shackle_mir::LiteralData<'db>) -> Literal<'db> {
		match literal {
			shackle_mir::LiteralData::Integer(integer_literal) => {
				Literal::Integer(*integer_literal)
			}
			shackle_mir::LiteralData::Float(float_literal) => Literal::Float(*float_literal),
			shackle_mir::LiteralData::Boolean(boolean_literal) => {
				Literal::Boolean(*boolean_literal)
			}
			shackle_mir::LiteralData::String(string_literal) => {
				Literal::String(string_literal.clone())
			}
			shackle_mir::LiteralData::Infinity => Literal::Infinity,
			shackle_mir::LiteralData::InfiniteIntSet => Literal::InfiniteIntSet,
			shackle_mir::LiteralData::InfiniteFloatSet => Literal::InfiniteFloatSet,
			shackle_mir::LiteralData::Identifier(shackle_mir::Identifier::Annotation(a)) => {
				Literal::Identifier(Identifier::Annotation(self.parent.annotation_map[*a]))
			}
			shackle_mir::LiteralData::Identifier(shackle_mir::Identifier::Declaration(d)) => {
				Literal::Identifier(Identifier::Slot(self.slots[*d]))
			}
			shackle_mir::LiteralData::Identifier(shackle_mir::Identifier::GlobalVar(g)) => {
				Literal::Identifier(Identifier::Slot(self.parent.global_slots[*g]))
			}
			shackle_mir::LiteralData::Identifier(shackle_mir::Identifier::Parameter(p)) => {
				Literal::Identifier(Identifier::Slot(self.parameter_slots[*p]))
			}
		}
	}

	fn add_instruction(
		&mut self,
		instruction: Instruction<'db>,
		origin: shackle_thir::source::Origin<'db>,
	) -> InstructionId<'db> {
		let idx = self.instructions.insert(instruction);
		self.origins.insert(idx, origin);
		idx
	}
}

#[cfg(test)]
mod tests {
	use expect_test::{Expect, expect};
	use salsa::Setter;
	use shackle_hir::{
		CompilerDatabase,
		input::{CompilerSettings, InlineModelFile, InputFiles},
	};
	use shackle_syntax::InputLang;
	use shackle_thir::db::final_thir;

	use super::lower_model;

	/// Lower a MiniZinc snippet through THIR and MIR, then snapshot the main
	/// function's IR instructions. Keeping the stdlib out makes the instruction
	/// IDs in these small examples stable and easy to inspect.
	fn check(source: &str, expected: Expect) {
		let mut db = CompilerDatabase::default();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let model_file = InlineModelFile::new(&db, source.to_owned(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let thir = final_thir(&db).unwrap();
		let mir = shackle_mir::lower::lower_from_thir(&db, thir);
		let ir = lower_model(&mir);
		expected.assert_debug_eq(&ir.functions[ir.main_entry_point].body);
	}

	#[test]
	fn lowers_assignments_and_calls() {
		check(
			r#"
				function int: increment(int: x) = 2;
				int: x = 1;
				int: y = increment(x);
			"#,
			expect![[r#"
    Arena {
        len: 2,
        data: {
            <Instruction::1>: Assign {
                slot: Relative(
                    1,
                ),
                expression: Value(
                    Literal(
                        Integer(
                            IntegerLiteral(
                                1,
                            ),
                        ),
                    ),
                ),
            },
            <Instruction::2>: Assign {
                slot: Relative(
                    1,
                ),
                expression: Call(
                    Call {
                        function: <Function::1>,
                        arguments: [
                            Literal(
                                Identifier(
                                    Slot(
                                        Relative(
                                            1,
                                        ),
                                    ),
                                ),
                            ),
                        ],
                    },
                ),
            },
        },
    }
"#]],
		);
	}

	#[test]
	fn lowers_if_then_else_to_jumps() {
		check(
			r#"
				function bool: condition() = true;
				int: value = if condition() then 1 else 2 endif;
			"#,
			expect![[r#"
    Arena {
        len: 5,
        data: {
            <Instruction::1>: Assign {
                slot: Relative(
                    1,
                ),
                expression: Call(
                    Call {
                        function: <Function::1>,
                        arguments: [],
                    },
                ),
            },
            <Instruction::2>: JumpIfFalse {
                condition: Literal(
                    Identifier(
                        Slot(
                            Relative(
                                1,
                            ),
                        ),
                    ),
                ),
                target: <Instruction::5>,
            },
            <Instruction::3>: Assign {
                slot: Relative(
                    1,
                ),
                expression: Value(
                    Literal(
                        Integer(
                            IntegerLiteral(
                                1,
                            ),
                        ),
                    ),
                ),
            },
            <Instruction::4>: Jump {
                target: <Instruction::6>,
            },
            <Instruction::5>: Assign {
                slot: Relative(
                    1,
                ),
                expression: Value(
                    Literal(
                        Integer(
                            IntegerLiteral(
                                2,
                            ),
                        ),
                    ),
                ),
            },
        },
    }
"#]],
		);
	}

	#[test]
	fn lowers_comprehensions_to_iterator_instructions() {
		check(
			r#"
				array [int] of int: values = [i | i in {1, 2}];
			"#,
			expect![[r#"
    Arena {
        len: 6,
        data: {
            <Instruction::1>: ComprehensionStart,
            <Instruction::2>: IteratorStart {
                value: Set(
                    Set {
                        members: [
                            Integer(
                                IntegerLiteral(
                                    1,
                                ),
                            ),
                            Integer(
                                IntegerLiteral(
                                    2,
                                ),
                            ),
                        ],
                    },
                ),
            },
            <Instruction::3>: IteratorNext {
                slot: Relative(
                    2,
                ),
                done: <Instruction::6>,
            },
            <Instruction::4>: ComprehensionAppend {
                value: Literal(
                    Identifier(
                        Slot(
                            Relative(
                                2,
                            ),
                        ),
                    ),
                ),
            },
            <Instruction::5>: Jump {
                target: <Instruction::3>,
            },
            <Instruction::6>: ComprehensionEnd {
                slot: Relative(
                    1,
                ),
            },
        },
    }
"#]],
		);
	}
}
