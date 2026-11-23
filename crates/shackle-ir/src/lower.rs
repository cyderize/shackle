//! Lowering of MIR to IR

use std::num::NonZeroU32;

use shackle_hir::{Db, constants::IdentifierRegistry};
use shackle_mir::{
	FreshVariable,
	liveness::{Liveness, SlotAllocator},
};
use shackle_utils::{
	InternedString,
	arena::{Arena, ArenaMap},
	hash::Map,
};

use crate::{
	Annotation, AnnotationCall, AnnotationId, Array, Call, CallType, Expression, Function,
	FunctionId, Identifier, Instruction, InstructionId, Literal, Origin, Program, Set, SlotId,
	StringId, Tuple, TupleAccess, Value,
};

/// Lower a model to IR
pub fn lower_model(db: &dyn Db, model: &shackle_mir::Model) -> Program {
	let mut ctx = LoweringContext {
		db,
		ids: IdentifierRegistry::lookup(db),
		mir: model,
		functions: Arena::new(),
		function_map: ArenaMap::new(),
		annotations: Arena::new(),
		annotation_map: ArenaMap::new(),
		global_slots: ArenaMap::new(),
		strings: Arena::new(),
		string_map: Map::new(),
		file_map: Map::new(),
	};

	for (i, function) in model.functions.iter() {
		let name = ctx.alloc_string(function.name);
		let idx = ctx.functions.insert(Function {
			name,
			origins: ArenaMap::new(),
			slot_count: 0,
			promise_total: function
				.annotations
				.iter()
				.any(|ann| ctx.mir.annotations[*ann].name == ctx.ids.annotations.promise_total),
			body: Arena::new(),
		});
		ctx.function_map.insert(i, idx);
	}

	for (i, annotation) in model.annotations.iter() {
		let name = ctx.alloc_string(annotation.name);
		let idx = ctx.annotations.insert(Annotation { name });
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
				temporary_slot_count: slot_count,
			};
			collector.lower_let(Destination::Return, body);
			let f = collector.parent.function_map[idx];
			collector.parent.functions[f].body = collector.instructions;
			collector.parent.functions[f].origins = collector.origins;
			collector.parent.functions[f].slot_count = collector.temporary_slot_count as usize;
		}
	}

	let mut inputs: Vec<_> = model
		.inputs
		.iter()
		.map(|id| {
			let name = model.global_vars[*id]
				.name
				.expect("input parameter must have a name");
			crate::Input {
				name: name.0.lookup(db).to_owned(),
				slot: ctx.global_slots[*id],
			}
		})
		.collect();
	inputs.sort_by(|a, b| a.name.cmp(&b.name));
	assert!(
		inputs.windows(2).all(|pair| pair[0].name != pair[1].name),
		"duplicate input name"
	);
	Program {
		inputs,
		global_slot_count: global_slot_count as usize,
		functions: ctx.functions,
		annotations: ctx.annotations,
		main_entry_point: ctx.function_map[model.main_entrypoint],
		output_entry_point: ctx.function_map[model.output_entrypoint],
		strings: ctx.strings,
	}
}

struct LoweringContext<'db> {
	db: &'db dyn Db,
	ids: &'db IdentifierRegistry<'db>,
	mir: &'db shackle_mir::Model<'db>,
	functions: Arena<Function>,
	function_map: ArenaMap<shackle_mir::Function<'db>, FunctionId>,
	annotations: Arena<Annotation>,
	annotation_map: ArenaMap<shackle_mir::Annotation<'db>, AnnotationId>,
	global_slots: ArenaMap<shackle_mir::GlobalVar<'db>, SlotId>,
	strings: Arena<String>,
	string_map: Map<InternedString<'db>, StringId>,
	file_map: Map<String, StringId>,
}

impl<'db> LoweringContext<'db> {
	fn alloc_string(&mut self, name: impl Into<InternedString<'db>>) -> StringId {
		let s = name.into();
		*self
			.string_map
			.entry(s)
			.or_insert_with(|| self.strings.insert(s.lookup(self.db)))
	}
}

struct FunctionLowerer<'a, 'db> {
	parent: &'a mut LoweringContext<'db>,
	instructions: Arena<Instruction>,
	origins: ArenaMap<Instruction, Origin>,
	function: &'a shackle_mir::Function<'db>,
	slots: ArenaMap<shackle_mir::Declaration<'db>, SlotId>,
	parameter_slots: ArenaMap<shackle_mir::Parameter<'db>, SlotId>,
	temporary_slot_count: u32,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Destination {
	Slot { slot: SlotId, output: bool },
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
						Destination::Slot {
							slot: self.parent.global_slots[global_assignment.global],
							output: self.parent.mir.global_vars[global_assignment.global].output,
						},
						&global_assignment.definition,
					);
				}
			}
		}
		match &l.result {
			shackle_mir::LetResult::Expression(e) => {
				self.lower_expression(destination, e);
			}
			shackle_mir::LetResult::Root => match destination {
				Destination::Return => {
					let _ = self.add_instruction(Instruction::ReturnRoot, l.origin);
				}
				Destination::Constraint => {}
				Destination::Slot { slot, output } => {
					let _ = self.add_instruction(
						Instruction::Assign {
							slot,
							expression: Expression::Value(Value::Literal(Literal::Boolean(
								shackle_hir::BooleanLiteral(true),
							))),
							output,
						},
						l.origin,
					);
				}
			},
		}
	}

	fn lower_declaration(&mut self, d: shackle_mir::DeclarationId<'db>) {
		self.lower_expression(
			Destination::Slot {
				slot: self.slots[d],
				output: false,
			},
			&self.function.data[d].definition,
		);
	}

	fn lower_constraint(&mut self, constraint: &shackle_mir::Constraint<'db>) {
		self.lower_expression(Destination::Constraint, &constraint.expression);
	}

	fn lower_expression(&mut self, destination: Destination, expr: &shackle_mir::Expression<'db>) {
		let expression = match &expr.data {
			shackle_mir::ExpressionData::Call(call) => {
				let function = self.parent.function_map[call.function];
				let mir_fn = &self.parent.mir.functions[call.function];
				let kind = if mir_fn.body.is_some() {
					CallType::Function
				} else {
					CallType::Predicate
				};
				let arguments = call
					.arguments
					.iter()
					.map(|arg| self.lower_value(&arg.data))
					.collect();
				let mut forward_annotations = false;
				let annotations = call
					.annotations
					.iter()
					.filter_map(|ann| match &ann {
						shackle_mir::Identifier::Annotation(a) => {
							if self.parent.mir.annotations[*a].name
								== self.parent.ids.annotations.mzn_forward_annotations
							{
								forward_annotations = true;
								None
							} else {
								Some(self.parent.annotation_map[*a].into())
							}
						}
						shackle_mir::Identifier::Declaration(d) => {
							Some(Identifier::Slot(self.slots[*d]))
						}
						shackle_mir::Identifier::GlobalVar(g) => {
							Some(Identifier::Slot(self.parent.global_slots[*g]))
						}
						shackle_mir::Identifier::Parameter(p) => {
							Some(Identifier::Slot(self.parameter_slots[*p]))
						}
					})
					.collect();
				Expression::Call(Call {
					function,
					kind,
					arguments,
					annotations,
					forward_annotations,
				})
			}
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
			shackle_mir::ExpressionData::FreshVariable(fv) => Expression::FreshVariable(match fv {
				FreshVariable::VarBool { is_defined } => FreshVariable::VarBool {
					is_defined: *is_defined,
				},
				FreshVariable::VarInt { domain, is_defined } => FreshVariable::VarInt {
					domain: self.lower_value(&domain.data),
					is_defined: *is_defined,
				},
				FreshVariable::VarFloat { domain, is_defined } => FreshVariable::VarFloat {
					domain: self.lower_value(&domain.data),
					is_defined: *is_defined,
				},
				FreshVariable::VarSetOfInt { domain, is_defined } => FreshVariable::VarSetOfInt {
					domain: self.lower_value(&domain.data),
					is_defined: *is_defined,
				},
			}),
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
							let iterator_value = self.lower_value(iter);
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
								last_iteration_idx = first_iteration.unwrap().0;
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
				let _ = self.add_instruction(
					Instruction::Jump {
						target: last_iteration_idx,
					},
					expr.origin,
				);
				let dest = match destination {
					Destination::Slot { slot, .. } => slot,
					Destination::Return => {
						self.temporary_slot_count += 1;
						SlotId::Relative(NonZeroU32::new(self.temporary_slot_count).unwrap())
					}
					Destination::Constraint => {
						unreachable!("a comprehension cannot be a constraint")
					}
				};
				let done =
					self.add_instruction(Instruction::ComprehensionEnd { slot: dest }, expr.origin);
				if matches!(destination, Destination::Return) {
					let _ = self.add_instruction(
						Instruction::Return {
							expression: Expression::Value(Value::Literal(Literal::Identifier(
								Identifier::Slot(dest),
							))),
						},
						expr.origin,
					);
				}
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
				Destination::Slot { slot, output } => Instruction::Assign {
					slot,
					expression,
					output,
				},
				Destination::Constraint => Instruction::Constraint { expression },
				Destination::Return => Instruction::Return { expression },
			},
			expr.origin,
		);
	}

	fn lower_value(&mut self, value: &shackle_mir::ValueData<'db>) -> Value {
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

	fn lower_literal(&mut self, literal: &shackle_mir::LiteralData<'db>) -> Literal {
		match literal {
			shackle_mir::LiteralData::Integer(integer_literal) => {
				Literal::Integer(*integer_literal)
			}
			shackle_mir::LiteralData::Float(float_literal) => Literal::Float(*float_literal),
			shackle_mir::LiteralData::Boolean(boolean_literal) => {
				Literal::Boolean(*boolean_literal)
			}
			shackle_mir::LiteralData::String(shackle_hir::StringLiteral(s)) => {
				Literal::String(self.parent.alloc_string(*s))
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
		instruction: Instruction,
		origin: shackle_thir::source::Origin<'db>,
	) -> InstructionId {
		let idx = self.instructions.insert(instruction);
		let name = origin.name(self.parent.db);
		let file = if let Some(idx) = self.parent.file_map.get(&name) {
			*idx
		} else {
			let idx = self.parent.strings.insert(name.clone());
			let _ = self.parent.file_map.insert(name, idx);
			idx
		};
		let span = origin.line_char(self.parent.db);
		self.origins.insert(idx, Origin { file, span });
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
		let ir = lower_model(&db, &mir);
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
        len: 3,
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
                        kind: Function,
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
                        annotations: [],
                        forward_annotations: false,
                    },
                ),
            },
            <Instruction::3>: ReturnRoot,
        },
    }
"#]],
		);
	}

	#[test]
	fn lowers_if_then_else_to_jumps() {
		check(
			r#"
				bool: mzn_root = true;
				function bool: condition() = true;
				int: value = if condition() then 1 else 2 endif;
			"#,
			expect![[r#"
    Arena {
        len: 7,
        data: {
            <Instruction::1>: Assign {
                slot: Relative(
                    1,
                ),
                expression: Value(
                    Literal(
                        Boolean(
                            BooleanLiteral(
                                true,
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
                        kind: Function,
                        arguments: [],
                        annotations: [],
                        forward_annotations: false,
                    },
                ),
            },
            <Instruction::3>: JumpIfFalse {
                condition: Literal(
                    Identifier(
                        Slot(
                            Relative(
                                1,
                            ),
                        ),
                    ),
                ),
                target: <Instruction::6>,
            },
            <Instruction::4>: Assign {
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
            <Instruction::5>: Jump {
                target: <Instruction::7>,
            },
            <Instruction::6>: Assign {
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
            <Instruction::7>: ReturnRoot,
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
        len: 7,
        data: {
            <Instruction::1>: ComprehensionStart,
            <Instruction::2>: IteratorStart {
                value: Set(
                    Set {
                        kind: Integer,
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
            <Instruction::7>: ReturnRoot,
        },
    }
"#]],
		);
	}
}
