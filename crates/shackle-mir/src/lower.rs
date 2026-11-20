//! Lowering from THIR to MIR

use std::ops::{Deref, DerefMut};

use derive_more::From;
use shackle_hir::{Db, constants::IdentifierRegistry};
use shackle_thir::{analyse::GlobalVariables, source::Origin};
use shackle_ty::{Ty, registry::TypeRegistry};
use shackle_utils::{arena::Arena, hash::Map};

use crate::{
	Annotation, AnnotationCall, AnnotationId, Array, Builtin, Call, Comprehension, Constraint,
	Declaration, Expression, ExpressionData, FreshVariable, Function, FunctionData, FunctionId,
	Generator, GlobalAssignment, GlobalVar, Identifier, IfThenElse, Let, LetItem, LetResult,
	Literal, LiteralData, Model, Parameter, Set, SetType, Tuple, TupleAccess, Value, ValueData,
};

struct LoweringContext<'db> {
	db: &'db dyn Db,
	model: &'db shackle_thir::Model<'db>,
	ids: &'db IdentifierRegistry<'db>,
	tys: &'db TypeRegistry<'db>,
	annotations: Arena<Annotation<'db>>,
	functions: Arena<Function<'db>>,
	global_vars: Arena<GlobalVar<'db>>,
	annotation_ids: Map<shackle_thir::AnnotationId<'db>, AnnotationId<'db>>,
	function_ids: Map<shackle_thir::FunctionId<'db>, FunctionId<'db>>,
	global_resolutions: Map<shackle_thir::ResolvedIdentifier<'db>, Identifier<'db>>,
}

impl<'db> LoweringContext<'db> {
	fn new(db: &'db dyn Db, model: &'db shackle_thir::Model<'db>) -> Self {
		let ids = IdentifierRegistry::lookup(db);
		let tys = TypeRegistry::lookup(db);
		Self {
			db,
			model,
			ids,
			tys,
			annotations: Arena::default(),
			functions: Arena::default(),
			global_vars: Arena::default(),
			annotation_ids: Map::default(),
			function_ids: Map::default(),
			global_resolutions: Map::default(),
		}
	}

	fn lower_annotation(&mut self, annotation: shackle_thir::AnnotationId<'db>) {
		let mut parameters = Arena::with_capacity(
			self.model[annotation]
				.parameters
				.as_ref()
				.map(|p| p.len() as u32)
				.unwrap_or(0),
		);
		if let Some(params) = &self.model[annotation].parameters {
			for p in params.iter() {
				let param = &self.model[*p];
				let _ = parameters.insert(Parameter {
					ty: param.ty(),
					// TODO: lower annotations
					annotations: Vec::new(),
					name: param.name(),
				});
			}
		}
		let idx = self.annotations.insert(Annotation {
			name: self.model[annotation].name.unwrap(),
			parameters,
			origin: self.model[annotation].origin(),
		});
		let _ = self.annotation_ids.insert(annotation, idx);
	}

	fn lower_function_signature(&mut self, function: shackle_thir::FunctionId<'db>) {
		if self.model[function].body().is_some()
			|| (self.model[function].return_type() == self.tys.var_bool
				&& !self.model[function].is_polymorphic())
		{
			let mut params = Vec::with_capacity(self.model[function].parameters().len());
			let mut data = FunctionData::default();
			for p in self.model[function].parameters().iter() {
				let param = &self.model[*p];
				let idx = data.parameters.insert(Parameter {
					ty: param.ty(),
					annotations: param
						.annotations()
						.iter()
						.map(|ann| match &**ann {
							shackle_thir::ExpressionData::Identifier(
								shackle_thir::ResolvedIdentifier::Annotation(a),
							) => self.annotation_ids[a],
							_ => unreachable!(
								"Only identifier annotations are allowed on parameters"
							),
						})
						.collect(),
					name: param.name(),
				});
				params.push(idx);
			}
			let result = Function {
				name: self.model[function].mangled_name(self.db),
				ty: self.model[function].return_type(),
				annotations: self.model[function]
					.annotations()
					.iter()
					.map(|ann| match &**ann {
						shackle_thir::ExpressionData::Identifier(
							shackle_thir::ResolvedIdentifier::Annotation(a),
						) => self.annotation_ids[a],
						_ => unreachable!("Only identifier annotations are allowed on functions"),
					})
					.collect(),
				parameters: params,
				body: None,
				data,
				origin: self.model[function].origin(),
			};

			let idx = self.functions.insert(result);
			let _ = self.function_ids.insert(function, idx);
		}
	}

	fn lower_function_body(&mut self, function: shackle_thir::FunctionId<'db>) {
		if self.model[function].body().is_some() {
			let idx = self.function_ids[&function];
			let data = std::mem::take(&mut self.functions[idx].data);
			let resolutions = self.model[function]
				.parameters()
				.iter()
				.zip(self.functions[idx].parameters.iter())
				.map(|(param, target)| ((*param).into(), (*target).into()))
				.collect();
			let mut fn_collector = Collector::new_with_data(
				self,
				data,
				resolutions,
				self.model[function]
					.annotations()
					.has(self.model, self.ids.annotations.promise_total),
			);
			let mut items = LetItems::default();
			let lowered = fn_collector
				.lower_expression(fn_collector.model[function].body().unwrap(), &mut items);
			let body = items.into_let(lowered);
			let data = std::mem::take(&mut fn_collector.data);
			self.functions[idx].body = Some(body);
			self.functions[idx].data = data;
		}
	}
}

trait Items<'db> {
	fn push(&mut self, item: impl Into<LetItem<'db>>);
}

#[derive(Debug, Default)]
struct LetItems<'db> {
	items: Vec<LetItem<'db>>,
}

impl<'db> LetItems<'db> {
	fn into_let(self, result: impl Into<LoweredExpression<'db>>) -> Let<'db> {
		let value = result.into();
		let origin = value.origin();
		Let {
			result: if let LoweredExpression::Root(_, _) = &value {
				LetResult::Root
			} else {
				value.into_expression().into()
			},
			items: self.items,
			origin,
		}
	}
}

impl<'db> Items<'db> for LetItems<'db> {
	fn push(&mut self, item: impl Into<LetItem<'db>>) {
		self.items.push(item.into());
	}
}

#[derive(Debug, Default)]
struct Generators<'db> {
	generators: Vec<Generator<'db>>,
}

impl<'db> Items<'db> for Generators<'db> {
	fn push(&mut self, item: impl Into<LetItem<'db>>) {
		match item.into() {
			LetItem::Declaration(d) => self
				.generators
				.push(Generator::Assignment { declaration: d }),
			LetItem::Constraint(c) => self.generators.push(Generator::Constraint(c)),
			_ => unreachable!("Illegal item in generator"),
		}
	}
}

/// Context for collecting items/expressions
struct Collector<'a, 'db> {
	parent: &'a mut LoweringContext<'db>,
	data: FunctionData<'db>,
	resolutions: Map<shackle_thir::ResolvedIdentifier<'db>, Identifier<'db>>,
	is_promise_total: bool,
}

impl<'a, 'db> Deref for Collector<'a, 'db> {
	type Target = LoweringContext<'db>;

	fn deref(&self) -> &Self::Target {
		self.parent
	}
}

impl<'a, 'db> DerefMut for Collector<'a, 'db> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		self.parent
	}
}

impl<'a, 'db> Collector<'a, 'db> {
	fn new(parent: &'a mut LoweringContext<'db>) -> Self {
		Self {
			parent,
			data: FunctionData::default(),
			resolutions: Map::default(),
			is_promise_total: false,
		}
	}

	fn new_with_data(
		parent: &'a mut LoweringContext<'db>,
		data: FunctionData<'db>,
		resolutions: Map<shackle_thir::ResolvedIdentifier<'db>, Identifier<'db>>,
		is_promise_total: bool,
	) -> Self {
		Self {
			parent,
			data,
			resolutions,
			is_promise_total,
		}
	}

	fn lower_declaration(
		&mut self,
		declaration: shackle_thir::DeclarationId<'db>,
		destination: &mut impl Items<'db>,
	) {
		let ty = self.model[declaration].ty();
		let origin = self.model[declaration].origin();
		let mut annotations = self.model[declaration]
			.annotations()
			.iter()
			.map(|ann| {
				self.lower_expression(ann, destination)
					.into_identifier(self, destination)
			})
			.collect::<Vec<_>>();
		let global_res = self.global_resolutions.get(&declaration.into()).copied();
		if let Some(g) = global_res {
			// Place annotations on the global var
			for ann in annotations.drain(..) {
				if matches!(ann, Identifier::Annotation(_)) {
					// Already added directly to global var
					continue;
				}
				let constraint = Constraint {
					expression: Box::new(Expression::new(
						Builtin::Annotate {
							variable: Box::new(Value::new(LiteralData::Identifier(g), ty, origin)),
							ann: Box::new(Value::new(
								LiteralData::Identifier(ann),
								self.tys.ann,
								origin,
							)),
						},
						self.tys.par_bool,
						origin,
					)),
					annotations: Vec::new(),
				};
				destination.push(constraint);
			}
		}

		let definition = if let Some(e) = self.model[declaration].definition() {
			let lowered = self.lower_expression(e, destination);
			lowered.into_expression()
		} else {
			// No RHS, so either a fresh var, or an input parameter
			let ty = self.model[declaration].ty();
			let origin = self.model[declaration].origin();
			if ty == self.tys.var_bool {
				Expression::new(
					FreshVariable::VarBool {
						is_defined: self.is_promise_total,
					},
					ty,
					self.model[declaration].origin(),
				)
			} else if ty == self.tys.var_int {
				Expression::new(
					FreshVariable::VarInt {
						domain: Box::new(match &**self.model[declaration].domain() {
							shackle_thir::DomainData::Bounded(b) => self
								.lower_expression(b, destination)
								.into_value(self, destination),
							shackle_thir::DomainData::Unbounded => {
								Value::new(LiteralData::InfiniteIntSet, self.tys.set_of_int, origin)
							}
							_ => unreachable!(),
						}),
						is_defined: self.is_promise_total,
					},
					ty,
					origin,
				)
			} else if ty == self.tys.var_float {
				Expression::new(
					FreshVariable::VarFloat {
						domain: Box::new(match &**self.model[declaration].domain() {
							shackle_thir::DomainData::Bounded(b) => self
								.lower_expression(b, destination)
								.into_value(self, destination),
							shackle_thir::DomainData::Unbounded => Value::new(
								LiteralData::InfiniteFloatSet,
								self.tys.set_of_float,
								origin,
							),
							_ => unreachable!(),
						}),
						is_defined: self.is_promise_total,
					},
					ty,
					origin,
				)
			} else if ty == self.tys.var_set_of_int {
				Expression::new(
					FreshVariable::VarSetOfInt {
						domain: Box::new(match &**self.model[declaration].domain() {
							shackle_thir::DomainData::Set(elem, None) => match &***elem {
								shackle_thir::DomainData::Bounded(b) => self
									.lower_expression(b, destination)
									.into_value(self, destination),
								shackle_thir::DomainData::Unbounded => Value::new(
									LiteralData::InfiniteIntSet,
									self.tys.set_of_int,
									origin,
								),
								_ => unreachable!(),
							},
							_ => unreachable!(),
						}),
						is_defined: self.is_promise_total,
					},
					ty,
					origin,
				)
			} else {
				// Input parameter doesn't need declaration
				return;
			}
		};

		if let Some(Identifier::GlobalVar(global)) = global_res {
			// Assign value to the global variable
			let constraint = GlobalAssignment {
				global,
				definition: Box::new(definition),
			};
			destination.push(constraint);
		} else {
			let decl = Declaration {
				name: self.model[declaration].name(),
				ty,
				definition,
				annotations,
			};
			let decl_idx = self.data.declarations.insert(decl);
			destination.push(decl_idx);
			// Register the resolution for this declaration
			let _ = self.resolutions.insert(declaration.into(), decl_idx.into());
		}
	}

	fn lower_constraint(
		&mut self,
		constraint: shackle_thir::ConstraintId<'db>,
		destination: &mut impl Items<'db>,
	) {
		let annotations = self.model[constraint]
			.annotations()
			.iter()
			.map(|ann| {
				self.lower_expression(ann, destination)
					.into_identifier(self, destination)
			})
			.collect();
		let expression = self.lower_expression(self.model[constraint].expression(), destination);

		if expression.ty() == self.tys.var_bool && !expression.is_call() {
			let value = expression.into_value(self, destination);
			let origin = value.origin;
			let function = self.function_ids[&self
				.model
				.lookup_function(
					self.db,
					self.ids.functions.mzn_post.into(),
					&[self.tys.var_bool],
				)
				.unwrap()
				.function];
			let call = Expression::new(
				Call {
					function,
					arguments: vec![value],
					annotations: Vec::new(),
				},
				self.tys.var_bool,
				origin,
			);
			let constraint = Constraint {
				expression: Box::new(call),
				annotations,
			};
			destination.push(constraint);
		} else {
			let constraint = Constraint {
				expression: Box::new(expression.into_expression()),
				annotations,
			};
			destination.push(constraint);
		}
	}

	/// Lower a shackle_thir expression to MIR
	fn lower_expression(
		&mut self,
		expression: &shackle_thir::Expression<'db>,
		destination: &mut impl Items<'db>,
	) -> LoweredExpression<'db> {
		let ty = expression.ty();
		self.lower_expression_with_ty(expression, ty, destination)
	}

	fn lower_expression_with_ty(
		&mut self,
		expression: &shackle_thir::Expression<'db>,
		ty: Ty<'db>,
		destination: &mut impl Items<'db>,
	) -> LoweredExpression<'db> {
		let origin = expression.origin();

		let lowered = match &**expression {
			shackle_thir::ExpressionData::BooleanLiteral(b) => {
				Value::new(LiteralData::Boolean(b.clone()), ty, origin).into()
			}
			shackle_thir::ExpressionData::IntegerLiteral(i) => {
				Value::new(LiteralData::Integer(i.clone()), ty, origin).into()
			}
			shackle_thir::ExpressionData::FloatLiteral(f) => {
				Value::new(LiteralData::Float(f.clone()), ty, origin).into()
			}
			shackle_thir::ExpressionData::StringLiteral(s) => {
				Value::new(LiteralData::String(s.clone()), ty, origin).into()
			}
			shackle_thir::ExpressionData::Infinity => {
				Value::new(LiteralData::Infinity, ty, origin).into()
			}
			shackle_thir::ExpressionData::Identifier(
				shackle_thir::ResolvedIdentifier::Annotation(a),
			) => Value::new(LiteralData::from(self.annotation_ids[a]), ty, origin).into(),
			shackle_thir::ExpressionData::Identifier(
				shackle_thir::ResolvedIdentifier::Declaration(d),
			) if self.model[*d].name() == Some(self.ids.names.root) => LoweredExpression::Root(ty, origin),
			shackle_thir::ExpressionData::Identifier(ident) => Value::new(
				LiteralData::from(
					*self
						.resolutions
						.get(ident)
						.or_else(|| self.global_resolutions.get(ident))
						.unwrap_or_else(|| {
							panic!(
								"Did not lower identifier {:?} at {}",
								ident,
								origin.pretty_print(self.db)
							)
						}),
				),
				ty,
				origin,
			)
			.into(),
			shackle_thir::ExpressionData::ArrayLiteral(al) => Value::new(
				Array {
					members: al
						.iter()
						.map(|e| {
							self.lower_expression(e, destination)
								.into_literal(self, destination)
						})
						.collect(),
				},
				ty,
				origin,
			)
			.into(),
			shackle_thir::ExpressionData::SetLiteral(sl) => Value::new(
				Set {
					kind: if ty == self.tys.set_of_int {
						SetType::Integer
					} else if ty == self.tys.set_of_float {
						SetType::Float
					} else {
						SetType::Discrete
					},
					members: sl
						.iter()
						.map(|e| {
							self.lower_expression(e, destination)
								.into_literal(self, destination)
						})
						.collect(),
				},
				ty,
				origin,
			)
			.into(),
			shackle_thir::ExpressionData::TupleLiteral(tl) => Value::new(
				Tuple {
					members: tl
						.iter()
						.map(|e| {
							self.lower_expression(e, destination)
								.into_literal(self, destination)
						})
						.collect(),
				},
				ty,
				origin,
			)
			.into(),
			shackle_thir::ExpressionData::TupleAccess(ta) => {
				let tuple = self
					.lower_expression(&ta.tuple, destination)
					.into_identifier(self, destination);
				Value::new(
					TupleAccess {
						tuple,
						field: ta.field,
					},
					ty,
					origin,
				)
				.into()
			}
			shackle_thir::ExpressionData::ArrayComprehension(comp) => {
				let mut generators = Generators::default();

				for g in comp.generators.iter() {
					match g {
						shackle_thir::Generator::Assignment {
							assignment,
							where_clause,
						} => {
							let definition = self.lower_expression(
								self.model[*assignment].definition().as_ref().unwrap(),
								&mut generators,
							);
							let asg_decl = Declaration {
								name: self.model[*assignment].name(),
								ty: self.model[*assignment].ty(),
								annotations: Vec::new(),
								definition: definition.into_expression(),
							};
							let asg_idx = self.data.declarations.insert(asg_decl);
							let _ = self
								.resolutions
								.insert((*assignment).into(), asg_idx.into());
							generators.generators.push(Generator::Assignment {
								declaration: asg_idx,
							});
							if let Some(w) = &where_clause {
								let where_value = self
									.lower_expression(w, &mut generators)
									.into_value(self, &mut generators);
								generators.generators.push(Generator::Where(where_value));
							}
						}
						shackle_thir::Generator::Iterator {
							declarations,
							collection,
							where_clause,
						} => {
							let collection_ty = collection.ty();
							let collection_origin = collection.origin();
							let lowered = self.lower_expression(collection, &mut generators);
							let collection = if declarations.len() > 1 {
								// Ensure collection is only created once
								Value::new(
									LiteralData::from(
										lowered.into_identifier(self, &mut generators),
									),
									collection_ty,
									collection_origin,
								)
							} else {
								lowered.into_value(self, &mut generators)
							};
							for d in declarations.iter() {
								let ty = self.model[*d].ty();
								let origin = self.model[*d].origin();
								let definition = Expression::new(
									ExpressionData::Iterator(collection.data.clone()),
									ty,
									origin,
								);
								let idx = self.data.declarations.insert(Declaration {
									name: self.model[*d].name(),
									annotations: Vec::new(),
									ty,
									definition,
								});
								let _ = self.resolutions.insert((*d).into(), idx.into());
								generators
									.generators
									.push(Generator::Iterator { element: idx });
							}
							if let Some(w) = &where_clause {
								let where_value = self
									.lower_expression(w, &mut generators)
									.into_value(self, &mut generators);
								generators.generators.push(Generator::Where(where_value));
							}
						}
					}
				}

				let template = self
					.lower_expression(&comp.template, &mut generators)
					.into_value(self, &mut generators);
				Expression::new(
					Comprehension {
						generators: generators.generators,
						template,
					},
					ty,
					origin,
				)
				.into()
			}
			shackle_thir::ExpressionData::IfThenElse(ite) => {
				let mut else_items = LetItems::default();
				let mut result = self.lower_expression(&ite.else_result, &mut else_items);
				for (i, branch) in ite.branches.iter().enumerate().rev() {
					let mut condition_items = LetItems::default();
					let condition = if i == 0 {
						self.lower_expression(&branch.condition, destination)
							.into_value(self, destination)
					} else {
						self.lower_expression(&branch.condition, &mut condition_items)
							.into_value(self, &mut condition_items)
					};
					let mut branch_items = LetItems::default();
					let lowered = self.lower_expression(&branch.result, &mut branch_items);
					let then = branch_items.into_let(lowered);

					result = Expression::new(
						IfThenElse {
							condition: Box::new(condition),
							then,
							else_expression: else_items.into_let(result),
						},
						ty.clone(),
						origin,
					)
					.into();
					else_items = condition_items;
				}
				result.into()
			}
			shackle_thir::ExpressionData::Case(_) => todo!(),
			shackle_thir::ExpressionData::Call(c) => {
				let mut arguments = c
					.arguments
					.iter()
					.map(|argument| {
						self.lower_expression(argument, destination)
							.into_value(self, destination)
					})
					.collect();
				match &c.function {
					shackle_thir::Callable::Function(f) => {
						if self.model[*f].body().is_none()
							&& let Some(builtin) = Builtin::new(
								self.db,
								self.model[*f].name().as_identifier(self.db),
								&mut arguments,
							) {
							return Expression::new(builtin, ty, origin).into();
						}
						let function = *self.function_ids.get(f).unwrap_or_else(|| {
							panic!(
								"Did not lower {:?} at {}, {:?}",
								f,
								origin.pretty_print(self.db),
								self.model[*f].body()
							)
						});
						let annotations = expression
							.annotations()
							.iter()
							.map(|ann| {
								self.lower_expression(ann, destination)
									.into_identifier(self, destination)
							})
							.collect();
						Expression::new(
							Call {
								function,
								arguments,
								annotations,
							},
							ty,
							origin,
						)
						.into()
					}
					shackle_thir::Callable::Annotation(a) => {
						let annotation = self.annotation_ids[a];
						Expression::new(
							AnnotationCall {
								annotation,
								arguments,
							},
							ty,
							origin,
						)
						.into()
					}
					_ => todo!(),
				}
			}
			shackle_thir::ExpressionData::Let(l) => {
				for item in l.items.iter() {
					match item {
						shackle_thir::LetItem::Constraint(c) => {
							self.lower_constraint(*c, destination);
						}
						shackle_thir::LetItem::Declaration(d) => {
							self.lower_declaration(*d, destination);
						}
					}
				}
				self.lower_expression(&l.in_expression, destination)
			}
			shackle_thir::ExpressionData::Absent
			| shackle_thir::ExpressionData::RecordLiteral(_)
			| shackle_thir::ExpressionData::SetComprehension(_)
			| shackle_thir::ExpressionData::RecordAccess(_)
			| shackle_thir::ExpressionData::Lambda(_) => unreachable!("Illegal expression"),
		};

		lowered
	}

	fn lower_solve(&mut self, destination: &mut impl Items<'db>) {
		let solve = self.model.solve().unwrap();
		let annotations = Box::new(Value::new(
			Array {
				members: solve
					.annotations()
					.iter()
					.map(|ann| {
						self.lower_expression(ann, destination)
							.into_literal(self, destination)
					})
					.collect(),
			},
			self.tys.array_of_ann,
			solve.origin(),
		));
		let builtin = if let Some(obj) = solve.objective() {
			let Identifier::GlobalVar(g) = self.global_resolutions[&obj.into()] else {
				unreachable!()
			};
			let objective = Box::new(Value::new(
				LiteralData::Identifier(g.into()),
				self.global_vars[g].ty,
				self.global_vars[g].origin,
			));

			if solve.is_maximize() {
				Builtin::SolveMaximize {
					annotations,
					objective,
				}
			} else {
				Builtin::SolveMinimize {
					annotations,
					objective,
				}
			}
		} else {
			Builtin::SolveSatisfy { annotations }
		};

		let constraint = Constraint {
			expression: Box::new(Expression::new(builtin, self.tys.par_bool, solve.origin())),
			annotations: Vec::new(),
		};
		destination.push(constraint);
	}
}

/// Create MIR from shackle_thir
pub fn lower_from_thir<'db>(db: &'db dyn Db, m: &'db shackle_thir::Model<'db>) -> Model<'db> {
	let ids = IdentifierRegistry::lookup(db);
	let tys = TypeRegistry::lookup(db);

	let mut ctx = LoweringContext::new(db, m);

	for (ann, _) in m.annotations() {
		ctx.lower_annotation(ann);
	}

	let global_vars = GlobalVariables::find(db, m);
	let mut inputs = Vec::new();
	for param in global_vars.input_parameters.iter() {
		let idx = ctx.global_vars.insert(GlobalVar {
			name: m[*param].name(),
			ty: m[*param].ty(),
			annotations: m[*param]
				.annotations()
				.iter()
				.filter_map(|ann| match &**ann {
					shackle_thir::ExpressionData::Identifier(
						shackle_thir::ResolvedIdentifier::Annotation(a),
					) => Some(ctx.annotation_ids[a]),
					_ => None,
				})
				.collect(),
			origin: m[*param].origin(),
		});
		let _ = ctx.global_resolutions.insert((*param).into(), idx.into());
		inputs.push(idx);
	}
	for var in global_vars.referred_variables.iter() {
		let idx = ctx.global_vars.insert(GlobalVar {
			name: m[*var].name(),
			ty: m[*var].ty(),
			annotations: m[*var]
				.annotations()
				.iter()
				.filter_map(|ann| match &**ann {
					shackle_thir::ExpressionData::Identifier(
						shackle_thir::ResolvedIdentifier::Annotation(a),
					) => Some(ctx.annotation_ids[a]),
					_ => None,
				})
				.collect(),
			origin: m[*var].origin(),
		});
		let _ = ctx.global_resolutions.insert((*var).into(), idx.into());
	}

	if let Some(solve) = m.solve()
		&& let Some(obj) = solve.objective()
	{
		let idx = ctx.global_vars.insert(GlobalVar {
			// name: m[obj].name(),
			name: None, // TODO: Fix name clash with old minizinc
			ty: m[obj].ty(),
			annotations: Vec::new(),
			origin: m[obj].origin(),
		});
		let _ = ctx.global_resolutions.insert(obj.into(), idx.into());
	}
	let mut main_collector = Collector::new(&mut ctx);
	let mut main_items = LetItems::default();
	let mut output_declarations = Vec::new();

	for item in m.top_level_items() {
		match item {
			shackle_thir::ItemId::Annotation(_) => {
				// Already done
			}
			shackle_thir::ItemId::Function(f) => {
				main_collector.lower_function_signature(f);
			}
			shackle_thir::ItemId::Constraint(c) => {
				main_collector.lower_constraint(c, &mut main_items);
			}
			shackle_thir::ItemId::Declaration(d) => {
				if m[d].annotations().has(m, ids.annotations.output_only) {
					output_declarations.push(d);
				} else {
					main_collector.lower_declaration(d, &mut main_items);
				}
			}
			shackle_thir::ItemId::Solve => {
				main_collector.lower_solve(&mut main_items);
			}
			_ => unreachable!("Illegal item {:?}", item),
		}
	}

	// Function signatures all lowered, so now we can lower function bodies
	for (function, _) in m.top_level_functions() {
		main_collector.lower_function_body(function);
	}

	let origin = Origin::Introduced("main entrypoint");
	let main_fn = Function {
		name: ids.functions.main,
		annotations: Vec::new(),
		ty: tys.var_bool,
		body: Some(Let {
			items: main_items.items,
			result: LetResult::Root,
			origin,
		}),
		data: main_collector.data,
		parameters: Vec::new(),
		origin,
	};
	let main_entrypoint = ctx.functions.insert(main_fn);

	let mut output_collector = Collector::new(&mut ctx);
	let mut output_items = LetItems::default();
	for d in output_declarations {
		output_collector.lower_declaration(d, &mut output_items);
	}

	let origin = Origin::Introduced("output_only entrypoint");
	let output_fn: Function<'_> = Function {
		name: ids.functions.main_output,
		annotations: Vec::new(),
		ty: tys.par_bool,
		body: Some(Let {
			items: output_items.items,
			result: LetResult::Root,
			origin,
		}),
		data: output_collector.data,
		parameters: Vec::new(),
		origin,
	};
	let output_entrypoint = ctx.functions.insert(output_fn);

	Model {
		inputs,
		main_entrypoint,
		output_entrypoint,
		global_vars: ctx.global_vars,
		annotations: ctx.annotations,
		functions: ctx.functions,
	}
}

#[derive(From)]
enum LoweredExpression<'db> {
	/// A literal
	Literal(Literal<'db>),
	/// Simple value
	Value(Value<'db>),
	/// Complex expression
	Expression(Expression<'db>),
	/// Root true
	#[from(skip)]
	Root(Ty<'db>, Origin<'db>),
}

impl<'db> LoweredExpression<'db> {
	fn ty(&self) -> Ty<'db> {
		match self {
			LoweredExpression::Value(v) => v.ty,
			LoweredExpression::Expression(e) => e.ty,
			LoweredExpression::Literal(l) => l.ty,
			LoweredExpression::Root(ty, _) => *ty,
		}
	}

	fn origin(&self) -> Origin<'db> {
		match self {
			LoweredExpression::Value(v) => v.origin,
			LoweredExpression::Expression(e) => e.origin,
			LoweredExpression::Literal(l) => l.origin,
			LoweredExpression::Root(_, origin) => *origin,
		}
	}

	fn into_expression(self) -> Expression<'db> {
		match self {
			LoweredExpression::Expression(e) => e,
			LoweredExpression::Value(v) => v.into(),
			LoweredExpression::Literal(l) => l.into(),
			LoweredExpression::Root(ty, origin) => Expression::new(
				ValueData::from(LiteralData::Boolean(shackle_hir::BooleanLiteral(true))),
				ty,
				origin,
			),
		}
	}

	fn into_value(
		self,
		collector: &mut Collector<'_, 'db>,
		destination: &mut impl Items<'db>,
	) -> Value<'db> {
		match self {
			LoweredExpression::Literal(l) => l.into(),
			LoweredExpression::Root(ty, origin) => Value::new(
				LiteralData::Boolean(shackle_hir::BooleanLiteral(true)),
				ty,
				origin,
			),
			LoweredExpression::Value(v) => v,
			LoweredExpression::Expression(e) => {
				if e.is_value() {
					e.into_value()
				} else {
					let ty = e.ty;
					let origin = e.origin;
					let idx = collector.data.declarations.insert(Declaration {
						ty,
						name: None,
						definition: e,
						annotations: Vec::new(),
					});
					destination.push(idx);
					Value::new(LiteralData::from(idx), ty, origin).into()
				}
			}
		}
	}

	fn into_literal(
		self,
		collector: &mut Collector<'_, 'db>,
		destination: &mut impl Items<'db>,
	) -> Literal<'db> {
		match self {
			LoweredExpression::Literal(l) => l,
			LoweredExpression::Root(ty, origin) => Literal::new(
				LiteralData::Boolean(shackle_hir::BooleanLiteral(true)),
				ty,
				origin,
			),
			LoweredExpression::Value(v) => {
				if v.is_literal() {
					v.into_literal()
				} else {
					let ty = v.ty;
					let origin = v.origin;
					let idx = collector.data.declarations.insert(Declaration {
						ty,
						name: None,
						definition: v.into(),
						annotations: Vec::new(),
					});
					destination.push(idx);
					Literal::new(idx, ty, origin).into()
				}
			}
			LoweredExpression::Expression(e) => {
				if e.is_literal() {
					e.into_literal()
				} else {
					let ty = e.ty;
					let origin = e.origin;
					let idx = collector.data.declarations.insert(Declaration {
						ty,
						name: None,
						definition: e,
						annotations: Vec::new(),
					});
					destination.push(idx);
					Literal::new(idx, ty, origin).into()
				}
			}
		}
	}

	fn into_identifier(
		self,
		collector: &mut Collector<'_, 'db>,
		destination: &mut impl Items<'db>,
	) -> Identifier<'db> {
		self.get_identifier().unwrap_or_else(|| {
			let e = self.into_expression();
			let ty = e.ty;
			let idx = collector.data.declarations.insert(Declaration {
				ty,
				name: None,
				definition: e,
				annotations: Vec::new(),
			});
			destination.push(idx);
			idx.into()
		})
	}

	fn is_call(&self) -> bool {
		matches!(self, LoweredExpression::Expression(e) if matches!(&e.data, ExpressionData::Call(_)))
	}

	fn get_identifier(&self) -> Option<Identifier<'db>> {
		match &self {
			LoweredExpression::Value(v) => match &v.data {
				ValueData::Literal(LiteralData::Identifier(ident)) => return Some(*ident),
				_ => (),
			},
			LoweredExpression::Literal(l) => match &l.data {
				LiteralData::Identifier(ident) => return Some(*ident),
				_ => (),
			},
			_ => (),
		}
		None
	}
}

#[cfg(test)]
mod tests {
	use std::path::PathBuf;

	use expect_test::{expect, expect_file};
	use salsa::Setter;
	use shackle_fmt::{MiniZincFormatOptions, format_str};
	use shackle_hir::{
		CompilerDatabase,
		input::{CompilerSettings, InlineModelFile, InputFiles, NamedModelFile},
	};
	use shackle_syntax::InputLang;
	use shackle_thir::{
		db::final_thir,
		pretty_print::PrettyPrinter as ThirPrettyPrinter,
		transform::{DEFAULT_TRANSFORMS, Transform, Transformer},
	};
	use shackle_ty::registry::TypeRegistry;

	use super::lower_from_thir;
	use crate::{Builtin, pretty_print::PrettyPrinter};

	#[test]
	fn test_mir_basic() {
		let mut db = CompilerDatabase::default();
		let model_file = InlineModelFile::new(
			&db,
			r#"
			test abort(string);
			predicate forall(array [int] of var bool);
			var bool: p = false;
			var bool: q = let {
				constraint p;
			} in true;
			"#
			.to_owned(),
			InputLang::MiniZinc,
		)
		.into();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let model = final_thir(&db).unwrap();
		let mir = lower_from_thir(&db, &model);
		let expected = expect![[r#"
    var bool: p = false;
    var bool: _DECL_2 = p;
    var bool: q = _DECL_2;
"#]];
		expected.assert_eq(&PrettyPrinter::new(&mir).print_model(&db));
	}

	#[test]
	fn test_mir_basic_2() {
		let mut db = CompilerDatabase::default();
		let model_file = InlineModelFile::new(
			&db,
			r#"
			bool: mzn_root = true;
			test abort(string: msg);
			annotation mzn_no_remove;
			predicate mzn_post(var bool: c) ::mzn_no_remove;
			annotation 'output';
			predicate foo(var bool: b) = b;
			var bool: x;
			constraint foo(x);
			"#
			.to_owned(),
			InputLang::MiniZinc,
		)
		.into();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let model = final_thir(&db).unwrap();
		let mir = lower_from_thir(&db, &model);
		let expected = expect![[r#"
    annotation mzn_no_remove;
    annotation 'output';
    function var bool: mzn_post_reif(var bool: c, var bool: _PARAM_2) :: promise_total = let {

    } in abort("mzn_post_reif required, but not supported by solver.");
    function var bool: mzn_post(var bool: c) :: promise_total;
    function var bool: foo_root(var bool: b) :: promise_total = let {
    constraint mzn_post(b);
    } in mzn_root;
    bool: mzn_root;
    var bool: x :: 'output';
    mzn_root = true;
    x = (let { var bool: _FRESH_VAR :: promise_total; } in _FRESH_VAR);
    constraint foo_root(x);
"#]];
		expected.assert_eq(&PrettyPrinter::new(&mir).print_model(&db));
	}

	#[test]
	fn test_mir_full_1() {
		let mut db = CompilerDatabase::default();
		let model_file = NamedModelFile::new(
			&db,
			PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/lower/fixtures/full_1.mzn"),
		)
		.into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let model = final_thir(&db).unwrap();
		let mir = lower_from_thir(&db, &model);
		let printed = PrettyPrinter::new(&mir).print_model(&db);
		let actual = format_str(printed, &MiniZincFormatOptions::default()).unwrap();
		let expected = expect_file!["../tests/lower/fixtures/full_1.expected.mzn"];
		expected.assert_eq(&actual);
	}

	#[test]
	fn test_mir_enum_1() {
		let mut db = CompilerDatabase::default();
		let model_file = NamedModelFile::new(
			&db,
			PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("tests/lower/fixtures/enum_1.mzn"),
		)
		.into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let model = final_thir(&db).unwrap();
		let mir = lower_from_thir(&db, &model);
		let printed = PrettyPrinter::new(&mir).print_model(&db);
		let actual = format_str(printed, &MiniZincFormatOptions::default()).unwrap();
		let expected = expect_file!["../tests/lower/fixtures/enum_1.expected.mzn"];
		expected.assert_eq(&actual);
	}

	#[test]
	fn test_mir_where_clause() {
		let mut db = CompilerDatabase::default();
		let model_file = InlineModelFile::new(
			&db,
			r#"
			bool: mzn_root = true;
			test foo(int: x, int: y) = true;
			any: x = [1 | i, j in {1, 2, 3} where foo(i, j)];
			"#
			.to_owned(),
			InputLang::MiniZinc,
		)
		.into();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let model = final_thir(&db).unwrap();
		let mir = lower_from_thir(&db, &model);
		let expected = expect![[r#"
    function bool: foo(int: x, int: y) :: promise_total = let {

    } in true;
    bool: mzn_root = true;
    array [int] of int: x = [1 | _DECL_2 = {1, 2, 3}, i in _DECL_2, j in _DECL_2, _DECL_5 = foo(i, j), _ = 1 where _DECL_5];
"#]];
		expected.assert_eq(&PrettyPrinter::new(&mir).print_model(&db));
	}

	#[test]
	fn standard_library_bodyless_functions_are_lowerable() {
		let mut db = CompilerDatabase::default();
		let model_file = InlineModelFile::new(&db, String::new(), InputLang::MiniZinc).into();
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		Transformer::set_transforms(
			&mut db,
			DEFAULT_TRANSFORMS.iter().copied().filter(|transform| {
				!matches!(
					transform,
					Transform::EliminateDeadCodeConservative | Transform::EliminateDeadCode
				)
			}),
		);
		let model = final_thir(&db).unwrap();
		let tys = TypeRegistry::lookup(&db);
		let builtin_signatures = Builtin::signatures(&db);
		let printer = ThirPrettyPrinter::new(&db, &model);

		let bad_functions = model
			.all_functions()
			.filter(|(_, function)| {
				let is_mir_builtin = builtin_signatures.contains(&(
					function.name().as_identifier(&db),
					function.parameters().len(),
				));
				let is_solver_builtin_predicate =
					function.return_type() == tys.var_bool && !function.is_polymorphic();
				function.body().is_none() && !is_mir_builtin && !is_solver_builtin_predicate
			})
			.map(|(id, _)| printer.pretty_print_signature(id.into()))
			.collect::<Vec<_>>();

		assert!(
			bad_functions.is_empty(),
			"bodyless standard-library functions must be MIR builtins or solver builtin predicates:\n{}",
			bad_functions.join("\n")
		);
	}
}
