//! Totalisation
//!
//! Rewrites model such that all expressions are total

use std::sync::Arc;

use rustc_hash::FxHashMap;

use crate::{
	constants::{IdentifierRegistry, TypeRegistry},
	hir::BooleanLiteral,
	thir::{
		analyse::{analyse_totality, Mode, ModeAnalyser, Totality},
		db::Thir,
		pretty_print::PrettyPrinter,
		source::Origin,
		traverse::{
			add_model, fold_declaration_id, fold_expression, fold_function_body, fold_function_id,
			Folder, ReplacementMap,
		},
		ArrayComprehension, ArrayLiteral, Branch, Call, Callable, Constraint, Declaration,
		DeclarationId, Domain, DomainData, Expression, ExpressionBuilder, ExpressionData, Function,
		FunctionId, FunctionItem, FunctionName, Generator, IfThenElse, IntegerLiteral, Item, Let,
		LetItem, LookupCall, Marker, Model, ResolvedIdentifier, SetLiteral, TupleAccess,
		TupleLiteral,
	},
	ty::{OptType, Ty},
	utils::{arena::ArenaMap, maybe_grow_stack, refmap::RefMap},
	Result,
};

struct Totaliser<'a, Dst: Marker> {
	ids: Arc<IdentifierRegistry>,
	tys: Arc<TypeRegistry>,
	totalised_model: Model<Dst>,
	replacement_map: ReplacementMap<Dst>,
	totality: ArenaMap<FunctionItem, Totality>,
	modes: RefMap<'a, Expression, Mode>,
	root_fn_modes: RefMap<'a, Expression, Mode>,
	root_fn_map: FxHashMap<FunctionId, FunctionId<Dst>>,
	root_fn_decl_map: FxHashMap<DeclarationId, DeclarationId<Dst>>,
	in_root_fns: bool,
}

impl<'a, Dst: Marker> Folder<'_, Dst> for Totaliser<'a, Dst> {
	fn model(&mut self) -> &mut Model<Dst> {
		&mut self.totalised_model
	}

	fn replacement_map(&mut self) -> &mut ReplacementMap<Dst, ()> {
		&mut self.replacement_map
	}

	fn add_model(&mut self, db: &dyn Thir, model: &Model) {
		add_model(self, db, model);
		// Add bodies for root versions of functions
		log::debug!("Adding bodies for root versions of functions");
		self.in_root_fns = true;
		for (f, i) in model.all_functions() {
			if i.body().is_some() && self.root_fn_map.contains_key(&f) {
				self.fold_function_body(db, model, f);
			}
		}
	}

	fn add_function(&mut self, db: &dyn Thir, model: &Model, f: FunctionId) {
		let totality = self.totality[f];

		log::debug!(
			"{} is {:?}",
			PrettyPrinter::new(db, model).pretty_print_signature(f.into()),
			totality
		);

		let orig_return = model[f].return_type();
		let return_type = if orig_return == self.tys.par_bool || orig_return == self.tys.var_bool {
			// Booleans remain boolean
			orig_return
		} else {
			match totality {
				Totality::Total => orig_return,
				Totality::ParPartial => Ty::tuple(db.upcast(), [self.tys.par_bool, orig_return]),
				Totality::VarPartial => Ty::tuple(db.upcast(), [self.tys.var_bool, orig_return]),
			}
		};

		if !matches!(totality, Totality::Total) {
			// Add root version
			let root_idx = self.add_fn_decl(db, model, f, model[f].name().root(db), orig_return);
			self.root_fn_map.insert(f, root_idx);
			for (idx, root) in model[f]
				.parameters()
				.iter()
				.zip(self.totalised_model[root_idx].parameters())
			{
				self.root_fn_decl_map.insert(*idx, *root);
			}
		}

		// Added after, so replacement map will contain totalised versions
		self.add_fn_decl(db, model, f, model[f].name(), return_type);
	}

	fn fold_function_id(&mut self, db: &dyn Thir, model: &Model, f: FunctionId) -> FunctionId<Dst> {
		if self.in_root_fns {
			self.root_fn_map[&f]
		} else {
			fold_function_id(self, db, model, f)
		}
	}

	fn fold_function_body(&mut self, db: &dyn Thir, model: &Model, f: FunctionId) {
		log::debug!("Adding body for {}", {
			let idx = self.fold_function_id(db, model, f);
			PrettyPrinter::new(db, &self.totalised_model).pretty_print_signature(idx.into())
		});
		fold_function_body(self, db, model, f);
	}

	fn fold_declaration_id(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		d: DeclarationId,
	) -> DeclarationId<Dst> {
		if self.in_root_fns {
			if let Some(idx) = self.root_fn_decl_map.get(&d) {
				return *idx;
			}
		}
		fold_declaration_id(self, db, model, d)
	}

	fn fold_expression(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		expression: &Expression,
	) -> Expression<Dst> {
		let result = maybe_grow_stack(|| {
			let origin = expression.origin();
			let mut e = match &**expression {
				ExpressionData::ArrayLiteral(al) => {
					self.totalise_collection_literal(db, model, al.iter(), origin, ArrayLiteral)
				}
				ExpressionData::SetLiteral(sl) => {
					self.totalise_collection_literal(db, model, sl.iter(), origin, SetLiteral)
				}
				ExpressionData::TupleLiteral(tl) => {
					self.totalise_collection_literal(db, model, tl.iter(), origin, TupleLiteral)
				}
				ExpressionData::ArrayComprehension(c) => {
					self.totalise_comprehension(db, model, c, origin)
				}
				ExpressionData::TupleAccess(ta) => {
					self.totalise_tuple_access(db, model, ta, origin)
				}
				ExpressionData::IfThenElse(ite) => {
					self.totalise_if_then_else(db, model, ite, origin, expression)
				}
				ExpressionData::Case(_c) => todo!(),
				ExpressionData::Call(c) => self.totalise_call(db, model, c, origin, expression),
				ExpressionData::Lambda(_l) => todo!(),
				_ => return fold_expression(self, db, model, expression),
			};
			e.annotations_mut().extend(
				expression
					.annotations()
					.iter()
					.map(|ann| self.fold_expression(db, model, ann)),
			);
			e
		});

		result
	}

	fn fold_let(&mut self, db: &dyn Thir, model: &Model, l: &Let) -> Let<Dst> {
		let mut definedness = Vec::new();
		let mut items = Vec::new();
		for i in l.items.iter() {
			match i {
				LetItem::Constraint(c) => {
					if self.get_mode(model[*c].expression()).is_root() {
						self.add_constraint(db, model, *c);
						items.push(LetItem::Constraint(self.fold_constraint_id(db, model, *c)));
					} else {
						// Turn into a declaration, and add to definedness
						let expression = self.fold_expression(db, model, model[*c].expression());
						let mut declaration = Declaration::from_expression(db, false, expression);
						declaration.annotations_mut().extend(
							model[*c]
								.annotations()
								.iter()
								.map(|ann| self.fold_expression(db, model, ann)),
						);
						let idx = self
							.totalised_model
							.add_declaration(Item::new(declaration, model[*c].origin()));
						let ident = Expression::new(
							db,
							&self.totalised_model,
							model[*c].origin(),
							ResolvedIdentifier::Declaration(idx),
						);
						definedness.push(ident);
						items.push(LetItem::Declaration(idx));
					}
				}
				LetItem::Declaration(d) => {
					let decl = &model[*d];
					let mut declaration = Declaration::new(
						false,
						self.totalise_domain(
							db,
							model,
							decl.domain(),
							&mut items,
							&mut definedness,
						),
					);
					if let Some(name) = decl.name() {
						declaration.set_name(name);
					}
					declaration.annotations_mut().extend(
						decl.annotations()
							.iter()
							.map(|ann| self.fold_expression(db, model, ann)),
					);
					let mut partial = false;
					if let Some(definition) = decl.definition() {
						let def = self.fold_expression(db, model, definition);
						if !self.is_total(db, model, definition, &def) {
							declaration.set_domain(Domain::unbounded(
								db,
								decl.domain().origin(),
								def.ty(),
							));
							partial = true;
						}
						declaration.set_definition(def);
						declaration.validate(db);
					}

					let idx = self
						.totalised_model
						.add_declaration(Item::new(declaration, model[*d].origin()));
					items.push(LetItem::Declaration(idx));
					let ident = Expression::new(
						db,
						&self.totalised_model,
						model[*d].origin(),
						ResolvedIdentifier::Declaration(idx),
					);
					if partial {
						definedness.push(Expression::new(
							db,
							&self.totalised_model,
							model[*d].origin(),
							TupleAccess {
								tuple: Box::new(ident.clone()),
								field: IntegerLiteral(1),
							},
						));
						// Ensure references to the variable now get the just the value
						let value_idx = self.totalised_model.add_declaration(Item::new(
							Declaration::from_expression(
								db,
								false,
								Expression::new(
									db,
									&self.totalised_model,
									model[*d].origin(),
									TupleAccess {
										tuple: Box::new(ident),
										field: IntegerLiteral(2),
									},
								),
							),
							model[*d].origin(),
						));
						self.replacement_map().insert_declaration(*d, value_idx);
						items.push(LetItem::Declaration(value_idx));
					} else {
						self.replacement_map().insert_declaration(*d, idx);
					}
				}
			}
		}
		let mut in_expression = self.fold_expression(db, model, &l.in_expression);
		if !self.is_total(db, model, &l.in_expression, &in_expression) {
			// In expression is partial
			let o = in_expression.origin();
			let declaration = Declaration::from_expression(db, false, in_expression);
			let idx = self
				.totalised_model
				.add_declaration(Item::new(declaration, o));
			let ident = Expression::new(
				db,
				&self.totalised_model,
				o,
				ResolvedIdentifier::Declaration(idx),
			);
			definedness.push(Expression::new(
				db,
				&self.totalised_model,
				o,
				TupleAccess {
					tuple: Box::new(ident.clone()),
					field: IntegerLiteral(1),
				},
			));
			items.push(LetItem::Declaration(idx));
			in_expression = Expression::new(
				db,
				&self.totalised_model,
				o,
				TupleAccess {
					tuple: Box::new(ident),
					field: IntegerLiteral(2),
				},
			);
		}

		if in_expression.ty() == self.tys.par_bool || in_expression.ty() == self.tys.var_bool {
			// Capture partiality here
			definedness.push(in_expression);
			let parts = std::mem::take(&mut definedness);
			if self.get_mode(&l.in_expression).is_root() {
				for def in parts {
					let o = def.origin();
					let constraint = Constraint::new(false, def);
					let constraint_idx = self
						.totalised_model
						.add_constraint(Item::new(constraint, o));
					items.push(LetItem::Constraint(constraint_idx));
				}
				in_expression = Expression::new(
					db,
					&self.totalised_model,
					l.in_expression.origin(),
					BooleanLiteral(true),
				);
			} else {
				in_expression = Expression::new(
					db,
					&self.totalised_model,
					l.in_expression.origin(),
					LookupCall {
						function: self.ids.forall.into(),
						arguments: vec![Expression::new(
							db,
							&self.totalised_model,
							l.in_expression.origin(),
							ArrayLiteral(parts),
						)],
					},
				);
			}
		}

		Let {
			items,
			in_expression: Box::new(if definedness.is_empty() {
				in_expression
			} else {
				let def = Expression::new(
					db,
					&self.totalised_model,
					l.in_expression.origin(),
					LookupCall {
						function: self.ids.forall.into(),
						arguments: vec![Expression::new(
							db,
							&self.totalised_model,
							l.in_expression.origin(),
							ArrayLiteral(definedness),
						)],
					},
				);
				Expression::new(
					db,
					&self.totalised_model,
					l.in_expression.origin(),
					TupleLiteral(vec![def, in_expression]),
				)
			}),
		}
	}
}

impl<'a, Dst: Marker> Totaliser<'a, Dst> {
	fn get_mode(&self, e: &Expression) -> Mode {
		if self.in_root_fns {
			self.root_fn_modes[e]
		} else {
			self.modes[e]
		}
	}

	fn add_fn_decl(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		f: FunctionId,
		name: FunctionName,
		return_type: Ty,
	) -> FunctionId<Dst> {
		let ff = &model[f];
		let mut function = Function::new(name, Domain::unbounded(db, ff.origin(), return_type));
		function.annotations_mut().extend(
			ff.annotations()
				.iter()
				.map(|ann| self.fold_expression(db, model, ann)),
		);
		function.set_parameters(ff.parameters().iter().map(|p| {
			self.add_parameter_declaration(db, model, *p);
			self.fold_declaration_id(db, model, *p)
		}));
		function.set_type_inst_vars(ff.type_inst_vars().iter().cloned());
		function.set_specialised(ff.specialised_from());
		if let Some(tys) = ff.mangled_param_tys() {
			function.set_mangled_param_tys(tys.to_vec());
		}
		let idx = self
			.totalised_model
			.add_function(Item::new(function, ff.origin()));
		self.replacement_map().insert_function(f, idx);

		log::debug!(
			"Added {}",
			PrettyPrinter::new(db, &self.totalised_model).pretty_print_signature(idx.into()),
		);

		idx
	}

	fn totalise_collection_literal<'b, T: ExpressionBuilder<Dst>>(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		values: impl Iterator<Item = &'b Expression>,
		origin: Origin,
		create: impl FnOnce(Vec<Expression<Dst>>) -> T,
	) -> Expression<Dst> {
		let mut is_partial = false;
		let members = values
			.map(|v| {
				let folded = self.fold_expression(db, model, v);
				let total = self.is_total(db, model, v, &folded);
				if !total {
					is_partial = true;
				}
				(total, folded)
			})
			.collect::<Vec<_>>();
		if is_partial {
			let mut items = Vec::with_capacity(members.len());
			let mut definedness = Vec::new();
			let mut values = Vec::with_capacity(members.len());
			for (t, v) in members {
				let o = v.origin();
				let decl = Declaration::from_expression(db, false, v);
				let idx = self.totalised_model.add_declaration(Item::new(decl, o));
				items.push(LetItem::Declaration(idx));
				let ident = Expression::new(
					db,
					&self.totalised_model,
					o,
					ResolvedIdentifier::Declaration(idx),
				);
				if t {
					values.push(ident);
				} else {
					definedness.push(Expression::new(
						db,
						&self.totalised_model,
						o,
						TupleAccess {
							tuple: Box::new(ident.clone()),
							field: IntegerLiteral(1),
						},
					));
					values.push(Expression::new(
						db,
						&self.totalised_model,
						o,
						TupleAccess {
							tuple: Box::new(ident),
							field: IntegerLiteral(2),
						},
					));
				}
			}
			let def = Expression::new(
				db,
				&self.totalised_model,
				origin,
				LookupCall {
					function: self.ids.forall.into(),
					arguments: vec![Expression::new(
						db,
						&self.totalised_model,
						origin,
						ArrayLiteral(definedness),
					)],
				},
			);
			let al = Expression::new(db, &self.totalised_model, origin, create(values));
			let in_expression = Expression::new(
				db,
				&self.totalised_model,
				origin,
				TupleLiteral(vec![def, al]),
			);
			Expression::new(
				db,
				&self.totalised_model,
				origin,
				Let {
					items,
					in_expression: Box::new(in_expression),
				},
			)
		} else {
			Expression::new(
				db,
				&self.totalised_model,
				origin,
				create(members.into_iter().map(|(_, e)| e).collect()),
			)
		}
	}

	fn totalise_comprehension(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		c: &ArrayComprehension,
		origin: Origin,
	) -> Expression<Dst> {
		self.totalise_comprehension_inner(db, model, c, origin, 0, None, Vec::new())
	}

	fn totalise_comprehension_inner(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		c: &ArrayComprehension,
		origin: Origin,
		mut gen_idx: usize,
		out_item: Option<DeclarationId<Dst>>,
		mut out_generators: Vec<Generator<Dst>>,
	) -> Expression<Dst> {
		let mut var_where_clauses = Vec::new();
		let mut inner = None;
		while gen_idx < c.generators.len() {
			let generator = &c.generators[gen_idx];
			gen_idx += 1;
			match generator {
				Generator::Assignment {
					assignment,
					where_clause,
				} => {
					let declaration = self.fold_declaration(db, model, &model[*assignment]);
					let partial = declaration.ty() != model[*assignment].ty();
					let o = model[*assignment].origin();
					let idx = self
						.totalised_model
						.add_declaration(Item::new(declaration, o));
					if partial {
						// Make sure identifier refers to actual value
						let ident = Expression::new(
							db,
							&self.totalised_model,
							o,
							ResolvedIdentifier::Declaration(idx),
						);

						let value_decl = Declaration::from_expression(
							db,
							false,
							Expression::new(
								db,
								&self.totalised_model,
								o,
								TupleAccess {
									tuple: Box::new(ident.clone()),
									field: IntegerLiteral(2),
								},
							),
						);
						let value_idx = self
							.totalised_model
							.add_declaration(Item::new(value_decl, o));
						self.replacement_map()
							.insert_declaration(*assignment, value_idx);

						let inner_generator = Generator::Assignment {
							assignment: value_idx,
							where_clause: where_clause
								.as_ref()
								.map(|w| self.fold_expression(db, model, w)),
						};

						inner = Some(self.totalise_comprehension_inner(
							db,
							model,
							c,
							origin,
							gen_idx,
							Some(idx),
							vec![inner_generator],
						));

						break;
					}
					self.replacement_map.insert_declaration(*assignment, idx);
					if model[*assignment]
						.annotations()
						.has(model, self.ids.mzn_var_where_clause)
					{
						var_where_clauses.push(Expression::new(
							db,
							&self.totalised_model,
							o,
							ResolvedIdentifier::Declaration(idx),
						));
					}
					out_generators.push(Generator::Assignment {
						assignment: idx,
						where_clause: where_clause
							.as_ref()
							.map(|w| self.fold_expression(db, model, w)),
					});
				}
				Generator::Iterator {
					declarations,
					collection,
					where_clause,
				} => {
					let folded = self.fold_expression(db, model, collection);
					let partial = !self.is_total(db, model, collection, &folded);
					let o = collection.origin();
					let indices = declarations
						.iter()
						.map(|d| {
							self.add_iterator_declaration(db, model, *d);
							self.fold_declaration_id(db, model, *d)
						})
						.collect::<Vec<_>>();
					if partial {
						let decl = Declaration::from_expression(db, false, folded);
						let idx = self.totalised_model.add_declaration(Item::new(decl, o));

						let ident = Expression::new(
							db,
							&self.totalised_model,
							o,
							ResolvedIdentifier::Declaration(idx),
						);

						let inner_generator = Generator::Iterator {
							declarations: indices,
							collection: Expression::new(
								db,
								&self.totalised_model,
								o,
								TupleAccess {
									tuple: Box::new(ident.clone()),
									field: IntegerLiteral(2),
								},
							),
							where_clause: where_clause
								.as_ref()
								.map(|w| self.fold_expression(db, model, w)),
						};

						inner = Some(self.totalise_comprehension_inner(
							db,
							model,
							c,
							origin,
							gen_idx,
							Some(idx),
							vec![inner_generator],
						));
						break;
					}
					out_generators.push(Generator::Iterator {
						declarations: indices,
						collection: folded,
						where_clause: where_clause
							.as_ref()
							.map(|w| self.fold_expression(db, model, w)),
					});
				}
			}
		}

		if out_generators.is_empty() {
			assert!(out_item.is_none());
			return inner.unwrap();
		}

		let mut definedness = out_item
			.iter()
			.map(|idx| {
				let o = self.totalised_model[*idx].origin();
				Expression::new(
					db,
					&self.totalised_model,
					o,
					TupleAccess {
						tuple: Box::new(Expression::new(
							db,
							&self.totalised_model,
							o,
							ResolvedIdentifier::Declaration(*idx),
						)),
						field: IntegerLiteral(1),
					},
				)
			})
			.collect::<Vec<_>>();

		let (template, template_partial, needs_flatten) = match inner {
			Some(v) => (v, true, true),
			None => {
				let folded = self.fold_expression(db, model, &c.template);
				let partial = !self.is_total(db, model, &c.template, &folded);
				(folded, partial, false)
			}
		};

		if template_partial {
			let t = if var_where_clauses.is_empty() {
				template
			} else {
				let o = template.origin();
				let decl = Declaration::from_expression(db, false, template);
				let idx = self.totalised_model.add_declaration(Item::new(decl, o));
				let ident = Expression::new(
					db,
					&self.totalised_model,
					o,
					ResolvedIdentifier::Declaration(idx),
				);

				var_where_clauses.push(Expression::new(
					db,
					&self.totalised_model,
					o,
					TupleAccess {
						tuple: Box::new(ident.clone()),
						field: IntegerLiteral(1),
					},
				));

				Expression::new(
					db,
					&self.totalised_model,
					o,
					Let {
						items: vec![LetItem::Declaration(idx)],
						in_expression: Box::new(Expression::new(
							db,
							&self.totalised_model,
							o,
							TupleLiteral(vec![
								Expression::new(
									db,
									&self.totalised_model,
									o,
									LookupCall {
										function: self.ids.exists.into(),
										arguments: vec![Expression::new(
											db,
											&self.totalised_model,
											o,
											ArrayLiteral(var_where_clauses),
										)],
									},
								),
								Expression::new(
									db,
									&self.totalised_model,
									o,
									TupleAccess {
										tuple: Box::new(ident),
										field: IntegerLiteral(2),
									},
								),
							]),
						)),
					},
				)
			};

			let comprehension = Expression::new(
				db,
				&self.totalised_model,
				origin,
				ArrayComprehension {
					generators: out_generators,
					indices: None,
					template: Box::new(t),
				},
			);
			let elem_ty = comprehension.ty().elem_ty(db.upcast()).unwrap();
			let mut items = out_item
				.iter()
				.map(|idx| LetItem::Declaration(*idx))
				.collect::<Vec<_>>();
			let o = c.template.origin();
			let decl = Declaration::from_expression(db, false, comprehension);
			let idx = self.totalised_model.add_declaration(Item::new(decl, o));
			items.push(LetItem::Declaration(idx));

			// Comprehension is defined if all elements are defined
			let def_iter_decl = Declaration::new(false, Domain::unbounded(db, o, elem_ty));
			let def_iter_idx = self
				.totalised_model
				.add_declaration(Item::new(def_iter_decl, o));
			let elements_defined = Expression::new(
				db,
				&self.totalised_model,
				o,
				ArrayComprehension {
					generators: vec![Generator::Iterator {
						declarations: vec![def_iter_idx],
						collection: Expression::new(
							db,
							&self.totalised_model,
							o,
							ResolvedIdentifier::Declaration(idx),
						),
						where_clause: None,
					}],
					indices: None,
					template: Box::new(Expression::new(
						db,
						&self.totalised_model,
						o,
						TupleAccess {
							tuple: Box::new(Expression::new(
								db,
								&self.totalised_model,
								o,
								ResolvedIdentifier::Declaration(def_iter_idx),
							)),
							field: IntegerLiteral(1),
						},
					)),
				},
			);
			definedness.push(Expression::new(
				db,
				&self.totalised_model,
				o,
				LookupCall {
					function: self.ids.forall.into(),
					arguments: vec![elements_defined],
				},
			));

			// Extract values of comprehension
			let val_iter_decl = Declaration::new(false, Domain::unbounded(db, o, elem_ty));
			let val_iter_idx = self
				.totalised_model
				.add_declaration(Item::new(val_iter_decl, o));

			let val_extract_decl = Declaration::from_expression(
				db,
				false,
				Expression::new(
					db,
					&self.totalised_model,
					o,
					TupleAccess {
						tuple: Box::new(Expression::new(
							db,
							&self.totalised_model,
							o,
							ResolvedIdentifier::Declaration(val_iter_idx),
						)),
						field: IntegerLiteral(2),
					},
				),
			);
			let val_extract_ty = val_extract_decl.ty();
			let val_extract_idx = self
				.totalised_model
				.add_declaration(Item::new(val_extract_decl, o));

			let mut ident = Expression::new(
				db,
				&self.totalised_model,
				o,
				ResolvedIdentifier::Declaration(val_extract_idx),
			);

			let mut generators = vec![
				Generator::Iterator {
					declarations: vec![val_iter_idx],
					collection: Expression::new(
						db,
						&self.totalised_model,
						o,
						ResolvedIdentifier::Declaration(idx),
					),
					where_clause: None,
				},
				Generator::Assignment {
					assignment: val_extract_idx,
					where_clause: None,
				},
			];

			if needs_flatten {
				// Was rewritten into nested comprehension, so now needs to be flattened out again
				let val_flat_decl = Declaration::new(
					false,
					Domain::unbounded(db, o, val_extract_ty.elem_ty(db.upcast()).unwrap()),
				);
				let val_flat_idx = self
					.totalised_model
					.add_declaration(Item::new(val_flat_decl, o));
				generators.push(Generator::Iterator {
					declarations: vec![val_flat_idx],
					collection: ident,
					where_clause: None,
				});
				ident = Expression::new(
					db,
					&self.totalised_model,
					o,
					ResolvedIdentifier::Declaration(val_flat_idx),
				);
			}

			let element_values = Expression::new(
				db,
				&self.totalised_model,
				o,
				ArrayComprehension {
					generators,
					indices: None,
					template: Box::new(ident),
				},
			);

			return Expression::new(
				db,
				&self.totalised_model,
				origin,
				Let {
					items,
					in_expression: Box::new(Expression::new(
						db,
						&self.totalised_model,
						origin,
						TupleLiteral(vec![
							Expression::new(
								db,
								&self.totalised_model,
								origin,
								LookupCall {
									function: self.ids.forall.into(),
									arguments: vec![Expression::new(
										db,
										&self.totalised_model,
										origin,
										ArrayLiteral(definedness),
									)],
								},
							),
							element_values,
						]),
					)),
				},
			);
		}

		// Inner/template is total, so
		let comprehension = Expression::new(
			db,
			&self.totalised_model,
			origin,
			ArrayComprehension {
				generators: out_generators,
				indices: None,
				template: Box::new(template),
			},
		);
		if definedness.is_empty() {
			return comprehension;
		}

		let items = out_item
			.iter()
			.map(|idx| LetItem::Declaration(*idx))
			.collect::<Vec<_>>();

		Expression::new(
			db,
			&self.totalised_model,
			origin,
			Let {
				items,
				in_expression: Box::new(Expression::new(
					db,
					&self.totalised_model,
					origin,
					TupleLiteral(vec![
						Expression::new(
							db,
							&self.totalised_model,
							origin,
							LookupCall {
								function: self.ids.forall.into(),
								arguments: vec![Expression::new(
									db,
									&self.totalised_model,
									origin,
									ArrayLiteral(definedness),
								)],
							},
						),
						comprehension,
					]),
				)),
			},
		)
	}

	fn totalise_tuple_access(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		ta: &TupleAccess,
		origin: Origin,
	) -> Expression<Dst> {
		let tuple = self.fold_expression(db, model, &ta.tuple);
		if !self.is_total(db, model, &ta.tuple, &tuple) {
			let decl = Declaration::from_expression(db, false, tuple);
			let idx = self
				.totalised_model
				.add_declaration(Item::new(decl, origin));
			let ident = Expression::new(
				db,
				&self.totalised_model,
				origin,
				ResolvedIdentifier::Declaration(idx),
			);
			let definedness = Expression::new(
				db,
				&self.totalised_model,
				origin,
				TupleAccess {
					tuple: Box::new(ident.clone()),
					field: IntegerLiteral(1),
				},
			);
			let value = Expression::new(
				db,
				&self.totalised_model,
				origin,
				TupleAccess {
					tuple: Box::new(Expression::new(
						db,
						&self.totalised_model,
						origin,
						TupleAccess {
							tuple: Box::new(ident),
							field: IntegerLiteral(2),
						},
					)),
					field: ta.field,
				},
			);

			if value.ty().is_bool(db.upcast()) {
				return Expression::new(
					db,
					&self.totalised_model,
					origin,
					Let {
						items: vec![LetItem::Declaration(idx)],
						in_expression: Box::new(Expression::new(
							db,
							&self.totalised_model,
							origin,
							LookupCall {
								function: self.ids.conj.into(),
								arguments: vec![definedness, value],
							},
						)),
					},
				);
			}

			return Expression::new(
				db,
				&self.totalised_model,
				origin,
				Let {
					items: vec![LetItem::Declaration(idx)],
					in_expression: Box::new(Expression::new(
						db,
						&self.totalised_model,
						origin,
						TupleLiteral(vec![definedness, value]),
					)),
				},
			);
		}
		Expression::new(
			db,
			&self.totalised_model,
			origin,
			TupleAccess {
				tuple: Box::new(tuple),
				field: ta.field,
			},
		)
	}

	fn totalise_if_then_else(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		ite: &IfThenElse,
		origin: Origin,
		expression: &Expression,
	) -> Expression<Dst> {
		let mut is_partial = false;
		let bs = ite
			.branches
			.iter()
			.map(|b| {
				let condition = self.fold_expression(db, model, &b.condition);
				let result = self.fold_expression(db, model, &b.result);
				let total = self.is_total(db, model, &b.result, &result);
				if !total {
					is_partial = true;
				}
				(condition, total, result)
			})
			.collect::<Vec<_>>();
		let er = self.fold_expression(db, model, &ite.else_result);
		let else_total = self.is_total(db, model, &ite.else_result, &er);
		if !else_total {
			is_partial = true;
		}

		// Unify types of partial/non-partial branches
		let (branches, else_result) = if is_partial {
			(
				bs.into_iter()
					.map(|(condition, total, r)| {
						let result = if total {
							Expression::new(
								db,
								&self.totalised_model,
								r.origin(),
								TupleLiteral(vec![
									Expression::new(
										db,
										&self.totalised_model,
										r.origin(),
										BooleanLiteral(true),
									),
									r,
								]),
							)
						} else {
							r
						};
						Branch { condition, result }
					})
					.collect::<Vec<_>>(),
				if else_total {
					Expression::new(
						db,
						&self.totalised_model,
						er.origin(),
						TupleLiteral(vec![
							Expression::new(
								db,
								&self.totalised_model,
								er.origin(),
								BooleanLiteral(true),
							),
							er,
						]),
					)
				} else {
					er
				},
			)
		} else {
			(
				bs.into_iter()
					.map(|(condition, _, result)| Branch { condition, result })
					.collect::<Vec<_>>(),
				er,
			)
		};

		// Partition into var and par groups
		let mut groups = Vec::new();
		for branch in branches {
			let var_condition = branch.condition.ty() == self.tys.var_bool;
			if groups
				.last()
				.map_or(true, |(was_var, _)| *was_var != var_condition)
			{
				groups.push((var_condition, vec![branch]));
			} else {
				groups.last_mut().unwrap().1.push(branch);
			}
		}

		let mut result = else_result;
		for (var_condition, branches) in groups.into_iter().rev() {
			if var_condition {
				let ty = branches.first().unwrap().result.ty();
				let mut items = Vec::new();
				let mut conditions = Vec::with_capacity(branches.len());
				let mut result_idents = Vec::with_capacity(branches.len());
				for branch in branches {
					let o = branch.result.origin();
					let decl = Declaration::from_expression(db, false, branch.result);
					let idx = self.totalised_model.add_declaration(Item::new(decl, o));
					items.push(LetItem::Declaration(idx));
					conditions.push(branch.condition);
					result_idents.push(Expression::new(
						db,
						&self.totalised_model,
						o,
						ResolvedIdentifier::Declaration(idx),
					));
				}
				let else_o = result.origin();
				let else_decl = Declaration::from_expression(db, false, result);
				let else_idx = self
					.totalised_model
					.add_declaration(Item::new(else_decl, else_o));
				items.push(LetItem::Declaration(else_idx));
				conditions.push(Expression::new(
					db,
					&self.totalised_model,
					else_o,
					BooleanLiteral(true),
				));
				result_idents.push(Expression::new(
					db,
					&self.totalised_model,
					else_o,
					ResolvedIdentifier::Declaration(else_idx),
				));

				let conditions_decl = Declaration::from_expression(
					db,
					false,
					Expression::new(db, &self.totalised_model, origin, ArrayLiteral(conditions)),
				);
				let conditions_idx = self
					.totalised_model
					.add_declaration(Item::new(conditions_decl, origin));
				let conditions_e = Expression::new(
					db,
					&self.totalised_model,
					origin,
					ResolvedIdentifier::Declaration(conditions_idx),
				);
				items.push(LetItem::Declaration(conditions_idx));

				let in_expression =
					self.decompose_tuple_ite(db, model, ty, origin, conditions_e, result_idents);
				result = Expression::new(
					db,
					&self.totalised_model,
					origin,
					Let {
						items,
						in_expression: Box::new(in_expression),
					},
				);
			} else {
				result = Expression::new(
					db,
					&self.totalised_model,
					origin,
					IfThenElse {
						branches,
						else_result: Box::new(result),
					},
				)
			}
		}

		if !self.is_total(db, model, expression, &result) && self.get_mode(expression).is_root() {
			let decl = Declaration::from_expression(db, false, result);
			let decl_idx = self
				.totalised_model
				.add_declaration(Item::new(decl, origin));
			let ident = Expression::new(
				db,
				&self.totalised_model,
				origin,
				ResolvedIdentifier::Declaration(decl_idx),
			);

			let constraint = Constraint::new(
				false,
				Expression::new(
					db,
					&self.totalised_model,
					origin,
					TupleAccess {
						tuple: Box::new(ident.clone()),
						field: IntegerLiteral(1),
					},
				),
			);
			let constraint_idx = self
				.totalised_model
				.add_constraint(Item::new(constraint, origin));

			result = Expression::new(
				db,
				&self.totalised_model,
				origin,
				Let {
					items: vec![
						LetItem::Declaration(decl_idx),
						LetItem::Constraint(constraint_idx),
					],
					in_expression: Box::new(Expression::new(
						db,
						&self.totalised_model,
						origin,
						TupleAccess {
							tuple: Box::new(ident),
							field: IntegerLiteral(2),
						},
					)),
				},
			)
		}

		result
	}

	fn decompose_tuple_ite(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		ty: Ty,
		origin: Origin,
		conditions: Expression<Dst>,
		results: Vec<Expression<Dst>>,
	) -> Expression<Dst> {
		if ty.is_tuple(db.upcast()) {
			let fields = ty
				.fields(db.upcast())
				.unwrap()
				.into_iter()
				.enumerate()
				.map(|(i, inner_ty)| {
					let inner_results = results
						.iter()
						.map(|r| {
							Expression::new(
								db,
								&self.totalised_model,
								origin,
								TupleAccess {
									tuple: Box::new(r.clone()),
									field: IntegerLiteral(i as i64 + 1),
								},
							)
						})
						.collect();
					maybe_grow_stack(|| {
						self.decompose_tuple_ite(
							db,
							model,
							inner_ty,
							origin,
							conditions.clone(),
							inner_results,
						)
					})
				})
				.collect();
			return Expression::new(db, &self.totalised_model, origin, TupleLiteral(fields));
		}

		Expression::new(
			db,
			&self.totalised_model,
			origin,
			LookupCall {
				function: self.ids.if_then_else.into(),
				arguments: vec![
					conditions,
					Expression::new(db, &self.totalised_model, origin, ArrayLiteral(results)),
				],
			},
		)
	}

	fn totalise_call(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		c: &Call,
		origin: Origin,
		e: &Expression,
	) -> Expression<Dst> {
		let mut definedness = Vec::new();
		let mut items = Vec::new();
		let mut is_partial = false;
		let function = match &c.function {
			Callable::Annotation(a) => Callable::Annotation(self.fold_annotation_id(db, model, *a)),
			Callable::Function(f) => Callable::Function((|| {
				if self.get_mode(e).is_root() {
					if let Some(idx) = self.root_fn_map.get(f) {
						return *idx;
					}
				}
				fold_function_id(self, db, model, *f)
			})()),
			_ => unreachable!(),
		};
		let arguments = c
			.arguments
			.iter()
			.map(|arg| {
				let v = self.fold_expression(db, model, arg);
				let total = self.is_total(db, model, arg, &v);
				if !total {
					is_partial = true;
				}
				(total, v)
			})
			.collect::<Vec<_>>();
		let totalised_function = match (&c.function, function) {
			(Callable::Expression(f1), Callable::Expression(f2)) if f1.ty() != f2.ty() => {
				let decl = Declaration::from_expression(db, false, *f2);
				let idx = self
					.totalised_model
					.add_declaration(Item::new(decl, f1.origin()));
				items.push(LetItem::Declaration(idx));
				let ident = Expression::new(
					db,
					&self.totalised_model,
					f1.origin(),
					ResolvedIdentifier::Declaration(idx),
				);
				definedness.push(Expression::new(
					db,
					&self.totalised_model,
					f1.origin(),
					TupleAccess {
						tuple: Box::new(ident.clone()),
						field: IntegerLiteral(1),
					},
				));
				Callable::Expression(Box::new(Expression::new(
					db,
					&self.totalised_model,
					f1.origin(),
					TupleAccess {
						tuple: Box::new(ident),
						field: IntegerLiteral(2),
					},
				)))
			}
			(_, f) => f,
		};
		let totalised_args = if is_partial {
			let mut totalised = Vec::with_capacity(arguments.len());
			for (b, arg) in arguments {
				if b {
					totalised.push(arg);
				} else {
					let o = arg.origin();
					let decl = Declaration::from_expression(db, false, arg);
					let idx = self.totalised_model.add_declaration(Item::new(decl, o));
					items.push(LetItem::Declaration(idx));
					let ident = Expression::new(
						db,
						&self.totalised_model,
						o,
						ResolvedIdentifier::Declaration(idx),
					);
					definedness.push(Expression::new(
						db,
						&self.totalised_model,
						o,
						TupleAccess {
							tuple: Box::new(ident.clone()),
							field: IntegerLiteral(1),
						},
					));
					totalised.push(Expression::new(
						db,
						&self.totalised_model,
						o,
						TupleAccess {
							tuple: Box::new(ident),
							field: IntegerLiteral(2),
						},
					));
				}
			}
			totalised
		} else {
			arguments.into_iter().map(|(_, v)| v).collect()
		};

		let mut val = Expression::new(
			db,
			&self.totalised_model,
			origin,
			Call {
				function: totalised_function,
				arguments: totalised_args,
			},
		);

		if definedness.is_empty() {
			return val;
		}

		if val.ty().is_bool(db.upcast()) {
			// Capture partiality in boolean
			definedness.push(val);
			return Expression::new(
				db,
				&self.totalised_model,
				origin,
				Let {
					items,
					in_expression: Box::new(Expression::new(
						db,
						&self.totalised_model,
						origin,
						LookupCall {
							function: self.ids.forall.into(),
							arguments: vec![Expression::new(
								db,
								&self.totalised_model,
								origin,
								ArrayLiteral(definedness),
							)],
						},
					)),
				},
			);
		}

		if !self.is_total(db, model, e, &val) {
			// Call returns partial value
			let o = e.origin();
			let decl = Declaration::from_expression(db, false, val);
			let idx = self.totalised_model.add_declaration(Item::new(decl, o));
			items.push(LetItem::Declaration(idx));
			let ident = Expression::new(
				db,
				&self.totalised_model,
				o,
				ResolvedIdentifier::Declaration(idx),
			);
			definedness.push(Expression::new(
				db,
				&self.totalised_model,
				o,
				TupleAccess {
					tuple: Box::new(ident.clone()),
					field: IntegerLiteral(1),
				},
			));
			val = Expression::new(
				db,
				&self.totalised_model,
				o,
				TupleAccess {
					tuple: Box::new(ident),
					field: IntegerLiteral(2),
				},
			);
		}

		let def = Expression::new(
			db,
			&self.totalised_model,
			origin,
			LookupCall {
				function: self.ids.forall.into(),
				arguments: vec![Expression::new(
					db,
					&self.totalised_model,
					origin,
					ArrayLiteral(definedness),
				)],
			},
		);
		Expression::new(
			db,
			&self.totalised_model,
			origin,
			Let {
				items,
				in_expression: Box::new(Expression::new(
					db,
					&self.totalised_model,
					origin,
					TupleLiteral(vec![def, val]),
				)),
			},
		)
	}

	fn totalise_domain(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		domain: &Domain,
		items: &mut Vec<LetItem<Dst>>,
		definedness: &mut Vec<Expression<Dst>>,
	) -> Domain<Dst> {
		maybe_grow_stack(|| {
			let origin = domain.origin();
			match &**domain {
				DomainData::Array(dim, elem) => Domain::array(
					db,
					origin,
					OptType::NonOpt,
					self.totalise_domain(db, model, dim, items, definedness),
					self.totalise_domain(db, model, elem, items, definedness),
				),
				DomainData::Set(elem) => Domain::set(
					db,
					origin,
					domain.ty().inst(db.upcast()).unwrap(),
					OptType::NonOpt,
					self.totalise_domain(db, model, elem, items, definedness),
				),
				DomainData::Bounded(e) => {
					let folded = self.fold_expression(db, model, &e);
					if self.is_total(db, model, &e, &folded) {
						return Domain::bounded(
							db,
							origin,
							domain.ty().inst(db.upcast()).unwrap(),
							OptType::NonOpt,
							folded,
						);
					}

					let o = e.origin();
					let declaration = Declaration::from_expression(db, false, folded);
					let idx = self
						.totalised_model
						.add_declaration(Item::new(declaration, o));
					items.push(LetItem::Declaration(idx));
					let ident = Expression::new(
						db,
						&self.totalised_model,
						o,
						ResolvedIdentifier::Declaration(idx),
					);
					definedness.push(Expression::new(
						db,
						&self.totalised_model,
						o,
						TupleAccess {
							tuple: Box::new(ident.clone()),
							field: IntegerLiteral(1),
						},
					));
					Domain::bounded(
						db,
						origin,
						domain.ty().inst(db.upcast()).unwrap(),
						OptType::NonOpt,
						Expression::new(
							db,
							&self.totalised_model,
							o,
							TupleAccess {
								tuple: Box::new(ident),
								field: IntegerLiteral(2),
							},
						),
					)
				}
				// All other domains are always unbounded (rewritten in an earlier pass)
				_ => self.fold_domain(db, model, domain),
			}
		})
	}

	fn is_total(
		&self,
		db: &dyn Thir,
		model: &Model,
		original: &Expression,
		folded: &Expression<Dst>,
	) -> bool {
		if folded.ty().is_subtype_of(db.upcast(), original.ty()) {
			return true;
		}
		let field_tys = folded.ty().fields(db.upcast()).unwrap();
		assert!(
			field_tys.len() == 2
				&& (field_tys[0] == self.tys.par_bool || field_tys[0] == self.tys.var_bool)
				&& field_tys[1].is_subtype_of(db.upcast(), original.ty()),
			"Totalisation of {} with type {} gave incorrect type {}",
			PrettyPrinter::new(db, model).pretty_print_expression(&original),
			original.ty().pretty_print(db.upcast()),
			folded.ty().pretty_print(db.upcast())
		);
		false
	}
}

/// Totalise a model
pub fn totalise(db: &dyn Thir, model: Model) -> Result<Model> {
	log::info!("Performing totalisation");
	let mut totaliser = Totaliser {
		ids: db.identifier_registry(),
		tys: db.type_registry(),
		replacement_map: ReplacementMap::default(),
		totalised_model: Model::with_capacities(&model.entity_counts()),
		modes: ModeAnalyser::new(db, &model).run(db),
		root_fn_modes: ModeAnalyser::root_functions(db, &model).run(db),
		totality: analyse_totality(db, &model),
		root_fn_map: FxHashMap::default(),
		root_fn_decl_map: FxHashMap::default(),
		in_root_fns: false,
	};
	totaliser.add_model(db, &model);
	log::info!("Finished totalisation");
	Ok(totaliser.totalised_model)
}

#[cfg(test)]
mod test {
	use expect_test::expect;

	use super::totalise;
	use crate::thir::transform::{
		comprehension::desugar_comprehension,
		domain_constraint::rewrite_domains,
		erase_opt::erase_opt,
		test::{check, check_no_stdlib},
		top_down_type::top_down_type,
		transformer,
		type_specialise::type_specialise,
	};

	#[test]
	fn test_totalise_par_let() {
		check_no_stdlib(
			totalise,
			r#"
                test forall(array [int] of bool);
				bool: x = let {
                    constraint false; 
                } in true;
			"#,
			expect!([r#"
    function bool: forall(array [int] of bool: _DECL_1);
    bool: x = let {
      bool: _DECL_2 = false;
    } in forall([_DECL_2, true]);
    solve satisfy;
"#]),
		)
	}

	#[test]
	fn test_totalise_par_fn() {
		check_no_stdlib(
			totalise,
			r#"
                test forall(array [int] of bool);
                function int: foo() = let {
                    constraint false;
                } in 1;
                bool: x = let {
                    int: a = foo();
                } in true;
                int: z = foo();
			"#,
			expect!([r#"
    function bool: forall(array [int] of bool: _DECL_1);
    function int: foo_root() = let {
      constraint false;
    } in 1;
    function tuple(bool, int): foo() = let {
      bool: _DECL_6 = false;
    } in (forall([_DECL_6]), 1);
    bool: x = let {
      tuple(bool, int): a = foo();
      int: _DECL_3 = (a).2;
    } in forall([(a).1, true]);
    int: z = foo_root();
    solve satisfy;
"#]),
		)
	}

	#[test]
	fn test_totalise_ite() {
		check_no_stdlib(
			totalise,
			r#"
                predicate forall(array [int] of var bool);
                test forall(array [int] of bool);
                function var bool: if_then_else(array [int] of var bool: c, array [int] of var bool: x);
                function var int: if_then_else(array [int] of var bool: c, array [int] of var int: x);
                function var int: foo(var bool: b) =
                    if b then
                        let {
                            constraint false;
                        } in 1
                    else
                        2
                    endif;
                function int: bar(var bool: b) =
                    if true then
                        let {
                            constraint false;
                        } in 1
                    else
                        2
                    endif;
			"#,
			expect!([r#"
    function var bool: forall(array [int] of var bool: _DECL_1);
    function bool: forall(array [int] of bool: _DECL_2);
    function var bool: if_then_else(array [int] of var bool: c, array [int] of var bool: x);
    function var int: if_then_else(array [int] of var bool: c, array [int] of var int: x);
    function var int: foo_root(var bool: b) = let {
      tuple(var bool, var int): _DECL_20 = let {
      tuple(bool, int): _DECL_17 = let {
      bool: _DECL_16 = false;
    } in (forall([_DECL_16]), 1);
      array [int] of var bool: _DECL_18 = [b];
      tuple(bool, int): _DECL_19 = (true, 2);
    } in (if_then_else(_DECL_18, [(_DECL_17).1]), if_then_else(_DECL_18, [(_DECL_17).2]));
      constraint (_DECL_20).1;
    } in (_DECL_20).2;
    function tuple(var bool, var int): foo(var bool: b) = let {
      tuple(bool, int): _DECL_12 = let {
      bool: _DECL_11 = false;
    } in (forall([_DECL_11]), 1);
      array [int] of var bool: _DECL_13 = [b];
      tuple(bool, int): _DECL_14 = (true, 2);
    } in (if_then_else(_DECL_13, [(_DECL_12).1]), if_then_else(_DECL_13, [(_DECL_12).2]));
    function int: bar_root(var bool: b) = if true then let {
      constraint false;
    } in 1 else 2 endif;
    function tuple(bool, int): bar(var bool: b) = if true then let {
      bool: _DECL_15 = false;
    } in (forall([_DECL_15]), 1) else (true, 2) endif;
    solve satisfy;
"#]),
		)
	}

	#[test]
	fn test_totalise_comp() {
		check_no_stdlib(
			totalise,
			r#"
                predicate forall(array [int] of var bool);
                test forall(array [int] of bool);
                function array [int] of int: foo() =
                    [1 | i in {1}, j in {2}, k in {3}];
                function array [int] of int: bar() =
                    [let { constraint false } in 1 | i in {1}, j in {2}, k in {3}];
                function set of int: iter() = let {
                    constraint false;
                } in {1};
                function array [int] of int: qux() =
                    [1 | i in {1}, j in iter(), k in {3}];
			"#,
			expect!([r#"
    function var bool: forall(array [int] of var bool: _DECL_1);
    function bool: forall(array [int] of bool: _DECL_2);
    function array [int] of int: foo() = [1 | i in {1}, j in {2}, k in {3}];
    function array [int] of int: bar_root() = [let {
      constraint false;
    } in 1 | i in {1}, j in {2}, k in {3}];
    function tuple(bool, array [int] of int): bar() = let {
      array [int] of tuple(bool, int): _DECL_10 = [let {
      bool: _DECL_9 = false;
    } in (forall([_DECL_9]), 1) | i in {1}, j in {2}, k in {3}];
    } in (forall([forall([(_DECL_11).1 | _DECL_11 in _DECL_10])]), [_DECL_13 | _DECL_12 in _DECL_10, _DECL_13 = (_DECL_12).2]);
    function set of int: iter_root() = let {
      constraint false;
    } in {1};
    function tuple(bool, set of int): iter() = let {
      bool: _DECL_14 = false;
    } in (forall([_DECL_14]), {1});
    function array [int] of int: qux_root() = [1 | i in {1}, j in iter_root(), k in {3}];
    function tuple(bool, array [int] of int): qux() = let {
      array [int] of tuple(bool, array [int] of int): _DECL_19 = [let {
      tuple(bool, set of int): _DECL_17 = iter();
    } in (forall([(_DECL_17).1]), [1 | j in (_DECL_17).2, k in {3}]) | i in {1}];
    } in (forall([forall([(_DECL_20).1 | _DECL_20 in _DECL_19])]), [_DECL_23 | _DECL_21 in _DECL_19, _DECL_22 = (_DECL_21).2, _DECL_23 in _DECL_22]);
    solve satisfy;
"#]),
		)
	}

	#[test]
	fn test_totalise_var_comp() {
		check(
			transformer(vec![
				rewrite_domains,
				top_down_type,
				type_specialise,
				desugar_comprehension,
				erase_opt,
				totalise,
			]),
			r#"
                predicate bar(int: x) = false;
                function set of int: qux() = let { constraint false } in {3, 4};
                function array [int] of var opt int: foo() =
                    [1 | i in {1, 2} where bar(i), j in qux()];
			"#,
			expect!([r#"
    function var bool: bar(int: x) = false;
    function set of int: qux_root() = let {
      constraint false;
    } in {3, 4};
    function tuple(bool, set of int): qux() = let {
      bool: _DECL_1 = false;
    } in (forall([_DECL_1]), {3, 4});
    function array [int] of tuple(var bool, var int): foo_root() = [let {
      tuple(var bool, var int): _DECL_3 = let {
      tuple(var bool, var int): _DECL_4 = (true, 1);
    } in _DECL_4;
      tuple(var bool, var int): _DECL_5 = let {
      tuple(var bool, var int): _DECL_6 = (false, 0);
    } in _DECL_6;
      array [int] of var bool: _DECL_7 = [_DECL_2, true];
    } in (if_then_else(_DECL_7, [(_DECL_3).1, (_DECL_5).1]), if_then_else(_DECL_7, [(_DECL_3).2, (_DECL_5).2])) | i in {1, 2}, _DECL_2 = bar(i), j in qux_root()];
    function tuple(var bool, array [int] of tuple(var bool, var int)): foo() = let {
      array [int] of tuple(var bool, array [int] of tuple(var bool, var int)): _DECL_8 = [let {
      tuple(bool, array [int] of tuple(var bool, var int)): _DECL_10 = let {
      tuple(bool, set of int): _DECL_11 = qux();
    } in (forall([(_DECL_11).1]), [let {
      tuple(var bool, var int): _DECL_12 = let {
      tuple(var bool, var int): _DECL_13 = (true, 1);
    } in _DECL_13;
      tuple(var bool, var int): _DECL_14 = let {
      tuple(var bool, var int): _DECL_15 = (false, 0);
    } in _DECL_15;
      array [int] of var bool: _DECL_16 = [_DECL_9, true];
    } in (if_then_else(_DECL_16, [(_DECL_12).1, (_DECL_14).1]), if_then_else(_DECL_16, [(_DECL_12).2, (_DECL_14).2])) | j in (_DECL_11).2]);
    } in (exists([_DECL_9, (_DECL_10).1]), (_DECL_10).2) | i in {1, 2}, _DECL_9 = bar(i)];
    } in (forall([forall([(_DECL_17).1 | _DECL_17 in _DECL_8])]), [_DECL_20 | _DECL_18 in _DECL_8, _DECL_19 = (_DECL_18).2, _DECL_20 in _DECL_19]);
"#]),
		)
	}

	#[test]
	fn test_totalise_bool_fns() {
		check(
			transformer(vec![
				rewrite_domains,
				top_down_type,
				type_specialise,
				desugar_comprehension,
				erase_opt,
				totalise,
			]),
			r#"
				function int: foo(int: x) = let {
					constraint false;
				} in 1;
                test bar(int: x) = let {
					int: f = foo(x);
				} in false;
				constraint bar(1);
			"#,
			expect!([r#"
    function int: foo_root(int: x) = let {
      constraint false;
    } in 1;
    function tuple(bool, int): foo(int: x) = let {
      bool: _DECL_1 = false;
    } in (forall([_DECL_1]), 1);
    function bool: bar_root(int: x) = let {
      int: f = foo_root(x);
      constraint false;
    } in true;
    function bool: bar(int: x) = let {
      tuple(bool, int): f = foo(x);
      int: _DECL_2 = (f).2;
    } in forall([(f).1, false]);
    constraint bar_root(1);
"#]),
		)
	}
}
