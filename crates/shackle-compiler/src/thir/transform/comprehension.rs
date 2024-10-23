//! Desugars comprehensions
//! - Move where clauses as early as possible
//! - Turn var comprehensions into comprehensions over optional values
//! - Change set comprehensions into array comprehensions surrounded by `array2set`.
//!

use std::sync::Arc;

use rustc_hash::FxHashMap;

use super::top_down_type::add_coercion;
use crate::{
	constants::IdentifierRegistry,
	hir::OptType,
	thir::{
		db::Thir,
		traverse::{fold_call, fold_expression, Folder, ReplacementMap, Visitor},
		Absent, ArrayComprehension, ArrayLiteral, Branch, Call, Callable, Declaration,
		DeclarationId, Expression, ExpressionData, Generator, IfThenElse, IntegerLiteral, Item,
		LookupCall, LookupIdentifier, Marker, Model, ResolvedIdentifier, SetComprehension, VarType,
	},
	utils::maybe_grow_stack,
	Result,
};

enum SurroundingCall {
	Forall,
	Exists,
	Sum,
	Other,
}

struct ComprehensionRewriter<Dst: Marker> {
	result: Model<Dst>,
	replacement_map: ReplacementMap<Dst>,
	ids: Arc<IdentifierRegistry>,
}

impl<Dst: Marker> Folder<'_, Dst> for ComprehensionRewriter<Dst> {
	fn model(&mut self) -> &mut Model<Dst> {
		&mut self.result
	}

	fn replacement_map(&mut self) -> &mut ReplacementMap<Dst> {
		&mut self.replacement_map
	}

	fn fold_array_comprehension(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		c: &ArrayComprehension,
	) -> ArrayComprehension<Dst> {
		self.rewrite_array_comprehension(db, model, c, SurroundingCall::Other)
	}

	fn fold_call(&mut self, db: &dyn Thir, model: &Model, call: &Call) -> Call<Dst> {
		if let Callable::Function(f) = &call.function {
			// forall, exists and sum comprehensions get special treatment
			let special_cases = [
				(self.ids.builtins.forall, SurroundingCall::Forall),
				(self.ids.builtins.exists, SurroundingCall::Exists),
				(self.ids.builtins.sum, SurroundingCall::Sum),
			];
			for (ident, surround) in special_cases {
				if model[*f].name() == ident && call.arguments.len() == 1 {
					let arg = &call.arguments[0];
					if let ExpressionData::ArrayComprehension(c) = &**arg {
						// May be able to rewrite into non-optional comprehension, so lookup function again
						let comprehension =
							self.rewrite_array_comprehension(db, model, c, surround);
						return LookupCall {
							function: ident.into(),
							arguments: vec![Expression::new(
								db,
								&self.result,
								arg.origin(),
								comprehension,
							)],
						}
						.resolve(db, &self.result)
						.0;
					}
				}
			}
		}
		fold_call(self, db, model, call)
	}

	fn fold_expression(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		expression: &Expression,
	) -> Expression<Dst> {
		maybe_grow_stack(|| {
			if let ExpressionData::SetComprehension(c) = &**expression {
				// Set comprehensions are turned into array comprehensions surrounded by array2set()
				let array = self.rewrite_set_comprehension(db, model, c, SurroundingCall::Other);
				return Expression::new(
					db,
					&self.result,
					expression.origin(),
					LookupCall {
						function: self.ids.builtins.array2set.into(),
						arguments: vec![Expression::new(
							db,
							&self.result,
							expression.origin(),
							array,
						)],
					},
				);
			}
			fold_expression(self, db, model, expression)
		})
	}
}

impl<Dst: Marker> ComprehensionRewriter<Dst> {
	fn rewrite_array_comprehension(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		c: &ArrayComprehension,
		surrounding: SurroundingCall,
	) -> ArrayComprehension<Dst> {
		let mut generators = c
			.generators
			.iter()
			.map(|g| self.fold_generator(db, model, g))
			.collect::<Vec<_>>();
		let folded_template = self.fold_expression(db, model, &c.template);
		let template =
			self.desugar_comprehension(db, &mut generators, folded_template, surrounding);
		let indices = c
			.indices
			.as_ref()
			.map(|i| Box::new(self.fold_expression(db, model, i)));

		ArrayComprehension {
			generators,
			indices,
			template: Box::new(template),
		}
	}

	fn rewrite_set_comprehension(
		&mut self,
		db: &dyn Thir,
		model: &Model,
		c: &SetComprehension,
		surrounding: SurroundingCall,
	) -> ArrayComprehension<Dst> {
		let mut generators = c
			.generators
			.iter()
			.map(|g| self.fold_generator(db, model, g))
			.collect::<Vec<_>>();
		let folded_template = self.fold_expression(db, model, &c.template);
		let template =
			self.desugar_comprehension(db, &mut generators, folded_template, surrounding);

		ArrayComprehension {
			generators,
			indices: None,
			template: Box::new(template),
		}
	}

	/// Move par where clauses in generators to earliest possible place, and rewrite var where clauses into optionality.
	///
	/// Returns desugared version of template.
	fn desugar_comprehension(
		&mut self,
		db: &dyn Thir,
		generators: &mut Vec<Generator<Dst>>,
		template: Expression<Dst>,
		surrounding: SurroundingCall,
	) -> Expression<Dst> {
		let mut gen_idx = FxHashMap::default();
		for (i, g) in generators.iter().enumerate() {
			for d in g.declarations() {
				gen_idx.insert(d, i + 1);
			}
		}
		let mut todo = Vec::new();
		let mut par_where = Vec::new();
		let mut var_where = Vec::new();
		for (i, g) in generators.iter_mut().enumerate() {
			match g {
				Generator::Iterator {
					declarations,
					collection,
					where_clause,
				} => {
					// Turn var set comprehension into fixed comprehension with where clause
					if collection.ty().is_var_set(db.upcast()) {
						let c = collection.clone();
						let has_set2iter = self
							.result
							.lookup_function(db, self.ids.functions.set2iter.into(), &[c.ty()])
							.map_or(false, |f| f.fn_entry.has_body);
						*collection = Expression::new(
							db,
							&self.result,
							c.origin(),
							LookupCall {
								function: if has_set2iter {
									self.ids.functions.set2iter.into()
								} else {
									self.ids.builtins.ub.into()
								},
								arguments: vec![c.clone()],
							},
						);
						for d in declarations.iter() {
							var_where.push((
								Expression::new(
									db,
									&self.result,
									c.origin(),
									LookupCall {
										function: self.ids.builtins.in_.into(),
										arguments: vec![
											Expression::new(
												db,
												&self.result,
												self.result[*d].origin(),
												*d,
											),
											c.clone(),
										],
									},
								),
								i + 1,
							));
						}
					}

					if let Some(w) = where_clause.take() {
						todo.push(w);
					}
				}
				Generator::Assignment { where_clause, .. } => {
					if let Some(w) = where_clause.take() {
						todo.push(w);
					}
				}
			}
		}

		// Break apart where clauses and sort them into var and par
		while let Some(w) = todo.pop() {
			if let ExpressionData::Call(c) = &*w {
				if let Callable::Function(f) = &c.function {
					if self.result[*f].name() == self.ids.builtins.and {
						todo.extend(c.arguments.iter().cloned());
						continue;
					} else if self.result[*f].name() == self.ids.builtins.forall
						&& c.arguments.len() == 1
					{
						if let ExpressionData::ArrayLiteral(al) = &*c.arguments[0] {
							todo.extend(al.iter().cloned());
							continue;
						}
					}
				}
			}
			let idx = ScopeTester::run(&self.result, &gen_idx, &w);
			if w.ty().inst(db.upcast()).unwrap() == VarType::Var {
				var_where.push((w, idx));
			} else {
				par_where.push((w, idx));
			}
		}

		let mut has_dummy = false;

		// Place par where clauses as early as possible into generators
		for (w, idx) in par_where {
			if idx == 0 && !has_dummy {
				has_dummy = true;
				let decl = Declaration::from_expression(
					db,
					false,
					Expression::new(db, &self.result, w.origin(), IntegerLiteral(0)),
				);
				let decl_idx = self.result.add_declaration(Item::new(decl, w.origin()));
				generators.insert(
					0,
					Generator::Assignment {
						assignment: decl_idx,
						where_clause: Some(w),
					},
				);
			} else {
				let index = if has_dummy { idx } else { idx - 1 };
				generators[index].update_where(|where_clause| {
					if let Some(old_where) = where_clause {
						Some(Expression::new(
							db,
							&self.result,
							w.origin(),
							LookupCall {
								function: self.ids.builtins.and.into(),
								arguments: vec![old_where, w],
							},
						))
					} else {
						Some(w)
					}
				})
			}
		}

		// Var where clauses need special treatment for undefinedness
		//
		// Since these should be able to guard against undefinedness in subsequent
		// generators, we can't just move them inside the body and remove the
		// where clause.
		//
		// Here we rewrite [x | i in foo where bar] (where bar is var bool)
		// into [if w then x else <> endif | i in foo, w = bar where w]
		//
		// This way the where clause stays around and can be handled during
		// totalisation but the option type part has already been introduced
		// and can be erased accordingly.
		if !var_where.is_empty() {
			// Add new assignment generators for each var where clause as early
			// as possible.
			let mut clauses = Vec::with_capacity(var_where.len());
			var_where.sort_by_key(|(_, i)| *i);
			for (w, idx) in var_where.into_iter().rev() {
				let origin = w.origin();
				let mut decl = Declaration::from_expression(db, false, w);
				decl.annotations_mut().push(Expression::new(
					db,
					&self.result,
					origin,
					LookupIdentifier(self.ids.annotations.mzn_var_where_clause),
				));
				let decl_idx = self.result.add_declaration(Item::new(decl, origin));
				let e = Expression::new(db, &self.result, origin, decl_idx);
				generators.insert(
					idx,
					Generator::Assignment {
						assignment: decl_idx,
						where_clause: None,
					},
				);
				clauses.push(e);
			}

			// Transform var where into optionality in the template
			let origin = clauses[0].origin();
			let condition = if clauses.len() > 1 {
				Expression::new(
					db,
					&self.result,
					origin,
					LookupCall {
						function: self.ids.builtins.forall.into(),
						arguments: vec![Expression::new(
							db,
							&self.result,
							origin,
							ArrayLiteral(clauses),
						)],
					},
				)
			} else {
				clauses.pop().unwrap()
			};

			return match surrounding {
				SurroundingCall::Forall => {
					// Rewrite var where clauses into implications
					Expression::new(
						db,
						&self.result,
						origin,
						LookupCall {
							function: self.ids.builtins.implies.into(),
							arguments: vec![condition, template],
						},
					)
				}
				SurroundingCall::Exists => {
					// Rewrite var where clauses into conjunctions
					Expression::new(
						db,
						&self.result,
						origin,
						LookupCall {
							function: self.ids.builtins.and.into(),
							arguments: vec![condition, template],
						},
					)
				}
				SurroundingCall::Sum => {
					if template.ty().inst(db.upcast()) == Some(VarType::Par) {
						// Rewrite var where clauses into linear sum
						Expression::new(
							db,
							&self.result,
							origin,
							LookupCall {
								function: self.ids.builtins.times.into(),
								arguments: vec![condition, template],
							},
						)
					} else {
						// Rewrite var where clauses into if-then-else
						Expression::new(
							db,
							&self.result,
							origin,
							IfThenElse {
								branches: vec![Branch {
									condition,
									result: template,
								}],
								else_result: Box::new(Expression::new(
									db,
									&self.result,
									origin,
									IntegerLiteral(0),
								)),
							},
						)
					}
				}
				SurroundingCall::Other => {
					// Rewrite var where clauses into optionality
					// Optionality coercion already done, so requires explicit types
					let opt_ty = template.ty().with_opt(db.upcast(), OptType::Opt);
					let literal = Expression::new(db, &self.result, origin, Absent);
					let absent = add_coercion(db, &mut self.result, opt_ty, literal);
					let result = add_coercion(db, &mut self.result, opt_ty, template);
					Expression::new(
						db,
						&self.result,
						origin,
						IfThenElse {
							branches: vec![Branch { condition, result }],
							else_result: Box::new(absent),
						},
					)
				}
			};
		}
		template
	}
}

struct ScopeTester<'a, T: Marker> {
	gen_idx: &'a FxHashMap<DeclarationId<T>, usize>,
	idx: usize,
}

impl<'a, T: Marker> Visitor<'_, T> for ScopeTester<'a, T> {
	fn visit_identifier(&mut self, _model: &Model<T>, identifier: &ResolvedIdentifier<T>) {
		if let ResolvedIdentifier::Declaration(idx) = identifier {
			if let Some(idx) = self.gen_idx.get(idx) {
				self.idx = self.idx.max(*idx);
			}
		}
	}
}

impl<'a, T: Marker> ScopeTester<'a, T> {
	/// Get the index plus one of the earliest comprehension to attach this to
	fn run(
		model: &Model<T>,
		gen_idx: &'a FxHashMap<DeclarationId<T>, usize>,
		expression: &Expression<T>,
	) -> usize {
		let mut st = Self { gen_idx, idx: 0 };
		st.visit_expression(model, expression);
		st.idx
	}
}

/// Desugar comprehensions
pub fn desugar_comprehension(db: &dyn Thir, model: Model) -> Result<Model> {
	log::info!("Desugaring comprehensions");
	let mut r = ComprehensionRewriter {
		ids: db.identifier_registry(),
		replacement_map: ReplacementMap::default(),
		result: Model::default(),
	};
	r.add_model(db, &model);
	Ok(r.result)
}

#[cfg(test)]
mod test {
	use expect_test::expect;

	use super::desugar_comprehension;
	use crate::thir::transform::test::check;

	#[test]
	fn test_desugar_array_comprehension_var_where() {
		check(
			desugar_comprehension,
			r#"
				predicate foo(var int: x);
				array [int] of var int: x;
				any: y = [x_i | x_i in x where foo(x_i)];
			"#,
			expect!([r#"
    function var bool: foo(var int: x);
    array [int] of var int: x;
    array [int] of var opt int: y = [if _DECL_1 then let {
      var opt int: _DECL_2 = x_i;
    } in _DECL_2 else let {
      var opt int: _DECL_3 = <>;
    } in _DECL_3 endif | x_i in x, _DECL_1 = foo(x_i)];
"#]),
		)
	}

	#[test]
	fn test_desugar_array_comprehension_var_set() {
		check(
			desugar_comprehension,
			r#"
				var set of int: x;
				any: y = [x_i | x_i in x];
			"#,
			expect!([r#"
    var set of int: x;
    array [int] of var opt int: y = [if _DECL_1 then let {
      opt int: _DECL_2 = x_i;
    } in _DECL_2 else let {
      opt int: _DECL_3 = <>;
    } in _DECL_3 endif | x_i in ub(x), _DECL_1 = 'in'(x_i, x)];
"#]),
		)
	}

	#[test]
	fn test_desugar_array_comprehension_complex() {
		check(
			desugar_comprehension,
			r"
				var set of int: x;
				predicate foo(var int: x);
				test bar(int: x);
				any: y = [x_i | x_i in x where foo(x_i), x_j in x where bar(x_j) /\ bar(x_i)];
			",
			expect!([r#"
    var set of int: x;
    function var bool: foo(var int: x);
    function bool: bar(int: x);
    array [int] of var opt int: y = [if forall([_DECL_3, _DECL_2, _DECL_1]) then let {
      opt int: _DECL_4 = x_i;
    } in _DECL_4 else let {
      opt int: _DECL_5 = <>;
    } in _DECL_5 endif | x_i in ub(x) where bar(x_i), _DECL_1 = 'in'(x_i, x), _DECL_2 = foo(x_i), x_j in ub(x) where bar(x_j), _DECL_3 = 'in'(x_j, x)];
"#]),
		)
	}

	#[test]
	fn test_desugar_array_comprehension_forall() {
		check(
			desugar_comprehension,
			r#"
				predicate foo(var int: x);
				var set of int: S;
				constraint forall (i in S) (foo(i));
			"#,
			expect!([r#"
    function var bool: foo(var int: x);
    var set of int: S;
    constraint forall(['->'(_DECL_1, foo(i)) | i in ub(S), _DECL_1 = 'in'(i, S)]);
"#]),
		)
	}

	#[test]
	fn test_desugar_array_comprehension_exists() {
		check(
			desugar_comprehension,
			r#"
				predicate foo(var int: x);
				var set of int: S;
				constraint exists (i in S) (foo(i));
			"#,
			expect!([r#"
    function var bool: foo(var int: x);
    var set of int: S;
    constraint exists(['/\'(_DECL_1, foo(i)) | i in ub(S), _DECL_1 = 'in'(i, S)]);
"#]),
		)
	}

	#[test]
	fn test_desugar_array_comprehension_sum_par() {
		check(
			desugar_comprehension,
			r#"
				var set of int: S;
				any: x = sum (i in S) (i);
			"#,
			expect!([r#"
    var set of int: S;
    var int: x = sum(['*'(_DECL_1, i) | i in ub(S), _DECL_1 = 'in'(i, S)]);
"#]),
		)
	}

	#[test]
	fn test_desugar_array_comprehension_sum_var() {
		check(
			desugar_comprehension,
			r#"
				var set of int: S;
				function var int: foo(int: x);
				any: x = sum (i in S) (foo(i));
				"#,
			expect!([r#"
    var set of int: S;
    function var int: foo(int: x);
    var int: x = sum([if _DECL_1 then foo(i) else 0 endif | i in ub(S), _DECL_1 = 'in'(i, S)]);
"#]),
		)
	}

	#[test]
	fn test_desugar_set_comprehension() {
		check(
			desugar_comprehension,
			r#"
				set of int: S;
				function var int: foo(int: x);
				any: x = { foo(i) | i in S };
				"#,
			expect!([r#"
    set of int: S;
    function var int: foo(int: x);
    var set of int: x = array2set([foo(i) | i in S]);
"#]),
		)
	}

	#[test]
	fn test_desugar_var_set_comprehension() {
		check(
			desugar_comprehension,
			r#"
				var set of int: S;
				function var int: foo(int: x);
				any: x = { foo(i) | i in S };
				"#,
			expect!([r#"
    var set of int: S;
    function var int: foo(int: x);
    var set of int: x = array2set([if _DECL_1 then let {
      var opt int: _DECL_2 = foo(i);
    } in _DECL_2 else let {
      var opt int: _DECL_3 = <>;
    } in _DECL_3 endif | i in ub(S), _DECL_1 = 'in'(i, S)]);
"#]),
		)
	}
}
