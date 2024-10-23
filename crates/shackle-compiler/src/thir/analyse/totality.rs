//! Determines if a function is total
//!

use std::sync::Arc;

use rustc_hash::FxHashMap;

use super::ModeAnalysis;
use crate::{
	constants::{IdentifierRegistry, TypeRegistry},
	thir::{
		db::Thir,
		traverse::{
			visit_array_comprehension, visit_callable, visit_constraint, visit_expression,
			visit_if_then_else, Visitor,
		},
		ArrayComprehension, Callable, ConstraintId, Expression, FunctionId, FunctionItem,
		IfThenElse, Model,
	},
	utils::{arena::ArenaMap, maybe_grow_stack},
};

/// Totality of a function
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Totality {
	/// Function is total
	Total,
	/// Function is partial but partiality is par
	ParPartial,
	/// Function is partial and partiality is var
	VarPartial,
}

/// Get the totality of the functions in this model
pub fn analyse_totality(
	db: &dyn Thir,
	model: &Model,
	modes: &ModeAnalysis<'_>,
) -> ArenaMap<FunctionItem, Totality> {
	let ids = db.identifier_registry();
	let mut todo = Vec::new();
	let mut result = ArenaMap::with_capacity(model.functions_len());
	let mut reverse_dependencies: FxHashMap<FunctionId, FxHashMap<FunctionId, bool>> =
		FxHashMap::default();
	for (idx, f) in model.all_functions() {
		result.insert(idx, Totality::Total);
		let already_total = f.annotations().has(model, ids.annotations.promise_total)
			|| f.name().is_root(db)
			|| f.body().is_none();
		if !already_total {
			if let Some(body) = f.body() {
				let mut v = TotalityVisitor {
					db,
					modes,
					ids: db.identifier_registry(),
					tys: db.type_registry(),
					dependencies: FxHashMap::default(),
					totality: Totality::Total,
					in_var_ite: false,
				};
				v.visit_expression(model, body);
				if v.totality < Totality::VarPartial {
					for (f, var_ite) in v.dependencies {
						// Totality of this function needs to be updated when the totality of these dependencies change
						reverse_dependencies
							.entry(f)
							.or_default()
							.insert(idx, var_ite);
					}
				}
				if v.totality != Totality::Total {
					// Trigger updates of functions which depend on this
					result[idx] = v.totality;
					todo.push((idx, v.totality));
				}
			}
		}
	}

	while let Some((idx, mut totality)) = todo.pop() {
		if let Some(deps) = reverse_dependencies.get(&idx) {
			for (f, var_ite) in deps.iter() {
				if *var_ite && totality == Totality::ParPartial {
					totality = Totality::VarPartial;
				}
				if result[*f] < totality {
					todo.push((*f, totality));
					result[*f] = totality;
				}
			}
		}
	}
	result
}

struct TotalityVisitor<'a> {
	db: &'a dyn Thir,
	modes: &'a ModeAnalysis<'a>,
	ids: Arc<IdentifierRegistry>,
	tys: Arc<TypeRegistry>,
	dependencies: FxHashMap<FunctionId, bool>,
	totality: Totality,
	in_var_ite: bool,
}

impl<'a> Visitor<'a> for TotalityVisitor<'a> {
	fn visit_constraint(&mut self, model: &'a Model, c: ConstraintId) {
		if self.modes.get(model[c].expression()).is_root() {
			// If the constraint in the non-root version of the function is in root context,
			// this must actually be a statically true constraint
			return;
		}
		if model[c].expression().ty() == self.tys.var_bool {
			self.totality = Totality::VarPartial;
		} else if self.totality == Totality::Total {
			self.totality = Totality::ParPartial;
		}
		visit_constraint(self, model, c);
	}

	fn visit_array_comprehension(&mut self, model: &'a Model, c: &'a ArrayComprehension) {
		let var_condition = c.generators.iter().any(|g| {
			g.var_where(self.db)
				|| g.declarations().any(|d| {
					model[d]
						.annotations()
						.has(model, self.ids.annotations.mzn_var_where_clause)
				})
		});
		let prev_var_ite = self.in_var_ite;
		if var_condition {
			self.in_var_ite = true;
		}
		visit_array_comprehension(self, model, c);
		self.in_var_ite = prev_var_ite;
		if self.totality == Totality::ParPartial && var_condition {
			self.totality = Totality::VarPartial;
		}
	}

	fn visit_if_then_else(&mut self, model: &'a Model, ite: &'a IfThenElse) {
		let var_condition = ite.has_var_condition(self.db);
		let prev_var_ite = self.in_var_ite;
		if var_condition {
			self.in_var_ite = true;
		}
		visit_if_then_else(self, model, ite);
		self.in_var_ite = prev_var_ite;
		if self.totality == Totality::ParPartial && var_condition {
			self.totality = Totality::VarPartial;
		}
	}

	fn visit_callable(&mut self, model: &'a Model, callable: &'a Callable) {
		if let Callable::Function(f) = callable {
			let ty = model[*f].return_type();
			if ty != self.tys.var_bool && ty != self.tys.par_bool {
				// This function is partial if the called function is partial
				self.dependencies.insert(*f, self.in_var_ite);
			}
		}
		visit_callable(self, model, callable);
	}

	fn visit_expression(&mut self, model: &'a Model, expression: &'a Expression) {
		if self.totality < Totality::VarPartial
			&& (expression.ty() != self.tys.par_bool && expression.ty() != self.tys.var_bool
				|| self.modes.get_in_root_fn(expression) != self.modes.get(expression))
		{
			// If an expression is boolean, we still allow it to affect the totality result if
			// its context is different in the root version of the function as this indicates
			// that it would benefit from having a separate root version generated
			maybe_grow_stack(|| {
				visit_expression(self, model, expression);
			});
		}
	}
}

#[cfg(test)]
mod test {
	use std::sync::Arc;

	use expect_test::{expect, Expect};

	use super::{analyse_totality, Totality};
	use crate::{
		db::Inputs,
		file::{InputFile, InputLang},
		thir::{analyse::ModeAnalysis, db::Thir, pretty_print::PrettyPrinter},
		CompilerDatabase,
	};

	fn check_totality(program: &str, expected: Expect) {
		let mut db = CompilerDatabase::default();
		db.set_ignore_stdlib(true);
		db.set_input_files(Arc::new(vec![InputFile::String(
			program.to_owned(),
			InputLang::MiniZinc,
		)]));
		let model = db.model_thir().take();
		let modes = ModeAnalysis::analyse(&db, &model);
		let result = analyse_totality(&db, &model, &modes);
		let printer = PrettyPrinter::new(&db, &model);
		let mut pretty = String::new();
		for (f, _) in model.top_level_functions() {
			pretty.push_str(&printer.pretty_print_signature(f.into()));
			match result[f] {
				Totality::Total => pretty.push_str(" :: total"),
				Totality::ParPartial => pretty.push_str(" :: par_partial"),
				Totality::VarPartial => pretty.push_str(" :: var_partial"),
			}
			pretty.push_str(";\n");
		}
		expected.assert_eq(&pretty);
	}

	#[test]
	fn test_totality_analysis_basic() {
		check_totality(
			r#"
            function int: foo(int: x) = x;
            function int: bar(int: x) = let {
                constraint false;
            } in 1;
            test qux(int: x) = let {
                constraint false;
            } in true;
            "#,
			expect![[r#"
    function int: foo(int: x) :: total;
    function int: bar(int: x) :: par_partial;
    function bool: qux(int: x) :: par_partial;
"#]],
		);
	}

	#[test]
	fn test_totality_analysis_calls() {
		check_totality(
			r#"
            function int: foo(int: x) = bar(x);
            function int: bar(int: x) = let {
                constraint false;
            } in 1;
            "#,
			expect![[r#"
    function int: foo(int: x) :: par_partial;
    function int: bar(int: x) :: par_partial;
"#]],
		);
	}

	#[test]
	fn test_totality_analysis_recursive() {
		check_totality(
			r#"
            function int: foo(int: x) = bar(x);
            function int: bar(int: x) = foo(x);

            function int: f(int: x) = let {
                any: a = g(x);
                constraint false;
            } in 1;
            function int: g(int: x) = let {
                any: a = f(x);
            } in 1;
            "#,
			expect![[r#"
    function int: foo(int: x) :: total;
    function int: bar(int: x) :: total;
    function int: f(int: x) :: par_partial;
    function int: g(int: x) :: par_partial;
"#]],
		);
	}

	#[test]
	fn test_totality_analysis_comprehension() {
		check_totality(
			r#"
            function set of int: foo() = let {
				var bool: b;
				constraint b;
			} in {1, 3, 5};
			function array [int] of int: bar() = [1 | i in foo()];
            "#,
			expect![[r#"
    function set of int: foo() :: var_partial;
    function array [int] of int: bar() :: var_partial;
"#]],
		);
	}

	#[test]
	fn test_totality_analysis_ite() {
		check_totality(
			r#"
            function int: foo(bool: b) =
				if b then
					let { constraint false; } in 1
				else
					2
				endif;
            function var int: bar(var bool: b) =
				if b then
					let { constraint false; } in 1
				else
					2
				endif;
            "#,
			expect![[r#"
    function int: foo(bool: b) :: par_partial;
    function var int: bar(var bool: b) :: var_partial;
"#]],
		);
	}

	#[test]
	fn test_totality_analysis_ite_call() {
		check_totality(
			r#"
			function int: foo() = let {
				constraint false;
			} in 1;
            function var int: bar(var bool: b) =
				if b then
					foo()
				else
					2
				endif;
            "#,
			expect![[r#"
    function int: foo() :: par_partial;
    function var int: bar(var bool: b) :: var_partial;
"#]],
		);
	}

	#[test]
	fn test_totality_analysis_abort() {
		check_totality(
			r#"
			test abort(string: msg);
			test bar(int: x);
			function int: foo(int: x) = let {
				constraint if bar(x) then true else abort("foo") endif;
			} in x;
            "#,
			expect![[r#"
    function bool: abort(string: msg) :: total;
    function bool: bar(int: x) :: total;
    function int: foo(int: x) :: total;
"#]],
		);
	}
}
