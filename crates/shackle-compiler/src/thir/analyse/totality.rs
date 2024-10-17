//! Determines if a function is total
//!

use std::sync::Arc;

use rustc_hash::FxHashMap;

use super::{Mode, ModeAnalyser};
use crate::{
	constants::{IdentifierRegistry, TypeRegistry},
	thir::{
		db::Thir,
		traverse::{
			visit_array_comprehension, visit_callable, visit_expression, visit_if_then_else,
			Visitor,
		},
		ArrayComprehension, Callable, ConstraintId, Expression, FunctionId, FunctionItem,
		IfThenElse, Model,
	},
	utils::{arena::ArenaMap, maybe_grow_stack, refmap::RefMap},
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
pub fn analyse_totality(db: &dyn Thir, model: &Model) -> ArenaMap<FunctionItem, Totality> {
	let modes = ModeAnalyser::root_functions(db, model).run(db);
	let ids = db.identifier_registry();
	let mut todo = Vec::new();
	let mut result = ArenaMap::with_capacity(model.functions_len());
	let mut reverse_dependencies: FxHashMap<FunctionId, FxHashMap<FunctionId, bool>> =
		FxHashMap::default();
	for (idx, f) in model.all_functions() {
		result.insert(idx, Totality::Total);
		let already_total = f.annotations().has(model, ids.promise_total)
			|| f.name().is_root(db)
			|| f.body().is_none();
		if !already_total {
			if let Some(body) = f.body() {
				let mut v = TotalityVisitor {
					db,
					mode: &modes,
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
	mode: &'a RefMap<'a, Expression, Mode>,
	ids: Arc<IdentifierRegistry>,
	tys: Arc<TypeRegistry>,
	dependencies: FxHashMap<FunctionId, bool>,
	totality: Totality,
	in_var_ite: bool,
}

impl<'a> Visitor<'a> for TotalityVisitor<'a> {
	fn visit_constraint(&mut self, model: &'a Model, c: ConstraintId) {
		if model[c].expression().ty() == self.tys.var_bool {
			self.totality = Totality::VarPartial;
		} else if self.totality == Totality::Total {
			self.totality = Totality::ParPartial;
		}
	}

	fn visit_array_comprehension(&mut self, model: &'a Model, c: &'a ArrayComprehension) {
		let var_condition = c.generators.iter().any(|g| {
			g.var_where(self.db)
				|| g.declarations().any(|d| {
					model[d]
						.annotations()
						.has(model, self.ids.mzn_var_where_clause)
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
				|| self.mode[expression].is_root())
		{
			// If an expression is boolean, we still allow it to affect the totality result if
			// it's in root context because in that case we should still produce a root version
			// during totalisation
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
		thir::{db::Thir, pretty_print::PrettyPrinter},
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
		let result = analyse_totality(&db, &model);
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
    function bool: qux(int: x) :: total;
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
}
