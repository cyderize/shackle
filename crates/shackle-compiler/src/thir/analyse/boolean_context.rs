//! Boolean (partiality) context analysis
//!
//! Determines if an expression is in root context or not, allowing us to then
//! totalise any partial expressions in non-root context in a later phase.

use std::{ops::Not, sync::Arc};

use rustc_hash::FxHashMap;

use crate::{
	constants::{IdentifierRegistry, TypeRegistry},
	hir::BooleanLiteral,
	thir::{
		db::Thir, follow::follow_expression, traverse::Visitor, Call, Callable, Case,
		DeclarationId, DomainData, Expression, ExpressionData, Generator, IfThenElse, ItemId, Let,
		LetItem, Marker, Model, ResolvedIdentifier,
	},
	utils::{maybe_grow_stack, refmap::RefMap},
};

/// The boolean context of an expression
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Mode {
	/// Boolean is fixed
	Root {
		/// True if root, false if root negative
		result: bool,
		/// Depth of the condition expression is inside
		conditional_depth: usize,
	},
	/// Positive, negative or mixed context
	NonRoot {
		/// Depth of the condition expression is inside
		conditional_depth: usize,
	},
}

impl Not for Mode {
	type Output = Mode;

	fn not(self) -> Self::Output {
		match self {
			Mode::Root {
				result,
				conditional_depth,
			} => Mode::Root {
				result: !result,
				conditional_depth,
			},
			_ => self,
		}
	}
}

impl Mode {
	/// Create a new root mode
	pub fn root() -> Self {
		Mode::Root {
			result: true,
			conditional_depth: 0,
		}
	}

	/// Whether this is root context
	pub fn is_root(&self) -> bool {
		matches!(self, Mode::Root { result: true, .. })
	}

	/// Whether this is root negative context
	pub fn is_root_neg(&self) -> bool {
		matches!(self, Mode::Root { result: false, .. })
	}

	/// Depth of the condition expression is inside
	pub fn conditional_depth(&self) -> usize {
		match self {
			Mode::Root {
				conditional_depth, ..
			}
			| Mode::NonRoot { conditional_depth } => *conditional_depth,
		}
	}

	/// Set depth of the condition expression is inside
	pub fn set_conditional_depth(&mut self, depth: usize) {
		match self {
			Mode::Root {
				conditional_depth, ..
			}
			| Mode::NonRoot { conditional_depth } => *conditional_depth = depth,
		}
	}

	/// Update this mode
	pub fn update(self, other: Mode) -> Mode {
		match (self, other) {
			(
				Mode::NonRoot {
					conditional_depth: d1,
				},
				Mode::Root {
					result,
					conditional_depth: d2,
				},
			) => {
				assert!(d1 <= d2, "Cannot refer to identifier defined more deeply");
				if d1 == d2 {
					return Mode::Root {
						result,
						conditional_depth: d1,
					};
				}
			}
			_ => (),
		}
		self
	}

	/// Get as a string which can be used as an annotation name for debugging
	pub fn as_str(&self) -> &'static str {
		if self.is_root() {
			"ctx_root"
		} else if self.is_root_neg() {
			"ctx_root_neg"
		} else {
			"ctx_non_root"
		}
	}
}

/// Boolean context analysis
///
/// Determines whether expression are in root or non-root context.
/// Also determines the context of subexpressions of function bodies assuming
/// that the function is in root context.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ModeAnalysis<'a, T: Marker = ()> {
	map: RefMap<'a, Expression<T>, Mode>,
	root_map: RefMap<'a, Expression<T>, Mode>,
}

impl<'a, T: Marker> ModeAnalysis<'a, T> {
	/// Run context analysis
	pub fn analyse(db: &dyn Thir, model: &'a Model<T>) -> ModeAnalysis<'a, T> {
		ModeAnalyser::run(db, model)
	}

	/// Get the context of the given expression
	pub fn get(&self, e: &'a Expression<T>) -> Mode {
		self.map[&e]
	}

	/// Get the context of the given expression assuming the containing function is root
	pub fn get_in_root_fn(&self, e: &'a Expression<T>) -> Mode {
		self.root_map
			.get(e)
			.copied()
			.unwrap_or_else(|| self.map[&e])
	}

	/// Update the mode for an expression and return the new mode if it was updated
	fn update(&mut self, e: &'a Expression<T>, mode: Mode) -> Option<Mode> {
		let mut inserted = false;
		let mut updated = false;
		let result = self.map.update_or_insert(
			e,
			|m| {
				let new_mode = m.update(mode);
				updated = *m != new_mode;
				*m = new_mode;
			},
			|| {
				inserted = true;
				mode
			},
		);
		if inserted || updated {
			Some(*result)
		} else {
			None
		}
	}

	/// Update the mode for an expression in the root fn map and return the new mode if it was updated
	fn update_in_root_fn(&mut self, e: &'a Expression<T>, mode: Mode) -> Option<Mode> {
		let orig_mode = self.get_in_root_fn(e);
		let new_mode = orig_mode.update(mode);
		let changed = orig_mode != new_mode;
		if changed {
			self.root_map.insert(e, new_mode);
			Some(new_mode)
		} else {
			None
		}
	}
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct TodoItem<'a, T: Marker> {
	/// The expression
	expression: &'a Expression<T>,
	/// The mode to be joined for this expression
	mode: Mode,
	/// True if this is an array where we need to propagate the context to the contained boolean values
	///
	/// E.g. for the argument of forall in root, or exists in root negative
	update_bools: bool,
}

/// Boolean context analyser
struct ModeAnalyser<'a, T: Marker> {
	model: &'a Model<T>,
	todo: Vec<TodoItem<'a, T>>,
	result: ModeAnalysis<'a, T>,
	ids: Arc<IdentifierRegistry>,
	tys: Arc<TypeRegistry>,
	booleans: FxHashMap<DeclarationId<T>, bool>,
	in_root_mode: bool,
}

impl<'a, T: Marker> ModeAnalyser<'a, T> {
	fn run(db: &dyn Thir, model: &'a Model<T>) -> ModeAnalysis<'a, T> {
		let mut analyser = ModeAnalyser {
			model,
			todo: Vec::new(),
			result: ModeAnalysis::default(),
			ids: db.identifier_registry(),
			tys: db.type_registry(),
			booleans: FxHashMap::default(),
			in_root_mode: false,
		};
		let root = Mode::root();
		let non_root = Mode::NonRoot {
			conditional_depth: 0,
		};
		// Analyse model items
		for item in model.top_level_items() {
			match item {
				ItemId::Annotation(_) => (),
				ItemId::Constraint(c) => {
					for ann in model[c].annotations().iter() {
						analyser.update(ann, root, false);
					}
					analyser.update(model[c].expression(), root, false);
				}
				ItemId::Declaration(d) => {
					for ann in model[d].annotations().iter() {
						analyser.update(ann, root, false);
					}
					for dom in model[d].domain().walk() {
						if let DomainData::Bounded(b) = &**dom {
							analyser.update(&b, root, false);
						}
					}
					if let Some(def) = model[d].definition() {
						if def.ty().is_bool(db.upcast()) {
							analyser.update(def, non_root, false);
						} else {
							analyser.update(def, root, false);
						}
					}
				}
				ItemId::Function(f) => {
					for ann in model[f].annotations().iter() {
						analyser.update(ann, root, false);
					}
					for p in model[f].parameters() {
						for ann in model[*p].annotations().iter() {
							analyser.update(ann, root, false);
						}
					}
					if let Some(b) = model[f].body() {
						let is_root = model[f].name().is_root(db)
							|| model[f]
								.annotations()
								.has(model, analyser.ids.annotations.promise_total);
						analyser.update(b, if is_root { root } else { non_root }, false);
					}
				}
				ItemId::Solve => {
					let solve = model.solve().unwrap();
					for ann in solve.annotations().iter() {
						analyser.update(ann, root, false);
					}
				}
				ItemId::Enumeration(_) | ItemId::Output(_) => unreachable!(),
			}
		}

		while let Some(it) = analyser.todo.pop() {
			analyser.run_iteration(db, &it);
		}

		// Analysis as if all functions are root and store separately
		analyser.in_root_mode = true;
		for (_, f) in model.top_level_functions() {
			if let Some(b) = f.body() {
				analyser.update(b, root, false);
			}
		}

		while let Some(it) = analyser.todo.pop() {
			analyser.run_iteration(db, &it);
		}

		analyser.result
	}

	fn run_iteration(&mut self, db: &dyn Thir, it: &TodoItem<'a, T>) {
		assert!(
			!it.update_bools
				|| it
					.expression
					.ty()
					.elem_ty(db.upcast())
					.unwrap_or(self.tys.error)
					.is_bool(db.upcast()),
			"Set update bools for non array of bool expression"
		);
		for ann in it.expression.annotations().iter() {
			self.update(ann, it.mode, false);
		}

		match &**it.expression {
			ExpressionData::Absent
			| ExpressionData::BooleanLiteral(_)
			| ExpressionData::IntegerLiteral(_)
			| ExpressionData::FloatLiteral(_)
			| ExpressionData::StringLiteral(_)
			| ExpressionData::Infinity => (), // No children to update
			ExpressionData::Identifier(i) => {
				if let ResolvedIdentifier::Declaration(d) = i {
					if let Some(def) = self.model[*d].definition() {
						self.update(def, it.mode, it.update_bools);
					}
				}
			}
			ExpressionData::ArrayLiteral(al) => {
				if it
					.expression
					.ty()
					.elem_ty(db.upcast())
					.unwrap()
					.is_bool(db.upcast())
				{
					if it.update_bools {
						// Argument to a logical predicate
						for e in al.iter() {
							self.update(e, it.mode, false);
						}
					} else {
						let non_root = Mode::NonRoot {
							conditional_depth: it.mode.conditional_depth(),
						};
						for e in al.iter() {
							self.update(e, non_root, false);
						}
					}
				} else {
					for e in al.iter() {
						self.update(e, it.mode, false);
					}
				}
			}
			ExpressionData::SetLiteral(sl) => {
				if it
					.expression
					.ty()
					.elem_ty(db.upcast())
					.unwrap()
					.is_bool(db.upcast())
				{
					let non_root = Mode::NonRoot {
						conditional_depth: it.mode.conditional_depth(),
					};
					for e in sl.iter() {
						self.update(e, non_root, false);
					}
				} else {
					for e in sl.iter() {
						self.update(e, it.mode, false);
					}
				}
			}
			ExpressionData::TupleLiteral(tl) => {
				for e in tl.iter() {
					if e.ty().is_bool(db.upcast()) {
						self.update(
							e,
							Mode::NonRoot {
								conditional_depth: it.mode.conditional_depth(),
							},
							false,
						);
					} else {
						self.update(e, it.mode, false);
					}
				}
			}
			ExpressionData::ArrayComprehension(c) => {
				for g in c.generators.iter() {
					match g {
						Generator::Iterator { collection, .. } => {
							self.update(collection, it.mode, false);
						}
						Generator::Assignment { assignment, .. } => {
							let mode = if self.model[*assignment].ty().is_bool(db.upcast()) {
								Mode::NonRoot {
									conditional_depth: it.mode.conditional_depth(),
								}
							} else {
								it.mode
							};
							self.update(self.model[*assignment].definition().unwrap(), mode, false);
						}
					}
					if let Some(w) = g.where_clause() {
						self.update(
							w,
							Mode::NonRoot {
								conditional_depth: it.mode.conditional_depth(),
							},
							false,
						);
					}
				}
				if let Some(e) = &c.indices {
					self.update(e, it.mode, false);
				}
				if it
					.expression
					.ty()
					.elem_ty(db.upcast())
					.unwrap()
					.is_bool(db.upcast())
				{
					if it.update_bools {
						self.update(
							&c.template,
							Mode::NonRoot {
								conditional_depth: it.mode.conditional_depth(),
							},
							false,
						);
					} else {
						self.update(&c.template, it.mode, false);
					}
				} else {
					self.update(&c.template, it.mode, false);
				}
			}
			ExpressionData::TupleAccess(ta) => {
				for e in follow_expression(self.model, &it.expression) {
					self.update(e, it.mode, false);
				}
				self.update(&ta.tuple, it.mode, false);
			}
			ExpressionData::IfThenElse(ite) => {
				let mut mode = it.mode;
				mode.set_conditional_depth(mode.conditional_depth() + 1);
				for b in ite.branches.iter() {
					self.update(
						&b.condition,
						Mode::NonRoot {
							conditional_depth: it.mode.conditional_depth(),
						},
						false,
					);
					if b.var_condition(db) {
						mode = Mode::NonRoot {
							conditional_depth: mode.conditional_depth(),
						};
					}
					self.update(&b.result, mode, it.update_bools);
				}
				self.update(&ite.else_result, mode, it.update_bools);
			}
			ExpressionData::Case(_) => todo!(),
			ExpressionData::Call(c) => (|| {
				match &c.function {
					Callable::Function(f) => {
						if (self.model[*f].name() == self.ids.builtins.mzn_not_par
							|| self.model[*f].name() == self.ids.builtins.mzn_not_var)
							&& c.arguments.len() == 1
						{
							self.update(&c.arguments[0], !it.mode, false);
							return;
						} else if ((self.model[*f].name() == self.ids.builtins.mzn_and_par
							|| self.model[*f].name() == self.ids.builtins.mzn_and_var)
							&& it.mode.is_root() || (self.model[*f].name()
							== self.ids.builtins.mzn_or_par
							|| self.model[*f].name() == self.ids.builtins.mzn_or_var)
							&& it.mode.is_root_neg())
							&& c.arguments.len() == 2
							&& c.arguments[0].ty().is_bool(db.upcast())
							&& c.arguments[1].ty().is_bool(db.upcast())
						{
							self.update(&c.arguments[0], it.mode, false);
							self.update(&c.arguments[1], it.mode, false);
						} else if ((self.model[*f].name() == self.ids.builtins.mzn_forall_par
							|| self.model[*f].name() == self.ids.builtins.mzn_forall_var)
							&& it.mode.is_root() || (self.model[*f].name()
							== self.ids.builtins.mzn_exists_par
							|| self.model[*f].name() == self.ids.builtins.mzn_exists_var)
							&& it.mode.is_root_neg())
							&& c.arguments.len() == 1
							&& c.arguments[0]
								.ty()
								.elem_ty(db.upcast())
								.unwrap_or(self.tys.error)
								.is_bool(db.upcast())
						{
							self.update(&c.arguments[0], it.mode, true);
							return;
						} else if (self.model[*f].name() == self.ids.builtins.mzn_clause_par
							|| self.model[*f].name() == self.ids.builtins.mzn_clause_var)
							&& c.arguments.len() == 2
							&& it.mode.is_root_neg()
						{
							self.update(&c.arguments[0], it.mode, true);
							self.update(&c.arguments[1], !it.mode, true);
							return;
						} else if (self.model[*f].name()
							== self.ids.functions.symmetry_breaking_constraint
							|| self.model[*f].name() == self.ids.functions.redundant_constraint)
							&& c.arguments.len() == 1
						{
							self.update(&c.arguments[0], it.mode, false);
							return;
						}
					}
					Callable::Expression(e) => {
						self.update(&e, it.mode, false);
					}
					Callable::Annotation(_) | Callable::AnnotationDestructure(_) => (),
					Callable::EnumConstructor(_) | Callable::EnumDestructor(_) => {
						unreachable!()
					}
				}
				for e in c.arguments.iter() {
					if e.ty().is_bool(db.upcast()) {
						self.update(
							e,
							Mode::NonRoot {
								conditional_depth: it.mode.conditional_depth(),
							},
							false,
						);
					} else {
						self.update(e, it.mode, false);
					}
				}
			})(),
			ExpressionData::Let(l) => {
				for item in l.items.iter() {
					match item {
						LetItem::Constraint(c) => {
							for ann in self.model[*c].annotations().iter() {
								self.update(ann, it.mode, false);
							}

							let in_expression = self.model[*c].expression();
							let mode = if BooleanVisitor::is_true(
								db,
								self.model,
								&mut self.booleans,
								in_expression,
							) {
								Mode::Root {
									result: true,
									conditional_depth: it.mode.conditional_depth(),
								}
							} else {
								it.mode
							};

							self.update(in_expression, mode, false);
						}
						LetItem::Declaration(d) => {
							for ann in self.model[*d].annotations().iter() {
								self.update(ann, it.mode, false);
							}
							for dom in self.model[*d].domain().walk() {
								if let DomainData::Bounded(b) = &**dom {
									self.update(&b, it.mode, false);
								}
							}
							if let Some(def) = self.model[*d].definition() {
								if self.model[*d].ty().is_bool(db.upcast()) {
									self.update(
										def,
										Mode::NonRoot {
											conditional_depth: it.mode.conditional_depth(),
										},
										false,
									);
								} else {
									self.update(def, it.mode, false);
								}
							}
						}
					}
				}
				self.update(&l.in_expression, it.mode, false);
			}
			ExpressionData::Lambda(_) => todo!(),
			ExpressionData::RecordAccess(_)
			| ExpressionData::SetComprehension(_)
			| ExpressionData::RecordLiteral(_) => {
				unreachable!()
			}
		}
	}

	/// Update the context of an expression.
	///
	/// Returns false if this context update has caused model failure.
	fn update(&mut self, expression: &'a Expression<T>, mode: Mode, update_bools: bool) {
		let new_mode = if self.in_root_mode {
			self.result.update_in_root_fn(expression, mode)
		} else {
			self.result.update(expression, mode)
		};

		// After update, subexpressions may need updating
		if let Some(m) = new_mode {
			self.todo.push(TodoItem {
				expression,
				mode: m,
				update_bools,
			});
		}
	}
}

struct BooleanVisitor<'a, T: Marker> {
	db: &'a dyn Thir,
	ids: Arc<IdentifierRegistry>,
	decls: &'a mut FxHashMap<DeclarationId<T>, bool>,
	is_true: bool,
}

impl<'a, T: Marker> BooleanVisitor<'a, T> {
	/// Whether or not the given expression is statically known to be true
	fn is_true(
		db: &'a dyn Thir,
		model: &'a Model<T>,
		decls: &'a mut FxHashMap<DeclarationId<T>, bool>,
		expression: &'a Expression<T>,
	) -> bool {
		let mut visitor = Self {
			db,
			decls,
			ids: db.identifier_registry(),
			is_true: true,
		};
		visitor.visit_expression(model, expression);
		visitor.is_true
	}
}

impl<'a, T: Marker> Visitor<'a, T> for BooleanVisitor<'a, T> {
	fn visit_expression(&mut self, model: &'a Model<T>, expression: &'a Expression<T>) {
		maybe_grow_stack(|| {
			match &**expression {
				ExpressionData::BooleanLiteral(b) => self.visit_boolean(model, b),
				ExpressionData::Identifier(i) => self.visit_identifier(model, i),
				ExpressionData::IfThenElse(ite) => self.visit_if_then_else(model, ite),
				ExpressionData::TupleAccess(_) => {
					if let Some(e) = follow_expression(model, expression).next() {
						self.visit_expression(model, e);
					} else {
						self.is_true = false
					}
				}
				ExpressionData::Case(c) => self.visit_case(model, c),
				ExpressionData::Call(c) => self.visit_call(model, c),
				ExpressionData::Let(l) => self.visit_let(model, l),
				_ => self.is_true = false,
			};
		});
	}

	fn visit_boolean(&mut self, _model: &'a Model<T>, b: &'a BooleanLiteral) {
		self.is_true &= b.0;
	}

	fn visit_identifier(&mut self, model: &'a Model<T>, identifier: &'a ResolvedIdentifier<T>) {
		if let ResolvedIdentifier::Declaration(d) = identifier {
			if let Some(t) = self.decls.get(d) {
				self.is_true &= *t;
			} else if let Some(def) = model[*d].definition() {
				// Cache result in case identifier used multiple times
				let is_true = BooleanVisitor::is_true(self.db, model, self.decls, def);
				self.decls.insert(*d, is_true);
				self.is_true &= is_true;
			} else {
				// No RHS, so unknown
				self.decls.insert(*d, false);
				self.is_true = false;
			}
		}
	}

	fn visit_call(&mut self, model: &'a Model<T>, call: &'a Call<T>) {
		if let Callable::Function(f) = &call.function {
			if model[*f].name() == self.ids.builtins.mzn_abort {
				return;
			}
		}
		self.is_true = false;
	}

	fn visit_let(&mut self, model: &'a Model<T>, l: &'a Let<T>) {
		for item in l.items.iter() {
			match item {
				LetItem::Declaration(_) => {
					// Do not know if this expression is fixed to true
					self.is_true = false;
					return;
				}
				LetItem::Constraint(c) => {
					self.visit_expression(model, model[*c].expression());
					if !self.is_true {
						return;
					}
				}
			}
		}
		self.visit_expression(model, &l.in_expression);
	}

	fn visit_if_then_else(&mut self, model: &'a Model<T>, ite: &'a IfThenElse<T>) {
		for branch in ite.branches.iter() {
			self.visit_expression(model, &branch.result);
			if !self.is_true {
				return;
			}
		}
		self.visit_expression(model, &ite.else_result);
	}

	fn visit_case(&mut self, model: &'a Model<T>, c: &'a Case<T>) {
		for branch in c.branches.iter() {
			self.visit_expression(model, &branch.result);
			if !self.is_true {
				return;
			}
		}
	}
}

#[cfg(test)]
mod test {
	use std::sync::Arc;

	use expect_test::{expect, Expect};
	use shackle_syntax::InputLang;

	use super::ModeAnalysis;
	use crate::{
		db::{FileReader, Inputs},
		file::InputFile,
		hir::ids::NodeRef,
		thir::{db::Thir, pretty_print::PrettyPrinter},
		CompilerDatabase,
	};

	fn check_bool_ctx(program: &str, expected: Expect, fn_root: bool) {
		let mut db = CompilerDatabase::default();
		db.set_ignore_stdlib(true);
		db.set_input_files(Arc::new(vec![InputFile::String(
			program.to_owned(),
			InputLang::MiniZinc,
		)]));
		let model_ref = db.input_models()[0];
		let model = db.model_thir().take();
		let result = ModeAnalysis::analyse(&db, &model);
		let mut printer = PrettyPrinter::new(&db, &model);
		printer.expression_annotator = Some(Box::new(|e| {
			if fn_root {
				Some(result.get_in_root_fn(e).as_str().to_owned())
			} else {
				Some(result.get(e).as_str().to_owned())
			}
		}));
		let to_print = model
			.top_level_items()
			.filter(|it| match model.item_origin(*it).node() {
				Some(NodeRef::Item(item)) => item.model_ref(&db) == model_ref,
				Some(NodeRef::Entity(entity)) => entity.item(&db).model_ref(&db) == model_ref,
				Some(NodeRef::Model(m)) => m == model_ref,
				None => true,
			});
		let mut pretty = String::new();
		for item in to_print {
			pretty.push_str(&printer.pretty_print_item(item));
			pretty.push_str(";\n");
		}
		expected.assert_eq(&pretty);
	}

	#[test]
	fn test_bool_ctx() {
		check_bool_ctx(
			r#"
			predicate forall(array [int] of var bool: b);
			predicate '\/'(var bool: x, var bool: y);
			predicate 'not'(var bool: x);
			var bool: a;
			var bool: b;
			constraint forall([a, b]);
			var bool: c;
			var int: d;
			function var bool: foo(var bool: c, var int: d);
			constraint foo(c, d);
			var bool: e;
			var bool: f;
			constraint not (e \/ f);
			var int: g = let {
				var int: h = 1;
				var bool: p = true;
				var bool: q = true;
				constraint q;
			} in h;
			"#,
			expect![[r#"
    function var bool: forall(array [int] of var bool: b);
    function var bool: '\/'(var bool: x, var bool: y);
    function var bool: 'not'(var bool: x);
    var bool: a;
    var bool: b;
    constraint forall([a:: ctx_root, b:: ctx_root]:: ctx_root):: ctx_root;
    var bool: c;
    var int: d;
    function var bool: foo(var bool: c, var int: d);
    constraint foo(c:: ctx_non_root, d:: ctx_root):: ctx_root;
    var bool: e;
    var bool: f;
    constraint 'not'('\/'(e:: ctx_root_neg, f:: ctx_root_neg):: ctx_root_neg):: ctx_root;
    var int: g = let {
      var int: h = 1:: ctx_root;
      var bool: p = true:: ctx_non_root;
      var bool: q = true:: ctx_root;
      constraint q:: ctx_root;
    } in h:: ctx_root:: ctx_root;
"#]],
			false,
		)
	}

	#[test]
	fn test_fn_ctx() {
		let program = r#"
			predicate '>'(var int: x, var int: y);
			function var int: '+'(var int: x, var int: y);
			function var int: foo(var int: x, var int: y) = let {
				constraint x > y;
			} in x + y
		"#;
		check_bool_ctx(
			program,
			expect![[r#"
    function var bool: '>'(var int: x, var int: y);
    function var int: '+'(var int: x, var int: y);
    function var int: foo(var int: x, var int: y) = let {
      constraint '>'(x:: ctx_non_root, y:: ctx_non_root):: ctx_non_root;
    } in '+'(x:: ctx_non_root, y:: ctx_non_root):: ctx_non_root:: ctx_non_root;
"#]],
			false,
		);
		check_bool_ctx(
			program,
			expect![[r#"
    function var bool: '>'(var int: x, var int: y);
    function var int: '+'(var int: x, var int: y);
    function var int: foo(var int: x, var int: y) = let {
      constraint '>'(x:: ctx_root, y:: ctx_root):: ctx_root;
    } in '+'(x:: ctx_root, y:: ctx_root):: ctx_root:: ctx_root;
"#]],
			true,
		);
	}

	#[test]
	fn test_bool_ctx_let() {
		check_bool_ctx(
			r#"
            function set of int: foo() = let {
				var bool: b;
				constraint b;
			} in {1, 3, 5};
		"#,
			expect![[r#"
    function set of int: foo() = let {
      var bool: b;
      constraint b:: ctx_non_root;
    } in {1:: ctx_non_root, 3:: ctx_non_root, 5:: ctx_non_root}:: ctx_non_root:: ctx_non_root;
"#]],
			false,
		);
	}

	#[test]
	fn test_bool_ctx_abort() {
		let program = r#"
			test abort(string: msg);
			test bar(int: x);
			function int: foo(int: x) = let {
				constraint if bar(x) then abort("foo") endif;
			} in x;
		"#;
		check_bool_ctx(
			program,
			expect![[r#"
    function bool: abort(string: msg);
    function bool: bar(int: x);
    function int: foo(int: x) = let {
      constraint if bar(x:: ctx_non_root):: ctx_non_root then abort("foo":: ctx_root):: ctx_root else true:: ctx_root endif:: ctx_root;
    } in x:: ctx_non_root:: ctx_non_root;
"#]],
			false,
		);
		check_bool_ctx(
			program,
			expect![[r#"
    function bool: abort(string: msg);
    function bool: bar(int: x);
    function int: foo(int: x) = let {
      constraint if bar(x:: ctx_non_root):: ctx_non_root then abort("foo":: ctx_root):: ctx_root else true:: ctx_root endif:: ctx_root;
    } in x:: ctx_root:: ctx_root;
"#]],
			true,
		);
	}
}
