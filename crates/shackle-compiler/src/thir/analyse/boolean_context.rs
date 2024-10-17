//! Boolean (partiality) context analysis
//!
//! Determines if an expression is in root context or not, allowing us to then
//! totalise any partial expressions in non-root context in a later phase.

use std::{ops::Not, sync::Arc};

use crate::{
	constants::{IdentifierRegistry, TypeRegistry},
	thir::{
		db::Thir, follow::follow_expression, Callable, DomainData, Expression, ExpressionData,
		Generator, ItemId, LetItem, Marker, Model, ResolvedIdentifier,
	},
	utils::refmap::RefMap,
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
pub struct ModeAnalyser<'a, T: Marker> {
	model: &'a Model<T>,
	todo: Vec<TodoItem<'a, T>>,
	map: RefMap<'a, Expression<T>, Mode>,
	ids: Arc<IdentifierRegistry>,
	tys: Arc<TypeRegistry>,
}

impl<'a, T: Marker> ModeAnalyser<'a, T> {
	/// Create a new analyser for the given model
	pub fn new(db: &dyn Thir, model: &'a Model<T>) -> Self {
		let mut analyser = ModeAnalyser {
			model,
			todo: Vec::new(),
			map: RefMap::default(),
			ids: db.identifier_registry(),
			tys: db.type_registry(),
		};
		let root = Mode::root();
		let non_root = Mode::NonRoot {
			conditional_depth: 0,
		};
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
								.has(model, analyser.ids.promise_total);
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
		analyser
	}

	/// Create a new analyser which considers functions to all be in root context
	///
	/// Note that only the function bodies are analysed
	pub fn root_functions(db: &dyn Thir, model: &'a Model<T>) -> Self {
		let mut analyser = ModeAnalyser {
			model,
			todo: Vec::new(),
			map: RefMap::default(),
			ids: db.identifier_registry(),
			tys: db.type_registry(),
		};
		let root = Mode::root();
		for (_, f) in model.top_level_functions() {
			if let Some(b) = f.body() {
				analyser.update(b, root, false);
			}
		}
		analyser
	}

	/// Run the context analysis
	pub fn run(mut self, db: &dyn Thir) -> RefMap<'a, Expression<T>, Mode> {
		while let Some(it) = self.todo.pop() {
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
								self.update(
									self.model[*assignment].definition().unwrap(),
									mode,
									false,
								);
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
							if self.model[*f].name() == self.ids.not && c.arguments.len() == 1 {
								self.update(&c.arguments[0], !it.mode, false);
								return;
							} else if (self.model[*f].name() == self.ids.conj && it.mode.is_root()
								|| self.model[*f].name() == self.ids.disj && it.mode.is_root_neg())
								&& c.arguments.len() == 2
								&& c.arguments[0].ty().is_bool(db.upcast())
								&& c.arguments[1].ty().is_bool(db.upcast())
							{
								self.update(&c.arguments[0], it.mode, false);
								self.update(&c.arguments[1], it.mode, false);
							} else if (self.model[*f].name() == self.ids.forall
								&& it.mode.is_root() || self.model[*f].name()
								== self.ids.exists && it
								.mode
								.is_root_neg()) && c.arguments.len() == 1
								&& c.arguments[0]
									.ty()
									.elem_ty(db.upcast())
									.unwrap_or(self.tys.error)
									.is_bool(db.upcast())
							{
								self.update(&c.arguments[0], it.mode, true);
								return;
							} else if self.model[*f].name() == self.ids.clause
								&& c.arguments.len() == 2
								&& it.mode.is_root_neg()
							{
								self.update(&c.arguments[0], it.mode, true);
								self.update(&c.arguments[1], !it.mode, true);
								return;
							} else if self.model[*f].name() == self.ids.assert
								&& c.arguments.len() > 1
							{
								self.update(
									&c.arguments[0],
									Mode::Root {
										result: true,
										conditional_depth: it.mode.conditional_depth(),
									},
									false,
								);
								for e in c.arguments[1..].iter() {
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
								return;
							} else if (self.model[*f].name()
								== self.ids.symmetry_breaking_constraint
								|| self.model[*f].name() == self.ids.redundant_constraint)
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
								self.update(self.model[*c].expression(), it.mode, false);
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
		self.map
	}

	/// Update the context of an expression.
	///
	/// Returns false if this context update has caused model failure.
	fn update(&mut self, expression: &'a Expression<T>, mode: Mode, update_bools: bool) -> bool {
		let mut ok = true;
		let mut inserted = false;
		let mut updated = false;
		let new_mode = *self.map.update_or_insert(
			expression,
			|m| match (*m, mode) {
				(Mode::Root { result: r1, .. }, Mode::Root { result: r2, .. }) => {
					// If these are opposite contexts then fail, otherwise keep as is
					if r1 != r2 {
						ok = false;
					}
				}
				(Mode::Root { .. }, _) => (), // Root stays root
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
						*m = Mode::Root {
							result,
							conditional_depth: d1,
						};
						updated = true;
					}
				}
				_ => (),
			},
			|| {
				inserted = true;
				mode
			},
		);
		// After update, subexpressions may need updating
		if updated || inserted {
			self.todo.push(TodoItem {
				expression,
				mode: new_mode,
				update_bools,
			});
		}
		ok
	}
}

#[cfg(test)]
mod test {
	use std::sync::Arc;

	use expect_test::{expect, Expect};

	use super::ModeAnalyser;
	use crate::{
		db::{FileReader, Inputs},
		file::{InputFile, InputLang},
		hir::ids::NodeRef,
		thir::{db::Thir, pretty_print::PrettyPrinter},
		CompilerDatabase,
	};

	fn check_bool_ctx(program: &str, expected: Expect, fn_root: bool) {
		let mut db = CompilerDatabase::default();
		db.set_input_files(Arc::new(vec![InputFile::String(
			program.to_owned(),
			InputLang::MiniZinc,
		)]));
		let model_ref = db.input_models()[0];
		let model = db.final_thir().unwrap();
		let analyser = if fn_root {
			ModeAnalyser::root_functions(&db, &model)
		} else {
			ModeAnalyser::new(&db, &model)
		};
		let result = analyser.run(&db);
		let mut printer = PrettyPrinter::new(&db, &model);
		printer.expression_annotator = Some(Box::new(|e| {
			result.get(e).map(|mode| {
				if mode.is_root() {
					"ctx_root"
				} else if mode.is_root_neg() {
					"ctx_root_neg"
				} else {
					"ctx_non_root"
				}
				.to_owned()
			})
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
    var bool: a :: ('output':: ctx_root);
    var bool: b :: ('output':: ctx_root);
    constraint 'forall<array [$T] of var bool>'([a:: ctx_root, b:: ctx_root]:: ctx_root):: ctx_root;
    var bool: c :: ('output':: ctx_root);
    var int: d :: ('output':: ctx_root);
    function var bool: foo(var bool: c, var int: d);
    constraint foo(c:: ctx_non_root, d:: ctx_root):: ctx_root;
    var bool: e :: ('output':: ctx_root);
    var bool: f :: ('output':: ctx_root);
    constraint 'not<var bool>'('\/<var bool, var bool>'(e:: ctx_root_neg, f:: ctx_root_neg):: ctx_root_neg):: ctx_root;
"#]],
			false,
		)
	}

	#[test]
	fn test_fn_ctx() {
		let program = r#"
			function var int: foo(var int: x, var int: y) = let {
				constraint x > y;
			} in x + y
		"#;
		check_bool_ctx(
			program,
			expect![[r#"
    function var int: foo(var int: x, var int: y) = let {
      constraint '><var $T, var $T>'(x:: ctx_non_root, y:: ctx_non_root):: ctx_non_root;
    } in '+<var int, var int>'(x:: ctx_non_root, y:: ctx_non_root):: ctx_non_root:: ctx_non_root;
"#]],
			false,
		);
		check_bool_ctx(
			program,
			expect![[r#"
    function var int: foo(var int: x, var int: y) = let {
      constraint '><var $T, var $T>'(x:: ctx_root, y:: ctx_root):: ctx_root;
    } in '+<var int, var int>'(x:: ctx_root, y:: ctx_root):: ctx_root:: ctx_root;
"#]],
			true,
		);
	}
}
