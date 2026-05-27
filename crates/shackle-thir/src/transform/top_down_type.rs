//! Top-down resolution of types.
//! - Assigns a real type to literals <>, [] and {}
//! - Make optional coercions explicit using let expressions to give them concrete types

use shackle_diagnostics::Result;
use shackle_hir::OptType;
use shackle_ty::{FunctionType, PolymorphicFunctionType, Ty, TyData, registry::TypeRegistry};
use shackle_utils::{maybe_grow_stack, refmap::RefMap};

use crate::{
	Callable, Db, Declaration, DeclarationId, Domain, EnumConstructorKind, Expression,
	ExpressionData, FunctionId, Item, Let, LetItem, Marker, Model,
	traverse::{Folder, ReplacementMap, add_declaration, add_function, fold_expression},
};

/// Create a let wrapping an expression into a declaration.
///
/// Helper to to make opt coercions explicit.
pub fn add_coercion<'db, T: Marker>(
	db: &'db dyn Db,
	model: &mut Model<'db, T>,
	ty: Ty<'db>,
	expression: Expression<'db, T>,
) -> Expression<'db, T> {
	if !expression.ty().contains_bottom(db) && !is_opt_subtype_of(db, expression.ty(), ty) {
		return expression;
	}
	let origin = expression.origin();
	let mut declaration = Declaration::new(false, Domain::unbounded(db, origin, ty));
	declaration.set_definition(expression);
	declaration.validate(db);
	let idx = model.add_declaration(Item::new(declaration, origin));
	Expression::new(
		db,
		model,
		origin,
		Let {
			items: vec![LetItem::Declaration(idx)],
			in_expression: Box::new(Expression::new(db, model, origin, idx)),
		},
	)
}

fn replace_bottom<'db>(db: &'db dyn Db, ty: Ty<'db>) -> Ty<'db> {
	// Replace bottom with int when it doesn't matter what type is used
	match ty.lookup(db) {
		TyData::Bottom(opt) => Ty::par_int(db).with_opt(db, *opt),
		TyData::Array { opt, dim, element } => {
			Ty::array(db, replace_bottom(db, *dim), replace_bottom(db, *element))
				.unwrap()
				.with_opt(db, *opt)
		}
		TyData::Set(inst, opt, elem) => Ty::par_set(db, replace_bottom(db, *elem))
			.unwrap()
			.with_inst(db, *inst)
			.unwrap()
			.with_opt(db, *opt),
		TyData::Tuple(opt, fs) => {
			Ty::tuple(db, fs.iter().map(|f| replace_bottom(db, *f))).with_opt(db, *opt)
		}
		TyData::Record(opt, fs) => {
			Ty::record(db, fs.iter().map(|(i, f)| (*i, replace_bottom(db, *f)))).with_opt(db, *opt)
		}
		TyData::Function(opt, f) => Ty::function(
			db,
			FunctionType {
				params: f.params.iter().map(|p| replace_bottom(db, *p)).collect(),
				return_type: replace_bottom(db, f.return_type),
			},
		)
		.with_opt(db, *opt),
		_ => ty,
	}
}

fn is_opt_subtype_of<'db>(db: &'db dyn Db, a: Ty<'db>, b: Ty<'db>) -> bool {
	if a.opt(db) == Some(OptType::NonOpt) && b.opt(db) == Some(OptType::Opt) {
		return true;
	}
	match (a.lookup(db), b.lookup(db)) {
		(TyData::Array { element: e1, .. }, TyData::Array { element: e2, .. })
		| (TyData::Set(_, _, e1), TyData::Set(_, _, e2)) => is_opt_subtype_of(db, *e1, *e2),
		(TyData::Tuple(_, f1), TyData::Tuple(_, f2)) => f1
			.iter()
			.zip(f2.iter())
			.any(|(t1, t2)| is_opt_subtype_of(db, *t1, *t2)),
		(TyData::Record(_, f1), TyData::Record(_, f2)) => f1
			.iter()
			.zip(f2.iter())
			.any(|((_, t1), (_, t2))| is_opt_subtype_of(db, *t1, *t2)),
		(TyData::Function(_, f1), TyData::Function(_, f2)) => {
			f1.params
				.iter()
				.zip(f2.params.iter())
				.any(|(t1, t2)| is_opt_subtype_of(db, *t2, *t1))
				|| is_opt_subtype_of(db, f1.return_type, f2.return_type)
		}
		_ => false,
	}
}

#[derive(Default)]
struct TopDownTyper<'a, 'db, Dst: Marker, Src: Marker = ()> {
	types: RefMap<'a, Expression<'db, Src>, Ty<'db>>,
	result: Model<'db, Dst>,
	replacement_map: ReplacementMap<'db, Dst, Src>,
}

impl<'a, 'db, Src: Marker, Dst: Marker> Folder<'a, 'db, Dst, Src>
	for TopDownTyper<'a, 'db, Dst, Src>
{
	fn model(&mut self) -> &mut Model<'db, Dst> {
		&mut self.result
	}

	fn replacement_map(&mut self) -> &mut ReplacementMap<'db, Dst, Src> {
		&mut self.replacement_map
	}

	fn add_declaration(
		&mut self,
		db: &'db dyn Db,
		model: &'a Model<'db, Src>,
		d: DeclarationId<'db, Src>,
	) {
		if let Some(def) = model[d].definition() {
			self.insert(def, model[d].ty());
		}
		let _ = add_declaration(self, db, model, d);
	}

	fn add_function(
		&mut self,
		db: &'db dyn Db,
		model: &'a Model<'db, Src>,
		f: FunctionId<'db, Src>,
	) {
		if let Some(body) = model[f].body() {
			self.insert(body, model[f].return_type());
		}
		let _ = add_function(self, db, model, f);
	}

	fn fold_declaration(
		&mut self,
		db: &'db dyn Db,
		model: &'a Model<'db, Src>,
		d: &'a Declaration<'db, Src>,
	) -> Declaration<'db, Dst> {
		let mut declaration =
			Declaration::new(d.top_level(), self.fold_domain(db, model, d.domain()));
		if let Some(name) = d.name() {
			declaration.set_name(name);
		}
		declaration.annotations_mut().extend(
			d.annotations()
				.iter()
				.map(|ann| self.fold_expression(db, model, ann)),
		);
		if let Some(def) = d.definition() {
			self.insert(def, d.ty());
			let _ = self.propagate_ty(db, model, def);
			let def = fold_expression(self, db, model, def);
			declaration.set_definition(def);
			declaration.validate(db);
		}
		declaration
	}

	fn fold_expression(
		&mut self,
		db: &'db dyn Db,
		model: &'a Model<'db, Src>,
		expression: &'a Expression<'db, Src>,
	) -> Expression<'db, Dst> {
		maybe_grow_stack(|| {
			let enter_expression = self.propagate_ty(db, model, expression);
			let folded = fold_expression(self, db, model, expression);
			if !enter_expression && let Some(ty) = self.get(expression) {
				return add_coercion(db, &mut self.result, ty, folded);
			}
			folded
		})
	}
}

impl<'a, 'db, Src: Marker, Dst: Marker> TopDownTyper<'a, 'db, Dst, Src> {
	fn insert(&mut self, e: &'a Expression<'db, Src>, ty: Ty<'db>) {
		let _ = self.types.insert(e, ty);
	}

	fn extend(&mut self, iter: impl Iterator<Item = (&'a Expression<'db, Src>, Ty<'db>)>) {
		self.types.extend(iter);
	}

	fn get(&self, e: &'a Expression<'db, Src>) -> Option<Ty<'db>> {
		self.types.get(e).copied()
	}

	fn propagate_ty(
		&mut self,
		db: &'db dyn Db,
		model: &'a Model<'db, Src>,
		expression: &'a Expression<'db, Src>,
	) -> bool {
		let ty = self.get(expression).unwrap_or_else(|| expression.ty());
		match &**expression {
			ExpressionData::ArrayLiteral(al) => {
				if al.is_empty() {
					return false;
				}
				self.extend(al.iter().map(|e| (e, ty.elem_ty(db).unwrap())))
			}
			ExpressionData::ArrayComprehension(c) => {
				self.insert(&*c.template, ty.elem_ty(db).unwrap())
			}
			ExpressionData::SetLiteral(sl) => {
				if sl.is_empty() {
					return false;
				}
				self.extend(sl.iter().map(|e| {
					(
						e,
						ty.elem_ty(db)
							.unwrap()
							.with_inst(db, ty.inst(db).unwrap())
							.unwrap(),
					)
				}))
			}
			ExpressionData::SetComprehension(c) => self.insert(
				&*c.template,
				ty.elem_ty(db)
					.unwrap()
					.with_inst(db, ty.inst(db).unwrap())
					.unwrap(),
			),
			ExpressionData::IfThenElse(ite) => self.extend(
				ite.branches
					.iter()
					.map(|b| &b.result)
					.chain([&*ite.else_result])
					.map(|e| (e, ty)),
			),
			ExpressionData::Case(c) => self.extend(c.branches.iter().map(|b| (&b.result, ty))),
			ExpressionData::TupleLiteral(tl) => self.extend(tl.iter().zip(ty.fields(db).unwrap())),
			ExpressionData::Let(l) => self.insert(&l.in_expression, ty),
			ExpressionData::Call(c) => {
				let params = match &c.function {
					Callable::Annotation(a) => model[*a]
						.parameters
						.as_ref()
						.unwrap()
						.iter()
						.map(|p| model[*p].ty())
						.collect::<Vec<_>>(),
					Callable::AnnotationDestructure(_) => vec![TypeRegistry::lookup(db).ann],
					Callable::EnumConstructor(e) => model[*e]
						.parameters
						.as_ref()
						.unwrap()
						.iter()
						.map(|p| model[*p].ty())
						.collect::<Vec<_>>(),
					Callable::EnumDestructor(_) => {
						let (_, ty) = EnumConstructorKind::from_ty(db, c.arguments[0].ty());
						vec![ty]
					}
					Callable::Expression(e) => e.ty().function_params(db).unwrap(),
					Callable::Function(f) => {
						if model[*f].is_polymorphic() {
							let bottom_up_tys =
								c.arguments.iter().map(|arg| arg.ty()).collect::<Vec<_>>();
							let overload = model[*f].function_entry(model).overload;
							let mut ty_vars = overload
								.instantiate_ty_params(db, &bottom_up_tys)
								.unwrap()
								.0;
							if model[*f].return_type().contains_type_inst_var(db) {
								// Also instantiate with top-down return type
								let _ = PolymorphicFunctionType::collect_instantiations(
									db,
									&mut |tv, t| {
										let prev = ty_vars.get_mut(&tv).unwrap();
										*prev =
											Ty::most_specific_supertype(db, [*prev, t]).unwrap();
										true
									},
									ty,
									model[*f].return_type(),
								);
							}
							for t in ty_vars.values_mut() {
								// Any bottom left in the ty vars must not matter, so just change to int
								*t = replace_bottom(db, *t);
							}
							model[*f]
								.function_entry(model)
								.overload
								.instantiate(db, &ty_vars)
								.params
								.to_vec()
						} else {
							model[*f]
								.parameters()
								.iter()
								.map(|p| model[*p].ty())
								.collect::<Vec<_>>()
						}
					}
				};
				self.extend(c.arguments.iter().zip(params));
				return false;
			}
			_ => return false,
		};
		true
	}
}

/// Compute real types for bottom types
pub fn top_down_type<'db>(db: &'db dyn Db, model: Model<'db>) -> Result<Model<'db>> {
	log::info!("Computing top-down types");
	let mut tdt = TopDownTyper::default();
	tdt.add_model(db, &model);
	Ok(tdt.result)
}

#[cfg(test)]
mod tests {
	use expect_test::expect;

	use super::top_down_type;
	use crate::transform::tests::check_no_stdlib;

	#[test]
	fn test_top_down_type_bottom() {
		check_no_stdlib(
			top_down_type,
			r#"
                    function set of int: foo(opt int);
                    any: a = foo(<>);
                    any: b = if true then [1] else [] endif;
                    tuple(int, set of int): c = (1, {});
					"#,
			expect!([r#"
    function set of int: foo(opt int: _DECL_1);
    set of int: a = foo(let {
      opt int: _DECL_2 = <>;
    } in _DECL_2);
    array [int] of int: b = if true then [1] else let {
      array [int] of int: _DECL_4 = [];
    } in _DECL_4 endif;
    tuple(int, set of int): c = (1, let {
      set of int: _DECL_6 = {};
    } in _DECL_6);
    solve satisfy;
"#]),
		)
	}

	#[test]
	fn test_top_down_type_bottom_polymorphic() {
		check_no_stdlib(
			top_down_type,
			r#"
                    function any $T: foo(any $T, array [$X] of any $U);
                    opt int: x = foo(<>, []);
					"#,
			expect!([r#"
    function any $T: foo(any $T: _DECL_1, array [$X] of any $U: _DECL_2);
    opt int: x = foo(let {
      opt int: _DECL_3 = <>;
    } in _DECL_3, let {
      array [int] of int: _DECL_4 = [];
    } in _DECL_4);
    solve satisfy;
"#]),
		)
	}

	#[test]
	fn test_top_down_type_opt() {
		check_no_stdlib(
			top_down_type,
			r#"
                    any: x = ([1, <>],);
                    function int: foo(opt int);
                    any: y = foo(3);
					opt int: z = let {
						any: b = 1;
					} in (<>, 1).1;
					"#,
			expect!([r#"
    tuple(array [int] of opt int): x = ([let {
      opt int: _DECL_1 = 1;
    } in _DECL_1, let {
      opt int: _DECL_2 = <>;
    } in _DECL_2],);
    function int: foo(opt int: _DECL_4);
    int: y = foo(let {
      opt int: _DECL_5 = 3;
    } in _DECL_5);
    opt int: z = let {
      int: b = 1;
    } in let {
      opt int: _DECL_9 = ((let {
      opt ..: _DECL_8 = <>;
    } in _DECL_8, 1)).1;
    } in _DECL_9;
    solve satisfy;
"#]),
		)
	}

	#[test]
	fn test_top_down_type_not_needed() {
		check_no_stdlib(
			top_down_type,
			r#"
                    opt int: x = <>;
                    array [int] of bool: y = [];
                    set of int: z = {};
					"#,
			expect!([r#"
    opt int: x = <>;
    array [int] of bool: y = [];
    set of int: z = {};
    solve satisfy;
"#]),
		)
	}

	#[test]
	fn test_top_down_type_array_opt() {
		check_no_stdlib(
			top_down_type,
			r#"
                    any: x = [1];
                    any: y = if true then x else [<>] endif;
                    array [int] of opt int: z = [1];
					"#,
			expect!([r#"
    array [int] of int: x = [1];
    array [int] of opt int: y = if true then let {
      array [int] of opt int: _DECL_2 = x;
    } in _DECL_2 else [let {
      opt int: _DECL_3 = <>;
    } in _DECL_3] endif;
    array [int] of opt int: z = [let {
      opt int: _DECL_5 = 1;
    } in _DECL_5];
    solve satisfy;
"#]),
		)
	}

	#[test]
	fn test_top_down_type_builtin() {
		check_no_stdlib(
			top_down_type,
			r#"
				annotation mzn_inline_call_by_name;
				function var bool: forall(array [$T] of var bool: x) :: mzn_inline_call_by_name =
					mzn_builtin("mzn_forall_var", x);
				function var bool: exists(array [$T] of var bool: x) :: mzn_inline_call_by_name =
					mzn_builtin("mzn_exists_var", x);
				function var bool: forall(array [int] of var bool: x) :: mzn_inline_call_by_name =
					mzn_builtin("mzn_forall_var", x);
				function var bool: exists(array [int] of var bool: x) :: mzn_inline_call_by_name =
					mzn_builtin("mzn_exists_var", x);
				"#,
			expect!([r#"
    annotation mzn_inline_call_by_name;
    function var bool: forall(array [$T] of var bool: x) :: (mzn_inline_call_by_name) = let {
      var bool: _DECL_5 = mzn_builtin("mzn_forall_var", x);
    } in _DECL_5;
    function var bool: exists(array [$T] of var bool: x) :: (mzn_inline_call_by_name) = let {
      var bool: _DECL_6 = mzn_builtin("mzn_exists_var", x);
    } in _DECL_6;
    function var bool: forall(array [int] of var bool: x) :: (mzn_inline_call_by_name) = let {
      var bool: _DECL_7 = mzn_builtin("mzn_forall_var", x);
    } in _DECL_7;
    function var bool: exists(array [int] of var bool: x) :: (mzn_inline_call_by_name) = let {
      var bool: _DECL_8 = mzn_builtin("mzn_exists_var", x);
    } in _DECL_8;
    solve satisfy;
"#]),
		)
	}
}
