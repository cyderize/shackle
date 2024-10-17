//! Lowering from THIR to MIR

use std::sync::atomic::AtomicU32;

use super::*;
use crate::{
	thir::{self, pretty_print::PrettyPrinter},
	utils::arena::Arena,
};

fn declaration_identifier(
	db: &dyn thir::db::Thir,
	m: &thir::Model,
	d: thir::DeclarationId,
) -> Identifier {
	m[d].name()
		.unwrap_or_else(|| Identifier::new(format!("_DECL_{}", Into::<u32>::into(d)), db.upcast()))
}

fn fresh_identifier(db: &dyn thir::db::Thir) -> Identifier {
	static COUNTER: AtomicU32 = AtomicU32::new(0);
	Identifier::new(
		format!(
			"_INTRODUCED_{}",
			COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst)
		),
		db.upcast(),
	)
}

/// Create MIR from THIR
pub fn lower_from_thir(db: &dyn thir::db::Thir, m: &thir::Model) -> Model {
	let tys = db.type_registry();

	let mut annotations = Arena::default();
	let mut functions = Arena::default();

	let mut root_collector = Collector::default();

	for item in m.top_level_items() {
		match item {
			thir::ItemId::Annotation(a) => {
				annotations.insert(Annotation {
					name: m[a].name.unwrap(),
					parameters: m[a]
						.parameters
						.as_ref()
						.iter()
						.flat_map(|ps| {
							ps.iter().map(|p| Parameter {
								ty: Collector::lower_ty(db, m[*p].ty()),
								name: declaration_identifier(db, m, *p),
							})
						})
						.collect(),
				});
			}
			thir::ItemId::Function(f) => {
				if m[f].body().is_some()
					|| (m[f].return_type() == tys.var_bool && !m[f].is_polymorphic())
				{
					functions.insert(Function {
						name: m[f].mangled_name(db),
						ty: Collector::lower_ty(db, m[f].return_type()),
						parameters: m[f]
							.parameters()
							.iter()
							.map(|p| Parameter {
								ty: Collector::lower_ty(db, m[*p].ty()),
								name: declaration_identifier(db, m, *p),
							})
							.collect(),
						body: m[f].body().map(|body| {
							let mut c = Collector::default();
							let result = c.lower_expression(db, m, body);
							c.finish(db, result)
						}),
					});
				}
			}
			thir::ItemId::Constraint(c) => root_collector.collect_constraint(db, &m, c),
			thir::ItemId::Declaration(d) => root_collector.collect_declaration(db, &m, d),
			thir::ItemId::Solve => (),
			_ => unreachable!("Illegal item {:?}", item),
		}
	}

	let entrypoint = Expression {
		ty: Ty::var_bool(),
		data: ExpressionData::Let(Let {
			items: root_collector.items,
			result: LetResult::Root,
		}),
		origin: Origin::Introduced("entrypoint"),
	};

	Model {
		entrypoint,
		annotations,
		functions,
	}
}

enum LoweredExpression {
	/// Simple value
	Value(Value),
	/// Complex expression
	Expression(Expression),
}

impl_enum_from!(LoweredExpression::Value);
impl_enum_from!(LoweredExpression::Expression);

impl From<Literal> for LoweredExpression {
	fn from(value: Literal) -> Self {
		Value::into(value.into())
	}
}

impl LoweredExpression {
	fn ty(&self) -> Ty {
		match self {
			LoweredExpression::Value(v) => v.ty.clone(),
			LoweredExpression::Expression(e) => e.ty.clone(),
		}
	}

	fn origin(&self) -> Origin {
		match self {
			LoweredExpression::Value(v) => v.origin,
			LoweredExpression::Expression(e) => e.origin,
		}
	}

	fn into_expression(self) -> Expression {
		match self {
			LoweredExpression::Expression(e) => e,
			LoweredExpression::Value(v) => v.into(),
		}
	}

	fn into_value(self, db: &dyn thir::db::Thir, c: &mut Collector) -> Value {
		match self {
			LoweredExpression::Expression(e) => {
				assert!(!matches!(
					&e.data,
					ExpressionData::Value(ValueData::Literal(LiteralData::Identifier(_)))
				));
				let name = fresh_identifier(db);
				let ty = e.ty.clone();
				let origin = e.origin;
				c.items.push(LetItem::Declaration(Declaration {
					ty: ty.clone(),
					domain: None,
					name,
					definition: Some(e),
					annotations: Vec::new(),
				}));
				Literal::new(name, ty, origin).into()
			}
			LoweredExpression::Value(v) => v,
		}
	}

	fn into_identifier(self, db: &dyn thir::db::Thir, c: &mut Collector) -> Identifier {
		match &self {
			LoweredExpression::Value(v) => match &v.data {
				ValueData::Literal(LiteralData::Identifier(ident)) => return *ident,
				_ => (),
			},
			_ => (),
		}
		let e = self.into_expression();
		let name = fresh_identifier(db);
		let ty = e.ty.clone();
		c.items.push(LetItem::Declaration(Declaration {
			ty: ty.clone(),
			domain: None,
			name,
			definition: Some(e),
			annotations: Vec::new(),
		}));
		name
	}
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
struct Collector {
	items: Vec<LetItem>,
}

impl Collector {
	/// Get a let expression with the current items and `result` as the 'in'.
	fn finish(mut self, db: &dyn thir::db::Thir, result: LoweredExpression) -> Expression {
		if self.items.is_empty() {
			return result.into_expression();
		}
		let ty = result.ty();
		let origin = result.origin();
		let ident = result.into_identifier(db, &mut self);
		Expression::new(
			Let {
				items: self.items,
				result: LetResult::Identifier(ident),
			},
			ty,
			origin,
		)
	}

	fn collect_constraint(
		&mut self,
		db: &dyn thir::db::Thir,
		model: &thir::Model,
		c: thir::ConstraintId,
	) {
		let expression = self
			.lower_expression(db, model, model[c].expression())
			.into_expression();
		self.items.push(LetItem::Constraint(Constraint {
			expression,
			annotations: Vec::new(),
		}));
	}

	fn collect_declaration(
		&mut self,
		db: &dyn thir::db::Thir,
		model: &thir::Model,
		d: thir::DeclarationId,
	) {
		let ty = Collector::lower_ty(db, model[d].ty());
		let definition = model[d].definition().map(|e| {
			if e.ty().contains_bottom(db.upcast()) {
				self.lower_expression_with_ty(db, model, e, ty.clone())
			} else {
				self.lower_expression(db, model, e)
			}
			.into_expression()
		});
		let domain = self.lower_domain(db, model, model[d].domain());
		self.items.push(LetItem::Declaration(Declaration {
			name: declaration_identifier(db, model, d),
			ty,
			domain,
			definition,
			annotations: Vec::new(),
		}));
	}

	/// Lower a THIR expression to MIR
	fn lower_expression(
		&mut self,
		db: &dyn thir::db::Thir,
		model: &thir::Model,
		expression: &thir::Expression,
	) -> LoweredExpression {
		let ty = Collector::lower_ty(db, expression.ty());
		self.lower_expression_with_ty(db, model, expression, ty)
	}

	fn lower_expression_with_ty(
		&mut self,
		db: &dyn thir::db::Thir,
		model: &thir::Model,
		expression: &thir::Expression,
		ty: Ty,
	) -> LoweredExpression {
		let origin = expression.origin();
		match &**expression {
			thir::ExpressionData::BooleanLiteral(b) => Literal::new(b.clone(), ty, origin).into(),
			thir::ExpressionData::IntegerLiteral(i) => Literal::new(i.clone(), ty, origin).into(),
			thir::ExpressionData::FloatLiteral(f) => Literal::new(f.clone(), ty, origin).into(),
			thir::ExpressionData::StringLiteral(s) => Literal::new(s.clone(), ty, origin).into(),
			thir::ExpressionData::Infinity => {
				Literal::new(LiteralData::Infinity, ty, origin).into()
			}
			thir::ExpressionData::Identifier(thir::ResolvedIdentifier::Annotation(a)) => {
				Literal::new(model[*a].name.unwrap(), ty, origin).into()
			}
			thir::ExpressionData::Identifier(thir::ResolvedIdentifier::Declaration(d)) => {
				Literal::new(declaration_identifier(db, model, *d), ty, origin).into()
			}
			thir::ExpressionData::ArrayLiteral(al) => Value::new(
				Array {
					members: al
						.iter()
						.map(|e| self.lower_expression(db, model, e).into_value(db, self))
						.collect(),
				},
				ty,
				origin,
			)
			.into(),
			thir::ExpressionData::SetLiteral(sl) => Value::new(
				Set {
					members: sl
						.iter()
						.map(|e| self.lower_expression(db, model, e).into_value(db, self))
						.collect(),
				},
				ty,
				origin,
			)
			.into(),
			thir::ExpressionData::TupleLiteral(tl) => Value::new(
				Tuple {
					members: tl
						.iter()
						.map(|e| self.lower_expression(db, model, e).into_value(db, self))
						.collect(),
				},
				ty,
				origin,
			)
			.into(),
			thir::ExpressionData::TupleAccess(ta) => {
				let tuple = self
					.lower_expression(db, model, &ta.tuple)
					.into_identifier(db, self);
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
			thir::ExpressionData::ArrayComprehension(comp) => Expression::new(
				Comprehension {
					generators: comp
						.generators
						.iter()
						.map(|g| match g {
							thir::Generator::Assignment {
								assignment,
								where_clause,
							} => Generator::Assignment {
								name: declaration_identifier(db, model, *assignment),
								definition: {
									let mut c = Collector::default();
									let lowered = c.lower_expression(
										db,
										model,
										model[*assignment].definition().unwrap(),
									);
									c.finish(db, lowered)
								},
								where_clause: where_clause.as_ref().map(|w| {
									let mut c = Collector::default();
									let lowered = c.lower_expression(db, model, w);
									c.finish(db, lowered)
								}),
							},
							thir::Generator::Iterator {
								declarations,
								collection,
								where_clause,
							} => Generator::Iterator {
								names: declarations
									.iter()
									.map(|d| declaration_identifier(db, model, *d))
									.collect(),
								collection: {
									let mut c = Collector::default();
									let lowered = c.lower_expression(db, model, collection);
									c.finish(db, lowered)
								},
								where_clause: where_clause.as_ref().map(|w| {
									let mut c = Collector::default();
									let lowered = c.lower_expression(db, model, w);
									c.finish(db, lowered)
								}),
							},
						})
						.collect(),
					expression: {
						let mut c = Collector::default();
						let lowered = c.lower_expression(db, model, &comp.template);
						Box::new(c.finish(db, lowered))
					},
				},
				ty,
				origin,
			)
			.into(),
			thir::ExpressionData::IfThenElse(ite) => {
				let mut c = Collector::default();
				let lowered = c.lower_expression(db, model, &ite.else_result);
				let mut result = c.finish(db, lowered);
				for branch in ite.branches.iter().rev() {
					let mut c = Collector::default();
					let condition = c
						.lower_expression(db, model, &branch.condition)
						.into_value(db, &mut c);
					result = c.finish(
						db,
						Expression::new(
							IfThenElse {
								condition: Box::new(condition),
								then: Box::new({
									let mut c = Collector::default();
									let result = c.lower_expression(db, model, &branch.result);
									c.finish(db, result)
								}),
								else_expression: Box::new(result),
							},
							ty.clone(),
							origin,
						)
						.into(),
					);
				}
				result.into()
			}
			thir::ExpressionData::Case(_) => todo!(),
			thir::ExpressionData::Call(c) => {
				let function = match &c.function {
					thir::Callable::Function(f) => model[*f].mangled_name(db),
					thir::Callable::Annotation(a) => model[*a].name.unwrap(),
					_ => todo!(),
				};
				let arguments = c
					.arguments
					.iter()
					.map(|e| self.lower_expression(db, model, e).into_value(db, self))
					.collect();
				Expression::new(
					Call {
						function,
						arguments,
					},
					ty,
					origin,
				)
				.into()
			}
			thir::ExpressionData::Let(l) => {
				for item in l.items.iter() {
					match item {
						thir::LetItem::Constraint(c) => self.collect_constraint(db, model, *c),
						thir::LetItem::Declaration(d) => self.collect_declaration(db, model, *d),
					}
				}
				self.lower_expression(db, model, &l.in_expression)
			}
			_ => unreachable!("Illegal expression"),
		}
	}

	fn lower_domain(
		&mut self,
		db: &dyn thir::db::Thir,
		model: &thir::Model,
		domain: &thir::Domain,
	) -> Option<Domain> {
		match &**domain {
			thir::DomainData::Bounded(b) => {
				let ident = self
					.lower_expression(db, model, &b)
					.into_identifier(db, self);
				return Some(Domain::Identifier(ident));
			}
			thir::DomainData::Set(s) => match &***s {
				thir::DomainData::Bounded(b) => {
					let ident = self
						.lower_expression(db, model, &b)
						.into_identifier(db, self);
					return Some(Domain::Identifier(ident));
				}
				_ => (),
			},
			_ => (),
		}
		assert!(
			domain
				.walk()
				.all(|d| !matches!(&**d, thir::DomainData::Bounded(_))),
			"Unexpected domain {}",
			PrettyPrinter::new(db, model).pretty_print_domain(domain)
		);
		None
	}

	/// Lower HIR/THIR type to MIR
	fn lower_ty(db: &dyn thir::db::Thir, ty: crate::ty::Ty) -> Ty {
		match ty.lookup(db.upcast()) {
			crate::ty::TyData::Boolean(inst, _) => Ty::Bool {
				dim: 0,
				is_var: inst == crate::ty::VarType::Var,
				is_set: false,
			},
			crate::ty::TyData::Integer(inst, _) => Ty::Int {
				dim: 0,
				is_var: inst == crate::ty::VarType::Var,
				is_set: false,
			},
			crate::ty::TyData::Float(inst, _) => Ty::Float {
				dim: 0,
				is_var: inst == crate::ty::VarType::Var,
				is_set: false,
			},
			crate::ty::TyData::String(_) => Ty::String { dim: 0 },
			crate::ty::TyData::Annotation(_) => Ty::Ann { dim: 0 },
			crate::ty::TyData::Tuple(_, fields) => Ty::Tuple {
				dim: 0,
				fields: fields.iter().map(|f| Collector::lower_ty(db, *f)).collect(),
			},
			crate::ty::TyData::Array { dim, element, .. } => {
				let mut e = Collector::lower_ty(db, element);
				e.set_dim(dim.field_len(db.upcast()).unwrap_or(1) as u8);
				e
			}
			crate::ty::TyData::Set(inst, _, element) => {
				let mut e = Collector::lower_ty(db, element);
				e.set_var(inst == crate::ty::VarType::Var);
				e.set_set(true);
				e
			}
			_ => unreachable!("Illegal type {}", ty.pretty_print(db.upcast())),
		}
	}
}

#[cfg(test)]
mod test {
	use std::sync::Arc;

	use expect_test::expect;

	use super::lower_from_thir;
	use crate::{
		db::Inputs,
		file::{InputFile, InputLang},
		mir::pretty_print::PrettyPrinter,
		thir::db::Thir,
		CompilerDatabase,
	};

	#[test]
	fn test_mir() {
		let mut db = CompilerDatabase::default();
		db.set_ignore_stdlib(true);
		db.set_input_files(Arc::new(vec![InputFile::String(
			r#"
			predicate forall(array [int] of var bool);
			var bool: p = true;
			var bool: q = let {
				constraint p;
			} in true;
			"#
			.to_owned(),
			InputLang::MiniZinc,
		)]));
		let model = db.final_thir().unwrap();
		let mir = lower_from_thir(&db, &model);
		let expected = expect![[r#"
    let {
      var bool: p = true;
      var bool: _DECL_3 = p;
      var bool: q = forall([_DECL_3, true]);
    } in true"#]];
		expected.assert_eq(&PrettyPrinter::print_expression(&db, &mir.entrypoint));
	}

	#[test]
	fn test_mir_complex() {
		let mut db = CompilerDatabase::default();
		db.set_input_files(Arc::new(vec![InputFile::String(
			r#"
			var bool: p = true;
			var bool: q = let {
				constraint p;
			} in true;
			var 1..3: x;
			function var int: foo(var int: v) = v div 2 + 1;
			constraint x = foo(x);
			"#
			.to_owned(),
			InputLang::MiniZinc,
		)]));
		let model = db.final_thir().unwrap();
		let mir = lower_from_thir(&db, &model);
		let expected = expect![[r#"
    let {
      ann: domain_propagation = domain;
      bool: debug_mode = mzn_internal_check_debug_mode();
      tuple(bool, int): mzn_min_version_required;
      bool: _INTRODUCED_1721 = 'absent<opt int>'(mzn_min_version_required);
      tuple(bool, int): _DECL_536 = 'deopt<opt int>'(mzn_min_version_required);
      int: _INTRODUCED_1722 = mzn_compiler_version();
      bool: _INTRODUCED_1723 = '<=<$T, $T>'(_DECL_536.2, _INTRODUCED_1722);
      bool: _INTRODUCED_1724 = 'forall<array [int] of bool>'([_DECL_536.1, _INTRODUCED_1723]);
      bool: _INTRODUCED_1725 = '\/<bool, bool>'(_INTRODUCED_1721, _INTRODUCED_1724);
      int: _INTRODUCED_1726 = 'deopt_root<opt int>'(mzn_min_version_required);
      string: _INTRODUCED_1727 = mzn_version_to_string(_INTRODUCED_1726);
      string: _INTRODUCED_1728 = '++<string, string>'("This model requires MiniZinc version ", _INTRODUCED_1727);
      string: _INTRODUCED_1729 = '++<string, string>'(_INTRODUCED_1728, " but you are running version ");
      int: _INTRODUCED_1730 = mzn_compiler_version();
      string: _INTRODUCED_1731 = mzn_version_to_string(_INTRODUCED_1730);
      string: _INTRODUCED_1732 = '++<string, string>'(_INTRODUCED_1729, _INTRODUCED_1731);
      constraint 'assert<bool, string>'(_INTRODUCED_1725, _INTRODUCED_1732);
      tuple(bool, bool): mzn_opt_only_range_domains;
      tuple(bool, bool): mzn_opt_annotate_defines_var;
      tuple(bool, bool): mzn_ignore_symmetry_breaking_constraints;
      tuple(bool, bool): mzn_ignore_redundant_constraints;
      tuple(bool, bool): mzn_half_reify_clause;
      ann: bounds_propagation = bounds;
      set of int: _DECL_809 = '..<int, int>'(1, 2);
      var bool: p = true;
      var bool: _DECL_2196 = p;
      var bool: q = 'forall<array [int] of var bool>'([_DECL_2196, true]);
      set of int: _INTRODUCED_10683 = '..<int, int>'(1, 3);
      var _INTRODUCED_10683: x;
      var int: _INTRODUCED_10689 = foo_root(x);
      constraint '=<any $T, any $T>'(x, _INTRODUCED_10689);
    } in true"#]];
		expected.assert_eq(&PrettyPrinter::print_expression(&db, &mir.entrypoint));
	}
}
