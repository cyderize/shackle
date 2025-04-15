use std::{
	ops::{Div, Mul},
	sync::Arc,
};

use rustc_hash::FxHashMap;
use shackle_diagnostics::{Error, Result, TypeMismatch};
use super::SignatureTypeContext;
use crate::{
	constants::IdentifierRegistry,
	hir::{
		db::Hir,
		ids::{EntityRef, ExpressionRef, ItemRef, LocalItemRef, NodeRef, PatternRef},
		Declaration, Dimension, Expression, Identifier, ItemData, Pattern, ScopeResult, Type, Unit,
	},
	utils::arena::{ArenaIndex, ArenaMap},
};

/// The RHS of a dimension item
pub struct DimensionDefinition {
	definition: Option<ProductExpression<Dimension>>,
}

/// THe RHS of a unit item
pub struct UnitDefinition {
	/// The dimension of this unit
	pub dimension: ArenaIndex<Dimension>,
	/// The definition of this unit
	pub definition: Option<DerivedUnit>,
}

/// A derived unit definition
pub struct DerivedUnit {
	/// The factor for this unit
	pub factor: ArenaIndex<Expression>,
	/// The units this is derived from
	pub unit: ProductExpression<Unit>,
}

/// A unit or dimension expression
pub enum ProductExpression<T> {
	/// A single value
	Value(ArenaIndex<T>),
	/// The product of two expressions
	Product(Box<Self>, Box<Self>),
	/// The division of two expressions
	Division(Box<Self>, Box<Self>),
}

/// A computed (but non-normalised) unit
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ComputedUnit {
	/// Unit for scalar value
	Unit(ComputedUnitData),
	/// Array
	Array {
		/// Units of dimensions
		dims: Box<ComputedUnit>,
		/// Unit of element
		element: Box<ComputedUnit>,
	},
	/// Tuple
	Tuple(Vec<ComputedUnit>),
	/// Record
	Record(Vec<(Identifier, ComputedUnit)>),
	/// Invalid unit
	Error,
}

impl Mul for ComputedUnit {
	type Output = Option<Self>;
	fn mul(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(ComputedUnit::Unit(u), ComputedUnit::Unit(v)) if !u.is_coord && !v.is_coord => {
				Some(ComputedUnit::Unit(u * v))
			}
			(ComputedUnit::Error, _) | (_, ComputedUnit::Error) => Some(ComputedUnit::Error),
			_ => None,
		}
	}
}

impl Div for ComputedUnit {
	type Output = Option<Self>;
	fn div(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(ComputedUnit::Unit(u), ComputedUnit::Unit(v)) if !u.is_coord && !v.is_coord => {
				Some(ComputedUnit::Unit(u / v))
			}
			(ComputedUnit::Error, _) | (_, ComputedUnit::Error) => Some(ComputedUnit::Error),
			_ => None,
		}
	}
}

/// A computed atomic unit
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ComputedUnitData {
	/// The terms in this unit
	pub terms: FxHashMap<ResolvedUnit, i64>,
	/// Whether this is a coordinate
	pub is_coord: bool,
}

impl ComputedUnitData {
	pub fn pretty_print(&self, db: &dyn Hir) -> String {
		let mut terms = self
			.terms
			.iter()
			.map(|(k, v)| {
				(
					match k {
						ResolvedUnit::Unit(u) => u.identifier(db).unwrap().pretty_print(db),
						ResolvedUnit::Expression(e) => "<index>".to_owned(),
					},
					*v,
				)
			})
			.collect::<Vec<_>>();
		terms.sort_by(|(k, _), (j, _)| k.cmp(j));
		terms
			.into_iter()
			.map(|(k, v)| format!("{}^{}", k, v))
			.collect::<Vec<_>>()
			.join("⋅")
	}
}

impl Mul for ComputedUnitData {
	type Output = Self;
	fn mul(mut self, rhs: Self) -> Self::Output {
		assert!(!self.is_coord);
		assert!(!rhs.is_coord);
		for (k, v) in rhs.terms {
			*self.terms.entry(k).or_insert(0) += v;
		}
		Self {
			terms: self.terms,
			is_coord: false,
		}
	}
}

impl Div for ComputedUnitData {
	type Output = Self;
	fn div(mut self, rhs: Self) -> Self::Output {
		assert!(!self.is_coord);
		assert!(!rhs.is_coord);
		for (k, v) in rhs.terms {
			*self.terms.entry(k).or_insert(0) -= v;
		}
		Self {
			terms: self.terms,
			is_coord: false,
		}
	}
}

/// Factor for unit
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum UnitFactor {
	/// Integer factor
	Integer(i64),
	/// Float factor
	Float(f64),
}

impl Mul for UnitFactor {
	type Output = Self;
	fn mul(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(UnitFactor::Integer(x), UnitFactor::Integer(y)) => UnitFactor::Integer(x * y),
			(UnitFactor::Float(x), UnitFactor::Integer(y)) => UnitFactor::Float(x * (y as f64)),
			(UnitFactor::Integer(x), UnitFactor::Float(y)) => UnitFactor::Float((x as f64) * y),
			(UnitFactor::Float(x), UnitFactor::Float(y)) => UnitFactor::Float(x * y),
		}
	}
}

impl Div for UnitFactor {
	type Output = Self;
	fn div(self, rhs: Self) -> Self::Output {
		match (self, rhs) {
			(UnitFactor::Integer(x), UnitFactor::Integer(y)) => {
				if x % y == 0 {
					UnitFactor::Integer(x / y)
				} else {
					UnitFactor::Float((x as f64) / (y as f64))
				}
			}
			(UnitFactor::Float(x), UnitFactor::Integer(y)) => UnitFactor::Float(x / (y as f64)),
			(UnitFactor::Integer(x), UnitFactor::Float(y)) => UnitFactor::Float((x as f64) / y),
			(UnitFactor::Float(x), UnitFactor::Float(y)) => UnitFactor::Float(x / y),
		}
	}
}

/// Reference to a unit
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum ResolvedUnit {
	/// Unit from item
	Unit(PatternRef),
	/// Unit from expression (for fine counting types)
	Expression(ExpressionRef),
}

trait UnitTypeContext {
	/// Add a declaration for a pattern
	fn add_declaration(&mut self, pattern: PatternRef, unit: ComputedUnit);
	/// Add a type for an expression
	fn add_expression(&mut self, expression: ExpressionRef, unit: ComputedUnit);
	/// Add an error
	fn add_diagnostic(&mut self, item: ItemRef, e: impl Into<Error>);

	/// Type a pattern (or lookup the type if already known)
	fn type_pattern(&mut self, db: &dyn Hir, pattern: PatternRef) -> ComputedUnit;
}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
struct SignatureUnits {
	/// Units of declarations
	pub patterns: FxHashMap<PatternRef, ComputedUnit>,
	/// Units of expressions
	pub expressions: FxHashMap<ExpressionRef, ComputedUnit>,
}

struct SignatureUnitContext {
	starting_item: ItemRef,
	data: SignatureUnits,
	diagnostics: Vec<Error>,
}

impl SignatureUnitContext {
	fn type_item(&mut self, db: &dyn Hir, item: ItemRef) {
		let model = &*item.model(db);
		let it = item.local_item_ref(db);
		let data = it.data(model);
		match it {
			LocalItemRef::Declaration(d) => {
				let mut typer = UnitTyper::new(db, self, item, data);
				let unit = if data[model[d].declared_type].is_complete(data) {
					typer.resolve_unit(model[d].declared_type)
				} else if let Some(def) = model[d].definition {
					let actual = typer.compute_unit(def);
					typer.complete_unit(model[d].declared_type, &actual)
				} else {
					// Would already get type error here
					ComputedUnit::Error
				};
				typer.add_pattern(model[d].pattern, unit);
			}
			LocalItemRef::Unit(u) => {
				let pattern = PatternRef::new(item, model[u].name);
				self.data.patterns.insert(
					pattern,
					ComputedUnit::Unit(ComputedUnitData {
						is_coord: false,
						terms: FxHashMap::from_iter([(ResolvedUnit::Unit(pattern), 1)]),
					}),
				);
			}
			_ => unreachable!("Item does not have a unit"),
		}
	}
}

impl UnitTypeContext for SignatureUnitContext {
	fn add_declaration(&mut self, pattern: PatternRef, unit: ComputedUnit) {
		assert!(
			self.data.patterns.insert(pattern, unit).is_none(),
			"Tried to add unit for {:?} twice",
			pattern
		);
	}
	fn add_expression(&mut self, expression: ExpressionRef, unit: ComputedUnit) {
		assert!(
			self.data.expressions.insert(expression, unit).is_none(),
			"Tried to add unit for {:?} twice",
			expression
		);
	}
	fn add_diagnostic(&mut self, item: ItemRef, e: impl Into<Error>) {
		if self.starting_item == item {
			self.diagnostics.push(e.into());
		}
	}
	fn type_pattern(&mut self, db: &dyn Hir, pattern: PatternRef) -> ComputedUnit {}
}

#[derive(Default, Clone, PartialEq, Eq, Debug)]
struct BodyUnits {
	/// Units of declarations
	pub patterns: ArenaMap<Pattern, ComputedUnit>,
	/// Units of expressions
	pub expressions: ArenaMap<Expression, ComputedUnit>,
}

struct BodyUnitContext {
	item: ItemRef,
	data: BodyUnits,
	diagnostics: Vec<Error>,
}

impl BodyUnitContext {
	fn type_item(&mut self, db: &dyn Hir, item: ItemRef) {
		let model = &*item.model(db);
		let it = item.local_item_ref(db);
		match it {
			LocalItemRef::Declaration(d) => {
				let data = &it.data(model);
				let signature = db.lookup_item_unit_signature(item);
				let expected = &signature.patterns[&PatternRef::new(item, model[d].pattern)];
				let mut typer = UnitTyper::new(db, self, item, data);
				if let Some(def) = model[d].definition {
					let unit = if data[model[d].declared_type].is_complete(data) {
						let actual = typer.compute_unit(def);
						if expected
					} else {
						// Would already get type error here
						ComputedUnit::Error
					};
					typer.add_expression(def, unit);
				}
			}
			LocalItemRef::Unit(u) => {
				let pattern = PatternRef::new(item, model[u].name);
				self.data.patterns.insert(
					pattern,
					ComputedUnit::Unit(ComputedUnitData {
						is_coord: false,
						terms: FxHashMap::from_iter([(ResolvedUnit::Unit(pattern), 1)]),
					}),
				);
			}
			_ => unreachable!("Item does not have a unit"),
		}
	}
}

impl UnitTypeContext for BodyUnitContext {
	fn add_declaration(&mut self, pattern: PatternRef, unit: ComputedUnit) {
		assert_eq!(pattern.item(), self.item);
		assert!(
			self.data.patterns.get(pattern.pattern()).is_none(),
			"Tried to add unit for {:?} twice",
			pattern
		);
		self.data.patterns.insert(pattern.pattern(), unit);
	}
	fn add_expression(&mut self, expression: ExpressionRef, unit: ComputedUnit) {
		assert_eq!(expression.item(), self.item);
		assert!(
			self.data.expressions.get(expression.expression()).is_none(),
			"Tried to add unit for {:?} twice",
			expression
		);
		self.data.expressions.insert(expression.expression(), unit);
	}
	fn add_diagnostic(&mut self, item: ItemRef, e: impl Into<Error>) {
		assert_eq!(item, self.item);
		self.diagnostics.push(e.into());
	}
	fn type_pattern(&mut self, db: &dyn Hir, pattern: PatternRef) -> ComputedUnit {
		if pattern.item() == self.item {
			if let Some(d) = self.data.patterns.get(pattern.pattern()) {
				return d.clone();
			}
		}
		let signature = db.lookup_item_unit_signature(pattern.item());
		signature.patterns[&pattern].clone()
	}
}

/// Get the signature of an item (ignores RHS of items except for `any` declarations)
pub fn collect_item_unit_signature(
	db: &dyn Hir,
	item: ItemRef,
) -> (Arc<SignatureUnits>, Arc<Vec<Error>>) {
	log::debug!("Unit type checking signature of {:?}", item);
	let mut ctx = SignatureUnitContext {
		data: SignatureUnits::default(),
		diagnostics: Vec::new(),
		starting_item: item,
	};
	ctx.type_item(db, item);
	(Arc::new(ctx.data), Arc::new(ctx.diagnostics))
}

/// Type-check expressions in an item (other than those used in the signature)
pub fn collect_item_unit_body(db: &dyn Hir, item: ItemRef) -> (Arc<BodyUnits>, Arc<Vec<Error>>) {
	log::debug!("Unit type checking body of {:?}", item);
	let model = item.model(db);
	let it = item.local_item_ref(db);
	let mut ctx = BodyUnitContext {
		data: BodyUnits::default(),
		diagnostics: Vec::new(),
		item,
	};
	ctx.type_item(db);
	(Arc::new(ctx.data), Arc::new(ctx.diagnostics))
}

pub struct UnitTyper<'a, T> {
	ids: Arc<IdentifierRegistry>,
	db: &'a dyn Hir,
	ctx: &'a mut T,
	item: ItemRef,
	data: &'a ItemData,
}

impl<'a, T: UnitTypeContext> UnitTyper<'a, T> {
	fn new(db: &'a dyn Hir, ctx: &'a mut T, item: ItemRef, data: &'a ItemData) -> Self {
		Self {
			ids: db.identifier_registry(),
			db,
			ctx,
			item,
			data,
		}
	}

	fn add_pattern(&mut self, p: ArenaIndex<Pattern>, unit: ComputedUnit) {
		match (&self.data[p], unit) {
			(Pattern::Identifier(_), u) => {
				self.ctx.add_declaration(PatternRef::new(self.item, p), u)
			}
			(Pattern::Tuple { fields }, ComputedUnit::Tuple(fs)) => {
				for (pat, u) in fields.iter().zip(fs) {
					self.add_pattern(*pat, u);
				}
			}
			(Pattern::Record { fields }, ComputedUnit::Record(fs)) => {
				let map = FxHashMap::from_iter(fields.iter().copied());
				for (i, f) in fs {
					if let Some(p) = map.get(&i) {
						self.add_pattern(*p, u);
					}
				}
			}
			(Pattern::Call { .. }, _) => todo!(),
			(_, _) => self
				.ctx
				.add_declaration(PatternRef::new(self.item, p), ComputedUnit::Error),
		}
	}

	/// Resolve the unit of a variable given its type
	fn resolve_unit(&mut self, t: ArenaIndex<Type>) -> ComputedUnit {
		match &self.data[t] {
			Type::Array {
				dimensions,
				element,
				..
			} => ComputedUnit::Array {
				dims: Box::new(self.resolve_unit(*dimensions)),
				element: Box::new(self.resolve_unit(*element)),
			},
			Type::Tuple { fields, .. } => {
				ComputedUnit::Tuple(fields.iter().map(|f| self.resolve_unit(*f)).collect())
			}
			Type::Record { fields, .. } => ComputedUnit::Record(
				fields
					.iter()
					.map(|(i, f)| (self.data[*i].identifier().unwrap(), self.resolve_unit(*f)))
					.collect(),
			),
			Type::Set { element, .. } => return self.resolve_unit(*element),
			Type::Primitive { unit, .. } => {
				if let Some(u) = unit {
					self.evaluate_unit(*u)
				} else {
					ComputedUnit::Unit(ComputedUnitData {
						terms: FxHashMap::default(),
						is_coord: false,
					})
				}
			}
			Type::Bounded { domain, .. } => self.compute_unit(*domain),
			_ => unreachable!("Not a unit"),
		}
	}

	/// Evaluate a unit from a type-inst and make sure it matches a RHS expression
	fn complete_unit(&mut self, t: ArenaIndex<Type>, actual: &ComputedUnit) -> ComputedUnit {
		match (&self.data[t], actual) {
			(Type::Missing, _) | (_, ComputedUnit::Error) => ComputedUnit::Error,
			(
				Type::Array {
					dimensions: d1,
					element: e1,
					..
				},
				ComputedUnit::Array {
					dims: d2,
					element: e2,
				},
			) => ComputedUnit::Array {
				dims: Box::new(self.complete_unit(*d1, d2)),
				element: Box::new(self.complete_unit(*e1, e2)),
			},
			(Type::Tuple { fields, .. }, ComputedUnit::Tuple(fs)) => {
				if fields.len() != fs.len() {
					// Would have given a type error
					return ComputedUnit::Error;
				}
				ComputedUnit::Tuple(
					fields
						.iter()
						.zip(fs.iter())
						.map(|(f1, f2)| self.complete_unit(*f1, f2))
						.collect(),
				)
			}
			(Type::Record { fields, .. }, ComputedUnit::Record(fs)) => {
				if fields.len() != fs.len() {
					// Would have given a type error
					return ComputedUnit::Error;
				}
				ComputedUnit::Record(
					fields
						.iter()
						.zip(fs.iter())
						.map(|((_, f1), (i, f2))| (*i, self.complete_unit(*f1, f2)))
						.collect(),
				)
			}
			_ => ComputedUnit::Error,
		}
	}

	/// Evaluate a unit expression (i.e. RHS of @ operator)
	fn evaluate_unit(&mut self, unit: ArenaIndex<Expression>) -> ComputedUnit {
		match &self.data[unit] {
			Expression::Identifier(i) => {
				let scope = self.db.lookup_item_scope(self.item);
				if let Some(p) = scope.find_variable(self.db, unit, *i) {
					return self.ctx.type_pattern(self.db, p);
				} else {
					return ComputedUnit::Error;
				}
			}
			Expression::Call(c) => match &self.data[c.function] {
				Expression::Identifier(i) => {
					if *i == self.ids.coord && c.arguments.len() == 1 {
						match self.evaluate_unit(c.arguments[0]) {
							ComputedUnit::Unit(mut u) => {
								u.is_coord = true;
								return ComputedUnit::Unit(u);
							}
							ComputedUnit::Error => return ComputedUnit::Error,
							_ => {
								let (src, span) =
									NodeRef::from(EntityRef::new(self.db, self.item, unit))
										.source_span(self.db);
								self.ctx.add_diagnostic(
									self.item,
									TypeMismatch {
										src,
										span,
										msg: format!("coord() can only be called on a scalar unit"),
									}
									.into(),
								);
								return ComputedUnit::Error;
							}
						}
					} else if *i == self.ids.functions.times && c.arguments.len() == 2 {
						let lhs = self.evaluate_unit(c.arguments[0]);
						let rhs = self.evaluate_unit(c.arguments[1]);
						if let Some(result) = lhs * rhs {
							return result;
						} else {
							let (src, span) =
								NodeRef::from(EntityRef::new(self.db, self.item, unit))
									.source_span(self.db);

							self.ctx.add_diagnostic(
								self.item,
								TypeMismatch {
									src,
									span,
									msg: format!("Units cannot be multiplied"),
								}
								.into(),
							);
							return ComputedUnit::Error;
						}
					} else if *i == self.ids.functions.float_div && c.arguments.len() == 2 {
						let lhs = self.evaluate_unit(c.arguments[0]);
						let rhs = self.evaluate_unit(c.arguments[1]);
						if let Some(result) = lhs / rhs {
							return result;
						} else {
							let (src, span) =
								NodeRef::from(EntityRef::new(self.db, self.item, unit))
									.source_span(self.db);
							self.ctx.add_diagnostic(
								self.item,
								TypeMismatch {
									src,
									span,
									msg: format!("Units cannot be divided"),
								}
								.into(),
							);
							return ComputedUnit::Error;
						}
					}
				}
				_ => (),
			},
			_ => (),
		}

		let (src, span) =
			NodeRef::from(EntityRef::new(self.db, self.item, unit)).source_span(self.db);
		self.ctx.add_diagnostic(
			self.item,
			TypeMismatch {
				src,
				span,
				msg: format!("Expected unit"),
			}
			.into(),
		);
		return ComputedUnit::Error;
	}

	/// Compute the unit for an expression
	fn compute_unit(&mut self, e: ArenaIndex<Expression>) -> ComputedUnit {}
}
