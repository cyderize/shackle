/// Types of 'bodies': everything that isn't part of a signature.
///
/// E.g.
/// - Annotations on items
/// - RHS of variable declarations
/// - Bodies of functions
use shackle_utils::hash::Map;
use shackle_diagnostics::Error;
use shackle_ty::{Ty, registry::TypeRegistry};
use shackle_utils::arena::ArenaMap;

use crate::{
	Db, Expression, ExpressionId, Item, Pattern, PatternId, PatternTy, TypeContext, Typer,
	constants::IdentifierRegistry, diagnostics::Errors, ids::PatternRef,
};

/// Collected types for an item body
#[derive(Clone, Debug, PartialEq, Eq, salsa::Update, Default)]
pub struct BodyTypes<'db> {
	/// Types of declarations
	pub patterns: ArenaMap<Pattern<'db>, PatternTy<'db>>,
	/// Types of expressions
	pub expressions: ArenaMap<Expression<'db>, Ty<'db>>,
	/// Identifier resolution
	pub identifier_resolution: Map<ExpressionId<'db>, PatternRef<'db>>,
	/// Pattern resolution
	pub pattern_resolution: Map<PatternId<'db>, PatternRef<'db>>,
}

impl<'db> Item<'db> {
	/// Get the body types for this item
	pub fn body_types(&self, db: &'db dyn Db) -> &'db BodyTypes<'db> {
		item_body_types(db, *self)
	}
}

#[salsa::tracked(returns(ref))]
fn item_body_types<'db>(db: &'db dyn Db, item: Item<'db>) -> BodyTypes<'db> {
	let mut ctx = BodyTypeContext::new(item);
	ctx.type_item(db);
	ctx.finish()
}

/// Context for typing an item body
#[derive(Debug)]
pub struct BodyTypeContext<'db> {
	item: Item<'db>,
	data: BodyTypes<'db>,
}

impl<'db> BodyTypeContext<'db> {
	/// Create a new signature type context
	pub fn new(item: impl Into<Item<'db>>) -> Self {
		Self {
			item: item.into(),
			data: BodyTypes::default(),
		}
	}

	/// Create a new signature type context with the given capacity for patterns/expressions
	pub fn with_capacity(item: impl Into<Item<'db>>, patterns: u32, expressions: u32) -> Self {
		Self {
			item: item.into(),
			data: BodyTypes {
				patterns: ArenaMap::with_capacity(patterns + 1),
				expressions: ArenaMap::with_capacity(expressions + 1),
				identifier_resolution: Map::default(),
				pattern_resolution: Map::default(),
			},
		}
	}

	/// Compute the type of the body of this item
	pub fn type_item(&mut self, db: &'db dyn Db) {
		let item = self.item;
		let data = item.data(db);
		let mut typer = Typer::new(db, self, item, data);
		let types = TypeRegistry::lookup(db);
		match item {
			Item::Annotation(_) => {}
			Item::Function(f) => {
				let it = f.function(db);
				let signature = item.signature(db);
				for ann in it.annotations.iter() {
					let _ = typer.typecheck_expression(*ann, types.ann);
				}
				for param in it.parameters.iter() {
					if let Some(p) = param.pattern {
						let param_ty = match &signature.patterns[&p] {
							PatternTy::Argument(t) | PatternTy::Destructuring(t) => *t,
							_ => unreachable!(),
						};
						for ann in param.annotations.iter() {
							typer.typecheck_declaration_annotation(*ann, param_ty);
						}
					}
				}

				if let Some(e) = it.body {
					match &signature.patterns[&it.pattern] {
						PatternTy::Function(function) => {
							let _ = typer.typecheck_expression(e, function.overload.return_type());
						}
						_ => unreachable!(),
					};
				}
			}
			Item::Declaration(d) => {
				let it = d.declaration(db);
				let signature = item.signature(db);
				let expected = match &signature.patterns[&it.pattern] {
					PatternTy::Variable(t) | PatternTy::Destructuring(t) => *t,
					_ => unreachable!(),
				};
				// Declarations with incomplete types would have been done during signature typing
				if data[it.declared_type].is_complete(data) {
					if let Some(e) = it.definition {
						let ids = IdentifierRegistry::lookup(db);
						let output_only = it.annotations.iter().any(|ann| match &data[*ann] {
							Expression::Identifier(i) => *i == ids.annotations.output_only,
							_ => false,
						});
						if output_only {
							typer.typecheck_output(e, expected);
						} else {
							let _ = typer.typecheck_expression(e, expected);
						}
					}
					for ann in it.annotations.iter() {
						typer.typecheck_declaration_annotation(*ann, expected);
					}
				}
			}
			Item::Output(o) => {
				let it = o.output(db);
				if let Some(s) = &it.section {
					let _ = typer.typecheck_expression(*s, types.string);
				}
				typer.typecheck_output(it.expression, types.array_of_string);
			}
			Item::Constraint(c) => {
				let it = c.constraint(db);
				let _ = typer.typecheck_expression(it.expression, types.var_bool);
				for ann in it.annotations.iter() {
					let _ = typer.typecheck_expression(*ann, types.ann);
				}
			}
			Item::Solve(s) => {
				let it = s.solve(db);
				for ann in it.annotations.iter() {
					let _ = typer.typecheck_expression(*ann, types.ann);
				}
			}
			Item::Assignment(a) => {
				let it = a.assignment(db);
				let expected = typer.collect_expression(it.assignee);
				let _ = typer.typecheck_expression(it.definition, expected);
			}
			Item::Enumeration(e) => {
				let it = e.enumeration(db);
				let signature = item.signature(db);
				let ty = match &signature.patterns[&it.pattern] {
					PatternTy::Enum(t) => *t,
					_ => unreachable!(),
				};
				for ann in it.annotations.iter() {
					typer.typecheck_declaration_annotation(*ann, ty);
				}
			}
			Item::EnumAssignment(e) => {
				let it = e.enum_assignment(db);
				let _ = typer.collect_expression(it.assignee);
			}
			Item::TypeAlias(t) => {
				let it = t.type_alias(db);
				for ann in it.annotations.iter() {
					let _ = typer.typecheck_expression(*ann, types.ann);
				}
			}
		}
	}

	/// Get results of typing
	pub fn finish(mut self) -> BodyTypes<'db> {
		self.data.patterns.shrink_to_fit();
		self.data.expressions.shrink_to_fit();
		self.data.identifier_resolution.shrink_to_fit();
		self.data.pattern_resolution.shrink_to_fit();
		self.data
	}
}

impl<'db> TypeContext<'db> for BodyTypeContext<'db> {
	fn add_declaration(
		&mut self,
		_db: &'db dyn Db,
		pattern: PatternId<'db>,
		declaration: PatternTy<'db>,
	) {
		assert!(
			matches!(
				self.data.patterns.get(pattern),
				None | Some(PatternTy::Computing)
			),
			"Tried to add declaration for {:?} twice",
			pattern
		);
		self.data.patterns.insert(pattern, declaration);
	}

	fn add_expression(&mut self, _db: &'db dyn Db, expression: ExpressionId<'db>, ty: Ty<'db>) {
		assert!(
			self.data.expressions.get(expression).is_none(),
			"Tried to add type for expression {:?} twice",
			expression
		);
		self.data.expressions.insert(expression, ty);
	}

	fn add_identifier_resolution(
		&mut self,
		_db: &'db dyn Db,
		expression: ExpressionId<'db>,
		resolution: PatternRef<'db>,
	) {
		let old = self
			.data
			.identifier_resolution
			.insert(expression, resolution);
		assert!(
			old.is_none(),
			"Tried to add identifier resolution for {:?} twice",
			expression
		);
	}

	fn add_pattern_resolution(
		&mut self,
		_db: &'db dyn Db,
		pattern: PatternId<'db>,
		resolution: PatternRef<'db>,
	) {
		let old = self.data.pattern_resolution.insert(pattern, resolution);
		assert!(
			old.is_none(),
			"Tried to add pattern resolution for {:?} twice",
			pattern
		);
	}

	fn add_diagnostic(&mut self, db: &'db dyn Db, item: Item<'db>, e: impl Into<Error>) {
		let error = e.into();
		assert_eq!(item, self.item, "Got error '{}' for wrong item", error);
		Errors::add(db, error);
	}

	fn type_pattern(&mut self, db: &'db dyn Db, pattern: PatternRef<'db>) -> PatternTy<'db> {
		let item = pattern.item(db);
		if item == self.item
			&& let Some(d) = self.data.patterns.get(pattern.pattern(db))
		{
			return d.clone();
		}
		let signature = item.signature(db);
		signature.patterns[&pattern.pattern(db)].clone()
	}
}
