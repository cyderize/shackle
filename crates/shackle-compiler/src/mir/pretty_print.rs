//! Pretty printing for MIR
//!
use super::*;

/// Pretty printing for MIR
pub struct PrettyPrinter;

impl PrettyPrinter {
	/// Print a model
	pub fn print_model(_db: &dyn crate::thir::db::Thir, _model: &Model) -> String {
		todo!()
	}

	/// Print an expression
	pub fn print_expression(db: &dyn crate::thir::db::Thir, expression: &Expression) -> String {
		PrettyPrinter::print_expression_data(db, &expression.data)
	}

	/// Print a value
	pub fn print_value(db: &dyn crate::thir::db::Thir, value: &Value) -> String {
		PrettyPrinter::print_value_data(db, &value.data)
	}

	/// Print a literal
	pub fn print_literal(db: &dyn crate::thir::db::Thir, literal: &Literal) -> String {
		PrettyPrinter::print_literal_data(db, &literal.data)
	}

	fn print_expression_data(db: &dyn crate::thir::db::Thir, data: &ExpressionData) -> String {
		match data {
			ExpressionData::Builtin(b) => match b {
				Builtin::GetParameter(p) => p.pretty_print(db.upcast()),
				Builtin::ArrayNd(c) => format!(
					"arrayNd({},{})",
					c.index_sets
						.iter()
						.map(|i| i.pretty_print(db.upcast()))
						.collect::<Vec<_>>()
						.join(", "),
					c.array.pretty_print(db.upcast())
				),
				Builtin::Abort(i) => format!("abort({})", i.pretty_print(db.upcast())),
				Builtin::Show(i) => format!("show({})", i.pretty_print(db.upcast())),
			},
			ExpressionData::Call(c) => {
				let f = c.function.pretty_print(db.upcast());
				let args = c
					.arguments
					.iter()
					.map(|v| PrettyPrinter::print_value(db, v))
					.collect::<Vec<_>>();
				format!("{}({})", f, args.join(", "))
			}
			ExpressionData::Comprehension(c) | ExpressionData::ForAllRoot(c) => {
				let template = PrettyPrinter::print_expression(db, &c.expression);
				let generators = c
					.generators
					.iter()
					.map(|g| match g {
						Generator::Assignment {
							name,
							definition,
							where_clause,
						} => format!(
							"{} = {}{}",
							name.pretty_print(db.upcast()),
							PrettyPrinter::print_expression(db, definition),
							where_clause
								.as_ref()
								.map(|w| format!(
									" where {}",
									PrettyPrinter::print_expression(db, w)
								))
								.unwrap_or_default()
						),
						Generator::Iterator {
							names,
							collection,
							where_clause,
						} => {
							format!(
								"{} in {}{}",
								names
									.iter()
									.map(|n| n.pretty_print(db.upcast()))
									.collect::<Vec<_>>()
									.join(", "),
								PrettyPrinter::print_expression(db, collection),
								where_clause
									.as_ref()
									.map(|w| format!(
										" where {}",
										PrettyPrinter::print_expression(db, w)
									))
									.unwrap_or_default()
							)
						}
					})
					.collect::<Vec<_>>()
					.join(", ");

				format!("[{} | {}]", template, generators)
			}
			ExpressionData::IfThenElse(ite) => {
				format!(
					"if {} then {} else {} endif",
					PrettyPrinter::print_value(db, &ite.condition),
					PrettyPrinter::print_expression(db, &ite.then),
					PrettyPrinter::print_expression(db, &ite.else_expression)
				)
			}
			ExpressionData::Let(l) => {
				let items = l
					.items
					.iter()
					.map(|i| match i {
						LetItem::Constraint(c) => format!(
							"  constraint{} {};",
							c.annotations
								.iter()
								.map(|ann| match ann {
									AnnotationRef::Identifier(ident) =>
										ident.pretty_print(db.upcast()),
									AnnotationRef::Reference(_) => todo!(),
								})
								.map(|s| format!(" :: {}", s))
								.collect::<Vec<_>>()
								.join(""),
							PrettyPrinter::print_expression(db, &c.expression)
						),
						LetItem::Declaration(d) => format!(
							"  {}: {}{}{};",
							d.domain
								.as_ref()
								.map(|dom| PrettyPrinter::print_domain(db, &d.ty, dom))
								.unwrap_or_else(|| PrettyPrinter::print_type(&d.ty)),
							d.name.pretty_print(db.upcast()),
							d.annotations
								.iter()
								.map(|ann| match ann {
									AnnotationRef::Identifier(ident) =>
										ident.pretty_print(db.upcast()),
									AnnotationRef::Reference(_) => todo!(),
								})
								.map(|s| format!(" :: {}", s))
								.collect::<Vec<_>>()
								.join(""),
							d.definition
								.as_ref()
								.map(|def| format!(
									" = {}",
									PrettyPrinter::print_expression(db, def)
								))
								.unwrap_or_default()
						),
					})
					.collect::<Vec<_>>()
					.join("\n");
				let result = match &l.result {
					LetResult::Root => "true".to_owned(),
					LetResult::Identifier(result) => result.pretty_print(db.upcast()),
				};
				format!("let {{\n{}\n}} in {}", items, result)
			}
			ExpressionData::Value(v) => PrettyPrinter::print_value_data(db, v),
		}
	}

	fn print_value_data(db: &dyn crate::thir::db::Thir, data: &ValueData) -> String {
		match data {
			ValueData::Array(a) => format!(
				"[{}]",
				a.members
					.iter()
					.map(|v| PrettyPrinter::print_value(db, v))
					.collect::<Vec<_>>()
					.join(", ")
			),
			ValueData::Literal(l) => PrettyPrinter::print_literal_data(db, l),
			ValueData::Set(s) => format!(
				"{{{}}}",
				s.members
					.iter()
					.map(|v| PrettyPrinter::print_value(db, v))
					.collect::<Vec<_>>()
					.join(", ")
			),
			ValueData::Tuple(t) => {
				let mut members = t
					.members
					.iter()
					.map(|v| PrettyPrinter::print_value(db, v))
					.collect::<Vec<_>>();
				if t.members.len() <= 1 {
					members.push("".to_owned());
				}
				format!("({})", members.join(", "))
			}
			ValueData::TupleAccess(ta) => {
				format!("{}.{}", ta.tuple.pretty_print(db.upcast()), ta.field.0)
			}
		}
	}

	fn print_literal_data(db: &dyn crate::thir::db::Thir, data: &LiteralData) -> String {
		match data {
			LiteralData::Boolean(b) => if b.0 { "true" } else { "false" }.to_owned(),
			LiteralData::Float(f) => format!("{}", f.value()),
			LiteralData::Identifier(i) => i.pretty_print(db.upcast()),
			LiteralData::Infinity => "infinity".to_owned(),
			LiteralData::Integer(i) => format!("{}", i.0),
			LiteralData::String(s) => format!("{:?}", s.value(db.upcast())),
		}
	}

	fn print_domain(db: &dyn crate::thir::db::Thir, ty: &Ty, dom: &Domain) -> String {
		let mut parts = Vec::new();
		if ty.is_var() {
			parts.push("var".to_owned());
		}
		if ty.is_set() {
			parts.push("set of".to_owned());
		}
		parts.push(match dom {
			Domain::Identifier(ident) => ident.pretty_print(db.upcast()),
			Domain::Set(s) => format!(
				"{{{}}}",
				s.members
					.iter()
					.map(|v| PrettyPrinter::print_value(db, v))
					.collect::<Vec<_>>()
					.join(", ")
			),
		});
		parts.join(" ")
	}

	fn print_type(ty: &ty::Ty) -> String {
		match ty {
			ty::Ty::Ann { dim } => {
				if *dim > 0 {
					let dims = std::iter::repeat("int".to_owned())
						.take(*dim as usize)
						.collect::<Vec<_>>()
						.join(", ");
					format!("array [{}] of ann", dims)
				} else {
					"ann".to_owned()
				}
			}
			ty::Ty::Bool {
				dim,
				is_var,
				is_set,
			} => {
				let mut parts = Vec::new();
				if *dim > 0 {
					let dims = std::iter::repeat("int".to_owned())
						.take(*dim as usize)
						.collect::<Vec<_>>()
						.join(", ");
					parts.push(format!("array [{}] of", dims));
				}
				if *is_var {
					parts.push("var".to_owned());
				}
				if *is_set {
					parts.push("set of".to_owned());
				}
				parts.push("bool".to_owned());
				parts.join(" ")
			}
			ty::Ty::Float {
				dim,
				is_var,
				is_set,
			} => {
				let mut parts = Vec::new();
				if *dim > 0 {
					let dims = std::iter::repeat("int".to_owned())
						.take(*dim as usize)
						.collect::<Vec<_>>()
						.join(", ");
					parts.push(format!("array [{}] of", dims));
				}
				if *is_var {
					parts.push("var".to_owned());
				}
				if *is_set {
					parts.push("set of".to_owned());
				}
				parts.push("float".to_owned());
				parts.join(" ")
			}
			ty::Ty::Int {
				dim,
				is_var,
				is_set,
			} => {
				let mut parts = Vec::new();
				if *dim > 0 {
					let dims = std::iter::repeat("int".to_owned())
						.take(*dim as usize)
						.collect::<Vec<_>>()
						.join(", ");
					parts.push(format!("array [{}] of", dims));
				}
				if *is_var {
					parts.push("var".to_owned());
				}
				if *is_set {
					parts.push("set of".to_owned());
				}
				parts.push("int".to_owned());
				parts.join(" ")
			}
			ty::Ty::String { dim } => {
				if *dim > 0 {
					let dims = std::iter::repeat("int".to_owned())
						.take(*dim as usize)
						.collect::<Vec<_>>()
						.join(", ");
					format!("array [{}] of string", dims)
				} else {
					"string".to_owned()
				}
			}
			ty::Ty::Tuple { dim, fields } => {
				if *dim > 0 {
					let dims = std::iter::repeat("int".to_owned())
						.take(*dim as usize)
						.collect::<Vec<_>>()
						.join(", ");
					format!(
						"array [{}] of tuple({})",
						dims,
						fields
							.iter()
							.map(|f| PrettyPrinter::print_type(f))
							.collect::<Vec<_>>()
							.join(", ")
					)
				} else {
					format!(
						"tuple({})",
						fields
							.iter()
							.map(|f| PrettyPrinter::print_type(f))
							.collect::<Vec<_>>()
							.join(", ")
					)
				}
			}
		}
	}
}
