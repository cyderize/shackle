//! Pretty printing for MIR
//!

use std::fmt::Write;

use shackle_hir::Db;

use crate::{
	Annotation, Builtin, Constraint, DeclarationId, Expression, ExpressionData, Function,
	Generator, Identifier, Let, LetItem, LetResult, Literal, LiteralData, Model, Solve, Value,
	ValueData,
};

/// Pretty printing for MIR
#[derive(Debug)]
pub struct PrettyPrinter<'db> {
	model: &'db Model<'db>,
}

impl<'db> PrettyPrinter<'db> {
	/// Create a new pretty printer
	pub fn new(model: &'db Model<'db>) -> Self {
		Self { model }
	}

	/// Print a model
	pub fn print_model(&self, db: &'db dyn Db) -> String {
		let mut w = String::new();
		for (_, a) in self.model.annotations.iter() {
			writeln!(&mut w, "{};", self.print_annotation(db, a)).unwrap();
		}
		for (f_idx, f) in self.model.functions.iter() {
			if f_idx == self.model.main_entrypoint || f_idx == self.model.output_entrypoint {
				continue;
			}
			writeln!(&mut w, "{};", self.print_function(db, f)).unwrap();
		}
		for (v_idx, v) in self.model.global_vars.iter() {
			writeln!(
				&mut w,
				"{}: {};",
				v.ty.pretty_print(db),
				v.name
					.map(|n| n.pretty_print(db))
					.unwrap_or_else(|| format!("_GLOBAL_{}", Into::<u32>::into(v_idx)))
			)
			.unwrap();
		}
		let main_entrypoint = &self.model.functions[self.model.main_entrypoint];
		for item in main_entrypoint.body.as_ref().unwrap().items.iter() {
			match item {
				LetItem::Declaration(d) => writeln!(
					&mut w,
					"{};",
					self.print_declaration(db, main_entrypoint, *d)
				)
				.unwrap(),
				LetItem::Constraint(c) => {
					writeln!(&mut w, "{};", self.print_constraint(db, main_entrypoint, c)).unwrap()
				}
				LetItem::GlobalAssignment(g) => writeln!(
					&mut w,
					"{} = {};",
					self.model.global_vars[g.global]
						.name
						.map(|n| n.pretty_print(db))
						.unwrap_or_else(|| format!("_GLOBAL_{}", Into::<u32>::into(g.global))),
					self.print_expression(db, main_entrypoint, &g.definition)
				)
				.unwrap(),
			}
		}
		let output_entrypoint = &self.model.functions[self.model.output_entrypoint];
		for item in output_entrypoint.body.as_ref().unwrap().items.iter() {
			writeln!(
				&mut w,
				"{};",
				self.print_declaration(db, output_entrypoint, *item.unwrap_declaration_ref())
			)
			.unwrap();
		}
		writeln!(
			&mut w,
			"solve {};",
			match &self.model.solve {
				Solve::Satisfy => "satisfy".to_owned(),
				Solve::Minimize(obj) => format!(
					"minimize {}",
					self.model.global_vars[*obj]
						.name
						.map(|n| n.pretty_print(db))
						.unwrap_or_else(|| format!("_GLOBAL_{}", Into::<u32>::into(*obj)))
				),
				Solve::Maximize(obj) => format!(
					"maximize {}",
					self.model.global_vars[*obj]
						.name
						.map(|n| n.pretty_print(db))
						.unwrap_or_else(|| format!("_GLOBAL_{}", Into::<u32>::into(*obj)))
				),
			}
		)
		.unwrap();
		w
	}

	/// Print a function item
	pub fn print_function(&self, db: &'db dyn Db, function: &Function<'db>) -> String {
		let mut w = String::new();
		write!(
			&mut w,
			"function {}: {}{} :: promise_total",
			function.ty.pretty_print(db),
			function.name.pretty_print(db),
			format!(
				"({})",
				function
					.parameters
					.iter()
					.map(|i| format!(
						"{}: {}",
						function.data[*i].ty.pretty_print(db),
						function.data[*i]
							.name
							.map(|n| n.pretty_print(db))
							.unwrap_or_else(|| format!("_PARAM_{}", Into::<u32>::into(*i)))
					))
					.collect::<Vec<_>>()
					.join(", ")
			)
		)
		.unwrap();
		if let Some(body) = &function.body {
			write!(&mut w, " = {}", self.print_let(db, &function, body)).unwrap();
		}
		w
	}

	/// Print an annotation item
	pub fn print_annotation(&self, db: &'db dyn Db, annotation: &Annotation<'db>) -> String {
		format!(
			"annotation {}{}",
			annotation.name.pretty_print(db),
			if annotation.parameters.is_empty() {
				"".to_owned()
			} else {
				format!(
					"({})",
					annotation
						.parameters
						.iter()
						.map(|(i, p)| format!(
							"{}: {}",
							p.ty.pretty_print(db),
							p.name
								.map(|n| n.pretty_print(db))
								.unwrap_or_else(|| format!("_PARAM_{}", Into::<u32>::into(i)))
						))
						.collect::<Vec<_>>()
						.join(", ")
				)
			}
		)
	}

	fn print_declaration(
		&self,
		db: &'db dyn Db,
		item: &Function<'db>,
		declaration: DeclarationId<'db>,
	) -> String {
		let decl = &item.data[declaration];
		format!(
			"{}: {}{} = {}",
			decl.ty.pretty_print(db),
			decl.name
				.map(|n| n.pretty_print(db))
				.unwrap_or_else(|| format!("_DECL_{}", Into::<u32>::into(declaration))),
			decl.annotations
				.iter()
				.map(|ann| self.print_identifier(db, item, ann))
				.map(|s| format!(" :: {}", s))
				.collect::<Vec<_>>()
				.join(""),
			self.print_expression(db, item, &decl.definition)
		)
	}

	fn print_constraint(
		&self,
		db: &'db dyn Db,
		item: &Function<'db>,
		constraint: &Constraint<'db>,
	) -> String {
		format!(
			"constraint{} {}",
			constraint
				.annotations
				.iter()
				.map(|ann| self.print_identifier(db, item, ann))
				.map(|s| format!(" :: {}", s))
				.collect::<Vec<_>>()
				.join(""),
			self.print_expression(db, item, &constraint.expression)
		)
	}

	/// Print a let
	pub fn print_let(&self, db: &'db dyn Db, item: &Function<'db>, l: &Let<'db>) -> String {
		let items = l
			.items
			.iter()
			.map(|i| match i {
				LetItem::Constraint(c) => {
					format!("{};", self.print_constraint(db, item, c))
				}
				LetItem::Declaration(d) => {
					format!("{};", self.print_declaration(db, item, *d))
				}
				LetItem::GlobalAssignment(_) => {
					unreachable!("Global assignments should be in main entrypoint")
				}
			})
			.collect::<Vec<_>>()
			.join("\n");
		let result = match &l.result {
			LetResult::Root => "true".to_owned(),
			LetResult::Identifier(result) => self.print_identifier(db, item, result),
		};
		format!("let {{\n{}\n}} in {}", items, result)
	}

	/// Print an expression
	pub fn print_expression(
		&self,
		db: &'db dyn Db,
		item: &Function<'db>,
		expression: &Expression<'db>,
	) -> String {
		self.print_expression_data(db, &item, &expression.data)
	}

	/// Print a value
	pub fn print_value(&self, db: &'db dyn Db, item: &Function<'db>, value: &Value<'db>) -> String {
		self.print_value_data(db, item, &value.data)
	}

	/// Print a literal
	pub fn print_literal(
		&self,
		db: &'db dyn Db,
		item: &Function<'db>,
		literal: &Literal<'db>,
	) -> String {
		self.print_literal_data(db, item, &literal.data)
	}

	fn print_expression_data(
		&self,
		db: &'db dyn Db,
		item: &Function<'db>,
		data: &ExpressionData<'db>,
	) -> String {
		match data {
			ExpressionData::Builtin(Builtin::VarBool) => {
				"(let { var bool: _FRESH_VAR :: promise_total; } in _FRESH_VAR)".to_owned()
			}
			ExpressionData::Builtin(Builtin::VarInt { domain }) => {
				format!(
					"(let {{ var {}: _FRESH_VAR; }} in _FRESH_VAR)",
					self.print_value(db, item, domain)
				)
			}
			ExpressionData::Builtin(Builtin::VarFloat { domain }) => {
				format!(
					"(let {{ var {}: _FRESH_VAR; }} in _FRESH_VAR)",
					self.print_value(db, item, domain)
				)
			}
			ExpressionData::Builtin(Builtin::VarSetOfInt { domain }) => {
				format!(
					"(let {{ var set of {}: _FRESH_VAR; }} in _FRESH_VAR)",
					self.print_value(db, item, domain)
				)
			}
			ExpressionData::Builtin(b) => format!(
				"{}({})",
				b.name(db).pretty_print(db),
				b.arguments()
					.into_iter()
					.map(|arg| self.print_value(db, item, arg))
					.collect::<Vec<_>>()
					.join(", ")
			),
			ExpressionData::Call(c) => {
				let f = self.model.functions[c.function].name.pretty_print(db);
				let args = c
					.arguments
					.iter()
					.map(|v| self.print_value(db, item, v))
					.collect::<Vec<_>>();
				format!("{}({})", f, args.join(", "))
			}
			ExpressionData::AnnotationCall(c) => {
				let f = self.model.annotations[c.annotation].name.pretty_print(db);
				let args = c
					.arguments
					.iter()
					.map(|v| self.print_value(db, item, v))
					.collect::<Vec<_>>();
				format!("{}({})", f, args.join(", "))
			}
			ExpressionData::Comprehension(c) => {
				let template = self.print_let(db, item, &c.template);
				let generators = c
					.generators
					.iter()
					.map(|g| match g {
						Generator::Assignment { declaration } => format!(
							"{} = {}",
							item.data[*declaration]
								.name
								.map(|n| n.pretty_print(db))
								.unwrap_or_else(|| format!(
									"_DECL_{}",
									Into::<u32>::into(*declaration)
								)),
							self.print_expression(db, item, &item.data[*declaration].definition),
						),
						Generator::Iterator { element } => {
							format!(
								"{} in {}",
								item.data[*element]
									.name
									.map(|n| n.pretty_print(db))
									.unwrap_or_else(|| format!(
										"_DECL_{}",
										Into::<u32>::into(*element)
									)),
								self.print_expression(db, item, &item.data[*element].definition),
							)
						}
						Generator::Constraint(c) => {
							format!(
								"_ = let {{ constraint {}; }} in 1",
								self.print_constraint(db, item, c)
							)
						}
						Generator::Where(w) => {
							format!("_ = 1 where {}", self.print_value(db, item, w))
						}
					})
					.collect::<Vec<_>>()
					.join(", ");

				format!("[{} | {}]", template, generators)
			}
			ExpressionData::Iterator(it) => self.print_value_data(db, item, it),
			ExpressionData::IfThenElse(ite) => {
				format!(
					"if {} then {} else {} endif",
					self.print_value(db, item, &ite.condition),
					self.print_let(db, item, &ite.then),
					self.print_let(db, item, &ite.else_expression)
				)
			}
			ExpressionData::Value(v) => self.print_value_data(db, item, v),
		}
	}

	fn print_value_data(
		&self,
		db: &'db dyn Db,
		item: &Function<'db>,
		data: &ValueData<'db>,
	) -> String {
		match data {
			ValueData::Array(a) => format!(
				"[{}]",
				a.members
					.iter()
					.map(|v| self.print_literal(db, item, v))
					.collect::<Vec<_>>()
					.join(", ")
			),
			ValueData::Literal(l) => self.print_literal_data(db, item, l),
			ValueData::Set(s) => format!(
				"{{{}}}",
				s.members
					.iter()
					.map(|v| self.print_literal(db, item, v))
					.collect::<Vec<_>>()
					.join(", ")
			),
			ValueData::Tuple(t) => {
				let mut members = t
					.members
					.iter()
					.map(|v| self.print_literal(db, item, v))
					.collect::<Vec<_>>();
				if t.members.len() <= 1 {
					members.push("".to_owned());
				}
				format!("({})", members.join(", "))
			}
			ValueData::TupleAccess(ta) => {
				format!(
					"{}.{}",
					self.print_identifier(db, item, &ta.tuple),
					ta.field.0
				)
			}
		}
	}

	fn print_literal_data(
		&self,
		db: &'db dyn Db,
		item: &Function<'db>,
		data: &LiteralData<'db>,
	) -> String {
		match data {
			LiteralData::Boolean(b) => if b.0 { "true" } else { "false" }.to_owned(),
			LiteralData::Float(f) => format!("{}", f.value()),
			LiteralData::Identifier(i) => self.print_identifier(db, item, i),
			LiteralData::Infinity => "infinity".to_owned(),
			LiteralData::InfiniteIntSet => "(-infinity..infinity)".to_owned(),
			LiteralData::InfiniteFloatSet => {
				"(let { float: l = -infinity; float: u = infinity; } in l..u)".to_owned()
			}
			LiteralData::Integer(i) => format!("{}", i.0),
			LiteralData::String(s) => format!("{:?}", s.value(db)),
		}
	}

	fn print_identifier(
		&self,
		db: &'db dyn Db,
		item: &Function<'db>,
		identifier: &Identifier<'db>,
	) -> String {
		match identifier {
			Identifier::Annotation(ann) => self.model.annotations[*ann].name.pretty_print(db),
			Identifier::Declaration(decl) => item.data[*decl]
				.name
				.map(|n| n.pretty_print(db))
				.unwrap_or_else(|| format!("_DECL_{}", Into::<u32>::into(*decl))),
			Identifier::Parameter(param) => item.data[*param]
				.name
				.map(|n| n.pretty_print(db))
				.unwrap_or_else(|| format!("_PARAM_{}", Into::<u32>::into(*param))),
			Identifier::GlobalVar(var) => self.model.global_vars[*var]
				.name
				.map(|n| n.pretty_print(db))
				.unwrap_or_else(|| format!("_GLOBAL_{}", Into::<u32>::into(*var))),
		}
	}
}
