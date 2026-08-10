use shackle_syntax::minizinc;
use tree_sitter_minizinc::Precedence;

use crate::{
	format::{Format, MiniZincFormatter},
	ir::Element,
};

impl<'tree> Format for minizinc::ArrayLiteral<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		formatter.format_list("[", "]", self.members())
	}
}

impl<'tree> Format for minizinc::ArrayMember<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		match self {
			minizinc::ArrayMember::Indexed(m) => m.format(formatter),
			minizinc::ArrayMember::Value(v) => v.format(formatter),
		}
	}
}

impl<'tree> Format for minizinc::ArrayLiteralMember<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		Element::sequence(vec![
			self.indices().format(formatter),
			Element::text(": "),
			self.value().format(formatter),
		])
	}
}

impl<'tree> Format for minizinc::ArrayLiteral2D<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let mut elements = Vec::new();
		let indices = self.column_indices().collect::<Vec<_>>();
		let rows = self.rows().collect::<Vec<_>>();
		if !indices.is_empty() {
			elements.push(Element::group(vec![Element::join(
				self.column_indices()
					.map(|i| Element::sequence(vec![i.format(formatter), Element::text(":")])),
				vec![Element::line_break_or_space()],
			)]));
		}
		elements.extend(rows.iter().map(|r| r.format(formatter)));

		Element::sequence(vec![
			Element::text("[|"),
			Element::group(vec![
				Element::indent(vec![
					Element::line_break_or_space(),
					Element::join(elements, vec![Element::text(" |"), Element::line_break()]),
				]),
				Element::line_break_or_space(),
			]),
			Element::text("|]"),
		])
	}
}

impl<'tree> Format for minizinc::ArrayLiteral2DRow<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let mut elements = Vec::new();
		if let Some(idx) = self.index() {
			elements.push(idx.format(formatter));
			elements.push(Element::text(": "));
		}
		elements.push(Element::join(
			self.members().map(|e| e.format(formatter)),
			vec![Element::text(","), Element::line_break_or_space()],
		));
		Element::group(elements)
	}
}

impl<'tree> Format for minizinc::ArrayLiteral3D<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let slices = self
			.slices()
			.map(|s| s.format(formatter))
			.collect::<Vec<_>>();
		Element::sequence(vec![
			Element::text("[|"),
			Element::group(vec![
				Element::indent(vec![
					Element::line_break_or_space(),
					Element::join(slices, vec![Element::text(","), Element::line_break()]),
				]),
				Element::line_break_or_space(),
			]),
			Element::text("|]"),
		])
	}
}

impl<'tree> Format for minizinc::ArrayLiteral3DSlice<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let rows = self.rows().map(|r| r.format(formatter)).collect::<Vec<_>>();
		Element::group(vec![
			Element::text("|"),
			Element::join(rows, vec![Element::text(" |"), Element::line_break()]),
			Element::text("|"),
		])
	}
}

impl<'tree> Format for minizinc::ArrayLiteral3DRow<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		Element::group(vec![Element::join(
			self.members().map(|e| e.format(formatter)),
			vec![Element::text(","), Element::line_break_or_space()],
		)])
	}
}

impl<'tree> Format for minizinc::ArrayAccess<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let needs_parentheses = !formatter.options().keep_parentheses
			&& Precedence::indexed_access().get() > formatter.precedence(&self.collection()).get();
		Element::sequence(vec![
			if needs_parentheses {
				formatter.parenthesise(self.collection())
			} else {
				self.collection().format(formatter)
			},
			formatter.format_list("[", "]", self.indices()),
		])
	}
}

impl<'tree> Format for minizinc::ArrayIndex<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		match self {
			minizinc::ArrayIndex::IndexSlice(x) => {
				formatter.attach_comments(self, vec![Element::text(x.operator())])
			}
			minizinc::ArrayIndex::Expression(e) => e.format(formatter),
		}
	}
}

impl<'tree> Format for minizinc::ArrayComprehension<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		Element::sequence(vec![
			Element::text("["),
			Element::group(vec![
				Element::indent(vec![
					Element::line_break_or_empty(),
					Element::sequence(if let Some(indices) = self.indices() {
						vec![indices.format(formatter), Element::text(": ")]
					} else {
						vec![]
					}),
					self.template().format(formatter),
					Element::text(" |"),
					Element::indent(vec![
						Element::line_break_or_space(),
						Element::join(
							self.generators().map(|g| g.format(formatter)),
							vec![Element::text(","), Element::line_break_or_space()],
						),
						Element::if_broken(vec![Element::text(",")]),
					]),
				]),
				Element::line_break_or_empty(),
			]),
			Element::text("]"),
		])
	}
}

impl<'tree> Format for minizinc::SetLiteral<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		formatter.format_list("{", "}", self.members())
	}
}

impl<'tree> Format for minizinc::SetComprehension<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		Element::sequence(vec![
			Element::text("{"),
			Element::group(vec![
				Element::indent(vec![
					Element::line_break_or_empty(),
					self.template().format(formatter),
					Element::text(" |"),
					Element::indent(vec![
						Element::line_break_or_space(),
						Element::join(
							self.generators().map(|g| g.format(formatter)),
							vec![Element::text(","), Element::line_break_or_space()],
						),
						Element::if_broken(vec![Element::text(",")]),
					]),
				]),
				Element::line_break_or_empty(),
			]),
			Element::text("}"),
		])
	}
}

impl<'tree> Format for minizinc::Generator<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let e = match self {
			minizinc::Generator::AssignmentGenerator(a) => a.format(formatter),
			minizinc::Generator::IteratorGenerator(i) => i.format(formatter),
		};
		formatter.attach_comments(self, vec![e])
	}
}

impl<'tree> Format for minizinc::IteratorGenerator<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let mut elements = vec![
			Element::join(
				self.patterns().map(|p| p.format(formatter)),
				vec![Element::text(", ")],
			),
			Element::text(" in"),
			Element::group(vec![Element::indent(vec![
				Element::line_break_or_space(),
				self.collection().format(formatter),
			])]),
		];
		if let Some(w) = self.where_clause() {
			elements.push(Element::group(vec![Element::indent(vec![
				Element::line_break_or_space(),
				Element::text("where "),
				w.format(formatter),
			])]));
		}
		Element::sequence(elements)
	}
}

impl<'tree> Format for minizinc::AssignmentGenerator<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let mut elements = vec![
			self.pattern().format(formatter),
			Element::text(" ="),
			Element::group(vec![Element::indent(vec![
				Element::line_break_or_space(),
				self.value().format(formatter),
			])]),
		];
		if let Some(w) = self.where_clause() {
			elements.push(Element::group(vec![Element::indent(vec![
				Element::line_break_or_space(),
				Element::text("where "),
				w.format(formatter),
			])]));
		}
		Element::sequence(elements)
	}
}

impl<'tree> Format for minizinc::TupleLiteral<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let members = self.members().collect::<Vec<_>>();
		if members.is_empty() {
			return Element::text("(,)");
		}
		if members.len() == 1 {
			return Element::group(vec![
				Element::text("("),
				Element::indent(vec![
					Element::line_break_or_empty(),
					members[0].format(formatter),
				]),
				Element::line_break_or_empty(),
				Element::text(",)"),
			]);
		}
		formatter.format_list("(", ")", members.into_iter())
	}
}

impl<'tree> Format for minizinc::TupleAccess<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let needs_parentheses = !formatter.options().keep_parentheses
			&& Precedence::tuple_access().get() > formatter.precedence(&self.tuple()).get();
		Element::sequence(vec![
			if needs_parentheses {
				formatter.parenthesise(self.tuple())
			} else {
				self.tuple().format(formatter)
			},
			Element::text("."),
			minizinc::Expression::IntegerLiteral(self.field()).format(formatter),
		])
	}
}

impl<'tree> Format for minizinc::RecordLiteral<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		formatter.format_list("(", ")", self.members())
	}
}

impl<'tree> Format for minizinc::RecordLiteralMember<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		Element::sequence(vec![
			minizinc::Expression::Identifier(self.name()).format(formatter),
			Element::text(": "),
			self.value().format(formatter),
		])
	}
}

impl<'tree> Format for minizinc::RecordAccess<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let needs_parentheses = !formatter.options().keep_parentheses
			&& Precedence::record_access().get() > formatter.precedence(&self.record()).get();
		Element::sequence(vec![
			if needs_parentheses {
				formatter.parenthesise(self.record())
			} else {
				self.record().format(formatter)
			},
			Element::text("."),
			minizinc::Expression::Identifier(self.field()).format(formatter),
		])
	}
}
