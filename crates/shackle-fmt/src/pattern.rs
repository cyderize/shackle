use shackle_syntax::{
	ast::AstNode,
	minizinc::{self, PatternNumericLiteral, pretty_print_identifier},
};

use crate::{
	format::{Format, MiniZincFormatter},
	ir::Element,
};

impl<'tree> Format for minizinc::Pattern<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let e = match self {
			minizinc::Pattern::Absent(a) => Element::text(a.cst_text(formatter.source())),
			minizinc::Pattern::Anonymous(a) => Element::text(a.cst_text(formatter.source())),
			minizinc::Pattern::BooleanLiteral(b) => {
				Element::text(if b.value() { "true" } else { "false" })
			}
			minizinc::Pattern::Call(c) => c.format(formatter),
			minizinc::Pattern::Identifier(i) => {
				Element::text(pretty_print_identifier(&i.name(formatter.source())))
			}
			minizinc::Pattern::PatternNumericLiteral(n) => n.format(formatter),
			minizinc::Pattern::StringLiteral(s) => Element::text(s.cst_text(formatter.source())),
			minizinc::Pattern::Tuple(t) => t.format(formatter),
			minizinc::Pattern::Record(r) => formatter.format_list("(", ")", r.fields()),
		};
		formatter.attach_comments(self, vec![e])
	}

	fn has_brackets(&self, _formatter: &MiniZincFormatter) -> bool {
		matches!(
			self,
			minizinc::Pattern::Tuple(_) | minizinc::Pattern::Record(_)
		)
	}
}

impl<'tree> Format for PatternNumericLiteral<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		if self.negated() {
			Element::sequence(vec![
				Element::text("-"),
				Element::text(self.value().cst_text(formatter.source())),
			])
		} else {
			Element::text(self.value().cst_text(formatter.source()))
		}
	}
}

impl<'tree> Format for minizinc::PatternCall<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		Element::sequence(vec![
			minizinc::Expression::Identifier(self.identifier()).format(formatter),
			formatter.format_list("(", ")", self.arguments()),
		])
	}
}

impl<'tree> Format for minizinc::PatternTuple<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		let fields = self.fields().collect::<Vec<_>>();
		if fields.is_empty() {
			return Element::text("(,)");
		}
		if fields.len() == 1 {
			return Element::group(vec![
				Element::text("("),
				Element::indent(vec![
					Element::line_break_or_empty(),
					fields[0].format(formatter),
				]),
				Element::line_break_or_empty(),
				Element::text(",)"),
			]);
		}
		formatter.format_list("(", ")", fields.into_iter())
	}
}

impl<'tree> Format for minizinc::PatternRecordField<'tree> {
	fn format(&self, formatter: &mut MiniZincFormatter) -> Element {
		Element::sequence(vec![
			minizinc::Expression::Identifier(self.name()).format(formatter),
			Element::text(": "),
			self.value().format(formatter),
		])
	}
}
