//! This crate provides minizinc language support for the [tree-sitter][] parsing library.
//!
//! Typically, you will use the [language][language func] function to add this language to a
//! tree-sitter [Parser][], and then use the parser to parse some code:
//!
//! ```
//! let code = "";
//! let mut parser = tree_sitter::Parser::new();
//! parser.set_language(&tree_sitter_minizinc::language()).expect("Error loading minizinc grammar");
//! let tree = parser.parse(code, None).unwrap();
//! ```
//!
//! [Language]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Language.html
//! [language func]: fn.language.html
//! [Parser]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Parser.html
//! [tree-sitter]: https://tree-sitter.github.io/

use tree_sitter::Language;

extern "C" {
	fn tree_sitter_minizinc() -> Language;
}

/// Get the tree-sitter [Language][] for this grammar.
///
/// [Language]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Language.html
pub fn language() -> Language {
	unsafe { tree_sitter_minizinc() }
}

/// The content of the [`node-types.json`][] file for this grammar.
///
/// [`node-types.json`]: https://tree-sitter.github.io/tree-sitter/using-parsers#static-node-types
pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

// Uncomment these to include any queries that this grammar contains

pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");
// pub const INJECTIONS_QUERY: &'static str = include_str!("../../queries/injections.scm");
// pub const LOCALS_QUERY: &'static str = include_str!("../../queries/locals.scm");
pub const TAGS_QUERY: &str = include_str!("../../queries/tags.scm");

/// Get identifier names
pub const IDENTIFIERS_QUERY: &str = include_str!("../../queries/identifiers.scm");

/// Get case expressions
pub const CASE_EXPRESSION_QUERY: &str = include_str!("../../queries/case_expressions.scm");

/// Get comments
pub const COMMENTS_QUERY: &str = include_str!("../../queries/comments.scm");

/// Grammar precedence value
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Precedence {
	/// Specified using `prec.left`
	Left(i64),
	/// Specified using `prec`
	Prec(i64),
	/// Specified using `prec.right`
	Right(i64),
	/// Non associative
	NonAssoc(i64),
}

impl Precedence {
	/// Get the precedence value
	pub fn get(&self) -> i64 {
		match self {
			Precedence::Left(i)
			| Precedence::Prec(i)
			| Precedence::Right(i)
			| Precedence::NonAssoc(i) => *i,
		}
	}
}

include!(concat!(env!("OUT_DIR"), "/precedence.rs"));

#[cfg(test)]
mod tests {
	#[test]
	fn test_can_load_grammar() {
		let mut parser = tree_sitter::Parser::new();
		parser
			.set_language(&super::language())
			.expect("Error loading minizinc language");
	}
}
