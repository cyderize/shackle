//! This crate provides Minizinc language support for the [tree-sitter][] parsing library.
//!
//! Typically, you will use the [LANGUAGE][] constant to add this language to a
//! tree-sitter [Parser][], and then use the parser to parse some code:
//!
//! ```
//! let code = r#"
//! "#;
//! let mut parser = tree_sitter::Parser::new();
//! let language = tree_sitter_minizinc::LANGUAGE;
//! parser
//!     .set_language(&language.into())
//!     .expect("Error loading Minizinc parser");
//! let tree = parser.parse(code, None).unwrap();
//! assert!(!tree.root_node().has_error());
//! ```
//!
//! [Parser]: https://docs.rs/tree-sitter/*/tree_sitter/struct.Parser.html
//! [tree-sitter]: https://tree-sitter.github.io/

use tree_sitter_language::LanguageFn;

extern "C" {
	fn tree_sitter_minizinc() -> *const ();
}

/// The tree-sitter [`LanguageFn`][LanguageFn] for this grammar.
///
/// [LanguageFn]: https://docs.rs/tree-sitter-language/*/tree_sitter_language/struct.LanguageFn.html
pub const LANGUAGE: LanguageFn = unsafe { LanguageFn::from_raw(tree_sitter_minizinc) };

/// The content of the [`node-types.json`][] file for this grammar.
///
/// [`node-types.json`]: https://tree-sitter.github.io/tree-sitter/using-parsers/6-static-node-types
pub const NODE_TYPES: &str = include_str!("../../src/node-types.json");

// NOTE: uncomment these to include any queries that this grammar contains:

pub const HIGHLIGHTS_QUERY: &str = include_str!("../../queries/highlights.scm");
// pub const INJECTIONS_QUERY: &str = include_str!("../../queries/injections.scm");
// pub const LOCALS_QUERY: &str = include_str!("../../queries/locals.scm");
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
			.set_language(&super::LANGUAGE.into())
			.expect("Error loading Minizinc parser");
	}
}
