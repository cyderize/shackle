//! Syntax representations.
//!
//! A concrete syntax tree is created by the parser.
//! This completely represents the source text, including comments and whitespace.
//!
//! Since this is not convenient for most stages of compilation, an abstract syntax tree is
//! generated which provides type-safe access to children. The AST includes all language constructs
//! (i.e. no desugaring is performed, just removal of non-semantic nodes).
//!
//! The AST is then lowered into HIR, which is the main representation used by the compiler.
//!

use std::path::Path;

pub mod ast;
pub mod cst;

// AST representations for different modelling languages
pub mod eprime;
pub mod minizinc;

/// Input languages
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum InputLang {
	/// MiniZinc modelling language
	MiniZinc,
	/// Essence' modelling language
	EPrime,
	/// DataZinc data input language
	DataZinc,
	/// JSON data input language
	Json,
}

impl InputLang {
	/// Guess the input language based on a file extension.
	///
	/// Note that this function will use MiniZinc as a fallback
	pub fn from_path(path: impl AsRef<Path>) -> Self {
		Self::from_extension(
			&path
				.as_ref()
				.extension()
				.map(|ext| ext.to_string_lossy())
				.unwrap_or_default(),
		)
	}

	/// Guess the input language based on a file extension.
	///
	/// Note that this function will use MiniZinc as a fallback
	pub fn from_extension(ext: &str) -> Self {
		match ext {
			"mzn" => Self::MiniZinc,
			"eprime" => Self::EPrime,
			"dzn" => Self::DataZinc,
			"json" => Self::Json,
			_ => Self::MiniZinc,
		}
	}
}

pub use tree_sitter_minizinc::{
	Precedence, is_absent, is_anonymous, is_any_type, is_infix_operator, is_postfix_operator,
	is_prefix_operator,
};
