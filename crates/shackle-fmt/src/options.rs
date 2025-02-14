use serde::{Deserialize, Serialize};

/// Trait for core formatting options
pub trait FormatOptions {
	/// Target maximum line length
	fn line_width(&self) -> usize;
	/// Whether to indent using tabs
	fn use_tabs(&self) -> bool;
	/// Size of indent
	fn indent_size(&self) -> usize;
}

/// Formatting options for MiniZinc
#[derive(Clone, Debug, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct MiniZincFormatOptions {
	/// Target maximum line length
	pub line_width: usize,
	/// Whether to indent using tabs
	pub use_tabs: bool,
	/// Size of indent
	pub indent_size: usize,

	/// Keep parentheses (except double parentheses)
	pub keep_parentheses: bool,
}

impl Default for MiniZincFormatOptions {
	fn default() -> Self {
		Self {
			line_width: 100,
			use_tabs: true,
			indent_size: 4,
			keep_parentheses: true,
		}
	}
}

impl FormatOptions for MiniZincFormatOptions {
	fn line_width(&self) -> usize {
		self.line_width
	}

	fn use_tabs(&self) -> bool {
		self.use_tabs
	}

	fn indent_size(&self) -> usize {
		self.indent_size
	}
}
