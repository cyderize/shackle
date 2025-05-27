//! Handling of errors and warnings during compilation

pub mod error;
pub mod warning;

use std::{
	path::{Path, PathBuf},
	sync::Arc,
};

pub use error::*;
use miette::{MietteSpanContents, SourceCode};
pub use warning::*;

/// Result type for Shackle operations
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Source file/text for error reporting
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SourceFile(Arc<SourceFileInner>);

pub use miette::SourceSpan;

/// Source file/text for error reporting
#[derive(Clone, PartialEq, Eq, Hash)]
pub(crate) enum SourceFileInner {
	/// File from the filesystem
	File {
		/// The file path
		path: PathBuf,
		/// The string contents
		contents: String,
	},
	/// Unnamed (e.g. provided inline on the command line)
	Unnamed {
		/// The string contents
		contents: String,
	},
	/// Introduced programmatically (should be avoided unless the construct is guaranteed not to produce an error)
	Introduced {
		/// A label to display for debugging
		label: &'static str,
	},
}

impl std::fmt::Debug for SourceFile {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("SourceFile")
			.field("name", &self.name())
			.field(
				"source",
				&format!("<{} byte string>", self.contents().len()),
			)
			.finish()
	}
}

impl SourceFile {
	/// Create a new source file
	pub fn new(path: PathBuf, contents: String) -> Self {
		Self(Arc::new(SourceFileInner::File { path, contents }))
	}

	/// Create a new unnamed source file
	pub fn unnamed(contents: String) -> Self {
		Self(Arc::new(SourceFileInner::Unnamed { contents }))
	}

	/// Create a new introduced source file
	pub fn introduced(label: &'static str) -> Self {
		Self(Arc::new(SourceFileInner::Introduced { label }))
	}

	/// Get the path for this source file if any
	pub fn path(&self) -> Option<&Path> {
		match self.0.as_ref() {
			SourceFileInner::File { path, .. } => Some(path.as_ref()),
			_ => None,
		}
	}

	/// Get the pretty name of this source file
	pub fn name(&self) -> String {
		match self.0.as_ref() {
			SourceFileInner::File { path, .. } => std::env::current_dir()
				.ok()
				.and_then(|c| c.canonicalize().ok())
				.and_then(move |c| path.strip_prefix(c).ok().map(|p| p.to_owned()))
				.unwrap_or_else(|| path.clone())
				.to_string_lossy()
				.to_string(),
			SourceFileInner::Unnamed { .. } => "<unnamed file>".to_owned(),
			SourceFileInner::Introduced { label } => (*label).to_owned(),
		}
	}

	/// Get the contents of this source file
	pub fn contents(&self) -> &str {
		match self.0.as_ref() {
			SourceFileInner::File { contents, .. } | SourceFileInner::Unnamed { contents } => {
				contents
			}
			SourceFileInner::Introduced { .. } => "",
		}
	}
}

impl SourceCode for SourceFile {
	fn read_span<'a>(
		&'a self,
		span: &SourceSpan,
		context_lines_before: usize,
		context_lines_after: usize,
	) -> Result<Box<dyn miette::SpanContents<'a> + 'a>, miette::MietteError> {
		let contents =
			self.contents()
				.read_span(span, context_lines_before, context_lines_after)?;

		Ok(Box::new(MietteSpanContents::new_named(
			self.name(),
			contents.data(),
			*contents.span(),
			contents.line(),
			contents.column(),
			contents.line_count(),
		)))
	}
}
