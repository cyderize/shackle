use std::path::Path;

use pretty_assertions::assert_str_eq;
use shackle_fmt::{MiniZincFormatOptions, format_str};
use tree_sitter::Parser;

pub(crate) fn check_format_file(path: &Path, options: &MiniZincFormatOptions) -> String {
	let source = std::fs::read_to_string(path)
		.unwrap_or_else(|err| panic!("Failed to read {} ({})", path.to_string_lossy(), err));
	let mut parser = Parser::new();
	parser
		.set_language(&tree_sitter_minizinc::LANGUAGE.into())
		.unwrap();
	let formatted = format_str(&source, options).unwrap_or_else(|e| {
		panic!("Failed to format {} {:?}", path.to_string_lossy(), e);
	});
	// TODO: Check that THIR for both matches as well
	let reformatted = format_str(&formatted, options).unwrap_or_else(|e| {
		panic!("Failed to reformat {} {:?}", path.to_string_lossy(), e);
	});
	assert_str_eq!(
		reformatted,
		formatted,
		"Second format of {} didn't match",
		path.to_string_lossy()
	);
	formatted
}
