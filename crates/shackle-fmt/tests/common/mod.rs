use std::path::Path;

use pretty_assertions::assert_str_eq;
use shackle_diagnostics::SourceFile;
use shackle_fmt::{format_model, MiniZincFormatOptions};
use shackle_syntax::{cst::Cst, minizinc::MznModel};
use tree_sitter::Parser;

pub fn check_format_file(path: &Path, options: &MiniZincFormatOptions) -> String {
	let source = std::fs::read_to_string(path)
		.unwrap_or_else(|err| panic!("Failed to read {} ({})", path.to_string_lossy(), err));
	let mut parser = Parser::new();
	parser
		.set_language(&tree_sitter_minizinc::language())
		.unwrap();
	let tree = parser.parse(source.as_bytes(), None).unwrap();
	let model = MznModel::new(Cst::new(tree, SourceFile::unnamed(source.to_owned())));
	let formatted = format_model(&model, options).unwrap_or_else(|e| {
		panic!("Failed to format {} {:?}", path.to_string_lossy(), e);
	});
	let formatted_tree = parser.parse(formatted.as_bytes(), None).unwrap();
	let formatted_model = MznModel::new(Cst::new(
		formatted_tree,
		SourceFile::unnamed(formatted.clone()),
	));
	// TODO: Check that THIR for both matches as well
	let reformatted = format_model(&formatted_model, options).unwrap_or_else(|e| {
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
