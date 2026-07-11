//! AST representation
//!
//! AST nodes are thin wrappers around CST nodes and provide type-safe access
//! methods. No desugaring is performed at this stage, so all language constructs
//! are available other than parentheses which are implicit in the tree structure.

use std::fmt::Debug;

pub mod container;
mod documentation;
pub mod expression;
pub mod item;
pub mod pattern;
pub mod primitive;
pub mod types;

pub use container::*;
pub use documentation::*;
pub use expression::*;
pub use item::*;
pub use pattern::*;
pub use primitive::*;
pub use types::*;

use super::{ast::Children, cst::Cst};

/// MznModel (wrapper for a CST).
///
/// A model is a single `.mzn` file.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct MznModel {
	/// Concrete syntax tree backing the model.
	cst: Cst,
}

impl MznModel {
	/// Create a model from a CST
	pub fn new(cst: Cst) -> Self {
		Self { cst }
	}

	/// Get the CST
	pub fn cst(&self) -> &Cst {
		&self.cst
	}

	/// Get the top level items in the model
	pub fn items(&self) -> Children<'_, Item<'_>> {
		self.cst().root().children_with_field_name("item").into()
	}
}

impl Debug for MznModel {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("Model")
			.field("items", &self.items())
			.finish()
	}
}

/// DznFile (wrapper for a CST).
///
/// A single `.dzn` file
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct DznFile {
	/// Concrete syntax tree backing the file.
	cst: Cst,
}

impl DznFile {
	/// Create a DZN file from a CST
	pub fn new(cst: Cst) -> Self {
		Self { cst }
	}

	/// Get the CST
	pub fn cst(&self) -> &Cst {
		&self.cst
	}

	/// Get the assignment items
	pub fn items(&self) -> Children<'_, Assignment<'_>> {
		self.cst().root().children_with_field_name("item").into()
	}
}

impl Debug for DznFile {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("DznFile")
			.field("items", &self.items())
			.finish()
	}
}

#[cfg(test)]
mod tests {
	use expect_test::{expect, expect_file};

	use crate::{
		ast::{AstNode, tests::*},
		cst::Cst,
		minizinc::{Declaration, MznModel},
	};

	#[test]
	fn test_model() {
		check_ast(
			r#"% Line comment"#,
			expect!([r#"
MznModel(
    Model {
        items: [],
    },
)
"#]),
		);
	}

	#[test]
	fn test_doc_simple_model() {
		check_ast_file(
			include_str!("../../../../docs/src/examples/simple-model.mzn"),
			expect_file!("../../../../docs/src/examples/simple-model-ast.txt"),
		);
	}

	#[test]
	fn test_doc_comment_association() {
		let source = "/*** @groupdef ignored */\n/** Value docs */\nint: value;\n/* ordinary */\nint: other;";
		let model = MznModel::new(Cst::new(source, crate::InputLang::MiniZinc));
		let mut items = model.items();
		let value = items.next().unwrap().cast::<Declaration>().unwrap();
		let other = items.next().unwrap().cast::<Declaration>().unwrap();
		assert_eq!(
			value.doc_comment().map(|comment| comment.text(source)),
			Some("/** Value docs */")
		);
		assert!(other.doc_comment().is_none());
		assert_eq!(
			model.cst().root().children().next().unwrap().kind(),
			"file_doc_comment"
		);
	}
}
