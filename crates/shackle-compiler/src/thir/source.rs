//! Source mapping between THIR and HIR nodes.
//!
//! Tracks desugarings performed when lowering HIR to THIR.

use std::fmt::Write;

use miette::SourceSpan;
use shackle_diagnostics::SourceFile;

use super::db::Thir;
use crate::hir::ids::{EntityRef, ItemRef, NodeRef};

/// The HIR node which produced a THIR node
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Origin {
	/// Comes from a real HIR node
	HirNode(NodeRef),
	/// Is introduced, and does not have a location
	Introduced(&'static str),
}

impl From<NodeRef> for Origin {
	fn from(node: NodeRef) -> Self {
		Self::HirNode(node)
	}
}

impl From<ItemRef> for Origin {
	fn from(item: ItemRef) -> Self {
		NodeRef::from(item).into()
	}
}

impl From<EntityRef> for Origin {
	fn from(entity: EntityRef) -> Self {
		NodeRef::from(entity).into()
	}
}

impl Origin {
	/// Get the underlying HIR node
	pub fn node(&self) -> Option<NodeRef> {
		match self {
			Origin::HirNode(node) => Some(*node),
			_ => None,
		}
	}

	/// Get the source file and span of this origin
	pub fn source_span(&self, db: &dyn Thir) -> (SourceFile, SourceSpan) {
		match self {
			Origin::HirNode(node) => node.source_span(db.upcast()),
			Origin::Introduced(name) => {
				(SourceFile::introduced(name), SourceSpan::new(0.into(), 0))
			}
		}
	}

	/// Pretty print this origin
	pub fn pretty_print(&self, db: &dyn Thir) -> String {
		let (src, span) = self.source_span(db);
		let mut from_line = 0;
		let mut from_char = 0;
		let mut to_line = 0;
		let mut to_char = 0;

		let mut iter = src.contents()[0..span.offset() + span.len()]
			.chars()
			.enumerate()
			.peekable();
		while let Some((i, char)) = iter.next() {
			if matches!(char, '\r' | '\n') {
				if i < span.offset() {
					from_line += 1;
					from_char = 0;
				}
				to_line += 1;
				to_char = 0;
				if char == '\r' {
					let _ = iter.next_if(|(_, c)| *c == '\n');
				}
			} else {
				if i < span.offset() {
					from_char += 1;
				}
				to_char += 1;
			}
		}
		let mut s = format!("{}:{}.{}", src.name(), from_line + 1, from_char + 1);
		if from_line != to_line {
			write!(&mut s, "-{}.{}", to_line + 1, to_char + 1).unwrap();
		} else if from_char != to_char {
			write!(&mut s, "-{}", to_char + 1).unwrap();
		}
		s
	}
}
