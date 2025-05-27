//! Storage and access for annotations
//!

use std::ops::{Deref, DerefMut};

use super::{
	Call, Callable, Expression, ExpressionData, Identifier, Marker, Model, ResolvedIdentifier,
};

/// Collection of annotations
#[derive(Clone, Debug, Hash, PartialEq, Eq, salsa::Update)]
pub struct Annotations<'db, T: Marker = ()> {
	annotations: Vec<Expression<'db, T>>,
}

impl<'db, T: Marker> Default for Annotations<'db, T> {
	fn default() -> Self {
		Self {
			annotations: Vec::new(),
		}
	}
}

impl<'db, T: Marker> Deref for Annotations<'db, T> {
	type Target = Vec<Expression<'db, T>>;

	fn deref(&self) -> &Self::Target {
		&self.annotations
	}
}

impl<'db, T: Marker> DerefMut for Annotations<'db, T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.annotations
	}
}

impl<'db, T: Marker> Annotations<'db, T> {
	/// Whether or not there is is an annotation atom with the given name
	pub fn has(&self, model: &'db Model<'db, T>, name: Identifier<'db>) -> bool {
		self.annotations.iter().any(|ann| match &**ann {
			ExpressionData::Identifier(ResolvedIdentifier::Annotation(item)) => {
				model[*item].name == Some(name)
			}
			_ => false,
		})
	}

	/// Find an annotation which is a call with the given name
	pub fn get_call(
		&self,
		model: &'db Model<'db, T>,
		name: Identifier<'db>,
	) -> Option<&Expression<'db, T>> {
		self.annotations.iter().find(|ann| match &***ann {
			ExpressionData::Call(Call {
				function: Callable::Annotation(item),
				..
			}) => model[*item].name == Some(name),
			_ => false,
		})
	}
}
