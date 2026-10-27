use varlen::prelude::FromIterPrefix;

use crate::{
	Value,
	error::ValueError,
	value::{DataView, InitEmpty, ValType, define_var_ref, value_storage},
};

/// Flattened boolean variable
#[derive(Debug)]
pub struct BoolVar {
	ident: u64,
	domain: Option<bool>,
	alias: Option<Value>,
	binding_domain: bool,
}

impl PartialEq for BoolVar {
	fn eq(&self, other: &Self) -> bool {
		self.ident == other.ident
			|| if let Some(alias) = self.alias.as_ref() {
				*alias.as_bool_var() == *other
			} else {
				false
			}
	}
}
impl Eq for BoolVar {}

impl BoolVar {
	/// Get the unique identifier of the variable.
	pub fn ident(&self) -> u64 {
		self.ident
	}

	/// Get the domain of the variable.
	pub fn domain(&self) -> Option<bool> {
		self.domain
	}

	/// Set the domain of the variable.
	pub fn set_domain(&mut self, domain: bool) {
		self.domain = Some(domain);
	}

	/// Clear the domain of the variable.
	pub fn clear_domain(&mut self) {
		self.domain = None;
	}

	/// Whether the domain has been tightened by a non-defining constraint.
	pub fn binding_domain(&self) -> bool {
		self.binding_domain
	}

	/// Whether the domain has been tightened by a non-defining constraint.
	pub fn set_binding_domain(&mut self, binding_domain: bool) {
		self.binding_domain = binding_domain;
	}

	/// Whether the variable is fixed (i.e., has a domain).
	pub fn is_fixed(&self) -> bool {
		self.domain.is_some()
	}

	/// Get the fixed value of the variable, if it is fixed.
	pub fn fix(&self) -> Result<Value, ValueError> {
		if let Some(domain) = self.domain {
			Ok(domain.into())
		} else {
			Err(ValueError {
				reason: "Variable is not fixed",
			})
		}
	}
}

define_var_ref!(BoolVar, BoolVarRef);

impl Value {
	pub(crate) fn new_bool_var(ident: u64) -> Value {
		Self::new_box(value_storage::Init {
			ty: ValType::BoolVar,
			len: 0,
			ref_count: 1.into(),
			weak_count: 0.into(),
			values: InitEmpty,
			idents: InitEmpty,
			ints: InitEmpty,
			floats: InitEmpty,
			bool_var: FromIterPrefix(
				[std::sync::Mutex::new(BoolVar {
					ident,
					domain: None,
					alias: None,
					binding_domain: false,
				})]
				.into_iter(),
			),
			int_var: InitEmpty,
			float_var: InitEmpty,
			int_set_var: InitEmpty,
			bytes: InitEmpty,
		})
	}

	pub(crate) fn as_bool_var(&self) -> BoolVarRef<'_> {
		let DataView::BoolVar(bv) = self.deref() else {
			panic!("Expected bool var, found {:?}", self)
		};
		bv
	}
}
