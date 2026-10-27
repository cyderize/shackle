use varlen::prelude::FromIterPrefix;

use crate::{
	Value,
	error::ValueError,
	value::{DataView, InitEmpty, ValType, define_var_ref, value_storage},
};

/// Flattened integer variable
#[derive(Debug)]
pub struct IntVar {
	ident: u64,
	domain: Option<Value>,
	alias: Option<Value>,
	binding_domain: bool,
}

impl PartialEq for IntVar {
	fn eq(&self, other: &Self) -> bool {
		self.ident == other.ident
			|| if let Some(alias) = self.alias.as_ref() {
				*alias.as_int_var() == *other
			} else {
				false
			}
	}
}
impl Eq for IntVar {}

impl IntVar {
	/// Get the unique identifier of the variable.
	pub fn ident(&self) -> u64 {
		self.ident
	}

	/// Get the domain of the variable.
	pub fn domain(&self) -> Option<Value> {
		self.domain.clone()
	}

	/// Set the domain of the variable.
	pub fn set_domain(&mut self, domain: Value) {
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

	/// Whether the variable is fixed
	pub fn is_fixed(&self) -> bool {
		if let Some(domain) = &self.domain {
			domain.as_float_set().card() == 1.into()
		} else {
			false
		}
	}

	/// Get the fixed value of the variable, if it is fixed.
	pub fn fix(&self) -> Result<Value, ValueError> {
		if let Some(domain) = &self.domain {
			let d = domain.as_int_set();
			if d.card() == 1.into() {
				return Ok(d.lower_bound().into());
			}
		}
		Err(ValueError {
			reason: "Variable is not fixed",
		})
	}
}

define_var_ref!(IntVar, IntVarRef);

impl Value {
	pub(crate) fn new_int_var(ident: u64, domain: Value) -> Value {
		Self::new_box(value_storage::Init {
			ty: ValType::IntVar,
			len: 0,
			ref_count: 1.into(),
			weak_count: 0.into(),
			values: InitEmpty,
			idents: InitEmpty,
			ints: InitEmpty,
			floats: InitEmpty,
			bool_var: InitEmpty,
			int_var: FromIterPrefix(
				[std::sync::Mutex::new(IntVar {
					ident,
					domain: Some(domain),
					alias: None,
					binding_domain: false,
				})]
				.into_iter(),
			),
			float_var: InitEmpty,
			int_set_var: InitEmpty,
			bytes: InitEmpty,
		})
	}

	pub(crate) fn as_int_var(&self) -> IntVarRef<'_> {
		let DataView::IntVar(iv) = self.deref() else {
			panic!("Expected int var, found {:?}", self)
		};
		iv
	}
}
