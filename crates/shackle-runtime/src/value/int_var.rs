use varlen::prelude::FromIterPrefix;

use crate::{
	Value,
	error::ValueError,
	nanozinc::DefiningConstraints,
	value::{DataView, InitEmpty, ValType, define_var_ref, value_storage},
};

/// Flattened integer variable
#[derive(Debug)]
pub struct IntVar {
	ident: u64,
	domain: Option<Value>,
	alias: Option<Value>,
	defining_constraints: DefiningConstraints,
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

	/// Whether the variable is fixed
	pub fn is_fixed(&self) -> bool {
		if let Some(domain) = &self.domain {
			domain.as_int_set().card() == 1.into()
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

	pub(super) fn take_dead_contents(&mut self) -> Vec<Value> {
		let mut values = Vec::new();
		if let Some(value) = self.domain.take() {
			values.push(value);
		}
		if let Some(value) = self.alias.take() {
			values.push(value);
		}
		values
	}

	/// Get the defining constraints of the variable.
	pub fn defining_constraints(&self) -> &DefiningConstraints {
		&self.defining_constraints
	}

	/// Set the defining constraints of the variable.
	pub fn set_defining_constraints(&mut self, defining_constraints: DefiningConstraints) {
		self.defining_constraints = defining_constraints;
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
					defining_constraints: DefiningConstraints::default(),
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
