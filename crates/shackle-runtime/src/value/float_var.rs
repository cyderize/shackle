use varlen::prelude::FromIterPrefix;

use crate::{
	Value,
	error::ValueError,
	nanozinc::DefiningConstraints,
	value::{DataView, InitEmpty, ValType, define_var_ref, value_storage},
};

/// Flattened float variable
#[derive(Debug)]
pub struct FloatVar {
	ident: u64,
	domain: Option<Value>,
	alias: Option<Value>,
	defining_constraints: DefiningConstraints,
	output: bool,
}

impl PartialEq for FloatVar {
	fn eq(&self, other: &Self) -> bool {
		self.ident == other.ident
			|| if let Some(alias) = self.alias.as_ref() {
				*alias.as_float_var() == *other
			} else {
				false
			}
	}
}
impl Eq for FloatVar {}

impl FloatVar {
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
			domain.as_float_set().card() == 1.into()
		} else {
			false
		}
	}

	/// Get the fixed value of the variable, if it is fixed.
	pub fn fix(&self) -> Result<Value, ValueError> {
		if let Some(domain) = &self.domain {
			let d = domain.as_float_set();
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

	/// Get the output flag of the variable.
	pub fn output(&self) -> bool {
		self.output
	}

	/// Set the output flag of the variable.
	pub fn set_output(&mut self, output: bool) {
		self.output = output;
	}
}

define_var_ref!(FloatVar, FloatVarRef);

impl Value {
	pub(crate) fn new_float_var(ident: u64, domain: Value) -> Value {
		Self::new_box(value_storage::Init {
			ty: ValType::FloatVar,
			len: 0,
			ref_count: 1.into(),
			weak_count: 0.into(),
			values: InitEmpty,
			idents: InitEmpty,
			ints: InitEmpty,
			floats: InitEmpty,
			bool_var: InitEmpty,
			int_var: InitEmpty,
			float_var: FromIterPrefix(
				[std::sync::Mutex::new(FloatVar {
					ident,
					domain: Some(domain),
					alias: None,
					defining_constraints: DefiningConstraints::default(),
					output: false,
				})]
				.into_iter(),
			),
			int_set_var: InitEmpty,
			bytes: InitEmpty,
		})
	}

	pub(crate) fn as_float_var(&self) -> FloatVarRef<'_> {
		let DataView::FloatVar(fv) = self.deref() else {
			panic!("Expected float var, found {:?}", self)
		};
		fv
	}
}
