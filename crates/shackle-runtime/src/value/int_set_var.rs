use rangelist::IntervalIterator;
use varlen::prelude::FromIterPrefix;

use crate::{
	Value,
	error::ValueError,
	nanozinc::DefiningConstraints,
	value::{DataView, InitEmpty, ValType, define_var_ref, value_storage},
};

/// Flattened integer set variable
#[derive(Debug)]
pub struct IntSetVar {
	ident: u64,
	domain: Option<Value>,
	card: Option<Value>,
	alias: Option<Value>,
	defining_constraints: DefiningConstraints,
}

impl PartialEq for IntSetVar {
	fn eq(&self, other: &Self) -> bool {
		self.ident == other.ident
			|| if let Some(alias) = self.alias.as_ref() {
				*alias.as_int_set_var() == *other
			} else {
				false
			}
	}
}
impl Eq for IntSetVar {}

impl IntSetVar {
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

	/// Get the cardinality of the variable.
	pub fn card(&self) -> Option<Value> {
		self.card.clone()
	}

	/// Set the cardinality of the variable.
	pub fn set_card(&mut self, card: Value) {
		self.card = Some(card);
	}

	/// Clear the cardinality of the variable.
	pub fn clear_card(&mut self) {
		self.card = None;
	}

	/// Whether the variable is fixed
	pub fn is_fixed(&self) -> bool {
		if let Some(domain) = &self.domain
			&& let Some(card) = &self.card
		{
			let c = card.as_int_set();
			if c.card() != 1.into() {
				return false;
			}
			domain.as_int_set().card() == c.lower_bound()
		} else {
			false
		}
	}

	/// Get the fixed value of the variable, if it is fixed.
	pub fn fix(&self) -> Result<Value, ValueError> {
		if let Some(domain) = &self.domain
			&& let Some(card) = &self.card
		{
			let c = card.as_int_set();
			let d = domain.as_int_set();
			if c.card() == 1.into() && d.card() == c.lower_bound() {
				return Ok(Value::from_iter(d.intervals()));
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
		if let Some(value) = self.card.take() {
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

define_var_ref!(IntSetVar, IntSetVarRef);

impl Value {
	pub(crate) fn new_int_set_var(ident: u64, domain: Value) -> Value {
		Self::new_box(value_storage::Init {
			ty: ValType::IntSetVar,
			len: 0,
			ref_count: 1.into(),
			weak_count: 0.into(),
			values: InitEmpty,
			idents: InitEmpty,
			ints: InitEmpty,
			floats: InitEmpty,
			bool_var: InitEmpty,
			int_var: InitEmpty,
			float_var: InitEmpty,
			int_set_var: FromIterPrefix(
				[std::sync::Mutex::new(IntSetVar {
					ident,
					domain: Some(domain),
					card: None,
					alias: None,
					defining_constraints: DefiningConstraints::default(),
				})]
				.into_iter(),
			),
			bytes: InitEmpty,
		})
	}

	pub(crate) fn as_int_set_var(&self) -> IntSetVarRef<'_> {
		let DataView::IntSetVar(isv) = self.deref() else {
			panic!("Expected int set var, found {:?}", self)
		};
		isv
	}
}
