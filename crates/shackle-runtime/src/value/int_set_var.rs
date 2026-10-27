use crate::{
	value::{define_var_ref, value_storage, DataView, InitEmpty, ValType},
	Value,
};
use varlen::prelude::FromIterPrefix;

#[derive(Debug)]
pub struct IntSetVar {
	ident: u64,
	_domain: Option<Value>,
	_card: Option<Value>,
	alias: Option<Value>,
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
					_domain: Some(domain),
					_card: None,
					alias: None,
				})]
				.into_iter(),
			),
			bytes: InitEmpty,
		})
	}

	pub(crate) fn as_int_set_var(&self) -> IntSetVarRef {
		let DataView::IntSetVar(isv) = self.deref() else {
			panic!("Expected int set var, found {:?}", self)
		};
		isv
	}
}
