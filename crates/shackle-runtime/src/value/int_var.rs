use crate::{
	value::{define_var_ref, value_storage, DataView, InitEmpty, ValType},
	Value,
};
use varlen::prelude::FromIterPrefix;

#[derive(Debug)]
pub struct IntVar {
	ident: u64,
	_domain: Option<Value>,
	alias: Option<Value>,
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
					_domain: Some(domain),
					alias: None,
				})]
				.into_iter(),
			),
			float_var: InitEmpty,
			int_set_var: InitEmpty,
			bytes: InitEmpty,
		})
	}

	pub(crate) fn as_int_var(&self) -> IntVarRef {
		let DataView::IntVar(iv) = self.deref() else {
			panic!("Expected int var, found {:?}", self)
		};
		iv
	}
}
