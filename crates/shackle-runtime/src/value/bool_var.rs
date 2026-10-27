use crate::{
	value::{define_var_ref, value_storage, DataView, InitEmpty, ValType},
	Value,
};
use varlen::prelude::FromIterPrefix;

#[derive(Debug)]
pub struct BoolVar {
	ident: u64,
	_domain: Option<bool>,
	alias: Option<Value>,
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
					_domain: None,
					alias: None,
				})]
				.into_iter(),
			),
			int_var: InitEmpty,
			float_var: InitEmpty,
			int_set_var: InitEmpty,
			bytes: InitEmpty,
		})
	}

	pub(crate) fn as_bool_var(&self) -> BoolVarRef {
		let DataView::BoolVar(bv) = self.deref() else {
			panic!("Expected bool var, found {:?}", self)
		};
		bv
	}
}
