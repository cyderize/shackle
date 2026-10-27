use crate::{
	value::{define_var_ref, value_storage, DataView, InitEmpty, ValType},
	Value,
};
use varlen::prelude::FromIterPrefix;

#[derive(Debug)]
pub struct FloatVar {
	ident: u64,
	_domain: Option<Value>,
	alias: Option<Value>,
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
					_domain: Some(domain),
					alias: None,
				})]
				.into_iter(),
			),
			int_set_var: InitEmpty,
			bytes: InitEmpty,
		})
	}

	pub(crate) fn as_float_var(&self) -> FloatVarRef {
		let DataView::FloatVar(fv) = self.deref() else {
			panic!("Expected float var, found {:?}", self)
		};
		fv
	}
}
