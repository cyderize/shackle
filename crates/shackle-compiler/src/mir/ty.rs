//! Module containing mid-level IR type representation
use std::rc::Rc;

/// A mid-level IR type
#[allow(variant_size_differences)]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ty {
	/// Type of a boolean
	Bool {
		/// Dimensions (0 if not an array)
		dim: u8,
		/// Whether this is a decision variable
		is_var: bool,
		/// Whether this is a set
		is_set: bool,
	},
	/// Type of an integer
	Int {
		/// Dimensions (0 if not an array)
		dim: u8,
		/// Whether this is a decision variable
		is_var: bool,
		/// Whether this is a set
		is_set: bool,
	},
	/// Type of a float
	Float {
		/// Dimensions (0 if not an array)
		dim: u8,
		/// Whether this is a decision variable
		is_var: bool,
		/// Whether this is a set
		is_set: bool,
	},
	/// Type of a string
	String {
		/// Dimensions (0 if not an array)
		dim: u8,
	},
	/// Type of an annotation
	Ann {
		/// Dimensions (0 if not an array)
		dim: u8,
	},
	/// Type of a tuple
	Tuple {
		/// Dimensions (0 if not an array)
		dim: u8,
		/// Types of the fields
		fields: Rc<[Ty]>,
	},
}

impl Ty {
	/// Par int
	pub fn int() -> Self {
		Ty::Int {
			dim: 0,
			is_var: false,
			is_set: false,
		}
	}

	/// Var bool
	pub fn var_int() -> Self {
		Ty::Int {
			dim: 0,
			is_var: true,
			is_set: false,
		}
	}

	/// Par bool
	pub fn bool() -> Self {
		Ty::Bool {
			dim: 0,
			is_var: false,
			is_set: false,
		}
	}

	/// Var bool
	pub fn var_bool() -> Self {
		Ty::Bool {
			dim: 0,
			is_var: true,
			is_set: false,
		}
	}

	/// Par float
	pub fn float() -> Self {
		Ty::Float {
			dim: 0,
			is_var: false,
			is_set: false,
		}
	}

	/// Var float
	pub fn var_float() -> Self {
		Ty::Float {
			dim: 0,
			is_var: true,
			is_set: false,
		}
	}

	/// Set of int
	pub fn set_of_int() -> Self {
		Ty::Int {
			dim: 0,
			is_var: false,
			is_set: true,
		}
	}

	/// Annotation
	pub fn ann() -> Self {
		Ty::Ann { dim: 0 }
	}

	/// Tuple
	pub fn tuple(fields: impl IntoIterator<Item = Ty>) -> Self {
		Ty::Tuple {
			dim: 0,
			fields: fields.into_iter().collect(),
		}
	}

	/// Get the dimensions of this type
	pub fn dim(&self) -> u8 {
		match self {
			Ty::Ann { dim }
			| Ty::Bool { dim, .. }
			| Ty::Float { dim, .. }
			| Ty::Int { dim, .. }
			| Ty::String { dim }
			| Ty::Tuple { dim, .. } => *dim,
		}
	}

	/// Set the dimensions of this type
	pub fn set_dim(&mut self, new_dim: u8) {
		match self {
			Ty::Ann { dim }
			| Ty::Bool { dim, .. }
			| Ty::Float { dim, .. }
			| Ty::Int { dim, .. }
			| Ty::String { dim }
			| Ty::Tuple { dim, .. } => *dim = new_dim,
		}
	}

	/// Get whether this is var
	pub fn is_var(&self) -> bool {
		match self {
			Ty::Bool { is_var, .. } | Ty::Float { is_var, .. } | Ty::Int { is_var, .. } => *is_var,
			_ => false,
		}
	}

	/// Set whether this is var (panics if not possible)
	pub fn set_var(&mut self, var: bool) {
		match self {
			Ty::Bool { is_var, .. }
			| Ty::Float {
				is_var,
				is_set: false,
				..
			}
			| Ty::Int { is_var, .. } => *is_var = var,
			_ => assert!(!var, "Type {:?} cannot be made var", self),
		}
	}

	/// Get whether this is a set
	pub fn is_set(&self) -> bool {
		match self {
			Ty::Bool { is_set, .. } | Ty::Float { is_set, .. } | Ty::Int { is_set, .. } => *is_set,
			_ => false,
		}
	}

	/// Set whether this is a set type (panics if not possible)
	pub fn set_set(&mut self, set: bool) {
		match self {
			Ty::Bool { is_set, .. } | Ty::Int { is_set, .. } => *is_set = set,
			_ => assert!(!set, "Type {:?} cannot be made a set", self),
		}
	}

	/// Get whether this is a boolean
	pub fn is_bool(&self) -> bool {
		matches!(
			self,
			Ty::Bool {
				dim: 0,
				is_set: false,
				..
			}
		)
	}
}
