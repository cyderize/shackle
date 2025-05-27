//! Item counts for pre-allocation of storage

/// Item counts
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ItemCounts {
	/// Number of annotation items
	pub annotations: u32,
	/// Number of constraint items
	pub constraints: u32,
	/// Number of declaration items
	pub declarations: u32,
	/// Number of enumeration items
	pub enumerations: u32,
	/// Number of function items
	pub functions: u32,
	/// Number of output items
	pub outputs: u32,
}

impl ItemCounts {
	/// Total number of items (will not include solve item)
	pub fn items(&self) -> u32 {
		self.annotations
			+ self.constraints
			+ self.declarations
			+ self.enumerations
			+ self.functions
			+ self.outputs
	}
}

impl From<shackle_hir::counts::EntityCounts> for ItemCounts {
	fn from(value: shackle_hir::counts::EntityCounts) -> Self {
		(&value).into()
	}
}

impl From<&shackle_hir::counts::EntityCounts> for ItemCounts {
	fn from(value: &shackle_hir::counts::EntityCounts) -> Self {
		Self {
			annotations: value.annotations,
			constraints: value.constraints,
			declarations: value.declarations,
			enumerations: value.enumerations,
			functions: value.functions,
			outputs: value.outputs,
		}
	}
}
