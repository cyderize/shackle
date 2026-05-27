//! Accumulators for diagnostics

use derive_more::{Deref, DerefMut, From, Into};
use salsa::Accumulator;
use shackle_diagnostics::{Error, Warning};

use crate::Db;

/// Accumulator for errors
#[derive(Debug, Clone, PartialEq, Eq, From, Into, Deref, DerefMut)]
#[salsa::accumulator]
pub struct Errors(Error);

impl Errors {
	/// Create and accumulate an error
	pub fn add(db: &dyn Db, error: impl Into<Error>) {
		let e = error.into();
		log::error!("{:#?}", e);
		Errors(e).accumulate(db);
	}

	/// Add multiple errors at once
	pub fn extend(db: &dyn Db, errors: impl IntoIterator<Item = impl Into<Error>>) {
		for error in errors {
			Self::add(db, error);
		}
	}
}

/// Accumulator for warnings
#[derive(Debug, Clone, PartialEq, Eq, From, Into, Deref, DerefMut)]
#[salsa::accumulator]
pub struct Warnings(Warning);

impl Warnings {
	/// Create and accumulate a warning
	pub fn add(db: &dyn Db, warning: impl Into<Warning>) {
		let w = warning.into();
		log::warn!("{}", w);
		Warnings(w).accumulate(db);
	}

	/// Add multiple warnings at once
	pub fn extend(db: &dyn Db, warnings: impl IntoIterator<Item = impl Into<Warning>>) {
		for warning in warnings {
			Self::add(db, warning);
		}
	}
}
