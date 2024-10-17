#![allow(missing_docs)]

//! Salsa database for MIR operations

use std::sync::Arc;

use super::{lower::lower_from_thir, Model};
use crate::{db::Upcast, thir::db::Thir, Result};

/// MIR queries
#[salsa::query_group(MirStorage)]
pub trait Mir: Thir + Upcast<dyn Thir> {
	/// Lower a model to MIR
	fn model_mir(&self) -> Result<Arc<Model>>;
}

fn model_mir(db: &dyn Mir) -> Result<Arc<Model>> {
	let model = db.final_thir()?;
	Ok(Arc::new(lower_from_thir(db.upcast(), &model)))
}
