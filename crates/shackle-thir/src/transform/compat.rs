//! Transform for making output runnable with old MiniZinc compiler

use rustc_hash::FxHashSet;
use shackle_diagnostics::Result;
use shackle_hir::{Db, Identifier, input::resolve_auto_includes};

use crate::{FunctionName, Model};

/// Mangle names to avoid conflicts with old MiniZinc standard library.
pub fn old_compat<'db>(db: &'db dyn Db, mut model: Model<'db>) -> Result<Model<'db>> {
	log::info!("Prefixing names for old MiniZinc compatibility");

	let auto_includes = FxHashSet::from_iter(resolve_auto_includes(db).iter().copied());

	for (_, a) in model.annotations_mut() {
		if a.origin()
			.node()
			.map(|node| auto_includes.contains(&node.model_file(db)))
			.unwrap_or(true)
			&& let Some(name) = a.name
		{
			let prefixed = format!("shackle_{}", name.lookup(db));
			a.name = Some(Identifier::new(db, prefixed));
		}
	}

	for (_, f) in model.top_level_functions_mut() {
		if f.origin()
			.node()
			.map(|node| auto_includes.contains(&node.model_file(db)))
			.unwrap_or(true)
			&& f.body().is_some()
			&& let FunctionName::Named(name) = f.name()
		{
			let prefixed = format!("shackle_{}", name.lookup(db));
			f.set_name(Identifier::new(db, prefixed));
		}
	}

	for (_, decl) in model.top_level_declarations_mut() {
		if decl
			.origin()
			.node()
			.map(|node| auto_includes.contains(&node.model_file(db)))
			.unwrap_or(true)
			&& let Some(name) = decl.name()
		{
			let prefixed = format!("shackle_{}", name.lookup(db));
			decl.set_name(Identifier::new(db, prefixed));
		}
	}

	Ok(model)
}
