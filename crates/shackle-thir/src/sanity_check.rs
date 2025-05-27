//! Sanity checks for THIR.
//!

use salsa::Setter;
use shackle_diagnostics::Error;
use shackle_hir::{
	CompilerDatabase,
	input::{CompilerSettings, InlineModelFile, InputFiles},
	run_hir_phase,
};
use shackle_syntax::InputLang;

use crate::{Db, lower::lower_model, pretty_print::PrettyPrinter};

/// Get the diagnostics for running the pretty printed THIR.
///
/// This should give no errors (as for the THIR to exist, it must have come
/// from a valid source program).
pub fn sanity_check_thir(db: &dyn Db) -> Vec<Error> {
	let initial_thir = lower_model(db);
	let model = initial_thir.get();

	// Pretty print with extra info for sanity checking types
	let mut printer = PrettyPrinter::new(db, model.as_ref());
	printer.old_compat = false;
	printer.expression_annotator = Some(Box::new(|e| {
		Some(format!("shackle_type({:?})", e.ty().pretty_print(db)))
	}));
	let code = printer.pretty_print();

	let mut new_db = CompilerDatabase::default();
	let _ = CompilerSettings::get(&new_db)
		.set_ignore_stdlib(&mut new_db)
		.to(true);
	let model_file = InlineModelFile::new(&new_db, code, InputLang::MiniZinc).into();
	let _ = InputFiles::get(&new_db)
		.set_files(&mut new_db)
		.to(vec![model_file]);
	run_hir_phase(&new_db).errors.into_iter().cloned().collect()
}
