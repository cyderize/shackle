//! Test utilities for the HIR lowering phase.

use std::sync::Arc;

use expect_test::Expect;
use shackle_syntax::InputLang;

use crate::{
	db::{CompilerDatabase, FileReader, Inputs},
	file::InputFile,
	hir::db::Hir,
	utils::DebugPrint,
};

/// Check that the lowering the item in the given language matches the expected
/// debug print.
pub fn check_lower_item_with_lang(language: InputLang, item: &str, expected: Expect) {
	let mut db = CompilerDatabase::default();
	db.set_ignore_stdlib(true);
	db.set_input_files(Arc::new(vec![InputFile::String(item.to_owned(), language)]));
	let model = db.input_models();
	let items = db.lookup_items(model[0]);
	let item = *items.last().unwrap();
	let debug_print = item.debug_print(&db);
	expected.assert_eq(&debug_print);
}

/// Alias for `check_lower_item_with_lang` with the MiniZinc language.
pub fn check_lower_item(item: &str, expected: Expect) {
	check_lower_item_with_lang(InputLang::MiniZinc, item, expected);
}

/// Alias for `check_lower_item_with_lang` with the EPrime language.
pub fn check_lower_item_eprime(item: &str, expected: Expect) {
	check_lower_item_with_lang(InputLang::EPrime, item, expected);
}
