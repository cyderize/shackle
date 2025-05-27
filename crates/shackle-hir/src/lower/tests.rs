use expect_test::Expect;
use salsa::{Database, Setter};
use shackle_syntax::InputLang;

use crate::{
	db::CompilerDatabase,
	input::{CompilerSettings, InlineModelFile, InputFiles},
	lower::lower_models,
};

pub(crate) fn check_lower_item_with_lang(language: InputLang, item: &str, expected: Expect) {
	let mut db = CompilerDatabase::default();
	let model_file = InlineModelFile::new(&db, item.to_owned(), language).into();
	let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
	let _ = CompilerSettings::get(&db)
		.set_ignore_stdlib(&mut db)
		.to(true);
	let models = lower_models(&db);
	let items = models[0].items(&db);
	let item = items.last().unwrap();
	db.attach(|db| {
		expected.assert_debug_eq(&Box::new(item.get_item_with_data_as_debug(db)));
	})
}

pub(crate) fn check_lower_item(item: &str, expected: Expect) {
	check_lower_item_with_lang(InputLang::MiniZinc, item, expected);
}

pub(crate) fn check_lower_item_eprime(item: &str, expected: Expect) {
	check_lower_item_with_lang(InputLang::EPrime, item, expected);
}
