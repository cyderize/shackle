//! Example
#![allow(unused_crate_dependencies)]

use salsa::Setter;
use shackle_hir::{
	CompilerDatabase,
	input::{InlineModelFile, InputFiles},
};
use shackle_thir::{
	lower::lower_model,
	pretty_print::PrettyPrinter,
	transform::{
		erase_enum::erase_enum, name_mangle::mangle_names, transformer,
		type_specialise::type_specialise,
	},
};

fn main() {
	let mut db = CompilerDatabase::default();
	let file = InlineModelFile::new(
		&db,
		r#"
			enum Foo = {A, B, C} ++ D(Bar);
			enum Bar = {E, F};
			any: x = B;
			any: y = D(E);
            "#
		.to_owned(),
		shackle_syntax::InputLang::MiniZinc,
	)
	.into();
	let _ = InputFiles::get(&db).set_files(&mut db).to(vec![file]);
	let thir: shackle_thir::db::Intermediate<shackle_thir::Model<'_>> = lower_model(&db);
	let mut transform = transformer(vec![type_specialise, mangle_names, erase_enum]);
	let result = transform(&db, thir.take()).unwrap();
	println!("{}", PrettyPrinter::new(&db, &result).pretty_print());
}
