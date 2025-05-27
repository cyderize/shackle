#![allow(unused_crate_dependencies, reason = "Crates used in main library")]
//! Test code formatting on the standard library

use common::check_format_file;
use expect_test::expect_file;
use shackle_fmt::MiniZincFormatOptions;
use shackle_hir::{db::CompilerDatabase, input::shackle_share_directory};

mod common;

#[test]
fn format_stdlib() {
	let db = CompilerDatabase::default();
	let share = shackle_share_directory(&db).clone().unwrap();
	let mut p = share.to_string_lossy().into_owned();
	p.push_str("/**/*.mzn");
	let options = MiniZincFormatOptions::default();
	for entry in glob::glob(&p).unwrap() {
		let path = entry.unwrap();
		let actual = check_format_file(&path, &options);
		let expected = expect_file![path];
		expected.assert_eq(&actual);
	}
}
