fn main() {
	let src_dir = std::path::Path::new("src");

	// `lib.rs` gates the query constants on these, so they have to be declared
	// here whether or not the corresponding query file exists.
	let queries_dir = std::path::Path::new("queries");
	for (file, cfg) in [
		("highlights.scm", "with_highlights_query"),
		("injections.scm", "with_injections_query"),
		("locals.scm", "with_locals_query"),
		("tags.scm", "with_tags_query"),
	] {
		println!("cargo::rustc-check-cfg=cfg({cfg})");
		if queries_dir.join(file).exists() {
			println!("cargo::rustc-cfg={cfg}");
		}
	}

	let mut c_config = cc::Build::new();
	c_config.std("c11").include(src_dir);

	#[cfg(target_env = "msvc")]
	c_config.flag("-utf-8");

	let parser_path = src_dir.join("parser.c");
	c_config.file(&parser_path);
	println!("cargo:rerun-if-changed={}", parser_path.to_str().unwrap());

	let scanner_path = src_dir.join("scanner.c");
	if scanner_path.exists() {
		c_config.file(&scanner_path);
		println!("cargo:rerun-if-changed={}", scanner_path.to_str().unwrap());
	}

	c_config.compile("tree-sitter-eprime");
}
