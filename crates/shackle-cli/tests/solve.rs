use std::{path::PathBuf, process::Command};

use expect_test::expect_file;

fn fixture(name: &str, extension: &str) -> PathBuf {
	std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
		.join("tests/solve")
		.join(name)
		.with_extension(extension)
}

#[test]
fn solve_basic_all_solutions() {
	let model = fixture("basic", "mzn");
	let expected_path = fixture("basic", "expected");

	let output = Command::new(env!("CARGO_BIN_EXE_shackle"))
		.args(["solve", model.to_str().unwrap(), "--", "--all-solutions"])
		.output()
		.unwrap();

	assert!(
		output.status.success(),
		"solve failed\nstatus: {}\nstdout:\n{}\nstderr:\n{}",
		output.status,
		String::from_utf8_lossy(&output.stdout),
		String::from_utf8_lossy(&output.stderr),
	);
	expect_file![expected_path].assert_eq(&String::from_utf8(output.stdout).unwrap());
}
