//! End-to-end NanoZinc compilation and data binding.
use std::{fs, process::Command};

#[test]
fn compile_data_in_both_formats() {
	let dir = tempfile::tempdir().unwrap();
	let model = dir.path().join("model.mzn");
	fs::write(&model, "int: n; opt int: o; array[1..2] of int: a; var int: x; constraint x <= n; constraint x >= a[1]; constraint x <= if occurs(o) then deopt(o) else 9 endif; solve minimize x;").unwrap();
	for (target, filename, data, expected) in [
		("nzn", "input.dzn", "n = 3; a = [2, 4];", 9),
		(
			"nanozinc",
			"input.json",
			"{\"n\": 3, \"a\": [2, 4], \"o\": 5}",
			5,
		),
	] {
		let data_path = dir.path().join(filename);
		fs::write(&data_path, data).unwrap();
		let result = Command::new(env!("CARGO_BIN_EXE_shackle"))
			.args(["compile", "--target", target])
			.arg(&model)
			.arg(&data_path)
			.output()
			.unwrap();
		assert!(
			result.status.success(),
			"{}",
			String::from_utf8_lossy(&result.stderr)
		);
		let output = fs::read_to_string(model.with_extension("shackle.nzn")).unwrap();
		assert!(
			output.contains("constraint int_le(_X_INTRODUCED_0, 3);"),
			"{output}"
		);
		assert!(
			output.contains("constraint int_le(2, _X_INTRODUCED_0);"),
			"{output}"
		);
		assert!(
			output.contains(&format!("constraint int_le(_X_INTRODUCED_0, {expected});")),
			"{output}"
		);
		assert!(
			output.contains("solve minimize _X_INTRODUCED_0;"),
			"{output}"
		);
	}
}

#[test]
fn rejects_data_for_data_independent_targets_and_preserves_existing_output_on_error() {
	let dir = tempfile::tempdir().unwrap();
	let model = dir.path().join("model.mzn");
	let data = dir.path().join("data.dzn");
	fs::write(
		&model,
		"int: n; var int: x; constraint x <= n; solve satisfy;",
	)
	.unwrap();
	fs::write(&data, "n = 3;").unwrap();
	for target in ["microzinc", "minizinc"] {
		let result = Command::new(env!("CARGO_BIN_EXE_shackle"))
			.args(["compile", "--target", target])
			.arg(&model)
			.arg(&data)
			.output()
			.unwrap();
		assert!(!result.status.success());
		assert!(
			String::from_utf8_lossy(&result.stderr)
				.contains("only supported for the NanoZinc target")
		);
	}
	let output = model.with_extension("shackle.nzn");
	fs::write(&output, "previous output").unwrap();
	let result = Command::new(env!("CARGO_BIN_EXE_shackle"))
		.args(["compile", "--target", "nzn"])
		.arg(&model)
		.output()
		.unwrap();
	assert!(!result.status.success());
	assert!(String::from_utf8_lossy(&result.stderr).contains("n"));
	assert_eq!(fs::read_to_string(output).unwrap(), "previous output");
}

#[test]
fn compile_without_data() {
	let dir = tempfile::tempdir().unwrap();
	let model = dir.path().join("model.mzn");
	fs::write(&model, "solve satisfy;").unwrap();
	let result = Command::new(env!("CARGO_BIN_EXE_shackle"))
		.args(["compile", "--target", "nzn"])
		.arg(&model)
		.output()
		.unwrap();
	assert!(
		result.status.success(),
		"{}",
		String::from_utf8_lossy(&result.stderr)
	);
	assert!(
		fs::read_to_string(model.with_extension("shackle.nzn"))
			.unwrap()
			.contains("solve satisfy;")
	);
}

#[test]
fn compile_model_and_data_defined_enums() {
	let dir = tempfile::tempdir().unwrap();
	let model = dir.path().join("model.mzn");
	for (source, filename, contents) in [
		(
			"enum E = {red, blue}; E: chosen; var E: x; constraint x = chosen; solve satisfy;",
			"data.dzn",
			"chosen = blue;",
		),
		(
			"enum E; E: chosen; var E: x; constraint x = chosen; solve satisfy;",
			"data.dzn",
			"E = {red, blue}; chosen = blue;",
		),
		(
			"enum E; E: chosen; var E: x; constraint x = chosen; solve satisfy;",
			"data.json",
			"{\"E\":[{\"c\":\"red\"},{\"c\":\"blue\"}],\"chosen\":\"blue\"}",
		),
	] {
		let data = dir.path().join(filename);
		fs::write(&model, source).unwrap();
		fs::write(&data, contents).unwrap();
		let result = Command::new(env!("CARGO_BIN_EXE_shackle"))
			.args(["compile", "--target", "nzn"])
			.arg(&model)
			.arg(&data)
			.output()
			.unwrap();
		assert!(
			result.status.success(),
			"{}",
			String::from_utf8_lossy(&result.stderr)
		);
		let output = fs::read_to_string(model.with_extension("shackle.nzn")).unwrap();
		assert!(output.contains("var 1..2: _X_INTRODUCED_0;"), "{output}");
		assert!(output.contains("_X_INTRODUCED_0, 2)"), "{output}");
		assert!(output.contains("solve satisfy;"), "{output}");
	}
}

fn compile_nanozinc_source(source: &str, data: &str) -> String {
	let dir = tempfile::tempdir().unwrap();
	let model = dir.path().join("model.mzn");
	let input = dir.path().join("data.json");
	fs::write(&model, source).unwrap();
	fs::write(&input, data).unwrap();
	let result = Command::new(env!("CARGO_BIN_EXE_shackle"))
		.args(["compile", "--target", "nzn"])
		.arg(&model)
		.arg(&input)
		.output()
		.unwrap();
	assert!(
		result.status.success(),
		"{source}\nData: {data}\n{}",
		String::from_utf8_lossy(&result.stderr)
	);
	fs::read_to_string(model.with_extension("shackle.nzn")).unwrap()
}

#[test]
fn compile_root_predicates_in_parameter_conditionals() {
	let source = r#"
		predicate foo(var int: x);
		predicate bar(var int: x) = if enabled then foo(x) else true endif;
		bool: enabled;
		var int: x;
		constraint bar(x);
		constraint if enabled then
			if enabled then foo(x) else abort("inactive branch") endif
		else true endif;
		solve satisfy;
	"#;
	for enabled in [false, true] {
		let output = compile_nanozinc_source(source, &format!(r#"{{"enabled":{enabled}}}"#));
		assert_eq!(
			output.matches("constraint foo(").count(),
			if enabled { 2 } else { 0 },
			"{output}"
		);
		assert!(output.contains("solve satisfy;"), "{output}");
	}
}

#[test]
fn compile_root_predicates_in_forall_collections() {
	for collection in ["[foo(x[1]), foo(x[2])]", "[foo(x[i]) | i in 1..2]"] {
		let output = compile_nanozinc_source(
			&format!(
				"predicate foo(var int: x); array[1..2] of var int: x;
				constraint forall({collection}); solve satisfy;"
			),
			"{}",
		);
		assert!(
			output.contains("constraint foo(_X_INTRODUCED_0);"),
			"{output}"
		);
		assert!(
			output.contains("constraint foo(_X_INTRODUCED_1);"),
			"{output}"
		);
		assert_eq!(output.matches("constraint foo(").count(), 2, "{output}");
	}
}

#[test]
fn compile_variable_array_index() {
	let output = compile_nanozinc_source(
		"array[1..2] of var 1..3: x; var 1..2: i;
		var int: y = x[i]; solve minimize y;",
		"{}",
	);
	assert!(
		output.contains("constraint array_var_int_element"),
		"{output}"
	);
	assert!(output.contains("solve minimize "), "{output}");
}

#[test]
fn compile_promise_total_boolean_returning_false() {
	// A total boolean function may return false without making the model inconsistent.
	let output = compile_nanozinc_source(
		"test foo(int: x) :: promise_total = false;
		constraint not foo(1); solve satisfy;",
		"{}",
	);
	assert!(output.contains("solve satisfy;"), "{output}");
}

#[test]
fn compile_promise_total_boolean_preserves_parameter_result() {
	// Branch results must remain values when the function is used outside root context.
	let source = "test foo(int: x) :: promise_total = if x > 0 then true else false endif;
		int: n; bool: expected; constraint foo(n) = expected; solve satisfy;";
	for (n, expected) in [(-1, false), (1, true)] {
		let output =
			compile_nanozinc_source(source, &format!(r#"{{"n":{n},"expected":{expected}}}"#));
		assert!(output.contains("solve satisfy;"), "{output}");
	}
}
