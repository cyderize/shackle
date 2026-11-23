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

#[test]
fn compile_promise_total_predicate_in_root_context() {
	let output = compile_nanozinc_source(
		"predicate foo(var bool: x) :: promise_total =
			let { var bool: y; constraint y <-> x; } in y;
		var bool: x; constraint foo(x); solve satisfy;",
		"{}",
	);
	// The root call must retain both the local definition and the assertion of its result.
	assert!(
		output.contains("(_X_INTRODUCED_1, _X_INTRODUCED_0);"),
		"{output}"
	);
	assert!(output.contains("(_X_INTRODUCED_1, 1);"), "{output}");
	assert!(output.contains("solve satisfy;"), "{output}");
}

#[test]
fn compile_nested_promise_total_predicates_preserves_value_definitions() {
	let output = compile_nanozinc_source(
		"predicate identity(var bool: x) :: promise_total =
			let { var bool: y; constraint y <-> x; } in y;
		predicate outer(var bool: x) :: promise_total =
			let { var bool: z; constraint z <-> identity(x); } in z;
		var bool: x; var bool: r = identity(x);
		constraint outer(x); constraint r; solve satisfy;",
		"{}",
	);
	// Root assertions remain posted while nested value calls keep their defining group.
	assert!(output.contains("% defined by group"), "{output}");
	assert!(
		output.contains("(_X_INTRODUCED_3, _X_INTRODUCED_0);"),
		"{output}"
	);
	assert!(
		output.contains("(_X_INTRODUCED_2, _X_INTRODUCED_3);"),
		"{output}"
	);
	assert!(output.contains("(_X_INTRODUCED_2, 1);"), "{output}");
	assert!(output.contains("solve satisfy;"), "{output}");
}

#[test]
fn compile_variable_boolean_conditional_in_root_context() {
	let output = compile_nanozinc_source(
		"var bool: b; var bool: x; var bool: y;
		constraint if b then x else y endif; solve satisfy;",
		"{}",
	);
	// The helper decomposes into guarded clauses, and its selected result is asserted.
	assert_eq!(
		output.matches("constraint bool_clause(").count(),
		2,
		"{output}"
	);
	assert!(output.contains("(_X_INTRODUCED_3, 1);"), "{output}");
	assert!(output.contains("solve satisfy;"), "{output}");
}

#[test]
fn compile_variable_tuple_conditional_preserves_false_boolean_field() {
	let output = compile_nanozinc_source(
		"var bool: b;
		any: t = if b then (false, 1) else (false, 2) endif;
		constraint not t.1; solve satisfy;",
		"{}",
	);
	// Both branches return false in the first field, so its negation must be satisfiable.
	assert!(
		!output.contains("constraint bool_clause([], [1]);"),
		"{output}"
	);
	assert!(output.contains("solve satisfy;"), "{output}");
}

#[test]
fn compile_parity_predicates_in_root_and_value_contexts() {
	for declaration in [
		"array[1..3] of var bool: x;",
		"enum E = {a, b, c}; array[E] of var bool: x;",
	] {
		for (expression, arguments) in [
			("constraint xorall(x);", 3),
			("var bool: r = xorall(x); constraint not r;", 4),
			("constraint iffall(x);", 4),
			("var bool: r = iffall(x); constraint not r;", 5),
		] {
			let output = compile_nanozinc_source(
				&format!("{declaration} {expression} solve satisfy;"),
				"{}",
			);
			let parity = output.split("constraint array_bool_xor([").nth(1).unwrap();
			let values = parity.split(']').next().unwrap();
			assert_eq!(values.split(',').count(), arguments, "{output}");
			assert_eq!(
				output.matches("constraint array_bool_xor([").count(),
				1,
				"{output}"
			);
			assert!(output.contains("solve satisfy;"), "{output}");
		}
	}
}

#[test]
fn compile_guarded_out_of_bounds_array_accesses() {
	for (source, inputs) in [
		(
			"array[1..1] of int: a = [5]; int: i; int: expected; constraint mzn_default_partial(a[i], 7) = expected;",
			[r#"{"i":0,"expected":7}"#, r#"{"i":1,"expected":5}"#],
		),
		(
			"array[1..1] of int: a = [5]; int: i; bool: expected; constraint (a[i] = 5) = expected;",
			[r#"{"i":0,"expected":false}"#, r#"{"i":1,"expected":true}"#],
		),
		(
			"array[1..1,1..2] of int: a = [|1,2|]; int: i; int: expected; constraint mzn_default_partial(a[1,i], 7) = expected;",
			[r#"{"i":0,"expected":7}"#, r#"{"i":2,"expected":2}"#],
		),
		(
			"array[1..0] of int: a = []; int: i; int: expected; constraint mzn_default_partial(a[i], 7) = expected;",
			[r#"{"i":0,"expected":7}"#, r#"{"i":1,"expected":7}"#],
		),
		(
			"array[1..1] of bool: a = [true]; int: i; bool: expected; constraint a[i] = expected;",
			[r#"{"i":0,"expected":false}"#, r#"{"i":1,"expected":true}"#],
		),
		(
			"array[1..1] of tuple(int,int): a = [(2,3)]; int: i; int: expected; constraint mzn_default_partial(a[i], (7,8)).2 = expected;",
			[r#"{"i":0,"expected":8}"#, r#"{"i":1,"expected":3}"#],
		),
	] {
		for data in inputs {
			let output = compile_nanozinc_source(&format!("{source} solve satisfy;"), data);
			assert!(output.contains("solve satisfy;"), "{output}");
			assert!(
				!output.contains("constraint bool_clause([], [1]);"),
				"{output}"
			);
		}
	}
}

#[test]
fn compile_empty_multidimensional_inputs() {
	for (source, data) in [
		(
			"array[1..0,1..0] of int: a; constraint length(a) = 0;",
			r#"{"a":[]}"#,
		),
		(
			"array[1..0,1..0,1..0] of int: a; constraint length(a) = 0;",
			r#"{"a":[]}"#,
		),
		(
			"array[int,int] of int: a; constraint card(index_set_2of2(a)) = 0;",
			r#"{"a":[]}"#,
		),
		(
			"tuple(array[int,int] of int,bool): t; constraint card(index_set_2of2(t.1)) = 0;",
			r#"{"t":[[],true]}"#,
		),
	] {
		let output = compile_nanozinc_source(&format!("{source} solve satisfy;"), data);
		assert!(output.contains("solve satisfy;"), "{output}");
	}
}

#[test]
fn empty_multidimensional_input_shape_mismatch_is_reported_without_panicking() {
	let dir = tempfile::tempdir().unwrap();
	let model = dir.path().join("model.mzn");
	let data = dir.path().join("data.json");
	fs::write(&model, "array[1..0,1..2] of int: a; solve satisfy;").unwrap();
	fs::write(&data, r#"{"a":[]}"#).unwrap();
	let result = Command::new(env!("CARGO_BIN_EXE_shackle"))
		.args(["compile", "--target", "nzn"])
		.arg(&model)
		.arg(&data)
		.output()
		.unwrap();
	let error = String::from_utf8_lossy(&result.stderr);
	assert!(!result.status.success(), "{error}");
	assert!(error.contains("Index set 2 of 'a'"), "{error}");
	assert!(!error.contains("panicked"), "{error}");
}

#[test]
fn compile_call_annotations_from_globals() {
	// Refer to the global from a function so it cannot become a main-local value.
	// The let body also prevents macro inlining from removing the call.
	let source = r#"
		annotation tag(int: n);
		predicate sink(var int: x, var int: y);
		function var int: value(var int: x) :: promise_total =
			let { var int: y; constraint sink(x, y); } in y;
		int: n;
		ann: label = tag(n);
		function var int: annotated(var int: x) :: promise_total =
			let { var int: y = value(x) :: label; } in y;
		var int: x;
		var int: y :: output = annotated(x);
		solve satisfy;
	"#;
	for n in [3, 17] {
		let output = compile_nanozinc_source(source, &format!(r#"{{"n":{n}}}"#));
		assert!(
			output
				.lines()
				.any(|line| line.starts_with("constraint sink(")
					&& line.ends_with(&format!(" :: tag({n});"))),
			"{output}"
		);
		assert_eq!(output.matches("constraint sink(").count(), 1, "{output}");
	}
}

#[test]
fn compile_call_annotations_from_parameters() {
	// Keep the let body: a macro wrapper would inline away the annotation parameter.
	let output = compile_nanozinc_source(
		r#"
			annotation tag(int: n);
			predicate sink(var int: x, var int: y);
			function var int: value(var int: x) :: promise_total =
				let { var int: y; constraint sink(x, y); } in y;
			function var int: annotated(var int: x, ann: label) :: promise_total =
				let { var int: y = value(x) :: label; } in y;
			var int: x;
			var int: y :: output = annotated(x, tag(3));
			var int: z :: output = annotated(x, tag(17));
			solve satisfy;
		"#,
		"{}",
	);
	for n in [3, 17] {
		assert!(
			output
				.lines()
				.any(|line| line.starts_with("constraint sink(")
					&& line.ends_with(&format!(" :: tag({n});"))),
			"{output}"
		);
	}
	assert_eq!(output.matches("constraint sink(").count(), 2, "{output}");
}

#[test]
fn compile_domains_of_fixed_values() {
	for (source, data) in [
		// show exercises conversion of the domain to a sequence of ranges.
		(
			"int: n; var int: x = n; constraint show(dom(x)) = show({n});",
			r#"{"n":42}"#,
		),
		// dom_array must union singleton and non-singleton domains together.
		(
			"int: n; var 2..3: y; array[1..3] of var int: x = [1,y,n];
		constraint dom_array(x) = {1,2,3,n};",
			r#"{"n":42}"#,
		),
		// Enums use the integer domain representation after enum erasure.
		(
			"enum E = {a,b,c}; var E: x = b; constraint dom(x) = {b};",
			"{}",
		),
	] {
		let output = compile_nanozinc_source(&format!("{source} solve satisfy;"), data);
		assert!(output.contains("solve satisfy;"), "{output}");
		assert!(
			!output.contains("constraint bool_clause([], [1]);"),
			"{output}"
		);
	}
}

#[test]
fn function_domain_bounds_are_initialised_before_calls() {
	// The wrapper calls a function whose generated bounds appear later
	// among top-level items. Those bounds must be ready when it is called.
	for (array, valid) in [("[|1,2|]", true), ("[|1,2,3|]", false)] {
		let dir = tempfile::tempdir().unwrap();
		let model = dir.path().join("model.mzn");
		fs::write(
			&model,
			format!(
				"array[int,int] of int: a = {array};
			predicate forward(array[int,int] of int: a) = check_shape(a);
			constraint forward(a);
			predicate check_shape(array[int,1..2] of int: x) = length(x) > 0;
			solve satisfy;"
			),
		)
		.unwrap();
		let result = Command::new(env!("CARGO_BIN_EXE_shackle"))
			.args(["compile", "--target", "nzn"])
			.arg(&model)
			.output()
			.unwrap();
		let error = String::from_utf8_lossy(&result.stderr);
		assert_eq!(result.status.success(), valid, "{error}");
		if valid {
			let output = fs::read_to_string(model.with_extension("shackle.nzn")).unwrap();
			assert!(output.contains("solve satisfy;"), "{output}");
		} else {
			assert!(error.contains("Index set 2 of 'x' is 1..2"), "{error}");
			assert!(!error.contains("panicked"), "{error}");
		}
	}
}

#[test]
fn compile_function_dependencies_on_later_globals() {
	for source in [
		// A parameter domain reached through multiple function calls.
		r#"
			test first() = second();
			test second() = third(2);
			constraint first();
			test third(1..n: x) = x = 2;
			int: n = 3;
		"#,
		// Return domains must contribute dependencies as well as parameter domains.
		r#"
			test use() = answer() = 2;
			constraint use();
			function 1..n: answer() = 2;
			int: n = 3;
		"#,
		// Recursive calls must not prevent discovery of globals in the body.
		r#"
			function int: f(int: x) = if x = 0 then n else f(x-1) endif;
			int: result = f(3);
			constraint result = 7;
			int: n = 7;
		"#,
		r#"
			function int: f(int: x) = if x = 0 then n else g(x-1) endif;
			function int: g(int: x) = if x = 0 then n else f(x-1) endif;
			int: result = f(3);
			constraint result = 7;
			int: n = 7;
		"#,
	] {
		let output = compile_nanozinc_source(&format!("{source} solve satisfy;"), "{}");
		assert!(output.contains("solve satisfy;"), "{output}");
		assert!(
			!output.contains("constraint bool_clause([], [1]);"),
			"{output}"
		);
	}
}
