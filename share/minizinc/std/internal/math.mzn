function int: 'div'(int: x, int: y) =
	let {
		constraint mzn_assert_warn(y != 0, "division by zero is undefined");
	} in shackle_div(x, y);
function int: shackle_div(int: x, int: y);

function $$E: min(set of $$E: s) =
	let {
		constraint mzn_assert_warn(card(s) > 0, "minimum of an empty set is undefined");
	} in shackle_min(s);
function $$E: shackle_min(set of $$E: x);

function $$E: max(set of $$E: s) =
	let {
		constraint mzn_assert_warn(card(s) > 0, "maximum of an empty set is undefined");
	} in shackle_max(s);
function $$E: shackle_max(set of $$E: x);

function $$E: arg_min(array [$$E] of bool: xs) =
	let {
		constraint mzn_assert_warn(length(xs) > 0, "arg_min of an empty array is undefined");
	} in shackle_arg_min(xs);
function $$E: arg_min(array [$$E] of $$T: xs) =
	let {
		constraint mzn_assert_warn(length(xs) > 0, "arg_min of an empty array is undefined");
	} in shackle_arg_min(xs);
function $$E: arg_min(array [$$E] of float: xs) =
	let {
		constraint mzn_assert_warn(length(xs) > 0, "arg_min of an empty array is undefined");
	} in shackle_arg_min(xs);
function $$E: shackle_arg_min(array [$$E] of $T: x);

function $$E: arg_max(array [$$E] of bool: xs) =
	let {
		constraint mzn_assert_warn(length(xs) > 0, "arg_max of an empty array is undefined");
	} in shackle_arg_max(xs);
function $$E: arg_max(array [$$E] of $$T: xs) =
	let {
		constraint mzn_assert_warn(length(xs) > 0, "arg_max of an empty array is undefined");
	} in shackle_arg_max(xs);
function $$E: arg_max(array [$$E] of float: xs) =
	let {
		constraint mzn_assert_warn(length(xs) > 0, "arg_max of an empty array is undefined");
	} in shackle_arg_max(xs);
function $$E: shackle_arg_max(array [$$E] of $T: x);

function int: pow(int: x, int: y) =
	let {
		constraint mzn_assert_warn(x != 0 \/ y >= 0, "negative power of zero is undefined");
	} in shackle_pow(x, y);
function int: shackle_pow(int: x, int: y);

function array [$$E] of $$F: inverse(array [$$F] of $$E: f) =
	let {
		constraint
			mzn_assert_warn(
				length(f) = 0 \/ enum2int(max(f)) - enum2int(min(f)) + 1 = length(f),
				"inverse on non-contiguous set of values is undefined",
			);
	} in shackle_inverse(f);
function array [$$E] of $$F: shackle_inverse(array [$$F] of $$E: f);

function set of $U: array_intersect(array [$T] of set of $U: x) :: promise_commutative =
	let {
		constraint
			 mzn_assert_warn(length(x) > 0, "array_intersect of an empty array is undefined");
	} in shackle_array_intersect(x);
function set of $U: shackle_array_intersect(array [$T] of set of $U: x);
