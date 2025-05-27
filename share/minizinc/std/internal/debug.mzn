function bool: mzn_add_warning(string: msg);
function bool: mzn_assert_warn(bool: b, string: msg) :: mzn_inline_call_by_name =
	let {
		bool: condition = b;
		constraint if condition then true else mzn_add_warning(msg) endif;
	} in condition;
