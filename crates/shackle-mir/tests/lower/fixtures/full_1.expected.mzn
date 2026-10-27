annotation shackle_output;
annotation shackle_promise_total;
annotation shackle_is_defined_var;
annotation shackle_promise_commutative;
annotation shackle_domain;
annotation shackle_mzn_internal_representation;
annotation shackle_bounds;
function array [int] of int: 'shackle_array1d<array [int] of int>'(array [int] of int: x)
	:: promise_total =
	let {
		int: _DECL_1 = 1;
		int: _DECL_2 = length(x);
		set of int: _DECL_3 = range_internal_int(_DECL_1, _DECL_2);
	} in mzn_array_kd((_DECL_3,), x);
function array [int] of var int: 'shackle_array1d<array [int] of var int>'(
	array [int] of var int: x,
)
	:: promise_total =
	let {
		int: _DECL_1 = 1;
		int: _DECL_2 = length(x);
		set of int: _DECL_3 = range_internal_int(_DECL_1, _DECL_2);
	} in mzn_array_kd((_DECL_3,), x);
function int: 'shackle_div_root<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = 'shackle_!=<int, int>'(y, 0);
		constraint if _DECL_1 then true else mzn_add_warning("division by zero is undefined") endif;
		constraint _DECL_1;
		constraint true;
	} in div_internal_int(x, y);
function int: 'shackle_mod_root<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = 'shackle_!=<int, int>'(y, 0);
		constraint if _DECL_1 then true else mzn_add_warning("modulus by zero is undefined") endif;
		constraint _DECL_1;
		constraint true;
	} in shackle_mod_internal(x, y);
function int: 'shackle_min_root<set of int>'(set of int: s) :: promise_total =
	let {
		int: _DECL_1 = card(s);
		int: _DECL_2 = 0;
		constraint '<'(_DECL_2, _DECL_1);
		bool: _DECL_3 = true;
		constraint
			if _DECL_3 then true else mzn_add_warning("minimum of an empty set is undefined") endif;
		constraint _DECL_3;
		constraint true;
	} in min_internal_set(s);
function int: 'shackle_max_root<set of int>'(set of int: s) :: promise_total =
	let {
		int: _DECL_1 = card(s);
		int: _DECL_2 = 0;
		constraint '<'(_DECL_2, _DECL_1);
		bool: _DECL_3 = true;
		constraint
			if _DECL_3 then true else mzn_add_warning("maximum of an empty set is undefined") endif;
		constraint _DECL_3;
		constraint true;
	} in max_internal_set(s);
function bool: 'shackle_forall<array [int] of bool>'(array [int] of bool: x) :: promise_total =
	let {
		array [int] of bool: _DECL_3 = [
			x_i |
				x_i in x,
				_DECL_2 = if x_i then false else true endif,
				_ = 1 where _DECL_2,
		];
		int: _DECL_4 = length(_DECL_3);
	} in '='(_DECL_4, 0);
function var bool: 'shackle_=<var int, var int>'(var int: x, var int: y) :: promise_total =
	let {
		bool: _DECL_3 = is_fixed(y);
	} in if _DECL_3 then
		let { int: _DECL_4 = fix(y) } in shackle_mzn_int_eq(x, _DECL_4)
	else
		let {
			bool: _DECL_1 = is_fixed(x);
		} in if _DECL_1 then
			let { int: _DECL_2 = fix(x) } in shackle_mzn_int_eq(_DECL_2, y)
		else
			shackle_mzn_int_eq(x, y)
		endif
	endif;
function var bool: shackle_mzn_int_eq(var int: x, var int: y) :: promise_total =
	if false then
		let { var bool: _DECL_1 = 'int_eq<var int, var int>'(x, y) } in _DECL_1
	else
		shackle_mzn_int_eq_t(x, y)
	endif;
function var bool: shackle_mzn_int_eq_t(var int: x, var int: y) :: promise_total =
	let {
		var bool: r = (let { var bool: _FRESH_VAR :: promise_total } in _FRESH_VAR);
		constraint int_eq_reif(x, y, r);
	} in r;
function bool: 'shackle_!=<int, int>'(int: x, int: y) :: promise_total =
	let { bool: _DECL_1 = '='(x, y) } in if _DECL_1 then false else true endif;
function var bool: 'shackle_!=<var int, int>'(var int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = is_fixed(x);
	} in if _DECL_1 then
		let { int: _DECL_2 = fix(x) } in 'shackle_!=<int, int>'(_DECL_2, y)
	else
		shackle_mzn_int_ne(x, y)
	endif;
function var bool: shackle_mzn_int_ne(var int: x, var int: y) :: promise_total =
	if false then
		let { var bool: _DECL_1 = 'int_ne<var int, var int>'(x, y) } in _DECL_1
	else
		shackle_mzn_int_ne_t(x, y)
	endif;
function var bool: shackle_mzn_int_ne_t(var int: x, var int: y) :: promise_total =
	let {
		var bool: r = (let { var bool: _FRESH_VAR :: promise_total } in _FRESH_VAR);
		constraint int_ne_reif(x, y, r);
	} in r;
function bool: 'shackle_<=<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_2 = '<'(x, y);
	} in if _DECL_2 then
		true
	else
		let { bool: _DECL_1 = '='(x, y) } in if _DECL_1 then true else false endif
	endif;
function var int: 'shackle_+<var int, var int>'(var int: x, var int: y) :: promise_total =
	'shackle_sum<array [int] of var int>'([x, y]);
function var int: 'shackle_sum<array [int] of var int>'(array [int] of var int: x)
	 :: promise_total =
	let {
		array [int] of bool: _DECL_4 = [_DECL_3 | _DECL_2 in x, _DECL_3 = is_fixed(_DECL_2)];
		bool: _DECL_5 = 'shackle_forall<array [int] of bool>'(_DECL_4);
	} in if _DECL_5 then
		let {
			array [int] of int: _DECL_8 = [_DECL_7 | _DECL_6 in x, _DECL_7 = fix(_DECL_6)];
			array [int] of int: _DECL_9 = arrayXd(x, _DECL_8);
		} in 'shackle_sum<array [int] of int>'(_DECL_9)
	else
		let {
			array [int] of var int: _DECL_1 = 'shackle_array1d<array [int] of var int>'(x);
		} in shackle_mzn_sum_t(_DECL_1)
	endif;
function var int: shackle_mzn_sum_t(array [int] of var int: x) :: promise_total =
	let {
		var int: r = (let { var (-infinity..infinity): _FRESH_VAR } in _FRESH_VAR);
		array [int] of int: _DECL_3 = [1 | x_i in x];
		int: _DECL_4 = '-'(1);
		array [int] of int: _DECL_5 =
			'shackle_++<array [int] of int, array [int] of int>'(_DECL_3, [_DECL_4]);
		array [int] of var int: _DECL_6 = 'shackle_array1d<array [int] of var int>'(x);
		array [int] of var int: _DECL_7 =
			'shackle_++<array [int] of var int, array [int] of var int>'(_DECL_6, [r]);
		constraint int_lin_eq(_DECL_5, _DECL_7, 0);
	} in r;
function int: 'shackle_sum<array [int] of int>'(array [int] of int: x) :: promise_total =
	let {
		int: _DECL_13 = length(x);
		bool: _DECL_14 = '='(_DECL_13, 0);
	} in if _DECL_14 then
		0
	else
		let {
			int: _DECL_10 = length(x);
			bool: _DECL_11 = '='(_DECL_10, 1);
		} in if _DECL_11 then
			let {
				array [int] of int: _DECL_12 = 'shackle_array1d<array [int] of int>'(x);
			} in mzn_element_internal(_DECL_12, 1)
		else
			let {
				array [int] of int: xx = 'shackle_array1d<array [int] of int>'(x);
				int: _DECL_2 = mzn_element_internal(xx, 1);
				int: _DECL_3 = mzn_element_internal(xx, 2);
				int: _DECL_4 = '+'(_DECL_2, _DECL_3);
				int: _DECL_5 = 3;
				int: _DECL_6 = length(xx);
				set of int: _DECL_7 = range_internal_int(_DECL_5, _DECL_6);
				array [int] of int: _DECL_8 = mzn_slice_internal(xx, [_DECL_7]);
				array [int] of int: _DECL_9 =
					'shackle_++<array [int] of int, array [int] of int>'([_DECL_4], _DECL_8);
			} in 'shackle_sum<array [int] of int>'(_DECL_9)
		endif
	endif;
function int: shackle_mod_internal(int: x, int: y) :: promise_total =
	let {
		int: _DECL_1 = div_internal_int(x, y);
		int: _DECL_2 = '*'(y, _DECL_1);
	} in '-'(x, _DECL_2);
function array [int] of var int: 'shackle_++<array [int] of var int, array [int] of var int>'(
	array [int] of var int: x,
	array [int] of var int: y,
) :: promise_total =
	[
		_DECL_13 |
			xx = 'shackle_array1d<array [int] of var int>'(x),
			yy = 'shackle_array1d<array [int] of var int>'(y),
			_DECL_3 = 1,
			_DECL_4 = length(x),
			_DECL_5 = length(y),
			_DECL_6 = '+'(_DECL_4, _DECL_5),
			_DECL_7 = range_internal_int(_DECL_3, _DECL_6),
			i in _DECL_7,
			_DECL_11 = length(x),
			_DECL_12 = 'shackle_<=<int, int>'(i, _DECL_11),
			_DECL_13 =
				if _DECL_12 then
					mzn_element_internal(xx, i)
				else
					let {
						int: _DECL_9 = length(x);
						int: _DECL_10 = '-'(i, _DECL_9);
					} in mzn_element_internal(yy, _DECL_10)
				endif,
	];
function array [int] of int: 'shackle_++<array [int] of int, array [int] of int>'(
	array [int] of int: x,
	array [int] of int: y,
) :: promise_total =
	[
		_DECL_13 |
			xx = 'shackle_array1d<array [int] of int>'(x),
			yy = 'shackle_array1d<array [int] of int>'(y),
			_DECL_3 = 1,
			_DECL_4 = length(x),
			_DECL_5 = length(y),
			_DECL_6 = '+'(_DECL_4, _DECL_5),
			_DECL_7 = range_internal_int(_DECL_3, _DECL_6),
			i in _DECL_7,
			_DECL_11 = length(x),
			_DECL_12 = 'shackle_<=<int, int>'(i, _DECL_11),
			_DECL_13 =
				if _DECL_12 then
					mzn_element_internal(xx, i)
				else
					let {
						int: _DECL_9 = length(x);
						int: _DECL_10 = '-'(i, _DECL_9);
					} in mzn_element_internal(yy, _DECL_10)
				endif,
	];
function int: 'shackle_min<int, int>'(int: x, int: y) :: promise_total =
	let { bool: _DECL_1 = '<'(x, y) } in if _DECL_1 then x else y endif;
function int: 'shackle_max<int, int>'(int: x, int: y) :: promise_total =
	let { bool: _DECL_1 = '<'(x, y) } in if _DECL_1 then y else x endif;
function set of int: shackle_compute_div_bounds_root(var int: x, var int: y) :: promise_total =
	let {
		int: _DECL_57 = lb(x);
		int: _DECL_58 = '-'(infinity);
		bool: _DECL_59 = '='(_DECL_57, _DECL_58);
		bool: _DECL_60 =
			if _DECL_59 then
				true
			else
				let {
					int: _DECL_55 = ub(x);
					bool: _DECL_56 = '='(_DECL_55, infinity);
				} in if _DECL_56 then true else false endif
			endif;
		bool: _DECL_61 =
			if _DECL_60 then
				true
			else
				let {
					int: _DECL_52 = lb(y);
					int: _DECL_53 = '-'(infinity);
					bool: _DECL_54 = '='(_DECL_52, _DECL_53);
				} in if _DECL_54 then true else false endif
			endif;
		bool: _DECL_62 =
			if _DECL_61 then
				true
			else
				let {
					int: _DECL_50 = ub(y);
					bool: _DECL_51 = '='(_DECL_50, infinity);
				} in if _DECL_51 then true else false endif
			endif;
	} in if _DECL_62 then
		let {
			int: _DECL_63 = '-'(infinity);
			int: _DECL_64 = infinity;
		} in range_internal_int(_DECL_63, _DECL_64)
	else
		let {
			int: _DECL_43 = lb(y);
			bool: _DECL_44 = '='(_DECL_43, 0);
			bool: _DECL_47 =
				if _DECL_44 then
					let {
						int: _DECL_45 = ub(y);
						bool: _DECL_46 = '='(_DECL_45, 0);
					} in if _DECL_46 then true else false endif
				else
					false
				endif;
		} in if _DECL_47 then
			let {
				int: _DECL_48 = '-'(infinity);
				int: _DECL_49 = infinity;
			} in range_internal_int(_DECL_48, _DECL_49)
		else
			let {
				int: _DECL_2 = lb(y);
				bool: _DECL_3 = '<'(_DECL_2, 0);
				set of int: _DECL_20 =
					if _DECL_3 then
						let {
							int: _DECL_4 = lb(x);
							int: _DECL_5 = lb(y);
							int: _DECL_6 = div_internal_int(_DECL_4, _DECL_5);
							int: _DECL_7 = ub(x);
							int: _DECL_8 = lb(y);
							int: _DECL_9 = div_internal_int(_DECL_7, _DECL_8);
							int: _DECL_10 = lb(x);
							int: _DECL_11 = ub(y);
							int: _DECL_12 = '-'(1);
							int: _DECL_13 = 'shackle_min<int, int>'(_DECL_11, _DECL_12);
							int: _DECL_14 = div_internal_int(_DECL_10, _DECL_13);
							int: _DECL_15 = ub(x);
							int: _DECL_16 = ub(y);
							int: _DECL_17 = '-'(1);
							int: _DECL_18 = 'shackle_min<int, int>'(_DECL_16, _DECL_17);
							int: _DECL_19 = div_internal_int(_DECL_15, _DECL_18);
						} in {_DECL_6, _DECL_9, _DECL_14, _DECL_19}
					else
						let { set of int: _DECL_1 = {} } in _DECL_1
					endif;
				int: _DECL_22 = ub(y);
				int: _DECL_23 = 0;
				bool: _DECL_24 = '<'(_DECL_23, _DECL_22);
				set of int: _DECL_39 =
					if _DECL_24 then
						let {
							int: _DECL_25 = lb(x);
							int: _DECL_26 = lb(y);
							int: _DECL_27 = 'shackle_max<int, int>'(_DECL_26, 1);
							int: _DECL_28 = div_internal_int(_DECL_25, _DECL_27);
							int: _DECL_29 = ub(x);
							int: _DECL_30 = lb(y);
							int: _DECL_31 = 'shackle_max<int, int>'(_DECL_30, 1);
							int: _DECL_32 = div_internal_int(_DECL_29, _DECL_31);
							int: _DECL_33 = lb(x);
							int: _DECL_34 = ub(y);
							int: _DECL_35 = div_internal_int(_DECL_33, _DECL_34);
							int: _DECL_36 = ub(x);
							int: _DECL_37 = ub(y);
							int: _DECL_38 = div_internal_int(_DECL_36, _DECL_37);
						} in {_DECL_28, _DECL_32, _DECL_35, _DECL_38}
					else
						let { set of int: _DECL_21 = {} } in _DECL_21
					endif;
				set of int: candidates = union_internal_int(_DECL_20, _DECL_39);
				int: _DECL_41 = 'shackle_min_root<set of int>'(candidates);
				int: _DECL_42 = 'shackle_max_root<set of int>'(candidates);
			} in range_internal_int(_DECL_41, _DECL_42)
		endif
	endif;
function var bool: 'int_eq<var int, var int>'(var int: x, var int: y) :: promise_total;
function var bool: int_eq_reif(var int: x, var int: y, var bool: b) :: promise_total;
function var bool: int_lin_eq(array [int] of int: as, array [int] of var int: xs, int: y)
	:: promise_total;
function var bool: 'int_ne<var int, var int>'(var int: x, var int: y) :: promise_total;
function var bool: int_ne_reif(var int: x, var int: y, var bool: b) :: promise_total;
function var bool: int_div(var int: x, var int: y, var int: z) :: promise_total;
function var int: 'shackle_div_root<var int, var int>'(var int: x, var int: y) :: promise_total =
	let {
		bool: _DECL_1 = is_fixed(x);
		bool: _DECL_2 = is_fixed(y);
		bool: _DECL_3 = 'shackle_forall<array [int] of bool>'([_DECL_1, _DECL_2]);
	} in if _DECL_3 then
		let {
			int: _DECL_4 = fix(x);
			int: _DECL_5 = fix(y);
		} in 'shackle_div_root<int, int>'(_DECL_4, _DECL_5)
	else
		shackle_div_t(x, y)
	endif;
function bool: 'shackle_occurs<int>'(int: x) :: promise_total = true;
function bool: 'shackle_occurs<opt int>'(tuple(bool, int): x) :: promise_total =
	if x.1 then 'shackle_occurs<int>'(x.2) else x.1 endif;
function bool: 'shackle_absent<int>'(int: x) :: promise_total = false;
function bool: 'shackle_absent<opt int>'(tuple(bool, int): x) :: promise_total =
	if x.1 then
		'shackle_absent<int>'(x.2)
	else
		let {
			bool: _DECL_1 = 'shackle_occurs<opt int>'(x);
		} in if _DECL_1 then false else true endif
	endif;
function int: 'shackle_deopt<int>'(int: x) :: promise_total = x;
function int: 'shackle_deopt_root<opt int>'(tuple(bool, int): x) :: promise_total =
	if x.1 then
		'shackle_deopt<int>'(x.2)
	else
		let {
			tuple(bool, int): _DECL_1 = x;
			bool: bx = _DECL_1.1;
			int: dx = _DECL_1.2;
			constraint bx;
		} in dx
	endif;
function tuple(bool, int): 'shackle_deopt<opt int>'(tuple(bool, int): x) :: promise_total =
	if x.1 then
		let { int: _DECL_6 = 'shackle_deopt<int>'(x.2) } in (true, _DECL_6)
	else
		let {
			tuple(bool, int): _DECL_1 = x;
			bool: bx = _DECL_1.1;
			int: dx = _DECL_1.2;
			bool: _DECL_4 = bx;
			bool: _DECL_5 = 'shackle_forall<array [int] of bool>'([_DECL_4]);
		} in (_DECL_5, dx)
	endif;
function string: shackle_mzn_version_to_string_root(int: v) :: promise_total =
	let {
		int: _DECL_1 = 'shackle_div_root<int, int>'(v, 10000);
		string: _DECL_2 = show_internal(_DECL_1);
		string: _DECL_3 = '++'(_DECL_2, ".");
		int: _DECL_4 = 'shackle_div_root<int, int>'(v, 1000);
		int: _DECL_5 = 'shackle_mod_root<int, int>'(_DECL_4, 10);
		string: _DECL_6 = show_internal(_DECL_5);
		string: _DECL_7 = '++'(_DECL_3, _DECL_6);
		string: _DECL_8 = '++'(_DECL_7, ".");
		int: _DECL_9 = 'shackle_mod_root<int, int>'(v, 100);
		string: _DECL_10 = show_internal(_DECL_9);
	} in '++'(_DECL_8, _DECL_10);
function var int: shackle_div_t(var int: x, var int: y) :: promise_total =
	let {
		set of int: _DECL_1 = shackle_compute_div_bounds_root(x, y);
		var int: z :: shackle_is_defined_var = (let { var _DECL_1: _FRESH_VAR } in _FRESH_VAR);
		constraint 'shackle_!=<var int, int>'(y, 0);
		constraint int_div(x, y, z);
	} in z;
function var int: foo_root(var int: v) :: promise_total =
	let {
		var int: _DECL_1 = 'shackle_div_root<var int, var int>'(v, 2);
	} in 'shackle_+<var int, var int>'(_DECL_1, 1);
tuple(bool, bool): shackle_mzn_absent_zero;
tuple(bool, int): shackle_mzn_min_version_required;
tuple(bool, int): shackle_mzn_max_version_required;
tuple(bool, bool): shackle_mzn_opt_only_range_domains;
tuple(bool, bool): shackle_mzn_opt_annotate_defines_var;
tuple(bool, bool): shackle_mzn_opt_annotate_computed_domains;
tuple(bool, bool): shackle_mzn_ignore_symmetry_breaking_constraints;
tuple(bool, bool): shackle_mzn_ignore_redundant_constraints;
tuple(bool, bool): shackle_mzn_half_reify_clause;
var int: _GLOBAL_10;
ann: shackle_domain_propagation = shackle_domain;
ann: shackle_bounds_propagation = shackle_bounds;
bool: _DECL_15 = 'shackle_absent<opt int>'(shackle_mzn_min_version_required);
bool: _DECL_16 =
	if _DECL_15 then
		true
	else
		let {
			tuple(bool, int): _DECL_10 =
				 'shackle_deopt<opt int>'(shackle_mzn_min_version_required);
			bool: _DECL_11 = _DECL_10.1;
			int: _DECL_12 = mzn_compiler_version();
			bool: _DECL_13 = 'shackle_<=<int, int>'(_DECL_10.2, _DECL_12);
			bool: _DECL_14 = 'shackle_forall<array [int] of bool>'([_DECL_11, _DECL_13]);
		} in if _DECL_14 then true else false endif
	endif;
constraint
	if _DECL_16 then
		true
	else
		let {
			int: _DECL_3 = 'shackle_deopt_root<opt int>'(shackle_mzn_min_version_required);
			string: _DECL_4 = shackle_mzn_version_to_string_root(_DECL_3);
			string: _DECL_5 = '++'("This model requires MiniZinc version ", _DECL_4);
			string: _DECL_6 = '++'(_DECL_5, " but you are running version ");
			int: _DECL_7 = mzn_compiler_version();
			string: _DECL_8 = shackle_mzn_version_to_string_root(_DECL_7);
			string: _DECL_9 = '++'(_DECL_6, _DECL_8);
		} in abort(_DECL_9)
	endif;
bool: _DECL_30 = 'shackle_absent<opt int>'(shackle_mzn_max_version_required);
bool: _DECL_31 =
	if _DECL_30 then
		true
	else
		let {
			tuple(bool, int): _DECL_24 =
				 'shackle_deopt<opt int>'(shackle_mzn_max_version_required);
			int: _DECL_25 = _DECL_24.2;
			int: _DECL_26 = mzn_compiler_version();
			bool: _DECL_27 = _DECL_24.1;
			bool: _DECL_28 = 'shackle_<=<int, int>'(_DECL_26, _DECL_25);
			bool: _DECL_29 = 'shackle_forall<array [int] of bool>'([_DECL_27, _DECL_28]);
		} in if _DECL_29 then true else false endif
	endif;
constraint
	if _DECL_31 then
		true
	else
		let {
			int: _DECL_17 = 'shackle_deopt_root<opt int>'(shackle_mzn_max_version_required);
			string: _DECL_18 = shackle_mzn_version_to_string_root(_DECL_17);
			string: _DECL_19 = '++'("This model only works with MiniZinc up to version ", _DECL_18);
			string: _DECL_20 = '++'(_DECL_19, " but you are running version ");
			int: _DECL_21 = mzn_compiler_version();
			string: _DECL_22 = shackle_mzn_version_to_string_root(_DECL_21);
			string: _DECL_23 = '++'(_DECL_20, _DECL_22);
		} in abort(_DECL_23)
	endif;
bool: shackle_debug_mode = mzn_internal_check_debug_mode();
var bool: p = true;
constraint p;
var bool: q = true;
int: _DECL_35 = 1;
int: _DECL_36 = 3;
set of int: _DECL_37 = range_internal_int(_DECL_35, _DECL_36);
var int: x :: shackle_output = (let { var _DECL_37: _FRESH_VAR } in _FRESH_VAR);
var int: _DECL_39 = foo_root(x);
constraint 'shackle_=<var int, var int>'(x, _DECL_39);
_GLOBAL_10 = 'shackle_+<var int, var int>'(x, 2);
solve maximize _GLOBAL_10;
