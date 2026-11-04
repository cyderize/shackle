annotation 'output';
annotation mzn_forward_annotations;
annotation empty_annotation;
annotation promise_total;
annotation is_defined_var;
annotation promise_commutative;
annotation domain;
annotation mzn_internal_representation;
annotation bounds;
function ann: 'defines_var<var int>'(var int: c) :: promise_total = empty_annotation;
function array [int] of int: 'array1d<array [int] of int>'(array [int] of int: x) :: promise_total =
	let {
		int: _DECL_1 = 1;
		int: _DECL_2 = length(x);
		set of int: _DECL_3 = range_internal_int(_DECL_1, _DECL_2);
	} in mzn_array_kd((_DECL_3,), x);
function array [int] of var int: 'array1d<array [int] of var int>'(array [int] of var int: x)
	:: promise_total =
	let {
		int: _DECL_1 = 1;
		int: _DECL_2 = length(x);
		set of int: _DECL_3 = range_internal_int(_DECL_1, _DECL_2);
	} in mzn_array_kd((_DECL_3,), x);
function int: 'div_root<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = '!=_root<int, int>'(y, 0);
		bool: _DECL_2 =
			if _DECL_1 then true else mzn_add_warning("division by zero is undefined") endif;
		constraint '='(_DECL_2, true);
		constraint _DECL_1;
		constraint '='(true, true);
	} in div_internal_int(x, y);
function int: 'mod_root<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = '!=_root<int, int>'(y, 0);
		bool: _DECL_2 =
			if _DECL_1 then true else mzn_add_warning("modulus by zero is undefined") endif;
		constraint '='(_DECL_2, true);
		constraint _DECL_1;
		constraint '='(true, true);
	} in mod_internal(x, y) :: mzn_forward_annotations;
function int: 'min_root<set of int>'(
	set of int: s,
)
	:: promise_total =
	let {
		int: _DECL_1 = card(s);
		int: _DECL_2 = 0;
		constraint '<'(_DECL_2, _DECL_1);
		bool: _DECL_3 = true;
		bool: _DECL_4 =
			if _DECL_3 then true else mzn_add_warning("minimum of an empty set is undefined") endif;
		constraint '='(_DECL_4, true);
		constraint _DECL_3;
		constraint '='(true, true);
	} in 'min_internal_set<set of int>'(s) :: mzn_forward_annotations;
function int: 'max_root<set of int>'(
	set of int: s,
)
	:: promise_total =
	let {
		int: _DECL_1 = card(s);
		int: _DECL_2 = 0;
		constraint '<'(_DECL_2, _DECL_1);
		bool: _DECL_3 = true;
		bool: _DECL_4 =
			if _DECL_3 then true else mzn_add_warning("maximum of an empty set is undefined") endif;
		constraint '='(_DECL_4, true);
		constraint _DECL_3;
		constraint '='(true, true);
	} in 'max_internal_set<set of int>'(s) :: mzn_forward_annotations;
function bool: 'forall<array [int] of bool>'(array [int] of bool: x) :: promise_total =
	let {
		array [int] of bool: _DECL_3 = [
			x_i |
				x_i in x,
				_DECL_2 = if x_i then false else true endif,
				_ = 1 where _DECL_2,
		];
		int: _DECL_4 = length(_DECL_3);
	} in '='(_DECL_4, 0);
function bool: '<-><bool, bool>'(bool: x, bool: y) :: promise_total =
	if x then y else if y then false else true endif endif;
function var bool: '<-><var bool, var bool>'(var bool: x, var bool: y) :: promise_total =
	let {
		bool: _DECL_1 = is_fixed(x);
		bool: _DECL_2 = is_fixed(y);
		bool: _DECL_3 =
			 'forall<array [int] of bool>'([_DECL_1, _DECL_2]) :: mzn_forward_annotations;
	} in if _DECL_3 then
		let {
			bool: _DECL_4 = fix(x);
			bool: _DECL_5 = fix(y);
		} in '<-><bool, bool>'(_DECL_4, _DECL_5) :: mzn_forward_annotations
	else
		mzn_iff(x, y) :: mzn_forward_annotations
	endif;
function var bool: mzn_iff(var bool: x, var bool: y) :: promise_total =
	if false then
		let {
			var bool: _DECL_1 = 'bool_eq<var bool, var bool>'(x, y) :: mzn_forward_annotations;
		} in _DECL_1
	else
		mzn_iff_t(x, y) :: mzn_forward_annotations
	endif;
function var bool: mzn_iff_t(var bool: x, var bool: y) :: promise_total =
	let {
		var bool: r :: promise_total = (
			let { var bool: _FRESH_VAR :: promise_total } in _FRESH_VAR
		);
		constraint bool_eq_reif(x, y, r) :: mzn_forward_annotations;
	} in r;
function var bool: '=<var bool, bool>'(var bool: x, bool: y) :: promise_total =
	'<-><var bool, var bool>'(x, y) :: mzn_forward_annotations;
function var bool: '=_root<var int, var int>'(var int: x, var int: y) :: promise_total =
	let {
		bool: _DECL_3 = is_fixed(y);
		var bool: _DECL_5 =
			if _DECL_3 then
				let {
					int: _DECL_4 = fix(y);
					constraint mzn_int_eq_root(x, _DECL_4) :: mzn_forward_annotations;
				} in true
			else
				let {
					bool: _DECL_1 =
						is_fixed(x);
				} in if _DECL_1 then
					let {
						int: _DECL_2 = fix(x);
						constraint mzn_int_eq_root(_DECL_2, y) :: mzn_forward_annotations;
					} in true
				else
					mzn_int_eq_root(x, y) :: mzn_forward_annotations
				endif
			endif;
		constraint _DECL_5;
	} in true;
function var bool: mzn_int_eq_root(var int: x, var int: y) :: promise_total =
	let { constraint 'int_eq<var int, var int>'(x, y) :: mzn_forward_annotations } in true;
function bool: '!=_root<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = '='(x, y);
		bool: _DECL_2 = if _DECL_1 then false else true endif;
		constraint _DECL_2;
	} in true;
function var bool: '!=_root<var int, int>'(var int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = is_fixed(x);
		var bool: _DECL_3 =
			if _DECL_1 then
				let {
					int: _DECL_2 = fix(x);
				} in '!=_root<int, int>'(_DECL_2, y) :: mzn_forward_annotations
			else
				mzn_int_ne_root(x, y) :: mzn_forward_annotations
			endif;
		constraint _DECL_3;
	} in true;
function var bool: mzn_int_ne_root(var int: x, var int: y) :: promise_total =
	let { constraint 'int_ne<var int, var int>'(x, y) :: mzn_forward_annotations } in true;
function bool: '<=<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_2 = '<'(x, y);
	} in if _DECL_2 then
		true
	else
		let { bool: _DECL_1 = '='(x, y) } in if _DECL_1 then true else false endif
	endif;
function var int: '+<var int, var int>'(var int: x, var int: y) :: promise_total =
	'sum<array [int] of var int>'([x, y]) :: mzn_forward_annotations;
function var int: 'sum<array [int] of var int>'(array [int] of var int: x) :: promise_total =
	let {
		array [int] of bool: _DECL_4 = [_DECL_3 | _DECL_2 in x, _DECL_3 = is_fixed(_DECL_2)];
		bool: _DECL_5 = 'forall<array [int] of bool>'(_DECL_4) :: mzn_forward_annotations;
	} in if _DECL_5 then
		let {
			array [int] of int: _DECL_8 = [_DECL_7 | _DECL_6 in x, _DECL_7 = fix(_DECL_6)];
			array [int] of int: _DECL_9 = arrayXd(x, _DECL_8);
		} in 'sum<array [int] of int>'(_DECL_9) :: mzn_forward_annotations
	else
		let {
			array [int] of var int: _DECL_1 = 'array1d<array [int] of var int>'(x);
		} in mzn_sum_t(_DECL_1) :: mzn_forward_annotations
	endif;
function var int: mzn_sum_t(array [int] of var int: x) :: promise_total =
	let {
		var int: r :: promise_total = (
			let { var (-infinity..infinity): _FRESH_VAR } in _FRESH_VAR
		);
		array [int] of int: _DECL_3 = [1 | x_i in x];
		int: _DECL_4 = '-'(1);
		array [int] of int: _DECL_5 =
			'++<array [int] of int, array [int] of int>'(_DECL_3, [_DECL_4]);
		array [int] of var int: _DECL_6 = 'array1d<array [int] of var int>'(x);
		array [int] of var int: _DECL_7 =
			'++<array [int] of var int, array [int] of var int>'(_DECL_6, [r]);
		constraint int_lin_eq(_DECL_5, _DECL_7, 0) :: mzn_forward_annotations;
	} in r;
function int: 'sum<array [int] of int>'(array [int] of int: x) :: promise_total =
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
				array [int] of int: _DECL_12 = 'array1d<array [int] of int>'(x);
			} in mzn_element_internal(_DECL_12, 1)
		else
			let {
				array [int] of int: xx = 'array1d<array [int] of int>'(x);
				int: _DECL_2 = mzn_element_internal(xx, 1);
				int: _DECL_3 = mzn_element_internal(xx, 2);
				int: _DECL_4 = '+'(_DECL_2, _DECL_3);
				int: _DECL_5 = 3;
				int: _DECL_6 = length(xx);
				set of int: _DECL_7 = range_internal_int(_DECL_5, _DECL_6);
				array [int] of int: _DECL_8 = mzn_slice_internal(xx, [_DECL_7]);
				array [int] of int: _DECL_9 =
					'++<array [int] of int, array [int] of int>'([_DECL_4], _DECL_8);
			} in 'sum<array [int] of int>'(_DECL_9) :: mzn_forward_annotations
		endif
	endif;
function int: mod_internal(int: x, int: y) :: promise_total =
	let {
		int: _DECL_1 = div_internal_int(x, y);
		int: _DECL_2 = '*'(y, _DECL_1);
	} in '-'(x, _DECL_2);
function array [int] of var int: '++<array [int] of var int, array [int] of var int>'(
	array [int] of var int: x,
	array [int] of var int: y,
) :: promise_total =
	[
		_DECL_13 |
			xx = 'array1d<array [int] of var int>'(x) :: mzn_forward_annotations,
			yy = 'array1d<array [int] of var int>'(y) :: mzn_forward_annotations,
			_DECL_3 = 1,
			_DECL_4 = length(x),
			_DECL_5 = length(y),
			_DECL_6 = '+'(_DECL_4, _DECL_5),
			_DECL_7 = range_internal_int(_DECL_3, _DECL_6),
			i in _DECL_7,
			_DECL_11 = length(x),
			_DECL_12 = '<=<int, int>'(i, _DECL_11) :: mzn_forward_annotations,
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
function array [int] of int: '++<array [int] of int, array [int] of int>'(
	array [int] of int: x,
	array [int] of int: y,
) :: promise_total =
	[
		_DECL_13 |
			xx = 'array1d<array [int] of int>'(x) :: mzn_forward_annotations,
			yy = 'array1d<array [int] of int>'(y) :: mzn_forward_annotations,
			_DECL_3 = 1,
			_DECL_4 = length(x),
			_DECL_5 = length(y),
			_DECL_6 = '+'(_DECL_4, _DECL_5),
			_DECL_7 = range_internal_int(_DECL_3, _DECL_6),
			i in _DECL_7,
			_DECL_11 = length(x),
			_DECL_12 = '<=<int, int>'(i, _DECL_11) :: mzn_forward_annotations,
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
function int: 'min<int, int>'(int: x, int: y) :: promise_total =
	let { bool: _DECL_1 = '<'(x, y) } in if _DECL_1 then x else y endif;
function int: 'max<int, int>'(int: x, int: y) :: promise_total =
	let { bool: _DECL_1 = '<'(x, y) } in if _DECL_1 then y else x endif;
function int: 'min_internal_set<set of int>'(set of int: s) :: promise_total =
	let { array [int] of int: _DECL_1 = set2array(s) } in mzn_element_internal(_DECL_1, 1);
function int: 'max_internal_set<set of int>'(set of int: s) :: promise_total =
	let {
		array [int] of int: _DECL_1 = set2array(s);
		int: _DECL_2 = card(s);
	} in mzn_element_internal(_DECL_1, _DECL_2);
function set of int: compute_div_bounds_root(var int: x, var int: y) :: promise_total =
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
							int: _DECL_13 = 'min<int, int>'(_DECL_11, _DECL_12);
							int: _DECL_14 = div_internal_int(_DECL_10, _DECL_13);
							int: _DECL_15 = ub(x);
							int: _DECL_16 = ub(y);
							int: _DECL_17 = '-'(1);
							int: _DECL_18 = 'min<int, int>'(_DECL_16, _DECL_17);
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
							int: _DECL_27 = 'max<int, int>'(_DECL_26, 1);
							int: _DECL_28 = div_internal_int(_DECL_25, _DECL_27);
							int: _DECL_29 = ub(x);
							int: _DECL_30 = lb(y);
							int: _DECL_31 = 'max<int, int>'(_DECL_30, 1);
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
				int: _DECL_41 = 'min_root<set of int>'(candidates);
				int: _DECL_42 = 'max_root<set of int>'(candidates);
			} in range_internal_int(_DECL_41, _DECL_42)
		endif
	endif;
function var bool: 'int_eq<var int, var int>'(var int: x, var int: y) :: promise_total;
function var bool: int_lin_eq(array [int] of int: as, array [int] of var int: xs, int: y)
	:: promise_total;
function var bool: 'int_ne<var int, var int>'(var int: x, var int: y) :: promise_total;
function var bool: int_div(var int: x, var int: y, var int: z) :: promise_total;
function var bool: 'bool_eq<var bool, var bool>'(var bool: x, var bool: y) :: promise_total;
function var bool: bool_eq_reif(var bool: x, var bool: y, var bool: b) :: promise_total;
function var int: 'div_root<var int, var int>'(var int: x, var int: y) :: promise_total =
	let {
		bool: _DECL_1 = is_fixed(x);
		bool: _DECL_2 = is_fixed(y);
		bool: _DECL_3 =
			 'forall<array [int] of bool>'([_DECL_1, _DECL_2]) :: mzn_forward_annotations;
	} in if _DECL_3 then
		let {
			int: _DECL_4 = fix(x);
			int: _DECL_5 = fix(y);
		} in 'div_root<int, int>'(_DECL_4, _DECL_5) :: mzn_forward_annotations
	else
		div_t(x, y) :: mzn_forward_annotations
	endif;
function bool: 'occurs<int>'(int: x) :: promise_total = true;
function bool: 'occurs<opt int>'(tuple(bool, int): x) :: promise_total =
	if x.1 then 'occurs<int>'(x.2) :: mzn_forward_annotations else x.1 endif;
function bool: 'absent<int>'(int: x) :: promise_total = false;
function bool: 'absent<opt int>'(tuple(bool, int): x) :: promise_total =
	if x.1 then
		'absent<int>'(x.2) :: mzn_forward_annotations
	else
		let {
			bool: _DECL_1 = 'occurs<opt int>'(x) :: mzn_forward_annotations;
		} in if _DECL_1 then false else true endif
	endif;
function int: 'deopt<int>'(int: x) :: promise_total = x;
function int: 'deopt_root<opt int>'(tuple(bool, int): x) :: promise_total =
	if x.1 then
		'deopt<int>'(x.2) :: mzn_forward_annotations
	else
		let {
			tuple(bool, int): _DECL_1 = x;
			bool: bx = _DECL_1.1;
			int: dx = _DECL_1.2;
			constraint '='(bx, true);
		} in dx
	endif;
function tuple(bool, int): 'deopt<opt int>'(tuple(bool, int): x) :: promise_total =
	if x.1 then
		let { int: _DECL_6 = 'deopt<int>'(x.2) :: mzn_forward_annotations } in (true, _DECL_6)
	else
		let {
			tuple(bool, int): _DECL_1 = x;
			bool: bx = _DECL_1.1;
			int: dx = _DECL_1.2;
			bool: _DECL_4 = bx;
			bool: _DECL_5 = 'forall<array [int] of bool>'([_DECL_4]);
		} in (_DECL_5, dx)
	endif;
function string: mzn_version_to_string_root(int: v) :: promise_total =
	let {
		int: _DECL_1 = 'div_root<int, int>'(v, 10000);
		string: _DECL_2 = show_internal(_DECL_1);
		string: _DECL_3 = '++'(_DECL_2, ".");
		int: _DECL_4 = 'div_root<int, int>'(v, 1000);
		int: _DECL_5 = 'mod_root<int, int>'(_DECL_4, 10);
		string: _DECL_6 = show_internal(_DECL_5);
		string: _DECL_7 = '++'(_DECL_3, _DECL_6);
		string: _DECL_8 = '++'(_DECL_7, ".");
		int: _DECL_9 = 'mod_root<int, int>'(v, 100);
		string: _DECL_10 = show_internal(_DECL_9);
	} in '++'(_DECL_8, _DECL_10);
function var int: div_t(var int: x, var int: y) :: promise_total =
	let {
		set of int: _DECL_1 = compute_div_bounds_root(x, y);
		var int: z :: is_defined_var :: promise_total = (
			let { var _DECL_1: _FRESH_VAR } in _FRESH_VAR
		);
		constraint '!=_root<var int, int>'(y, 0) :: mzn_forward_annotations;
		ann: _DECL_3 = 'defines_var<var int>'(z) :: mzn_forward_annotations;
		constraint int_div(x, y, z) :: _DECL_3mzn_forward_annotations;
		constraint '=<var bool, bool>'(true, true);
	} in z;
function var int: foo_root(var int: v) :: promise_total =
	let {
		var int: _DECL_1 = 'div_root<var int, var int>'(v, 2);
	} in '+<var int, var int>'(_DECL_1, 1) :: mzn_forward_annotations;
tuple(bool, bool): mzn_absent_zero;
tuple(bool, int): mzn_min_version_required;
tuple(bool, int): mzn_max_version_required;
tuple(bool, bool): mzn_opt_only_range_domains;
tuple(bool, bool): mzn_opt_annotate_defines_var;
tuple(bool, bool): mzn_opt_annotate_computed_domains;
tuple(bool, bool): mzn_ignore_symmetry_breaking_constraints;
tuple(bool, bool): mzn_ignore_redundant_constraints;
tuple(bool, bool): mzn_opt_eager_linear_aggregation;
tuple(bool, bool): mzn_half_reify_clause;
var int: x;
var int: _GLOBAL_12;
ann: domain_propagation = domain;
ann: bounds_propagation = bounds;
bool: _DECL_15 = 'absent<opt int>'(mzn_min_version_required);
bool: _DECL_16 =
	if _DECL_15 then
		true
	else
		let {
			tuple(bool, int): _DECL_10 = 'deopt<opt int>'(mzn_min_version_required);
			bool: _DECL_11 = _DECL_10.1;
			int: _DECL_12 = mzn_compiler_version();
			bool: _DECL_13 = '<=<int, int>'(_DECL_10.2, _DECL_12);
			bool: _DECL_14 = 'forall<array [int] of bool>'([_DECL_11, _DECL_13]);
		} in if _DECL_14 then true else false endif
	endif;
bool: _DECL_17 =
	if _DECL_16 then
		true
	else
		let {
			int: _DECL_3 = 'deopt_root<opt int>'(mzn_min_version_required);
			string: _DECL_4 = mzn_version_to_string_root(_DECL_3);
			string: _DECL_5 = '++'("This model requires MiniZinc version ", _DECL_4);
			string: _DECL_6 = '++'(_DECL_5, " but you are running version ");
			int: _DECL_7 = mzn_compiler_version();
			string: _DECL_8 = mzn_version_to_string_root(_DECL_7);
			string: _DECL_9 = '++'(_DECL_6, _DECL_8);
		} in abort(_DECL_9)
	endif;
constraint '='(_DECL_17, true);
bool: _DECL_31 = 'absent<opt int>'(mzn_max_version_required);
bool: _DECL_32 =
	if _DECL_31 then
		true
	else
		let {
			tuple(bool, int): _DECL_25 = 'deopt<opt int>'(mzn_max_version_required);
			int: _DECL_26 = _DECL_25.2;
			int: _DECL_27 = mzn_compiler_version();
			bool: _DECL_28 = _DECL_25.1;
			bool: _DECL_29 = '<=<int, int>'(_DECL_27, _DECL_26);
			bool: _DECL_30 = 'forall<array [int] of bool>'([_DECL_28, _DECL_29]);
		} in if _DECL_30 then true else false endif
	endif;
bool: _DECL_33 =
	if _DECL_32 then
		true
	else
		let {
			int: _DECL_18 = 'deopt_root<opt int>'(mzn_max_version_required);
			string: _DECL_19 = mzn_version_to_string_root(_DECL_18);
			string: _DECL_20 = '++'("This model only works with MiniZinc up to version ", _DECL_19);
			string: _DECL_21 = '++'(_DECL_20, " but you are running version ");
			int: _DECL_22 = mzn_compiler_version();
			string: _DECL_23 = mzn_version_to_string_root(_DECL_22);
			string: _DECL_24 = '++'(_DECL_21, _DECL_23);
		} in abort(_DECL_24)
	endif;
constraint '='(_DECL_33, true);
bool: debug_mode = mzn_internal_check_debug_mode();
var bool: p = true;
constraint '=<var bool, bool>'(p, true);
var bool: q = true;
constraint annotate(x, 'output');
int: _DECL_37 = 1;
int: _DECL_38 = 3;
set of int: _DECL_39 = range_internal_int(_DECL_37, _DECL_38);
x = (let { var _DECL_39: _FRESH_VAR } in _FRESH_VAR);
var int: _DECL_40 = foo_root(x);
constraint '=_root<var int, var int>'(x, _DECL_40);
_GLOBAL_12 = '+<var int, var int>'(x, 2);
solve maximize _GLOBAL_12;
