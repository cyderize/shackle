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
		array [int] of int: _DECL_4 = mzn_array_kd((_DECL_3,), x);
	} in _DECL_4;
function array [int] of var int: 'shackle_array1d<array [int] of var int>'(
	array [int] of var int: x,
)
	:: promise_total =
	let {
		int: _DECL_1 = 1;
		int: _DECL_2 = length(x);
		set of int: _DECL_3 = range_internal_int(_DECL_1, _DECL_2);
		array [int] of var int: _DECL_4 = mzn_array_kd((_DECL_3,), x);
	} in _DECL_4;
function int: 'shackle_div_root<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = 'shackle_!=<int, int>'(y, 0);
		constraint
			if _DECL_1 then
				let { bool: _DECL_2 = true } in _DECL_2
			else
				let { bool: _DECL_3 = mzn_add_warning("division by zero is undefined") } in _DECL_3
			endif;
		constraint _DECL_1;
		constraint true;
		int: _DECL_4 = div_internal_int(x, y);
	} in _DECL_4;
function int: 'shackle_mod_root<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = 'shackle_!=<int, int>'(y, 0);
		constraint
			if _DECL_1 then
				let { bool: _DECL_2 = true } in _DECL_2
			else
				let { bool: _DECL_3 = mzn_add_warning("modulus by zero is undefined") } in _DECL_3
			endif;
		constraint _DECL_1;
		constraint true;
		int: _DECL_4 = shackle_mod_internal(x, y);
	} in _DECL_4;
function int: 'shackle_min_root<set of int>'(set of int: s) :: promise_total =
	let {
		int: _DECL_1 = card(s);
		int: _DECL_2 = 0;
		constraint '<'(_DECL_2, _DECL_1);
		bool: _DECL_3 = true;
		constraint
			if _DECL_3 then
				let { bool: _DECL_4 = true } in _DECL_4
			else
				let {
					bool: _DECL_5 = mzn_add_warning("minimum of an empty set is undefined");
				} in _DECL_5
			endif;
		constraint _DECL_3;
		constraint true;
		int: _DECL_6 = min_internal_set(s);
	} in _DECL_6;
function int: 'shackle_max_root<set of int>'(set of int: s) :: promise_total =
	let {
		int: _DECL_1 = card(s);
		int: _DECL_2 = 0;
		constraint '<'(_DECL_2, _DECL_1);
		bool: _DECL_3 = true;
		constraint
			if _DECL_3 then
				let { bool: _DECL_4 = true } in _DECL_4
			else
				let {
					bool: _DECL_5 = mzn_add_warning("maximum of an empty set is undefined");
				} in _DECL_5
			endif;
		constraint _DECL_3;
		constraint true;
		int: _DECL_6 = max_internal_set(s);
	} in _DECL_6;
function bool: 'shackle_forall<array [int] of bool>'(array [int] of bool: x) :: promise_total =
	let {
		array [int] of bool: _DECL_6 = [
			x_i |
				x_i in x,
				_DECL_5 =
					if x_i then
						let { bool: _DECL_3 = false } in _DECL_3
					else
						let { bool: _DECL_4 = true } in _DECL_4
					endif,
				_ = 1 where _DECL_5,
		];
		int: _DECL_7 = length(_DECL_6);
		bool: _DECL_8 = '='(_DECL_7, 0);
	} in _DECL_8;
function var bool: 'shackle_=<var int, var int>'(var int: x, var int: y) :: promise_total =
	let {
		bool: _DECL_5 = is_fixed(y);
		var bool: _DECL_9 =
			if _DECL_5 then
				let {
					int: _DECL_6 = fix(y);
					var bool: _DECL_7 = shackle_mzn_int_eq(x, _DECL_6);
				} in _DECL_7
			else
				let {
					bool: _DECL_1 = is_fixed(x);
					var bool: _DECL_8 =
						if _DECL_1 then
							let {
								int: _DECL_2 = fix(x);
								var bool: _DECL_3 = shackle_mzn_int_eq(_DECL_2, y);
							} in _DECL_3
						else
							let { var bool: _DECL_4 = shackle_mzn_int_eq(x, y) } in _DECL_4
						endif;
				} in _DECL_8
			endif;
	} in _DECL_9;
function var bool: shackle_mzn_int_eq(var int: x, var int: y) :: promise_total =
	let {
		var bool: _DECL_3 =
			if false then
				let { var bool: _DECL_1 = 'int_eq<var int, var int>'(x, y) } in _DECL_1
			else
				let { var bool: _DECL_2 = shackle_mzn_int_eq_t(x, y) } in _DECL_2
			endif;
	} in _DECL_3;
function var bool: shackle_mzn_int_eq_t(var int: x, var int: y) :: promise_total =
	let {
		var bool: r = (let { var bool: _FRESH_VAR :: promise_total } in _FRESH_VAR);
		constraint int_eq_reif(x, y, r);
	} in r;
function bool: 'shackle_!=<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = '='(x, y);
		bool: _DECL_4 =
			if _DECL_1 then
				let { bool: _DECL_2 = false } in _DECL_2
			else
				let { bool: _DECL_3 = true } in _DECL_3
			endif;
	} in _DECL_4;
function var bool: 'shackle_!=<var int, int>'(var int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = is_fixed(x);
		var bool: _DECL_5 =
			if _DECL_1 then
				let {
					int: _DECL_2 = fix(x);
					bool: _DECL_3 = 'shackle_!=<int, int>'(_DECL_2, y);
				} in _DECL_3
			else
				let { var bool: _DECL_4 = shackle_mzn_int_ne(x, y) } in _DECL_4
			endif;
	} in _DECL_5;
function var bool: shackle_mzn_int_ne(var int: x, var int: y) :: promise_total =
	let {
		var bool: _DECL_3 =
			if false then
				let { var bool: _DECL_1 = 'int_ne<var int, var int>'(x, y) } in _DECL_1
			else
				let { var bool: _DECL_2 = shackle_mzn_int_ne_t(x, y) } in _DECL_2
			endif;
	} in _DECL_3;
function var bool: shackle_mzn_int_ne_t(var int: x, var int: y) :: promise_total =
	let {
		var bool: r = (let { var bool: _FRESH_VAR :: promise_total } in _FRESH_VAR);
		constraint int_ne_reif(x, y, r);
	} in r;
function bool: 'shackle_<=<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_4 = '<'(x, y);
		bool: _DECL_7 =
			if _DECL_4 then
				let { bool: _DECL_5 = true } in _DECL_5
			else
				let {
					bool: _DECL_1 = '='(x, y);
					bool: _DECL_6 =
						if _DECL_1 then
							let { bool: _DECL_2 = true } in _DECL_2
						else
							let { bool: _DECL_3 = false } in _DECL_3
						endif;
				} in _DECL_6
			endif;
	} in _DECL_7;
function var int: 'shackle_+<var int, var int>'(var int: x, var int: y) :: promise_total =
	let { var int: _DECL_1 = 'shackle_sum<array [int] of var int>'([x, y]) } in _DECL_1;
function var int: 'shackle_sum<array [int] of var int>'(array [int] of var int: x)
	 :: promise_total =
	let {
		array [int] of bool: _DECL_5 = [
			let { bool: _DECL_4 = is_fixed(_DECL_3) } in _DECL_4 |
				_DECL_3 in x,
		];
		bool: _DECL_6 = 'shackle_forall<array [int] of bool>'(_DECL_5);
		var int: _DECL_14 =
			if _DECL_6 then
				let {
					array [int] of int: _DECL_10 = [
						let { int: _DECL_9 = fix(_DECL_8) } in _DECL_9 |
							_DECL_8 in x,
					];
					array [int] of int: _DECL_11 = arrayXd(x, _DECL_10);
					int: _DECL_12 = 'shackle_sum<array [int] of int>'(_DECL_11);
				} in _DECL_12
			else
				let {
					array [int] of var int: _DECL_1 = 'shackle_array1d<array [int] of var int>'(x);
					var int: _DECL_13 = shackle_mzn_sum_t(_DECL_1);
				} in _DECL_13
			endif;
	} in _DECL_14;
function var int: shackle_mzn_sum_t(array [int] of var int: x) :: promise_total =
	let {
		var int: r = (let { var (-infinity..infinity): _FRESH_VAR } in _FRESH_VAR);
		array [int] of int: _DECL_5 = [let { int: _DECL_4 = 1 } in _DECL_4 | x_i in x];
		int: _DECL_6 = '-'(1);
		array [int] of int: _DECL_7 =
			'shackle_++<array [int] of int, array [int] of int>'(_DECL_5, [_DECL_6]);
		array [int] of var int: _DECL_8 = 'shackle_array1d<array [int] of var int>'(x);
		array [int] of var int: _DECL_9 =
			'shackle_++<array [int] of var int, array [int] of var int>'(_DECL_8, [r]);
		constraint int_lin_eq(_DECL_7, _DECL_9, 0);
	} in r;
function int: 'shackle_sum<array [int] of int>'(array [int] of int: x) :: promise_total =
	let {
		int: _DECL_15 = length(x);
		bool: _DECL_16 = '='(_DECL_15, 0);
		int: _DECL_19 =
			if _DECL_16 then
				let { int: _DECL_17 = 0 } in _DECL_17
			else
				let {
					int: _DECL_10 = length(x);
					bool: _DECL_11 = '='(_DECL_10, 1);
					int: _DECL_18 =
						if _DECL_11 then
							let {
								array [int] of int: _DECL_12 =
									'shackle_array1d<array [int] of int>'(x);
								int: _DECL_13 = mzn_element_internal(_DECL_12, 1);
							} in _DECL_13
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
									'shackle_++<array [int] of int, array [int] of int>'(
										[_DECL_4],
										_DECL_8,
									);
								int: _DECL_14 = 'shackle_sum<array [int] of int>'(_DECL_9);
							} in _DECL_14
						endif;
				} in _DECL_18
			endif;
	} in _DECL_19;
function int: shackle_mod_internal(int: x, int: y) :: promise_total =
	let {
		int: _DECL_1 = div_internal_int(x, y);
		int: _DECL_2 = '*'(y, _DECL_1);
		int: _DECL_3 = '-'(x, _DECL_2);
	} in _DECL_3;
function array [int] of var int: 'shackle_++<array [int] of var int, array [int] of var int>'(
	array [int] of var int: x,
	array [int] of var int: y,
)
	:: promise_total =
	let {
		array [
			int,
		] of var int: _DECL_17 = [
			let {
				int: _DECL_12 = length(x);
				bool: _DECL_13 = 'shackle_<=<int, int>'(i, _DECL_12);
				var int: _DECL_16 =
					if _DECL_13 then
						let { var int: _DECL_14 = mzn_element_internal(_DECL_1, i) } in _DECL_14
					else
						let {
							int: _DECL_10 = length(x);
							int: _DECL_11 = '-'(i, _DECL_10);
							var int: _DECL_15 = mzn_element_internal(_DECL_2, _DECL_11);
						} in _DECL_15
					endif;
			} in _DECL_16 |
				_DECL_1 = 'shackle_array1d<array [int] of var int>'(x),
				_DECL_2 = 'shackle_array1d<array [int] of var int>'(y),
				_DECL_3 = 1,
				_DECL_4 = length(x),
				_DECL_5 = length(y),
				_DECL_6 = '+'(_DECL_4, _DECL_5),
				_DECL_7 = range_internal_int(_DECL_3, _DECL_6),
				i in _DECL_7,
		];
	} in _DECL_17;
function array [int] of int: 'shackle_++<array [int] of int, array [int] of int>'(
	array [int] of int: x,
	array [int] of int: y,
)
	:: promise_total =
	let {
		array [
			int,
		] of int: _DECL_17 = [
			let {
				int: _DECL_12 = length(x);
				bool: _DECL_13 = 'shackle_<=<int, int>'(i, _DECL_12);
				int: _DECL_16 =
					if _DECL_13 then
						let { int: _DECL_14 = mzn_element_internal(_DECL_1, i) } in _DECL_14
					else
						let {
							int: _DECL_10 = length(x);
							int: _DECL_11 = '-'(i, _DECL_10);
							int: _DECL_15 = mzn_element_internal(_DECL_2, _DECL_11);
						} in _DECL_15
					endif;
			} in _DECL_16 |
				_DECL_1 = 'shackle_array1d<array [int] of int>'(x),
				_DECL_2 = 'shackle_array1d<array [int] of int>'(y),
				_DECL_3 = 1,
				_DECL_4 = length(x),
				_DECL_5 = length(y),
				_DECL_6 = '+'(_DECL_4, _DECL_5),
				_DECL_7 = range_internal_int(_DECL_3, _DECL_6),
				i in _DECL_7,
		];
	} in _DECL_17;
function int: 'shackle_min<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = '<'(x, y);
		int: _DECL_2 = if _DECL_1 then x else y endif;
	} in _DECL_2;
function int: 'shackle_max<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = '<'(x, y);
		int: _DECL_2 = if _DECL_1 then y else x endif;
	} in _DECL_2;
function set of int: shackle_compute_div_bounds_root(var int: x, var int: y) :: promise_total =
	let {
		int: _DECL_71 = lb(x);
		int: _DECL_72 = '-'(infinity);
		bool: _DECL_73 = '='(_DECL_71, _DECL_72);
		bool: _DECL_76 =
			if _DECL_73 then
				let { bool: _DECL_74 = true } in _DECL_74
			else
				let {
					int: _DECL_67 = ub(x);
					bool: _DECL_68 = '='(_DECL_67, infinity);
					bool: _DECL_75 =
						if _DECL_68 then
							let { bool: _DECL_69 = true } in _DECL_69
						else
							let { bool: _DECL_70 = false } in _DECL_70
						endif;
				} in _DECL_75
			endif;
		bool: _DECL_79 =
			if _DECL_76 then
				let { bool: _DECL_77 = true } in _DECL_77
			else
				let {
					int: _DECL_62 = lb(y);
					int: _DECL_63 = '-'(infinity);
					bool: _DECL_64 = '='(_DECL_62, _DECL_63);
					bool: _DECL_78 =
						if _DECL_64 then
							let { bool: _DECL_65 = true } in _DECL_65
						else
							let { bool: _DECL_66 = false } in _DECL_66
						endif;
				} in _DECL_78
			endif;
		bool: _DECL_82 =
			if _DECL_79 then
				let { bool: _DECL_80 = true } in _DECL_80
			else
				let {
					int: _DECL_58 = ub(y);
					bool: _DECL_59 = '='(_DECL_58, infinity);
					bool: _DECL_81 =
						if _DECL_59 then
							let { bool: _DECL_60 = true } in _DECL_60
						else
							let { bool: _DECL_61 = false } in _DECL_61
						endif;
				} in _DECL_81
			endif;
		set of int: _DECL_87 =
			if _DECL_82 then
				let {
					int: _DECL_83 = '-'(infinity);
					int: _DECL_84 = infinity;
					set of int: _DECL_85 = range_internal_int(_DECL_83, _DECL_84);
				} in _DECL_85
			else
				let {
					int: _DECL_45 = lb(y);
					bool: _DECL_46 = '='(_DECL_45, 0);
					bool: _DECL_53 =
						if _DECL_46 then
							let {
								int: _DECL_47 = ub(y);
								bool: _DECL_48 = '='(_DECL_47, 0);
								bool: _DECL_51 =
									if _DECL_48 then
										let { bool: _DECL_49 = true } in _DECL_49
									else
										let { bool: _DECL_50 = false } in _DECL_50
									endif;
							} in _DECL_51
						else
							let { bool: _DECL_52 = false } in _DECL_52
						endif;
					set of int: _DECL_86 =
						if _DECL_53 then
							let {
								int: _DECL_54 = '-'(infinity);
								int: _DECL_55 = infinity;
								set of int: _DECL_56 = range_internal_int(_DECL_54, _DECL_55);
							} in _DECL_56
						else
							let {
								int: _DECL_2 = lb(y);
								bool: _DECL_3 = '<'(_DECL_2, 0);
								set of int: _DECL_21 =
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
											int: _DECL_13 =
												'shackle_min<int, int>'(_DECL_11, _DECL_12);
											int: _DECL_14 = div_internal_int(_DECL_10, _DECL_13);
											int: _DECL_15 = ub(x);
											int: _DECL_16 = ub(y);
											int: _DECL_17 = '-'(1);
											int: _DECL_18 =
												'shackle_min<int, int>'(_DECL_16, _DECL_17);
											int: _DECL_19 = div_internal_int(_DECL_15, _DECL_18);
											set of int: _DECL_20 = {
												_DECL_6,
												_DECL_9,
												_DECL_14,
												_DECL_19,
											};
										} in _DECL_20
									else
										let { set of int: _DECL_1 = {} } in _DECL_1
									endif;
								int: _DECL_23 = ub(y);
								int: _DECL_24 = 0;
								bool: _DECL_25 = '<'(_DECL_24, _DECL_23);
								set of int: _DECL_41 =
									if _DECL_25 then
										let {
											int: _DECL_26 = lb(x);
											int: _DECL_27 = lb(y);
											int: _DECL_28 = 'shackle_max<int, int>'(_DECL_27, 1);
											int: _DECL_29 = div_internal_int(_DECL_26, _DECL_28);
											int: _DECL_30 = ub(x);
											int: _DECL_31 = lb(y);
											int: _DECL_32 = 'shackle_max<int, int>'(_DECL_31, 1);
											int: _DECL_33 = div_internal_int(_DECL_30, _DECL_32);
											int: _DECL_34 = lb(x);
											int: _DECL_35 = ub(y);
											int: _DECL_36 = div_internal_int(_DECL_34, _DECL_35);
											int: _DECL_37 = ub(x);
											int: _DECL_38 = ub(y);
											int: _DECL_39 = div_internal_int(_DECL_37, _DECL_38);
											set of int: _DECL_40 = {
												_DECL_29,
												_DECL_33,
												_DECL_36,
												_DECL_39,
											};
										} in _DECL_40
									else
										let { set of int: _DECL_22 = {} } in _DECL_22
									endif;
								set of int: candidates = union_internal_int(_DECL_21, _DECL_41);
								int: _DECL_43 = 'shackle_min_root<set of int>'(candidates);
								int: _DECL_44 = 'shackle_max_root<set of int>'(candidates);
								set of int: _DECL_57 = range_internal_int(_DECL_43, _DECL_44);
							} in _DECL_57
						endif;
				} in _DECL_86
			endif;
	} in _DECL_87;
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
		var int: _DECL_8 =
			if _DECL_3 then
				let {
					int: _DECL_4 = fix(x);
					int: _DECL_5 = fix(y);
					int: _DECL_6 = 'shackle_div_root<int, int>'(_DECL_4, _DECL_5);
				} in _DECL_6
			else
				let { var int: _DECL_7 = shackle_div_t(x, y) } in _DECL_7
			endif;
	} in _DECL_8;
function bool: 'shackle_occurs<int>'(int: x) :: promise_total =
	let { bool: _DECL_1 = true } in _DECL_1;
function bool: 'shackle_occurs<opt int>'(tuple(bool, int): x) :: promise_total =
	let {
		bool: _DECL_3 =
			if x.1 then
				let { bool: _DECL_1 = 'shackle_occurs<int>'(x.2) } in _DECL_1
			else
				let { bool: _DECL_2 = x.1 } in _DECL_2
			endif;
	} in _DECL_3;
function bool: 'shackle_absent<int>'(int: x) :: promise_total =
	let { bool: _DECL_1 = false } in _DECL_1;
function bool: 'shackle_absent<opt int>'(tuple(bool, int): x) :: promise_total =
	let {
		bool: _DECL_6 =
			if x.1 then
				let { bool: _DECL_4 = 'shackle_absent<int>'(x.2) } in _DECL_4
			else
				let {
					bool: _DECL_1 = 'shackle_occurs<opt int>'(x);
					bool: _DECL_5 =
						if _DECL_1 then
							let { bool: _DECL_2 = false } in _DECL_2
						else
							let { bool: _DECL_3 = true } in _DECL_3
						endif;
				} in _DECL_5
			endif;
	} in _DECL_6;
function int: 'shackle_deopt<int>'(int: x) :: promise_total = x;
function int: 'shackle_deopt_root<opt int>'(tuple(bool, int): x) :: promise_total =
	let {
		int: _DECL_5 =
			if x.1 then
				let { int: _DECL_4 = 'shackle_deopt<int>'(x.2) } in _DECL_4
			else
				let {
					tuple(bool, int): _DECL_1 = x;
					bool: bx = _DECL_1.1;
					int: dx = _DECL_1.2;
					constraint bx;
				} in dx
			endif;
	} in _DECL_5;
function tuple(bool, int): 'shackle_deopt<opt int>'(tuple(bool, int): x) :: promise_total =
	let {
		tuple(
			bool,
			int,
		): _DECL_9 =
			if x.1 then
				let {
					int: _DECL_6 = 'shackle_deopt<int>'(x.2);
					tuple(bool, int): _DECL_7 = (true, _DECL_6);
				} in _DECL_7
			else
				let {
					tuple(bool, int): _DECL_1 = x;
					bool: bx = _DECL_1.1;
					int: dx = _DECL_1.2;
					bool: _DECL_4 = bx;
					bool: _DECL_5 = 'shackle_forall<array [int] of bool>'([_DECL_4]);
					tuple(bool, int): _DECL_8 = (_DECL_5, dx);
				} in _DECL_8
			endif;
	} in _DECL_9;
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
		string: _DECL_11 = '++'(_DECL_8, _DECL_10);
	} in _DECL_11;
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
		var int: _DECL_2 = 'shackle_+<var int, var int>'(_DECL_1, 1);
	} in _DECL_2;
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
bool: _DECL_17 = 'shackle_absent<opt int>'(shackle_mzn_min_version_required);
bool: _DECL_20 =
	if _DECL_17 then
		let { bool: _DECL_18 = true } in _DECL_18
	else
		let {
			tuple(bool, int): _DECL_10 =
				 'shackle_deopt<opt int>'(shackle_mzn_min_version_required);
			bool: _DECL_11 = _DECL_10.1;
			int: _DECL_12 = mzn_compiler_version();
			bool: _DECL_13 = 'shackle_<=<int, int>'(_DECL_10.2, _DECL_12);
			bool: _DECL_14 = 'shackle_forall<array [int] of bool>'([_DECL_11, _DECL_13]);
			bool: _DECL_19 =
				if _DECL_14 then
					let { bool: _DECL_15 = true } in _DECL_15
				else
					let { bool: _DECL_16 = false } in _DECL_16
				endif;
		} in _DECL_19
	endif;
constraint
	if _DECL_20 then
		let { bool: _DECL_21 = true } in _DECL_21
	else
		let {
			int: _DECL_3 = 'shackle_deopt_root<opt int>'(shackle_mzn_min_version_required);
			string: _DECL_4 = shackle_mzn_version_to_string_root(_DECL_3);
			string: _DECL_5 = '++'("This model requires MiniZinc version ", _DECL_4);
			string: _DECL_6 = '++'(_DECL_5, " but you are running version ");
			int: _DECL_7 = mzn_compiler_version();
			string: _DECL_8 = shackle_mzn_version_to_string_root(_DECL_7);
			string: _DECL_9 = '++'(_DECL_6, _DECL_8);
			bool: _DECL_22 = abort(_DECL_9);
		} in _DECL_22
	endif;
bool: _DECL_38 = 'shackle_absent<opt int>'(shackle_mzn_max_version_required);
bool: _DECL_41 =
	if _DECL_38 then
		let { bool: _DECL_39 = true } in _DECL_39
	else
		let {
			tuple(bool, int): _DECL_30 =
				 'shackle_deopt<opt int>'(shackle_mzn_max_version_required);
			int: _DECL_31 = _DECL_30.2;
			int: _DECL_32 = mzn_compiler_version();
			bool: _DECL_33 = _DECL_30.1;
			bool: _DECL_34 = 'shackle_<=<int, int>'(_DECL_32, _DECL_31);
			bool: _DECL_35 = 'shackle_forall<array [int] of bool>'([_DECL_33, _DECL_34]);
			bool: _DECL_40 =
				if _DECL_35 then
					let { bool: _DECL_36 = true } in _DECL_36
				else
					let { bool: _DECL_37 = false } in _DECL_37
				endif;
		} in _DECL_40
	endif;
constraint
	if _DECL_41 then
		let { bool: _DECL_42 = true } in _DECL_42
	else
		let {
			int: _DECL_23 = 'shackle_deopt_root<opt int>'(shackle_mzn_max_version_required);
			string: _DECL_24 = shackle_mzn_version_to_string_root(_DECL_23);
			string: _DECL_25 = '++'("This model only works with MiniZinc up to version ", _DECL_24);
			string: _DECL_26 = '++'(_DECL_25, " but you are running version ");
			int: _DECL_27 = mzn_compiler_version();
			string: _DECL_28 = shackle_mzn_version_to_string_root(_DECL_27);
			string: _DECL_29 = '++'(_DECL_26, _DECL_28);
			bool: _DECL_43 = abort(_DECL_29);
		} in _DECL_43
	endif;
bool: shackle_debug_mode = mzn_internal_check_debug_mode();
var bool: p = true;
constraint p;
var bool: q = true;
int: _DECL_47 = 1;
int: _DECL_48 = 3;
set of int: _DECL_49 = range_internal_int(_DECL_47, _DECL_48);
var int: x :: shackle_output = (let { var _DECL_49: _FRESH_VAR } in _FRESH_VAR);
var int: _DECL_51 = foo_root(x);
constraint 'shackle_=<var int, var int>'(x, _DECL_51);
_GLOBAL_10 = 'shackle_+<var int, var int>'(x, 2);
solve maximize _GLOBAL_10;
