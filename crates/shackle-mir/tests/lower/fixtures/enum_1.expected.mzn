annotation 'output';
annotation mzn_internal_generated;
annotation mzn_forward_annotations;
annotation mzn_no_remove;
annotation promise_total;
annotation promise_commutative;
annotation domain;
annotation mzn_internal_representation;
annotation bounds;
function set of int: 'index_set<array [int] of tuple(string, array [int] of tuple(int, set of int))>'(
	array [int] of tuple(string, array [int] of tuple(int, set of int)): x,
) :: promise_total = let { tuple(set of int): _DECL_1 = index_sets(x) } in _DECL_1.1;
function set of int: 'index_set<array [int] of tuple(string, array [int] of tuple(int, set of int), int)>'(
	array [int] of tuple(string, array [int] of tuple(int, set of int), int): x,
) :: promise_total = let { tuple(set of int): _DECL_1 = index_sets(x) } in _DECL_1.1;
function set of int: 'index_set<array [int] of tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int))>'(
	array [int] of tuple(
		int,
		array [int] of tuple(string, array [int] of tuple(int, set of int), int),
	): x,
) :: promise_total = let { tuple(set of int): _DECL_1 = index_sets(x) } in _DECL_1.1;
function set of int: 'index_set<array [int] of bool>'(array [int] of bool: x) :: promise_total =
	let { tuple(set of int): _DECL_1 = index_sets(x) } in _DECL_1.1;
function set of int: 'index_set<array [int] of tuple(int, set of int)>'(
	array [int] of tuple(int, set of int): x,
) :: promise_total = let { tuple(set of int): _DECL_1 = index_sets(x) } in _DECL_1.1;
function set of int: 'index_set<array [int] of int>'(array [int] of int: x) :: promise_total =
	let { tuple(set of int): _DECL_1 = index_sets(x) } in _DECL_1.1;
function array [int] of bool: 'array1d<array [int] of bool>'(array [int] of bool: x)
	:: promise_total =
	let {
		int: _DECL_1 = 1;
		int: _DECL_2 = length(x);
		set of int: _DECL_3 = range_internal_int(_DECL_1, _DECL_2);
	} in mzn_array_kd((_DECL_3,), x);
function array [int] of string: 'array1d<array [int] of string>'(array [int] of string: x)
	:: promise_total =
	let {
		int: _DECL_1 = 1;
		int: _DECL_2 = length(x);
		set of int: _DECL_3 = range_internal_int(_DECL_1, _DECL_2);
	} in mzn_array_kd((_DECL_3,), x);
function array [int] of int: 'array1d<array [int] of int>'(array [int] of int: x) :: promise_total =
	let {
		int: _DECL_1 = 1;
		int: _DECL_2 = length(x);
		set of int: _DECL_3 = range_internal_int(_DECL_1, _DECL_2);
	} in mzn_array_kd((_DECL_3,), x);
function bool: '<=<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_2 = '<'(x, y);
	} in if _DECL_2 then
		true
	else
		let { bool: _DECL_1 = '='(x, y) } in if _DECL_1 then true else false endif
	endif;
function array [int] of string: '++<array [int] of string, array [int] of string>'(
	array [int] of string: x,
	array [int] of string: y,
) :: promise_total =
	[
		_DECL_13 |
			xx = 'array1d<array [int] of string>'(x) :: mzn_forward_annotations,
			yy = 'array1d<array [int] of string>'(y) :: mzn_forward_annotations,
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
function string: concat(array [int] of string: s) :: promise_total =
	let {
		int: _DECL_13 = length(s);
		bool: _DECL_14 = '='(_DECL_13, 0);
	} in if _DECL_14 then
		""
	else
		let {
			int: _DECL_10 = length(s);
			bool: _DECL_11 = '='(_DECL_10, 1);
		} in if _DECL_11 then
			let {
				array [int] of string: _DECL_12 = 'array1d<array [int] of string>'(s);
			} in mzn_element_internal(_DECL_12, 1)
		else
			let {
				array [int] of string: xx = 'array1d<array [int] of string>'(s);
				string: _DECL_2 = mzn_element_internal(xx, 1);
				string: _DECL_3 = mzn_element_internal(xx, 2);
				string: _DECL_4 = '++'(_DECL_2, _DECL_3);
				int: _DECL_5 = 3;
				int: _DECL_6 = length(xx);
				set of int: _DECL_7 = range_internal_int(_DECL_5, _DECL_6);
				array [int] of string: _DECL_8 = mzn_slice_internal(xx, [_DECL_7]);
				array [int] of string: _DECL_9 =
					'++<array [int] of string, array [int] of string>'([_DECL_4], _DECL_8);
			} in concat(_DECL_9) :: mzn_forward_annotations
		endif
	endif;
function array [int] of tuple(
	string,
	array [int] of tuple(int, set of int),
): 'array1d<set of int, array [int] of tuple(string, array [int] of tuple(int, set of int))>'(
	set of int: S,
	array [int] of tuple(string, array [int] of tuple(int, set of int)): x,
)
	:: promise_total =
	let {
		int: expected = card(S);
		int: actual = length(x);
		bool: _DECL_6 = '='(expected, actual);
		constraint
			if _DECL_6 then
				mzn_root
			else
				let {
					string: _DECL_3 = show_internal(actual);
					string: _DECL_4 = show_internal(expected);
					string: _DECL_5 =
						concat([
							"mismatch in array dimensions: the array contains ",
							_DECL_3,
							" elements, but ",
							_DECL_4,
							" elements were expected according to the given index set.",
						]);
					constraint abort(_DECL_5);
				} in mzn_root
			endif;
	} in mzn_array_kd((S,), x);
function bool: 'exists<array [int] of bool>'(array [int] of bool: x) :: promise_total =
	let {
		array [int] of bool: _DECL_2 = [x_i | x_i in x, _ = 1 where x_i];
		int: _DECL_3 = length(_DECL_2);
		int: _DECL_4 = 0;
	} in '<'(_DECL_4, _DECL_3);
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
function int: mod_internal(int: x, int: y) :: promise_total =
	let {
		int: _DECL_1 = div_internal_int(x, y);
		int: _DECL_2 = '*'(y, _DECL_1);
	} in '-'(x, _DECL_2);
function array [int] of tuple(int, int): 'mzn_set_to_ranges<set of int>'(set of int: S)
	:: promise_total =
	let {
		array [int] of int: ranges = set_to_ranges_internal_int(S);
	} in [
		(_DECL_6, _DECL_8) |
			_DECL_2 = 'index_set<array [int] of int>'(ranges) :: mzn_forward_annotations,
			i in _DECL_2,
			_DECL_4 = mod_internal(i, 2),
			_DECL_5 = '='(_DECL_4, 1),
			_ = 1 where _DECL_5,
			_DECL_6 = mzn_element_internal(ranges, i),
			_DECL_7 = '+'(i, 1),
			_DECL_8 = mzn_element_internal(ranges, _DECL_7),
	];
function bool: 'in<int, set of int>'(int: x, set of int: y) :: promise_total =
	let {
		array [int] of bool: _DECL_11 = [
			_DECL_10 |
				_DECL_1 = 0,
				_DECL_2 = [],
				_DECL_3 = 'forall<array [int] of bool>'(_DECL_2),
				_ = 1 where _DECL_3,
				_DECL_4 = 'mzn_set_to_ranges<set of int>'(y),
				_DECL_5 in _DECL_4,
				a = _DECL_5.1,
				b = _DECL_5.2,
				_DECL_8 = '<=<int, int>'(a, x),
				_DECL_10 =
					if _DECL_8 then
						let {
							bool: _DECL_9 = '<=<int, int>'(x, b);
						} in if _DECL_9 then true else false endif
					else
						false
					endif,
		];
	} in 'exists<array [int] of bool>'(_DECL_11) :: mzn_forward_annotations;
function bool: '!=<int, int>'(int: x, int: y) :: promise_total =
	let { bool: _DECL_1 = '='(x, y) } in if _DECL_1 then false else true endif;
function string: join(string: d, array [int] of string: s) :: promise_total =
	let {
		int: _DECL_14 = length(s);
		bool: _DECL_15 = '='(_DECL_14, 0);
	} in if _DECL_15 then
		""
	else
		let {
			array [int] of string: _DECL_13 = [
				_DECL_12 |
					xx = 'array1d<array [int] of string>'(s),
					_DECL_2 = 1,
					_DECL_3 = length(s),
					_DECL_4 = '*'(_DECL_3, 2),
					_DECL_5 = '-'(_DECL_4, 1),
					_DECL_6 = range_internal_int(_DECL_2, _DECL_5),
					i in _DECL_6,
					_DECL_8 = mod_internal(i, 2),
					_DECL_9 = '='(_DECL_8, 1),
					_DECL_12 =
						if _DECL_9 then
							let {
								int: _DECL_10 = div_internal_int(i, 2);
								int: _DECL_11 = '+'(_DECL_10, 1);
							} in mzn_element_internal(xx, _DECL_11)
						else
							d
						endif,
			];
		} in concat(_DECL_13) :: mzn_forward_annotations
	endif;
function tuple(bool, string): 'show<set of int>'(set of int: x) :: promise_total =
	let {
		array [int] of int: ranges = set_to_ranges_internal_int(x);
		int: _DECL_8 = length(ranges);
		bool: _DECL_9 = '='(_DECL_8, 2);
		bool: _DECL_34 =
			if _DECL_9 then
				let {
					int: _DECL_10 = 1;
					bool: _DECL_11 =
						'mzn_array_access_valid<array [int] of int, int>'(ranges, _DECL_10);
					bool: _DECL_12 = 'forall<array [int] of bool>'([_DECL_11]);
					bool: _DECL_13 = 'forall<array [int] of bool>'([_DECL_11]);
					int: _DECL_14 =
						if _DECL_13 then mzn_element_internal(ranges, _DECL_10) else 0 endif;
					tuple(bool, int): _DECL_15 = (_DECL_12, _DECL_14);
					bool: _DECL_16 = _DECL_15.1;
					bool: _DECL_17 = 'forall<array [int] of bool>'([_DECL_16]);
					int: _DECL_18 = _DECL_15.2;
					tuple(bool, int): _DECL_19 = (_DECL_17, _DECL_18);
					int: _DECL_20 = 2;
					bool: _DECL_21 =
						'mzn_array_access_valid<array [int] of int, int>'(ranges, _DECL_20);
					bool: _DECL_22 = 'forall<array [int] of bool>'([_DECL_21]);
					bool: _DECL_23 = 'forall<array [int] of bool>'([_DECL_21]);
					int: _DECL_24 =
						if _DECL_23 then mzn_element_internal(ranges, _DECL_20) else 0 endif;
					tuple(bool, int): _DECL_25 = (_DECL_22, _DECL_24);
					bool: _DECL_26 = _DECL_25.1;
					bool: _DECL_27 = 'forall<array [int] of bool>'([_DECL_26]);
					int: _DECL_28 = _DECL_25.2;
					tuple(bool, int): _DECL_29 = (_DECL_27, _DECL_28);
					bool: _DECL_30 = _DECL_19.1;
					bool: _DECL_31 = _DECL_29.1;
					bool: _DECL_32 = '!=<int, int>'(_DECL_19.2, _DECL_29.2);
					bool: _DECL_33 = 'forall<array [int] of bool>'([_DECL_30, _DECL_31, _DECL_32]);
				} in if _DECL_33 then true else false endif
			else
				false
			endif;
		tuple(
			bool,
			string,
		): _DECL_77 =
			if _DECL_34 then
				let {
					int: _DECL_35 = 1;
					bool: _DECL_36 =
						'mzn_array_access_valid<array [int] of int, int>'(ranges, _DECL_35);
					bool: _DECL_37 = 'forall<array [int] of bool>'([_DECL_36]);
					bool: _DECL_38 = 'forall<array [int] of bool>'([_DECL_36]);
					int: _DECL_39 =
						if _DECL_38 then mzn_element_internal(ranges, _DECL_35) else 0 endif;
					tuple(bool, int): _DECL_40 = (_DECL_37, _DECL_39);
					bool: _DECL_41 = _DECL_40.1;
					bool: _DECL_42 = 'forall<array [int] of bool>'([_DECL_41]);
					int: _DECL_43 = _DECL_40.2;
					tuple(bool, int): _DECL_44 = (_DECL_42, _DECL_43);
					int: _DECL_45 = _DECL_44.2;
					bool: _DECL_46 = _DECL_44.1;
					bool: _DECL_47 = 'forall<array [int] of bool>'([_DECL_46]);
					bool: _DECL_48 = _DECL_44.1;
					bool: _DECL_49 = 'forall<array [int] of bool>'([_DECL_48]);
					string: _DECL_50 = if _DECL_49 then show_internal(_DECL_45) else "" endif;
					tuple(bool, string): _DECL_51 = (_DECL_47, _DECL_50);
					bool: _DECL_52 = _DECL_51.1;
					bool: _DECL_53 = 'forall<array [int] of bool>'([_DECL_52]);
					string: _DECL_54 = '++'(_DECL_51.2, "..");
					tuple(bool, string): _DECL_55 = (_DECL_53, _DECL_54);
					int: _DECL_56 = 2;
					bool: _DECL_57 =
						'mzn_array_access_valid<array [int] of int, int>'(ranges, _DECL_56);
					bool: _DECL_58 = 'forall<array [int] of bool>'([_DECL_57]);
					bool: _DECL_59 = 'forall<array [int] of bool>'([_DECL_57]);
					int: _DECL_60 =
						if _DECL_59 then mzn_element_internal(ranges, _DECL_56) else 0 endif;
					tuple(bool, int): _DECL_61 = (_DECL_58, _DECL_60);
					bool: _DECL_62 = _DECL_61.1;
					bool: _DECL_63 = 'forall<array [int] of bool>'([_DECL_62]);
					int: _DECL_64 = _DECL_61.2;
					tuple(bool, int): _DECL_65 = (_DECL_63, _DECL_64);
					int: _DECL_66 = _DECL_65.2;
					bool: _DECL_67 = _DECL_65.1;
					bool: _DECL_68 = 'forall<array [int] of bool>'([_DECL_67]);
					bool: _DECL_69 = _DECL_65.1;
					bool: _DECL_70 = 'forall<array [int] of bool>'([_DECL_69]);
					string: _DECL_71 = if _DECL_70 then show_internal(_DECL_66) else "" endif;
					tuple(bool, string): _DECL_72 = (_DECL_68, _DECL_71);
					bool: _DECL_73 = _DECL_55.1;
					bool: _DECL_74 = _DECL_72.1;
					bool: _DECL_75 = 'forall<array [int] of bool>'([_DECL_73, _DECL_74]);
					string: _DECL_76 = '++'(_DECL_55.2, _DECL_72.2);
				} in (_DECL_75, _DECL_76)
			else
				let {
					array [int] of string: _DECL_4 = [
						_DECL_3 |
							x_i in x,
							_DECL_3 = show_internal(x_i),
					];
					string: _DECL_5 = join(", ", _DECL_4);
					string: _DECL_6 = '++'("{", _DECL_5);
					string: _DECL_7 = '++'(_DECL_6, "}");
				} in (true, _DECL_7)
			endif;
		bool: _DECL_78 = _DECL_77.1;
		bool: _DECL_79 = 'forall<array [int] of bool>'([_DECL_78]);
		string: _DECL_80 = _DECL_77.2;
	} in (_DECL_79, _DECL_80);
function bool: 'mzn_array_access_valid<int, set of int, int>'(int: idx, set of int: idx_set, int: n)
	:: promise_total =
	let {
		bool: _DECL_35 = 'in<int, set of int>'(idx, idx_set) :: mzn_forward_annotations;
	} in if _DECL_35 then
		true
	else
		let {
			int: _DECL_13 = 0;
			bool: _DECL_14 = '<'(_DECL_13, n);
			tuple(bool, string): _DECL_29 =
				if _DECL_14 then
					let {
						string: _DECL_15 = "array access out of bounds, index set ";
						string: _DECL_16 = show_internal(n);
						string: _DECL_17 = " is ";
						tuple(bool, string): _DECL_18 = 'show<set of int>'(idx_set);
						string: _DECL_19 = ", but given index is ";
						string: _DECL_20 = show_internal(idx);
						bool: _DECL_21 = _DECL_18.1;
						bool: _DECL_22 = 'forall<array [int] of bool>'([_DECL_21]);
						string: _DECL_23 = _DECL_18.2;
						array [int] of string: _DECL_24 = [
							_DECL_15,
							_DECL_16,
							_DECL_17,
							_DECL_23,
							_DECL_19,
							_DECL_20,
						];
						tuple(bool, array [int] of string): _DECL_25 = (_DECL_22, _DECL_24);
						bool: _DECL_26 = _DECL_25.1;
						bool: _DECL_27 = 'forall<array [int] of bool>'([_DECL_26]);
						string: _DECL_28 = concat(_DECL_25.2);
					} in (_DECL_27, _DECL_28)
				else
					let {
						string: _DECL_1 = "array access out of bounds, index set is ";
						tuple(bool, string): _DECL_2 = 'show<set of int>'(idx_set);
						string: _DECL_3 = ", but given index is ";
						string: _DECL_4 = show_internal(idx);
						bool: _DECL_5 = _DECL_2.1;
						bool: _DECL_6 = 'forall<array [int] of bool>'([_DECL_5]);
						string: _DECL_7 = _DECL_2.2;
						array [int] of string: _DECL_8 = [_DECL_1, _DECL_7, _DECL_3, _DECL_4];
						tuple(bool, array [int] of string): _DECL_9 = (_DECL_6, _DECL_8);
						bool: _DECL_10 = _DECL_9.1;
						bool: _DECL_11 = 'forall<array [int] of bool>'([_DECL_10]);
						string: _DECL_12 = concat(_DECL_9.2);
					} in (_DECL_11, _DECL_12)
				endif;
			bool: _DECL_30 = _DECL_29.1;
			bool: _DECL_31 = mzn_add_warning(_DECL_29.2);
			bool: _DECL_32 = 'forall<array [int] of bool>'([_DECL_30, _DECL_31]);
			bool: _DECL_33 = 'forall<array [int] of bool>'([_DECL_32]);
			bool: _DECL_34 = if _DECL_33 then false else false endif;
		} in 'forall<array [int] of bool>'([_DECL_32, _DECL_34])
	endif;
function bool: 'mzn_array_access_valid<array [int] of int, int>'(array [int] of int: x, int: idx)
	:: promise_total =
	let {
		set of int: _DECL_1 = 'index_set<array [int] of int>'(x);
	} in 'mzn_array_access_valid<int, set of int, int>'(idx, _DECL_1, 0) :: mzn_forward_annotations;
function bool: 'mzn_array_access_valid_root<array [int] of int, int>'(
	array [int] of int: x,
	int: idx,
)
	:: promise_total =
	let {
		set of int: _DECL_1 = 'index_set<array [int] of int>'(x);
		constraint
			'mzn_array_access_valid_root<int, set of int, int>'(idx, _DECL_1, 0)
				:: mzn_forward_annotations;
	} in mzn_root;
function string: 'show_root<set of int>'(set of int: x) :: promise_total =
	let {
		array [int] of int: ranges = set_to_ranges_internal_int(x);
		int: _DECL_7 = length(ranges);
		bool: _DECL_8 = '='(_DECL_7, 2);
		bool: _DECL_33 =
			if _DECL_8 then
				let {
					int: _DECL_9 = 1;
					bool: _DECL_10 =
						'mzn_array_access_valid<array [int] of int, int>'(ranges, _DECL_9);
					bool: _DECL_11 = 'forall<array [int] of bool>'([_DECL_10]);
					bool: _DECL_12 = 'forall<array [int] of bool>'([_DECL_10]);
					int: _DECL_13 =
						if _DECL_12 then mzn_element_internal(ranges, _DECL_9) else 0 endif;
					tuple(bool, int): _DECL_14 = (_DECL_11, _DECL_13);
					bool: _DECL_15 = _DECL_14.1;
					bool: _DECL_16 = 'forall<array [int] of bool>'([_DECL_15]);
					int: _DECL_17 = _DECL_14.2;
					tuple(bool, int): _DECL_18 = (_DECL_16, _DECL_17);
					int: _DECL_19 = 2;
					bool: _DECL_20 =
						'mzn_array_access_valid<array [int] of int, int>'(ranges, _DECL_19);
					bool: _DECL_21 = 'forall<array [int] of bool>'([_DECL_20]);
					bool: _DECL_22 = 'forall<array [int] of bool>'([_DECL_20]);
					int: _DECL_23 =
						if _DECL_22 then mzn_element_internal(ranges, _DECL_19) else 0 endif;
					tuple(bool, int): _DECL_24 = (_DECL_21, _DECL_23);
					bool: _DECL_25 = _DECL_24.1;
					bool: _DECL_26 = 'forall<array [int] of bool>'([_DECL_25]);
					int: _DECL_27 = _DECL_24.2;
					tuple(bool, int): _DECL_28 = (_DECL_26, _DECL_27);
					bool: _DECL_29 = _DECL_18.1;
					bool: _DECL_30 = _DECL_28.1;
					bool: _DECL_31 = '!=<int, int>'(_DECL_18.2, _DECL_28.2);
					bool: _DECL_32 = 'forall<array [int] of bool>'([_DECL_29, _DECL_30, _DECL_31]);
				} in if _DECL_32 then true else false endif
			else
				false
			endif;
	} in if _DECL_33 then
		let {
			int: _DECL_34 = 1;
			constraint 'mzn_array_access_valid_root<array [int] of int, int>'(ranges, _DECL_34);
			int: _DECL_35 = mzn_element_internal(ranges, _DECL_34);
			string: _DECL_36 = show_internal(_DECL_35);
			string: _DECL_37 = '++'(_DECL_36, "..");
			int: _DECL_38 = 2;
			constraint 'mzn_array_access_valid_root<array [int] of int, int>'(ranges, _DECL_38);
			int: _DECL_39 = mzn_element_internal(ranges, _DECL_38);
			string: _DECL_40 = show_internal(_DECL_39);
		} in '++'(_DECL_37, _DECL_40)
	else
		let {
			array [int] of string: _DECL_4 = [_DECL_3 | x_i in x, _DECL_3 = show_internal(x_i)];
			string: _DECL_5 = join(", ", _DECL_4);
			string: _DECL_6 = '++'("{", _DECL_5);
		} in '++'(_DECL_6, "}")
	endif;
function bool: 'mzn_array_access_valid_root<int, set of int, int>'(
	int: idx,
	set of int: idx_set,
	int: n,
) :: promise_total =
	let {
		bool: _DECL_9 = 'in<int, set of int>'(idx, idx_set) :: mzn_forward_annotations;
		constraint
			if _DECL_9 then
				mzn_root
			else
				let {
					int: _DECL_3 = 0;
					bool: _DECL_4 = '<'(_DECL_3, n);
					string: _DECL_8 =
						if _DECL_4 then
							let {
								string: _DECL_5 = show_internal(n);
								string: _DECL_6 = 'show_root<set of int>'(idx_set);
								string: _DECL_7 = show_internal(idx);
							} in concat([
								"array access out of bounds, index set ",
								_DECL_5,
								" is ",
								_DECL_6,
								", but given index is ",
								_DECL_7,
							])
						else
							let {
								string: _DECL_1 = 'show_root<set of int>'(idx_set);
								string: _DECL_2 = show_internal(idx);
							} in concat([
								"array access out of bounds, index set is ",
								_DECL_1,
								", but given index is ",
								_DECL_2,
							])
						endif;
					constraint mzn_add_warning(_DECL_8);
					constraint false;
					constraint true;
				} in mzn_root
			endif;
	} in mzn_root;
function bool: 'mzn_array_access_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int)), int>'(
	array [int] of tuple(string, array [int] of tuple(int, set of int)): x,
	int: idx,
) :: promise_total =
	let {
		set of int: _DECL_1 =
			'index_set<array [int] of tuple(string, array [int] of tuple(int, set of int))>'(x);
		constraint
			'mzn_array_access_valid_root<int, set of int, int>'(idx, _DECL_1, 0)
				:: mzn_forward_annotations;
	} in mzn_root;
function bool: 'mzn_array_access_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int), int), int>'(
	array [int] of tuple(string, array [int] of tuple(int, set of int), int): x,
	int: idx,
) :: promise_total =
	let {
		set of int: _DECL_1 =
			'index_set<array [int] of tuple(string, array [int] of tuple(int, set of int), int)>'(
				x,
			);
		constraint
			'mzn_array_access_valid_root<int, set of int, int>'(idx, _DECL_1, 0)
				:: mzn_forward_annotations;
	} in mzn_root;
function bool: 'mzn_array_access_valid_root<array [int] of tuple(int, set of int), int>'(
	array [int] of tuple(int, set of int): x,
	int: idx,
) :: promise_total =
	let {
		set of int: _DECL_1 = 'index_set<array [int] of tuple(int, set of int)>'(x);
		constraint
			'mzn_array_access_valid_root<int, set of int, int>'(idx, _DECL_1, 0)
				:: mzn_forward_annotations;
	} in mzn_root;
function bool: 'mzn_array_access_valid<array [int] of tuple(int, set of int), int>'(
	array [int] of tuple(int, set of int): x,
	int: idx,
) :: promise_total =
	let {
		set of int: _DECL_1 = 'index_set<array [int] of tuple(int, set of int)>'(x);
	} in 'mzn_array_access_valid<int, set of int, int>'(idx, _DECL_1, 0) :: mzn_forward_annotations;
function bool: 'mzn_array_access_valid_root<array [int] of tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)), int>'(
	array [int] of tuple(
		int,
		array [int] of tuple(string, array [int] of tuple(int, set of int), int),
	): x,
	int: idx,
) :: promise_total =
	let {
		set of int: _DECL_1 =
			'index_set<array [int] of tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int))>'(
				x,
			);
		constraint
			'mzn_array_access_valid_root<int, set of int, int>'(idx, _DECL_1, 0)
				:: mzn_forward_annotations;
	} in mzn_root;
function bool: 'mzn_array_access_valid<array [int] of bool, int>'(array [int] of bool: x, int: idx)
	:: promise_total =
	let {
		set of int: _DECL_1 = 'index_set<array [int] of bool>'(x);
	} in 'mzn_array_access_valid<int, set of int, int>'(idx, _DECL_1, 0) :: mzn_forward_annotations;
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
function int: 'product<array [int] of int>'(array [int] of int: x) :: promise_total =
	let {
		int: _DECL_13 = length(x);
		bool: _DECL_14 = '='(_DECL_13, 0);
	} in if _DECL_14 then
		1
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
				int: _DECL_4 = '*'(_DECL_2, _DECL_3);
				int: _DECL_5 = 3;
				int: _DECL_6 = length(xx);
				set of int: _DECL_7 = range_internal_int(_DECL_5, _DECL_6);
				array [int] of int: _DECL_8 = mzn_slice_internal(xx, [_DECL_7]);
				array [int] of int: _DECL_9 =
					'++<array [int] of int, array [int] of int>'([_DECL_4], _DECL_8);
			} in 'product<array [int] of int>'(_DECL_9) :: mzn_forward_annotations
		endif
	endif;
function int: 'min_internal_set<set of int>'(set of int: s) :: promise_total =
	let { array [int] of int: _DECL_1 = set2array(s) } in mzn_element_internal(_DECL_1, 1);
function int: 'min_root<set of int>'(
	set of int: s,
)
	:: promise_total =
	let {
		int: _DECL_1 = card(s);
		int: _DECL_2 = 0;
		bool: _DECL_3 = '<'(_DECL_2, _DECL_1);
		constraint
			if _DECL_3 then
				mzn_root
			else
				let {
					constraint mzn_add_warning("minimum of an empty set is undefined");
					constraint false;
					constraint true;
				} in mzn_root
			endif;
	} in 'min_internal_set<set of int>'(s) :: mzn_forward_annotations;
function bool: 'subset<set of int, set of int>'(set of int: x, set of int: y) :: promise_total =
	let {
		array [int] of tuple(int, int): xr = 'mzn_set_to_ranges<set of int>'(x);
		array [int] of tuple(int, int): yr = 'mzn_set_to_ranges<set of int>'(y);
		array [int] of bool: _DECL_12 = [
			_DECL_11 |
				x_i in xr,
				_DECL_10 =
					[
						_DECL_9 |
							y_j in yr,
							_DECL_5 = x_i.1,
							_DECL_6 = y_j.1,
							_DECL_7 = '<=<int, int>'(_DECL_6, _DECL_5),
							_DECL_9 =
								if _DECL_7 then
									let {
										bool: _DECL_8 = '<=<int, int>'(x_i.2, y_j.2);
									} in if _DECL_8 then true else false endif
								else
									false
								endif,
					],
				_DECL_11 = 'exists<array [int] of bool>'(_DECL_10) :: mzn_forward_annotations,
		];
	} in 'forall<array [int] of bool>'(_DECL_12) :: mzn_forward_annotations;
function bool: 'mzn_slice_valid_root<set of int, set of int, int>'(
	set of int: idx,
	set of int: idx_set,
	int: n,
)
	:: promise_total =
	let {
		int: _DECL_10 = card(idx);
		bool: _DECL_11 = '='(_DECL_10, 1);
		constraint
			if _DECL_11 then
				let {
					int: _DECL_12 = 'min_root<set of int>'(idx);
					constraint
						'mzn_array_access_valid_root<int, set of int, int>'(_DECL_12, idx_set, n)
							:: mzn_forward_annotations;
				} in mzn_root
			else
				let {
					bool: _DECL_9 =
						'subset<set of int, set of int>'(idx, idx_set) :: mzn_forward_annotations;
					constraint
						if _DECL_9 then
							mzn_root
						else
							let {
								int: _DECL_3 = 0;
								bool: _DECL_4 = '<'(_DECL_3, n);
								string: _DECL_8 =
									if _DECL_4 then
										let {
											string: _DECL_5 = show_internal(n);
											string: _DECL_6 = 'show_root<set of int>'(idx_set);
											string: _DECL_7 = 'show_root<set of int>'(idx);
										} in concat([
											"array slice out of bounds, index set ",
											_DECL_5,
											" is ",
											_DECL_6,
											", but given range is ",
											_DECL_7,
										])
									else
										let {
											string: _DECL_1 = 'show_root<set of int>'(idx_set);
											string: _DECL_2 = 'show_root<set of int>'(idx);
										} in concat([
											"array slice out of bounds, index set is ",
											_DECL_1,
											", but given range is ",
											_DECL_2,
										])
									endif;
								constraint mzn_add_warning(_DECL_8);
								constraint false;
								constraint true;
							} in mzn_root
						endif;
				} in mzn_root
			endif;
	} in mzn_root;
function bool: 'mzn_slice_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int)), set of int>'(
	array [int] of tuple(string, array [int] of tuple(int, set of int)): x,
	set of int: idx,
) :: promise_total =
	let {
		set of int: _DECL_1 =
			'index_set<array [int] of tuple(string, array [int] of tuple(int, set of int))>'(x);
		constraint
			'mzn_slice_valid_root<set of int, set of int, int>'(idx, _DECL_1, 0)
				:: mzn_forward_annotations;
	} in mzn_root;
function bool: 'mzn_slice_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int)), tuple(set of int)>'(
	array [int] of tuple(string, array [int] of tuple(int, set of int)): x,
	tuple(set of int): idx,
) :: promise_total =
	let {
		constraint
			'mzn_slice_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int)), set of int>'(
				x,
				idx.1,
			) :: mzn_forward_annotations;
	} in mzn_root;
function tuple(
	int,
	array [int] of tuple(string, array [int] of tuple(int, set of int), int),
): 'mzn_get_enum_root<array [int] of tuple(string, array [int] of tuple(int, set of int))>'(
	array [int] of tuple(string, array [int] of tuple(int, set of int)): e,
) :: promise_total =
	let {
		array [int] of int: _DECL_6 = [
			_DECL_5 |
				x in e,
				_DECL_4 = [_DECL_3 | y in x.2, _DECL_3 = card(y.2)],
				_DECL_5 = 'product<array [int] of int>'(_DECL_4),
		];
		int: _DECL_7 = 'sum<array [int] of int>'(_DECL_6) :: mzn_forward_annotations;
		array [int] of tuple(string, array [int] of tuple(int, set of int), int): _DECL_29 = [
			(name, params, offset) |
				_DECL_8 = 1,
				_DECL_9 = length(e),
				_DECL_10 = range_internal_int(_DECL_8, _DECL_9),
				i in _DECL_10,
				_ =
					let {
						constraint
							'mzn_array_access_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int)), int>'(
								e,
								i,
							);
					} in 1,
				_DECL_12 = mzn_element_internal(e, i),
				name = _DECL_12.1,
				params = _DECL_12.2,
				_DECL_27 =
					[
						_DECL_26 |
							_DECL_15 = 1,
							_DECL_16 = '-'(i, 1),
							_DECL_17 = range_internal_int(_DECL_15, _DECL_16),
							_DECL_18 = (_DECL_17,),
							_ =
								let {
									constraint
										'mzn_slice_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int)), tuple(set of int)>'(
											e,
											_DECL_18,
										);
								} in 1,
							_DECL_19 = _DECL_18.1,
							_DECL_20 = mzn_slice_internal(e, [_DECL_19]),
							_DECL_21 =
								'array1d<set of int, array [int] of tuple(string, array [int] of tuple(int, set of int))>'(
									_DECL_17,
									_DECL_20,
								),
							x in _DECL_21,
							_DECL_25 = [_DECL_24 | y in x.2, _DECL_24 = card(y.2)],
							_DECL_26 = 'product<array [int] of int>'(_DECL_25),
					],
				offset = 'sum<array [int] of int>'(_DECL_27),
		];
	} in (_DECL_7, _DECL_29);
function tuple(
	int,
	array [int] of tuple(string, array [int] of tuple(int, set of int), int),
): 'mzn_get_enum_root<array [int] of string>'(array [int] of string: e) :: promise_total =
	let {
		array [int] of tuple(string, array [int] of tuple(int, set of int)): _DECL_3 = [
			(e_i, _DECL_2) |
				e_i in e,
				_DECL_2 = [],
		];
	} in 'mzn_get_enum_root<array [int] of tuple(string, array [int] of tuple(int, set of int))>'(
		_DECL_3,
	) :: mzn_forward_annotations;
function set of int: mzn_defining_set(
	tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)): x,
)
	:: promise_total =
	let {
		int: _DECL_1 = 1;
		int: _DECL_2 = x.1;
	} in range_internal_int(_DECL_1, _DECL_2);
function int: 'mzn_construct_enum_root<tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)), int>'(
	tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)): e,
	int: i,
) :: promise_total =
	let {
		array [int] of tuple(string, array [int] of tuple(int, set of int), int): _DECL_1 = e.2;
		constraint
			'mzn_array_access_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int), int), int>'(
				_DECL_1,
				i,
			);
		tuple(string, array [int] of tuple(int, set of int), int): _DECL_2 =
			mzn_element_internal(_DECL_1, i);
	} in '+'(_DECL_2.3, 1);
function int: 'mod_root<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = '!=<int, int>'(y, 0) :: mzn_forward_annotations;
		constraint
			if _DECL_1 then
				mzn_root
			else
				let {
					constraint mzn_add_warning("modulus by zero is undefined");
					constraint false;
					constraint true;
				} in mzn_root
			endif;
	} in mod_internal(x, y) :: mzn_forward_annotations;
function int: 'div_root<int, int>'(int: x, int: y) :: promise_total =
	let {
		bool: _DECL_1 = '!=<int, int>'(y, 0) :: mzn_forward_annotations;
		constraint
			if _DECL_1 then
				mzn_root
			else
				let {
					constraint mzn_add_warning("division by zero is undefined");
					constraint false;
					constraint true;
				} in mzn_root
			endif;
	} in div_internal_int(x, y);
function int: 'max_internal_set<set of int>'(set of int: s) :: promise_total =
	let {
		array [int] of int: _DECL_1 = set2array(s);
		int: _DECL_2 = card(s);
	} in mzn_element_internal(_DECL_1, _DECL_2);
function int: 'max_root<set of int>'(
	set of int: s,
)
	:: promise_total =
	let {
		int: _DECL_1 = card(s);
		int: _DECL_2 = 0;
		bool: _DECL_3 = '<'(_DECL_2, _DECL_1);
		constraint
			if _DECL_3 then
				mzn_root
			else
				let {
					constraint mzn_add_warning("maximum of an empty set is undefined");
					constraint false;
					constraint true;
				} in mzn_root
			endif;
	} in 'max_internal_set<set of int>'(s) :: mzn_forward_annotations;
function tuple(bool, int): 'max<set of int>'(set of int: s) :: promise_total =
	let {
		int: _DECL_4 = card(s);
		int: _DECL_5 = 0;
		bool: _DECL_6 = '<'(_DECL_5, _DECL_4);
		bool: _DECL_7 =
			if _DECL_6 then
				true
			else
				let {
					bool: _DECL_1 = mzn_add_warning("maximum of an empty set is undefined");
					bool: _DECL_2 = 'forall<array [int] of bool>'([_DECL_1]);
					bool: _DECL_3 = if _DECL_2 then false else false endif;
				} in 'forall<array [int] of bool>'([_DECL_1, _DECL_3])
			endif;
		bool: _DECL_8 = 'forall<array [int] of bool>'([_DECL_7]);
		bool: _DECL_9 = 'forall<array [int] of bool>'([_DECL_7]);
		int: _DECL_10 =
			if _DECL_9 then
				'max_internal_set<set of int>'(s) :: mzn_forward_annotations
			else
				0
			endif;
	} in (_DECL_8, _DECL_10);
function tuple(bool, int): 'min<set of int>'(set of int: s) :: promise_total =
	let {
		int: _DECL_4 = card(s);
		int: _DECL_5 = 0;
		bool: _DECL_6 = '<'(_DECL_5, _DECL_4);
		bool: _DECL_7 =
			if _DECL_6 then
				true
			else
				let {
					bool: _DECL_1 = mzn_add_warning("minimum of an empty set is undefined");
					bool: _DECL_2 = 'forall<array [int] of bool>'([_DECL_1]);
					bool: _DECL_3 = if _DECL_2 then false else false endif;
				} in 'forall<array [int] of bool>'([_DECL_1, _DECL_3])
			endif;
		bool: _DECL_8 = 'forall<array [int] of bool>'([_DECL_7]);
		bool: _DECL_9 = 'forall<array [int] of bool>'([_DECL_7]);
		int: _DECL_10 =
			if _DECL_9 then
				'min_internal_set<set of int>'(s) :: mzn_forward_annotations
			else
				0
			endif;
	} in (_DECL_8, _DECL_10);
function bool: mzn_set_is_contiguous(set of int: X) :: promise_total =
	let {
		int: _DECL_16 = card(X);
		bool: _DECL_17 = '='(_DECL_16, 0);
	} in if _DECL_17 then
		true
	else
		let {
			tuple(bool, int): _DECL_1 = 'max<set of int>'(X);
			tuple(bool, int): _DECL_2 = 'min<set of int>'(X);
			bool: _DECL_3 = _DECL_1.1;
			bool: _DECL_4 = _DECL_2.1;
			bool: _DECL_5 = 'forall<array [int] of bool>'([_DECL_3, _DECL_4]);
			int: _DECL_6 = '-'(_DECL_1.2, _DECL_2.2);
			tuple(bool, int): _DECL_7 = (_DECL_5, _DECL_6);
			bool: _DECL_8 = _DECL_7.1;
			bool: _DECL_9 = 'forall<array [int] of bool>'([_DECL_8]);
			int: _DECL_10 = '+'(_DECL_7.2, 1);
			tuple(bool, int): _DECL_11 = (_DECL_9, _DECL_10);
			bool: _DECL_12 = _DECL_11.1;
			int: _DECL_13 = card(X);
			bool: _DECL_14 = '='(_DECL_13, _DECL_11.2);
			bool: _DECL_15 = 'forall<array [int] of bool>'([_DECL_12, _DECL_14]);
		} in if _DECL_15 then true else false endif
	endif;
function array [
	int,
] of int: 'mzn_destruct_enum_root<tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)), int, int>'(
	tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)): e,
	int: i,
	int: v,
) :: promise_total =
	let {
		array [int] of tuple(string, array [int] of tuple(int, set of int), int): _DECL_1 = e.2;
		constraint
			'mzn_array_access_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int), int), int>'(
				_DECL_1,
				i,
			);
		tuple(string, array [int] of tuple(int, set of int), int): _DECL_2 =
			mzn_element_internal(_DECL_1, i);
		string: name = _DECL_2.1;
		array [int] of tuple(int, set of int): params = _DECL_2.2;
		int: offset = _DECL_2.3;
		int: lower = '+'(offset, 1);
		int: _DECL_10 = length(e.2);
		bool: _DECL_11 = '='(i, _DECL_10);
		int: upper =
			if _DECL_11 then
				e.1
			else
				let {
					array [int] of tuple(
						string,
						array [int] of tuple(int, set of int),
						int,
					): _DECL_7 = e.2;
					int: _DECL_8 = '+'(i, 1);
					constraint
						'mzn_array_access_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int), int), int>'(
							_DECL_7,
							_DECL_8,
						);
					tuple(string, array [int] of tuple(int, set of int), int): _DECL_9 =
						mzn_element_internal(_DECL_7, _DECL_8);
				} in _DECL_9.3
			endif;
		set of int: _DECL_15 = range_internal_int(lower, upper);
		bool: _DECL_16 = 'in<int, set of int>'(v, _DECL_15) :: mzn_forward_annotations;
		constraint
			if _DECL_16 then
				mzn_root
			else
				let {
					string: _DECL_13 = '++'("Cannot destructure value using ", name);
					string: _DECL_14 = '++'(_DECL_13, "(...).");
					constraint mzn_add_warning(_DECL_14);
					constraint false;
					constraint true;
				} in mzn_root
			endif;
	} in [
		r |
			_DECL_17 =
				'index_set<array [int] of tuple(int, set of int)>'(params)
					:: mzn_forward_annotations,
			p in _DECL_17,
			_DECL_19 = '-'(v, offset),
			_DECL_20 = '-'(_DECL_19, 1),
			_DECL_28 =
				[
					_DECL_27 |
						_DECL_21 = '+'(p, 1),
						_DECL_22 = 'index_set<array [int] of tuple(int, set of int)>'(params),
						_DECL_23 = 'max_root<set of int>'(_DECL_22),
						_DECL_24 = range_internal_int(_DECL_21, _DECL_23),
						j in _DECL_24,
						_ =
							let {
								constraint
									'mzn_array_access_valid_root<array [int] of tuple(int, set of int), int>'(
										params,
										j,
									);
							} in 1,
						_DECL_26 = mzn_element_internal(params, j),
						_DECL_27 = card(_DECL_26.2),
				],
			_DECL_29 = 'product<array [int] of int>'(_DECL_28),
			_DECL_30 = 'div_root<int, int>'(_DECL_20, _DECL_29),
			_ =
				let {
					constraint
						'mzn_array_access_valid_root<array [int] of tuple(int, set of int), int>'(
							params,
							p,
						);
				} in 1,
			_DECL_31 = mzn_element_internal(params, p),
			_DECL_32 = card(_DECL_31.2),
			d = 'mod_root<int, int>'(_DECL_30, _DECL_32),
			_DECL_37 =
				'mzn_array_access_valid<array [int] of tuple(int, set of int), int>'(params, p),
			_DECL_38 = 'forall<array [int] of bool>'([_DECL_37]),
			_DECL_40 = 'forall<array [int] of bool>'([_DECL_37]),
			_DECL_41 =
				if _DECL_40 then
					mzn_element_internal(params, p)
				else
					let { set of int: _DECL_39 = {} } in (0, _DECL_39)
				endif,
			_DECL_42 = (_DECL_38, _DECL_41),
			_DECL_43 = _DECL_42.1,
			_DECL_44 = _DECL_42.2,
			_DECL_45 = _DECL_44.2,
			_DECL_46 = (_DECL_43, _DECL_45),
			_DECL_47 = _DECL_46.1,
			_DECL_48 = mzn_set_is_contiguous(_DECL_46.2),
			_DECL_49 = 'forall<array [int] of bool>'([_DECL_47, _DECL_48]),
			r =
				if _DECL_49 then
					let {
						constraint
							'mzn_array_access_valid_root<array [int] of tuple(int, set of int), int>'(
								params,
								p,
							);
						tuple(int, set of int): _DECL_50 = mzn_element_internal(params, p);
						int: _DECL_51 = 'min_root<set of int>'(_DECL_50.2);
					} in '+'(_DECL_51, d)
				else
					let {
						constraint
							'mzn_array_access_valid_root<array [int] of tuple(int, set of int), int>'(
								params,
								p,
							);
						tuple(int, set of int): _DECL_34 = mzn_element_internal(params, p);
						array [int] of int: _DECL_35 = set2array(_DECL_34.2);
						int: _DECL_36 = '+'(d, 1);
						constraint
							'mzn_array_access_valid_root<array [int] of int, int>'(
								_DECL_35,
								_DECL_36,
							);
					} in mzn_element_internal(_DECL_35, _DECL_36)
				endif,
	];
function bool: 'max<bool, bool>'(bool: x, bool: y) :: promise_total =
	let { bool: _DECL_1 = '<'(x, y) } in if _DECL_1 then y else x endif;
function array [int] of bool: '++<array [int] of bool, array [int] of bool>'(
	array [int] of bool: x,
	array [int] of bool: y,
) :: promise_total =
	[
		_DECL_13 |
			xx = 'array1d<array [int] of bool>'(x) :: mzn_forward_annotations,
			yy = 'array1d<array [int] of bool>'(y) :: mzn_forward_annotations,
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
function bool: 'max_internal_array<array [int] of bool>'(array [int] of bool: x) :: promise_total =
	let {
		array [int] of bool: xx = 'array1d<array [int] of bool>'(x);
		int: _DECL_14 = length(xx);
		bool: _DECL_15 = '='(_DECL_14, 1);
	} in if _DECL_15 then
		mzn_element_internal(xx, 1)
	else
		let {
			int: _DECL_10 = length(x);
			bool: _DECL_11 = '='(_DECL_10, 2);
		} in if _DECL_11 then
			let {
				bool: _DECL_12 = mzn_element_internal(xx, 1);
				bool: _DECL_13 = mzn_element_internal(xx, 2);
			} in 'max<bool, bool>'(_DECL_12, _DECL_13) :: mzn_forward_annotations
		else
			let {
				bool: _DECL_2 = mzn_element_internal(xx, 1);
				bool: _DECL_3 = mzn_element_internal(xx, 2);
				bool: _DECL_4 = 'max<bool, bool>'(_DECL_2, _DECL_3);
				int: _DECL_5 = 3;
				int: _DECL_6 = length(xx);
				set of int: _DECL_7 = range_internal_int(_DECL_5, _DECL_6);
				array [int] of bool: _DECL_8 = mzn_slice_internal(xx, [_DECL_7]);
				array [int] of bool: _DECL_9 =
					'++<array [int] of bool, array [int] of bool>'([_DECL_4], _DECL_8);
			} in 'max_internal_array<array [int] of bool>'(_DECL_9) :: mzn_forward_annotations
		endif
	endif;
function bool: 'max<array [int] of bool>'(array [int] of bool: x) :: promise_total =
	let {
		int: _DECL_4 = length(x);
		int: _DECL_5 = 0;
		bool: _DECL_6 = '<'(_DECL_5, _DECL_4);
		bool: _DECL_7 =
			if _DECL_6 then
				true
			else
				let {
					bool: _DECL_1 = mzn_add_warning("maximum of an empty array is undefined");
					bool: _DECL_2 = 'forall<array [int] of bool>'([_DECL_1]);
					bool: _DECL_3 = if _DECL_2 then false else false endif;
				} in 'forall<array [int] of bool>'([_DECL_1, _DECL_3])
			endif;
		bool: _DECL_8 = 'forall<array [int] of bool>'([_DECL_7]);
		bool: _DECL_10 =
			if _DECL_8 then
				let {
					array [int] of bool: _DECL_9 = 'array1d<array [int] of bool>'(x);
				} in 'max_internal_array<array [int] of bool>'(_DECL_9) :: mzn_forward_annotations
			else
				false
			endif;
	} in 'forall<array [int] of bool>'([_DECL_7, _DECL_10]);
function int: arg_max_internal_root(array [int] of bool: xs) :: promise_total =
	let {
		array [int] of int: matches = [
			i |
				_DECL_1 = 'index_set<array [int] of bool>'(xs),
				i in _DECL_1,
				_DECL_3 = 'mzn_array_access_valid<array [int] of bool, int>'(xs, i),
				_DECL_4 = 'forall<array [int] of bool>'([_DECL_3]),
				_DECL_5 = if _DECL_4 then mzn_element_internal(xs, i) else false endif,
				_DECL_6 = 'forall<array [int] of bool>'([_DECL_3, _DECL_5]),
				_DECL_7 = 'max<array [int] of bool>'(xs),
				_DECL_8 = '='(_DECL_6, _DECL_7),
				_ = 1 where _DECL_8,
		];
		int: _DECL_10 = 1;
		constraint
			'mzn_array_access_valid_root<array [int] of int, int>'(matches, _DECL_10)
				:: mzn_forward_annotations;
	} in mzn_element_internal(matches, _DECL_10);
function int: arg_max_root(array [int] of bool: xs) :: promise_total =
	let {
		int: _DECL_1 = length(xs);
		int: _DECL_2 = 0;
		bool: _DECL_3 = '<'(_DECL_2, _DECL_1);
		constraint
			if _DECL_3 then
				mzn_root
			else
				let {
					constraint mzn_add_warning("arg_max of an empty array is undefined");
					constraint false;
					constraint true;
				} in mzn_root
			endif;
	} in arg_max_internal_root(xs) :: mzn_forward_annotations;
function tuple(string, array [int] of tuple(int, int)): mzn_unpack_enum_root(
	tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)): e,
	int: v,
) :: promise_total =
	let {
		array [int] of bool: _DECL_3 = [_DECL_2 | i in e.2, _DECL_2 = '<=<int, int>'(v, i.3)];
		array [int] of bool: _DECL_4 =
			'++<array [int] of bool, array [int] of bool>'(_DECL_3, [true]);
		int: _DECL_5 = arg_max_root(_DECL_4);
		int: i = '-'(_DECL_5, 1);
		array [int] of tuple(string, array [int] of tuple(int, set of int), int): _DECL_7 = e.2;
		constraint
			'mzn_array_access_valid_root<array [int] of tuple(string, array [int] of tuple(int, set of int), int), int>'(
				_DECL_7,
				i,
			);
		tuple(string, array [int] of tuple(int, set of int), int): _DECL_8 =
			mzn_element_internal(_DECL_7, i);
		string: name = _DECL_8.1;
		array [int] of tuple(int, set of int): def = _DECL_8.2;
		int: offset = _DECL_8.3;
		int: _DECL_13 = length(def);
		int: _DECL_14 = 0;
		bool: _DECL_15 = '<'(_DECL_14, _DECL_13);
		array [int] of tuple(int, int): args =
			if _DECL_15 then
				let {
					array [int] of int: d =
						'mzn_destruct_enum_root<tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)), int, int>'(
							e,
							i,
							v,
						);
					array [int] of tuple(int, int): s = [
						(_DECL_20, _DECL_21) |
							_DECL_17 = 'index_set<array [int] of int>'(d),
							idx in _DECL_17,
							_ =
								let {
									constraint
										'mzn_array_access_valid_root<array [int] of tuple(int, set of int), int>'(
											def,
											idx,
										);
								} in 1,
							_DECL_19 = mzn_element_internal(def, idx),
							_DECL_20 = _DECL_19.1,
							_ =
								let {
									constraint
										'mzn_array_access_valid_root<array [int] of int, int>'(
											d,
											idx,
										);
								} in 1,
							_DECL_21 = mzn_element_internal(d, idx),
					];
				} in s
			else
				let { array [int] of tuple(int, int): _DECL_12 = [] } in _DECL_12
			endif;
	} in (name, args);
function string: mzn_show_enum_root(
	array [int] of tuple(
		int,
		array [int] of tuple(string, array [int] of tuple(int, set of int), int),
	): enums,
	int: e,
	int: v,
) :: promise_total =
	let {
		constraint
			'mzn_array_access_valid_root<array [int] of tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)), int>'(
				enums,
				e,
			);
		tuple(
			int,
			array [int] of tuple(string, array [int] of tuple(int, set of int), int),
		): _DECL_1 = mzn_element_internal(enums, e);
		tuple(string, array [int] of tuple(int, int)): _DECL_2 = mzn_unpack_enum_root(_DECL_1, v);
		string: name = _DECL_2.1;
		array [int] of tuple(int, int): ctors = _DECL_2.2;
		int: _DECL_5 = length(ctors);
		int: _DECL_6 = 0;
		bool: _DECL_7 = '<'(_DECL_6, _DECL_5);
		string: args =
			if _DECL_7 then
				let {
					array [int] of string: _DECL_12 = [
						_DECL_11 |
							ctor in ctors,
							_DECL_9 = '='(ctor.1, 0),
							_DECL_11 =
								if _DECL_9 then
									let { int: _DECL_10 = ctor.2 } in show_internal(_DECL_10)
								else
									mzn_show_enum_root(enums, ctor.1, ctor.2)
								endif,
					];
					string: _DECL_13 = join(", ", _DECL_12);
					string: _DECL_14 = '++'("(", _DECL_13);
				} in '++'(_DECL_14, ")")
			else
				""
			endif;
	} in '++'(name, args);
function var bool: 'bool_eq<var bool, var bool>'(var bool: x, var bool: y) :: promise_total;
function var bool: mzn_post_root(var bool: c) :: promise_total =
	let {
		constraint 'bool_eq<var bool, var bool>'(c, true) :: mzn_forward_annotations;
		constraint true;
	} in mzn_root;
function var bool: mzn_post(var bool: c) :: promise_total =
	let { constraint 'bool_eq<var bool, var bool>'(c, true) :: mzn_forward_annotations } in true;
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
			constraint bx;
		} in dx
	endif;
function tuple(bool, int): 'deopt<opt int>'(tuple(bool, int): x) :: promise_total =
	if x.1 then
		let { int: _DECL_8 = 'deopt<int>'(x.2) :: mzn_forward_annotations } in (true, _DECL_8)
	else
		let {
			tuple(bool, int): _DECL_1 = x;
			bool: bx = _DECL_1.1;
			int: dx = _DECL_1.2;
			bool: _DECL_4 = bx;
			bool: _DECL_5 = 'forall<array [int] of bool>'([_DECL_4]);
			bool: _DECL_6 = 'forall<array [int] of bool>'([_DECL_4]);
			int: _DECL_7 = if _DECL_6 then dx else 0 endif;
		} in (_DECL_5, _DECL_7)
	endif;
function string: mzn_version_to_string_root(int: v) :: promise_total =
	let {
		int: _DECL_1 = 'div_root<int, int>'(v, 1000000);
		string: _DECL_2 = show_internal(_DECL_1);
		string: _DECL_3 = '++'(_DECL_2, ".");
		int: _DECL_4 = 'div_root<int, int>'(v, 1000);
		int: _DECL_5 = 'mod_root<int, int>'(_DECL_4, 1000);
		string: _DECL_6 = show_internal(_DECL_5);
		string: _DECL_7 = '++'(_DECL_3, _DECL_6);
		string: _DECL_8 = '++'(_DECL_7, ".");
		int: _DECL_9 = 'mod_root<int, int>'(v, 1000);
		string: _DECL_10 = show_internal(_DECL_9);
	} in '++'(_DECL_8, _DECL_10);
function string: 'show_root<Bar>'(int: x) :: promise_total =
	mzn_show_enum_root([_GLOBAL_13, _GLOBAL_14], 2, x) :: mzn_forward_annotations;
function string: 'show_root<set of Bar>'(set of int: x) :: promise_total =
	let {
		array [int] of string: _DECL_3 = [_DECL_2 | x_i in x, _DECL_2 = 'show_root<Bar>'(x_i)];
		string: _DECL_4 = join(", ", _DECL_3);
		string: _DECL_5 = '++'("{", _DECL_4);
	} in '++'(_DECL_5, "}");
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
array [int] of string: Bar;
bool: mzn_root;
tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)): _GLOBAL_13;
tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)): _GLOBAL_14;
var int: x :: output;
var int: y :: output;
string: z :: output;
mzn_root = true;
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
constraint
	if _DECL_16 then
		mzn_root
	else
		let {
			int: _DECL_3 = 'deopt_root<opt int>'(mzn_min_version_required);
			string: _DECL_4 = mzn_version_to_string_root(_DECL_3);
			string: _DECL_5 = '++'("This model requires MiniZinc version ", _DECL_4);
			string: _DECL_6 = '++'(_DECL_5, " but you are running version ");
			int: _DECL_7 = mzn_compiler_version();
			string: _DECL_8 = mzn_version_to_string_root(_DECL_7);
			string: _DECL_9 = '++'(_DECL_6, _DECL_8);
			constraint abort(_DECL_9);
		} in mzn_root
	endif;
bool: _DECL_32 = 'absent<opt int>'(mzn_max_version_required);
bool: _DECL_33 =
	if _DECL_32 then
		true
	else
		let {
			tuple(bool, int): _DECL_24 = 'deopt<opt int>'(mzn_max_version_required);
			int: _DECL_25 = _DECL_24.2;
			int: _DECL_26 = mzn_compiler_version();
			bool: _DECL_27 = _DECL_24.1;
			bool: _DECL_28 = _DECL_24.1;
			bool: _DECL_29 = 'forall<array [int] of bool>'([_DECL_28]);
			bool: _DECL_30 = if _DECL_29 then '<=<int, int>'(_DECL_26, _DECL_25) else false endif;
			bool: _DECL_31 = 'forall<array [int] of bool>'([_DECL_27, _DECL_30]);
		} in if _DECL_31 then true else false endif
	endif;
constraint
	if _DECL_33 then
		mzn_root
	else
		let {
			int: _DECL_17 = 'deopt_root<opt int>'(mzn_max_version_required);
			string: _DECL_18 = mzn_version_to_string_root(_DECL_17);
			string: _DECL_19 = '++'("This model only works with MiniZinc up to version ", _DECL_18);
			string: _DECL_20 = '++'(_DECL_19, " but you are running version ");
			int: _DECL_21 = mzn_compiler_version();
			string: _DECL_22 = mzn_version_to_string_root(_DECL_21);
			string: _DECL_23 = '++'(_DECL_20, _DECL_22);
			constraint abort(_DECL_23);
		} in mzn_root
	endif;
bool: debug_mode = mzn_internal_check_debug_mode();
array [int] of tuple(int, set of int): _DECL_35 = [];
tuple(string, array [int] of tuple(int, set of int)): _DECL_36 = ("A", _DECL_35);
array [int] of tuple(int, set of int): _DECL_37 = [];
tuple(string, array [int] of tuple(int, set of int)): _DECL_38 = ("B", _DECL_37);
_GLOBAL_13 =
	'mzn_get_enum_root<array [int] of tuple(string, array [int] of tuple(int, set of int))>'([
		_DECL_36,
		_DECL_38,
	]);
set of int: _DECL_39 = mzn_defining_set(_GLOBAL_13);
int: A =
	'mzn_construct_enum_root<tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)), int>'(
		_GLOBAL_13,
		1,
	);
int: B =
	'mzn_construct_enum_root<tuple(int, array [int] of tuple(string, array [int] of tuple(int, set of int), int)), int>'(
		_GLOBAL_13,
		2,
	);
x = (let { var _DECL_39: _FRESH_VAR } in _FRESH_VAR);
_GLOBAL_14 = 'mzn_get_enum_root<array [int] of string>'(Bar);
set of int: _DECL_42 = mzn_defining_set(_GLOBAL_14);
y = (let { var _DECL_42: _FRESH_VAR } in _FRESH_VAR);
z = 'show_root<set of Bar>'(_DECL_42);
