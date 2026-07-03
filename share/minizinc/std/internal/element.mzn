% Array access

% Array access using par indices
function any $T: '[]'(array [$E] of any $T: x, $E: idx) :: mzn_inline =
	let { constraint mzn_array_access_valid(x, idx) } in mzn_element_internal(x, idx);
% Array access using var indices
function var $T: '[]'(array [$E] of var $T: x, var $E: idx) :: mzn_inline =
	if mzn_array_access_known_valid(x, idx) then
		shackle_element_t(x, idx)
	else
		shackle_element(x, idx)
	endif;
% Treat access of optional values with non-optional indices using tuple form 
function var opt $T: '[]'(array [$E] of var opt $T: x, var $E: idx) :: mzn_inline =
	mzn_construct_opt('[]'(mzn_destruct_array_opt(x), idx));

% Unchecked internal array par array access
function any $T: mzn_element_internal(array [$E] of any $T: x, $F: idx);

% To allow var dispatch to par version
function any $T: shackle_element_t(array [$E] of any $T: x, $E: idx) :: promise_total =
	mzn_element_internal(x, idx);
function any $T: shackle_element(array [$E] of any $T: x, $E: idx) = '[]'(x, idx);

% 1D var array access
function var $T: shackle_element_t(array [$$E] of var $T: x, var $$E: idx) :: promise_total =
	shackle_element_t(index2int(x), enum2int(idx));
function var $T: shackle_element(array [$$E] of var $T: x, var $$E: idx) =
	let {
		var index_set(x): idx2 =
			mzn_totalise(mzn_array_access_valid(x, idx), idx, min(index_set(x)));
	} in shackle_element_t(x, idx2);
function var $$T: shackle_element_t(array [$$E] of var $$T: x, var $$E: idx) :: promise_total =
	to_enum_internal(enum_of(x), shackle_element_t(index2int(enum2int(x)), enum2int(idx)));
function var $$T: shackle_element(array [$$E] of var $$T: x, var $$E: idx) =
	let {
		var index_set(x): idx2 =
			mzn_totalise(mzn_array_access_valid(x, idx), idx, min(index_set(x)));
	} in shackle_element_t(x, idx2);
function var $T: shackle_element(array [$$E] of var $T: x, tuple(var $$E): idx) =
	shackle_element(x, idx.1);

% 2D var array access
function var $T: shackle_element(array [$$E, $$F] of var $T: x, tuple(var $$E, var $$F): idx) =
	if is_fixed(idx.1) then
		shackle_element(x[fix(idx.1), ..], idx.2)
	elseif is_fixed(idx.2) then
		shackle_element(x[.., fix(idx.2)], idx.1)
	else
		shackle_element(index2int(x), (enum2int(idx.1), enum2int(idx.2)))
	endif;
function var $$T: shackle_element(array [$$E, $$F] of var $$T: x, tuple(var $$E, var $$F): idx) =
	if is_fixed(idx.1) then
		shackle_element(x[fix(idx.1), ..], idx.2)
	elseif is_fixed(idx.2) then
		shackle_element(x[.., fix(idx.2)], idx.1)
	else
		to_enum_internal(
			enum_of(x),
			shackle_element(enum2int(x), (enum2int(idx.1), enum2int(idx.2))),
		)
	endif;

% 2D var array access known to be total
function var $T: shackle_element_t(array [$$E, $$F] of var $T: x, tuple(var $$E, var $$F): idx)
	:: promise_total =
	if is_fixed(idx.1) then
		let {
			any: idx_set = index_sets(x);
		} in shackle_element_t(
			array1d(
				idx_set.2,
				mzn_slice_internal(x, [{enum2int(fix(idx.1))}, enum2int(idx_set.2)]),
			),
			idx.2,
		)
	elseif is_fixed(idx.2) then
		let {
			any: idx_set = index_sets(x);
		} in shackle_element_t(
			array1d(
				idx_set.1,
				mzn_slice_internal(x, [enum2int(idx_set.1), {enum2int(fix(idx.2))}]),
			),
			idx.1,
		)
	else
		shackle_element_t(index2int(x), (enum2int(idx.1), enum2int(idx.2)))
	endif;
function var $$T: shackle_element_t(array [$$E, $$F] of var $$T: x, tuple(var $$E, var $$F): idx)
	:: promise_total =
	if is_fixed(idx.1) then
		let {
			any: idx_set = index_sets(x);
		} in shackle_element_t(
			array1d(
				idx_set.2,
				mzn_slice_internal(x, [{enum2int(fix(idx.1))}, enum2int(idx_set.2)]),
			),
			idx.2,
		)
	elseif is_fixed(idx.2) then
		let {
			any: idx_set = index_sets(x);
		} in shackle_element_t(
			array1d(
				idx_set.1,
				mzn_slice_internal(x, [enum2int(idx_set.1), {enum2int(fix(idx.2))}]),
			),
			idx.1,
		)
	else
		to_enum_internal(
			enum_of(x),
			shackle_element_t(enum2int(x), (enum2int(idx.1), enum2int(idx.2))),
		)
	endif;

% Multidimensional var array access
function var $T: shackle_element(array [$E] of var $T: x, var $F: idx) =
	let {
		any: pairs = mzn_index_pairs(x, idx);
		constraint mzn_fixed_array_access_valid(x, idx);
		any: xx =
			mzn_slice_internal(x, [if is_fixed(i) then {fix(i)} else is endif | (i, is) in pairs]);
		any: var_pairs = [p | p in pairs where not is_fixed(p.1)];
	} in if length(var_pairs) == 1 then
		let {
			any: (i, is) = mzn_element_internal(var_pairs, 1);
		} in shackle_element(xx, i - min(is) + 1)
	elseif length(var_pairs) == 2 then
		let {
			any: (i1, is1) = mzn_element_internal(var_pairs, 1);
			any: (i2, is2) = mzn_element_internal(var_pairs, 2);
		} in shackle_element(array2d(is1, is2, xx), (i1 - min(is1) + 1, i2 - min(is2) + 1))
	else
		let {
			any: index =
				(
					1 +
						sum (
							a in index_set(var_pairs),
							(i, is) = mzn_element_internal(var_pairs, a),
						) (
							(i - min(is)) *
								product (b in a + 1..max(index_set(var_pairs))) (
									card(mzn_element_internal(var_pairs, b).2)
								)
						)
				) :: domain_propagation;
		} in shackle_element(xx, index)
	endif;
function var $T: shackle_element_t(array [$E] of var $T: x, var $F: idx) :: promise_total =
	let {
		any: pairs = mzn_index_pairs(x, idx);
		any: xx =
			mzn_slice_internal(x, [if is_fixed(i) then {fix(i)} else is endif | (i, is) in pairs]);
		any: var_pairs = [p | p in pairs where not is_fixed(p.1)];
	} in if length(var_pairs) == 1 then
		let {
			any: (i, is) = mzn_element_internal(var_pairs, 1);
		} in shackle_element_t(xx, i - min(is) + 1)
	elseif length(var_pairs) == 2 then
		let {
			any: (i1, is1) = mzn_element_internal(var_pairs, 1);
			any: (i2, is2) = mzn_element_internal(var_pairs, 2);
		} in shackle_element_t(array2d(is1, is2, xx), (i1 - min(is1) + 1, i2 - min(is2) + 1))
	else
		let {
			any: index =
				(
					1 +
						sum (
							a in index_set(var_pairs),
							(i, is) = mzn_element_internal(var_pairs, a),
						) (
							(i - min(is)) *
								product (b in a + 1..max(index_set(var_pairs))) (
									card(mzn_element_internal(var_pairs, b).2)
								)
						)
				) :: domain_propagation;
		} in shackle_element_t(xx, index)
	endif;

% Helper to check validity of runtime fixed indices
test mzn_fixed_array_access_valid(array [$E] of any $T: x, var $F: idx) :: mzn_unreachable;
test mzn_fixed_array_access_valid(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(var $$E, var $$F, var $$G): idx,
) =
	let {
		any: idx_set = index_sets(x);
		constraint if is_fixed(idx.1) then mzn_array_access_valid(fix(idx.1), idx_set.1, 1) endif;
		constraint if is_fixed(idx.2) then mzn_array_access_valid(fix(idx.2), idx_set.2, 2) endif;
		constraint if is_fixed(idx.3) then mzn_array_access_valid(fix(idx.3), idx_set.3, 3) endif;
	} in true;
test mzn_fixed_array_access_valid(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H): idx,
	tuple(int, int, int, int): n,
) =
	let {
		any: idx_set = index_sets(x);
		constraint if is_fixed(idx.1) then mzn_array_access_valid(fix(idx.1), idx_set.1, 1) endif;
		constraint if is_fixed(idx.2) then mzn_array_access_valid(fix(idx.2), idx_set.2, 2) endif;
		constraint if is_fixed(idx.3) then mzn_array_access_valid(fix(idx.3), idx_set.3, 3) endif;
		constraint if is_fixed(idx.4) then mzn_array_access_valid(fix(idx.4), idx_set.4, 4) endif;
	} in true;
test mzn_fixed_array_access_valid(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H, var $$I): idx,
	tuple(int, int, int, int, int): n,
) =
	let {
		any: idx_set = index_sets(x);
		constraint if is_fixed(idx.1) then mzn_array_access_valid(fix(idx.1), idx_set.1, 1) endif;
		constraint if is_fixed(idx.2) then mzn_array_access_valid(fix(idx.2), idx_set.2, 2) endif;
		constraint if is_fixed(idx.3) then mzn_array_access_valid(fix(idx.3), idx_set.3, 3) endif;
		constraint if is_fixed(idx.4) then mzn_array_access_valid(fix(idx.4), idx_set.4, 4) endif;
		constraint if is_fixed(idx.5) then mzn_array_access_valid(fix(idx.5), idx_set.5, 5) endif;
	} in true;
test mzn_fixed_array_access_valid(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H, var $$I, var $$J): idx,
	tuple(int, int, int, int, int, int): n,
) =
	let {
		any: idx_set = index_sets(x);
		constraint if is_fixed(idx.1) then mzn_array_access_valid(fix(idx.1), idx_set.1, 1) endif;
		constraint if is_fixed(idx.2) then mzn_array_access_valid(fix(idx.2), idx_set.2, 2) endif;
		constraint if is_fixed(idx.3) then mzn_array_access_valid(fix(idx.3), idx_set.3, 3) endif;
		constraint if is_fixed(idx.4) then mzn_array_access_valid(fix(idx.4), idx_set.4, 4) endif;
		constraint if is_fixed(idx.5) then mzn_array_access_valid(fix(idx.5), idx_set.5, 5) endif;
		constraint if is_fixed(idx.6) then mzn_array_access_valid(fix(idx.6), idx_set.6, 6) endif;
	} in true;

% Helper to turn runtime fixed indexing into slicing
function array [int] of tuple(var int, set of int): mzn_index_pairs(
	array [$E] of any $T: x,
	var $F: idx,
) :: mzn_unreachable;
function array [int] of tuple(var int, set of int): mzn_index_pairs(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(var $$E, var $$F, var $$G): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in [
		(enum2int(idx.1), enum2int(idx_set.1)),
		(enum2int(idx.2), enum2int(idx_set.2)),
		(enum2int(idx.3), enum2int(idx_set.3)),
	];
function array [int] of tuple(var int, set of int): mzn_index_pairs(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in [
		(enum2int(idx.1), enum2int(idx_set.1)),
		(enum2int(idx.2), enum2int(idx_set.2)),
		(enum2int(idx.3), enum2int(idx_set.3)),
		(enum2int(idx.4), enum2int(idx_set.4)),
	];
function array [int] of tuple(var int, set of int): mzn_index_pairs(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H, var $$I): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in [
		(enum2int(idx.1), enum2int(idx_set.1)),
		(enum2int(idx.2), enum2int(idx_set.2)),
		(enum2int(idx.3), enum2int(idx_set.3)),
		(enum2int(idx.4), enum2int(idx_set.4)),
		(enum2int(idx.5), enum2int(idx_set.5)),
	];
function array [int] of tuple(var int, set of int): mzn_index_pairs(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H, var $$I, var $$J): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in [
		(enum2int(idx.1), enum2int(idx_set.1)),
		(enum2int(idx.2), enum2int(idx_set.2)),
		(enum2int(idx.3), enum2int(idx_set.3)),
		(enum2int(idx.4), enum2int(idx_set.4)),
		(enum2int(idx.5), enum2int(idx_set.5)),
		(enum2int(idx.6), enum2int(idx_set.6)),
	];

% Element on integers
function var int: shackle_element_t(array [int] of var int: x, var int: idx) :: promise_total =
	let {
		var dom_bounds_array(x): r :: is_defined_var;
		constraint mzn_array_access_valid(x, idx);
		constraint array_var_int_element_nonshifted(idx, x, r) :: defines_var(r);
	} in r;

function var int: shackle_element_t(array [int, int] of var int: x, tuple(var int, var int): idx)
	:: promise_total =
	let {
		var dom_bounds_array(x): r :: is_defined_var;
		constraint mzn_array_access_valid(x, idx);
		constraint array_var_int_element2d_nonshifted(idx.1, idx.2, x, r) :: defines_var(r);
	} in r;

% Element on floats
function var float: shackle_element_t(array [int] of var float: x, var int: idx) :: promise_total =
	let {
		var lb_array(x)..ub_array(x): r :: is_defined_var;
		constraint mzn_array_access_valid(x, idx);
		constraint array_var_float_element_nonshifted(idx, x, r) :: defines_var(r);
	} in r;

function var float: shackle_element_t(
	array [int, int] of var float: x,
	tuple(var int, var int): idx,
)
	:: promise_total =
	let {
		var lb_array(x)..ub_array(x): r :: is_defined_var;
		constraint mzn_array_access_valid(x, idx);
		constraint array_var_float_element2d_nonshifted(idx.1, idx.2, x, r) :: defines_var(r);
	} in r;

% Element on sets
function var set of int: shackle_element_t(array [int] of var set of int: x, var int: idx)
	:: promise_total =
	let {
		var set of min(ub_array(x))..max(ub_array(x)): r :: is_defined_var;
		constraint mzn_array_access_valid(x, idx);
		constraint array_var_set_element_nonshifted(idx, x, r) :: defines_var(r);
	} in r;

function var set of int: shackle_element_t(
	array [int, int] of var set of int: x,
	tuple(var int, var int): idx,
)
	:: promise_total =
	let {
		var set of min(ub_array(x))..max(ub_array(x)): r :: is_defined_var;
		constraint mzn_array_access_valid(x, idx);
		constraint array_var_set_element2d_nonshifted(idx.1, idx.2, x, r) :: defines_var(r);
	} in r;

% Element on booleans
function var bool: shackle_element_t(array [int] of var bool: x, var int: idx) :: promise_total =
	let {
		var bool: r :: is_defined_var;
		constraint mzn_array_access_valid(x, idx);
		constraint array_var_bool_element_nonshifted(idx, x, r) :: defines_var(r);
	} in r;

function var bool: shackle_element_t(array [int, int] of var bool: x, tuple(var int, var int): idx)
	:: promise_total =
	let {
		var bool: r :: is_defined_var;
		constraint mzn_array_access_valid(x, idx);
		constraint array_var_bool_element2d_nonshifted(idx.1, idx.2, x, r) :: defines_var(r);
	} in r;

% Par index validity check
test mzn_array_access_valid($$E: idx, set of $$E: idx_set, int: n) =
	mzn_assert_warn(
		idx in idx_set,
		if n > 0 then
			"array access out of bounds, index set \(n) is \(idx_set), but given index is \(idx)"
		else
			"array access out of bounds, index set is \(idx_set), but given index is \(idx)"
		endif,
	);
test mzn_array_access_valid(array [$E] of any $T: x, $F: idx) :: mzn_unreachable;
test mzn_array_access_valid(array [$$E] of any $T: x, $$E: idx) =
	mzn_array_access_valid(idx, index_set(x), 0);
test mzn_array_access_valid(array [$$E] of any $T: x, tuple($$E): idx) =
	mzn_array_access_valid(x, idx.1);
test mzn_array_access_valid(array [$$E, $$F] of any $T: x, tuple($$E, $$F): idx) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_array_access_valid(idx.1, idx_set.1, 1),
		mzn_array_access_valid(idx.2, idx_set.2, 2),
	]);
test mzn_array_access_valid(array [$$E, $$F, $$G] of any $T: x, tuple($$E, $$F, $$G): idx) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_array_access_valid(idx.1, idx_set.1, 1),
		mzn_array_access_valid(idx.2, idx_set.2, 2),
		mzn_array_access_valid(idx.3, idx_set.3, 3),
	]);
test mzn_array_access_valid(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple($$E, $$F, $$G, $$H): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_array_access_valid(idx.1, idx_set.1, 1),
		mzn_array_access_valid(idx.2, idx_set.2, 2),
		mzn_array_access_valid(idx.3, idx_set.3, 3),
		mzn_array_access_valid(idx.4, idx_set.4, 4),
	]);
test mzn_array_access_valid(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple($$E, $$F, $$G, $$H, $$I): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_array_access_valid(idx.1, idx_set.1, 1),
		mzn_array_access_valid(idx.2, idx_set.2, 2),
		mzn_array_access_valid(idx.3, idx_set.3, 3),
		mzn_array_access_valid(idx.4, idx_set.4, 4),
		mzn_array_access_valid(idx.5, idx_set.5, 5),
	]);
test mzn_array_access_valid(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple($$E, $$F, $$G, $$H, $$I, $$J): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_array_access_valid(idx.1, idx_set.1, 1),
		mzn_array_access_valid(idx.2, idx_set.2, 2),
		mzn_array_access_valid(idx.3, idx_set.3, 3),
		mzn_array_access_valid(idx.4, idx_set.4, 4),
		mzn_array_access_valid(idx.5, idx_set.5, 5),
		mzn_array_access_valid(idx.6, idx_set.6, 6),
	]);

% Var index validity check
predicate mzn_array_access_valid(array [$E] of any $T: x, var $E: idx) :: mzn_unreachable;
predicate mzn_array_access_valid(array [$$E] of any $T: x, var $$E: idx) = idx in index_set(x);
predicate mzn_array_access_valid(array [$$E] of any $T: x, tuple(var $$E): idx) =
	mzn_array_access_valid(x, idx.1);
predicate mzn_array_access_valid(array [$$E, $$F] of any $T: x, tuple(var $$E, var $$F): idx) =
	let { any: idx_set = index_sets(x) } in forall([idx.1 in idx_set.1, idx.2 in idx_set.2]);
predicate mzn_array_access_valid(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(var $$E, var $$F, var $$G): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([idx.1 in idx_set.1, idx.2 in idx_set.2, idx.3 in idx_set.3]);
predicate mzn_array_access_valid(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([idx.1 in idx_set.1, idx.2 in idx_set.2, idx.3 in idx_set.3, idx.4 in idx_set.4]);
predicate mzn_array_access_valid(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H, var $$I): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		idx.1 in idx_set.1,
		idx.2 in idx_set.2,
		idx.3 in idx_set.3,
		idx.4 in idx_set.4,
		idx.5 in idx_set.5,
	]);
predicate mzn_array_access_valid(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H, var $$I, var $$J): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		idx.1 in idx_set.1,
		idx.2 in idx_set.2,
		idx.3 in idx_set.3,
		idx.4 in idx_set.4,
		idx.5 in idx_set.5,
		idx.6 in idx_set.6,
	]);

% Var index bounds optimisation
test mzn_array_access_known_valid(array [$E] of any $T: x, var $F: idx) :: mzn_unreachable;
test mzn_array_access_known_valid(array [$$E] of any $T: x, var $$E: idx) =
	has_bounds(idx) /\ lb(idx)..ub(idx) subset index_set(x);
test mzn_array_access_known_valid(array [$$E] of any $T: x, tuple(var $$E): idx) =
	mzn_array_access_known_valid(x, idx.1);
test mzn_array_access_known_valid(array [$$E, $$F] of any $T: x, tuple(var $$E, var $$F): idx) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
	]);
test mzn_array_access_known_valid(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(var $$E, var $$F, var $$G): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
		has_bounds(idx.3) /\ lb(idx.3)..ub(idx.3) subset idx_set.3,
	]);
test mzn_array_access_known_valid(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
		has_bounds(idx.3) /\ lb(idx.3)..ub(idx.3) subset idx_set.3,
		has_bounds(idx.4) /\ lb(idx.4)..ub(idx.4) subset idx_set.4,
	]);
test mzn_array_access_known_valid(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H, var $$I): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
		has_bounds(idx.3) /\ lb(idx.3)..ub(idx.3) subset idx_set.3,
		has_bounds(idx.4) /\ lb(idx.4)..ub(idx.4) subset idx_set.4,
		has_bounds(idx.5) /\ lb(idx.5)..ub(idx.5) subset idx_set.5,
	]);
test mzn_array_access_known_valid(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(var $$E, var $$F, var $$G, var $$H, var $$I, var $$J): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
		has_bounds(idx.3) /\ lb(idx.3)..ub(idx.3) subset idx_set.3,
		has_bounds(idx.4) /\ lb(idx.4)..ub(idx.4) subset idx_set.4,
		has_bounds(idx.5) /\ lb(idx.5)..ub(idx.5) subset idx_set.5,
		has_bounds(idx.6) /\ lb(idx.6)..ub(idx.6) subset idx_set.6,
	]);
