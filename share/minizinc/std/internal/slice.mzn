% Array slicing
%
% These give the sliced portion of the array as a list and need the be coerced to have the correct output index sets

function array [int] of any $T: mzn_slice_internal(
	array [$X] of any $T: x,
	array [int] of set of int: i,
);

% Slice of 1D array
function array [int] of any $T: mzn_slice(
	array [$$E] of any $T: x,
	tuple(set of $$E): i,
) :: mzn_inline =
	let {
		constraint mzn_slice_valid(x, i);
	} in mzn_slice_internal(index2int(x), [enum2int(i.1)]);

% Slice of 2D array
function array [int] of any $T: mzn_slice(
	array [$$E, $$F] of any $T: x,
	tuple(set of $$E, set of $$F): i,
) :: mzn_inline =
	let {
		constraint mzn_slice_valid(x, i);
	} in mzn_slice_internal(
		index2int(x),
		[enum2int(i.1), enum2int(i.2)],
	);

% Slice of 3D array
function array [int] of any $T: mzn_slice(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(set of $$E, set of $$F, set of $$G): i,
) :: mzn_inline =
	let {
		constraint mzn_slice_valid(x, i);
	} in mzn_slice_internal(
		index2int(x),
		[enum2int(i.1), enum2int(i.2), enum2int(i.3)],
	);

% Slice of 4D array
function array [int] of any $T: mzn_slice(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(set of $$E, set of $$F, set of $$G, set of $$H): i,
) :: mzn_inline =
	let {
		constraint mzn_slice_valid(x, i);
	} in mzn_slice_internal(
		index2int(x),
		[enum2int(i.1), enum2int(i.2), enum2int(i.3), enum2int(i.4)],
	);

% Slice of 5D array
function array [int] of any $T: mzn_slice(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(set of $$E, set of $$F, set of $$G, set of $$H, set of $$I): i,
) :: mzn_inline =
	let {
		constraint mzn_slice_valid(x, i);
	} in mzn_slice_internal(
		index2int(x),
		[
			enum2int(i.1),
			enum2int(i.2),
			enum2int(i.3),
			enum2int(i.4),
			enum2int(i.5),
		],
	);

% Slice of 6D array
function array [int] of any $T: mzn_slice(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(
		set of $$E,
		set of $$F,
		set of $$G,
		set of $$H,
		set of $$I,
		set of $$J,
	): i,
) :: mzn_inline =
	let {
		constraint mzn_slice_valid(x, i);
	} in mzn_slice_internal(
		index2int(x),
		[
			enum2int(i.1),
			enum2int(i.2),
			enum2int(i.3),
			enum2int(i.4),
			enum2int(i.5),
			enum2int(i.6),
		],
	);

% Slice validity check
test mzn_slice_valid(set of $$E: idx, set of $$E: idx_set, int: n) =
	if card(idx) = 1 then
		mzn_array_access_valid(min(idx), idx_set, n)
	else
		mzn_assert_warn(
			idx subset idx_set,
			if n > 0 then
				"array slice out of bounds, index set \(n) is \(
					idx_set
				), but given range is \(idx)"
			else
				"array slice out of bounds, index set is \(
					idx_set
				), but given range is \(idx)"
			endif,
		)
	endif;

test mzn_slice_valid(array [$$E] of any $T: x, set of $$E: idx) =
	mzn_slice_valid(idx, index_set(x), 0);
test mzn_slice_valid(array [$$E] of any $T: x, tuple(set of $$E): idx) =
	mzn_slice_valid(x, idx.1);
test mzn_slice_valid(
	array [$$E, $$F] of any $T: x,
	tuple(set of $$E, set of $$F): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_slice_valid(idx.1, idx_set.1, 1),
		mzn_slice_valid(idx.2, idx_set.2, 2),
	]);
test mzn_slice_valid(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(set of $$E, set of $$F, set of $$G): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_slice_valid(idx.1, idx_set.1, 1),
		mzn_slice_valid(idx.2, idx_set.2, 2),
		mzn_slice_valid(idx.3, idx_set.3, 3),
	]);
test mzn_slice_valid(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(set of $$E, set of $$F, set of $$G, set of $$H): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_slice_valid(idx.1, idx_set.1, 1),
		mzn_slice_valid(idx.2, idx_set.2, 2),
		mzn_slice_valid(idx.3, idx_set.3, 3),
		mzn_slice_valid(idx.4, idx_set.4, 4),
	]);
test mzn_slice_valid(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(set of $$E, set of $$F, set of $$G, set of $$H, set of $$I): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_slice_valid(idx.1, idx_set.1, 1),
		mzn_slice_valid(idx.2, idx_set.2, 2),
		mzn_slice_valid(idx.3, idx_set.3, 3),
		mzn_slice_valid(idx.4, idx_set.4, 4),
		mzn_slice_valid(idx.5, idx_set.5, 5),
	]);
test mzn_slice_valid(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(
		set of $$E,
		set of $$F,
		set of $$G,
		set of $$H,
		set of $$I,
		set of $$J,
	): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		mzn_slice_valid(idx.1, idx_set.1, 1),
		mzn_slice_valid(idx.2, idx_set.2, 2),
		mzn_slice_valid(idx.3, idx_set.3, 3),
		mzn_slice_valid(idx.4, idx_set.4, 4),
		mzn_slice_valid(idx.5, idx_set.5, 5),
		mzn_slice_valid(idx.6, idx_set.6, 6),
	]);