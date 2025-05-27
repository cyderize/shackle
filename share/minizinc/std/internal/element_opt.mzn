% Optional array access
function opt $T: '[]'(array [$$E] of $T: x, tuple(opt $$E): idx) :: mzn_inline_call_by_name =
	shackle_element_opt_dispatch(x, idx.1);
function opt $T: '[]'(array [$$E, $$F] of $T: x, tuple(opt $$E, opt $$F): idx)
	:: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function opt $T: '[]'(array [$$E, $$F, $$G] of $T: x, tuple(opt $$E, opt $$F, opt $$G): idx)
	:: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function opt $T: '[]'(
	array [$$E, $$F, $$G, $$H] of $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function opt $T: '[]'(
	array [$$E, $$F, $$G, $$H, $$I] of $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function opt $T: '[]'(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I, opt $$J): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);

function opt $T: '[]'(array [$$E] of opt $T: x, tuple(opt $$E): idx) :: mzn_inline_call_by_name =
	shackle_element_opt_dispatch(x, idx.1);
function opt $T: '[]'(array [$$E, $$F] of opt $T: x, tuple(opt $$E, opt $$F): idx)
	:: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function opt $T: '[]'(array [$$E, $$F, $$G] of opt $T: x, tuple(opt $$E, opt $$F, opt $$G): idx)
	:: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function opt $T: '[]'(
	array [$$E, $$F, $$G, $$H] of opt $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function opt $T: '[]'(
	array [$$E, $$F, $$G, $$H, $$I] of opt $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function opt $T: '[]'(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of opt $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I, opt $$J): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);

function var opt $T: '[]'(array [$$E] of var $T: x, opt $$E: idx) :: mzn_inline_call_by_name =
	shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(array [$$E] of var $T: x, tuple(opt $$E): idx)
	 :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx.1);
function var opt $T: '[]'(array [$$E, $$F] of var $T: x, tuple(opt $$E, opt $$F): idx)
	:: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(array [$$E, $$F, $$G] of var $T: x, tuple(opt $$E, opt $$F, opt $$G): idx)
	:: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(
	array [$$E, $$F, $$G, $$H] of var $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(
	array [$$E, $$F, $$G, $$H, $$I] of var $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of var $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I, opt $$J): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);

function var opt $T: '[]'(array [$$E] of var $T: x, var opt $$E: idx) :: mzn_inline_call_by_name =
	shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(array [$$E] of var $T: x, tuple(var opt $$E): idx)
	:: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx.1);
function var opt $T: '[]'(array [$$E, $$F] of var $T: x, tuple(var opt $$E, var opt $$F): idx)
	:: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(
	array [$$E, $$F, $$G] of var $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(
	array [$$E, $$F, $$G, $$H] of var $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(
	array [$$E, $$F, $$G, $$H, $$I] of var $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);
function var opt $T: '[]'(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of var $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I, var opt $$J): idx,
) :: mzn_inline_call_by_name = shackle_element_opt_dispatch(x, idx);

% Par optional array access
function opt $T: shackle_element_opt_dispatch(array [$$E] of $T: x, opt $$E: idx) :: mzn_inline =
	let { constraint mzn_array_access_valid(x, idx) } in shackle_element_opt(x, idx);
function opt $T: shackle_element_opt_dispatch(array [$E] of $T: x, $F: idx) :: mzn_inline =
	let { constraint mzn_array_access_valid(x, idx) } in shackle_element_opt(x, idx);
function opt $T: shackle_element_opt_dispatch(array [$$E] of opt $T: x, opt $$E: idx)
	 :: mzn_inline =
	let { constraint mzn_array_access_valid(x, idx) } in shackle_element_opt(x, idx);
function opt $T: shackle_element_opt_dispatch(array [$E] of opt $T: x, $F: idx) :: mzn_inline =
	let { constraint mzn_array_access_valid(x, idx) } in shackle_element_opt(x, idx);

% Var optional array access
function var opt $T: shackle_element_opt_dispatch(array [$$E] of var $T: x, var opt $$E: idx)
	:: mzn_inline =
	if mzn_array_access_known_valid(x, idx) then
		shackle_element_opt_root(x, idx)
	else
		shackle_element_opt(x, idx)
	endif;
function var opt $T: shackle_element_opt_dispatch(array [$E] of var $T: x, var $F: idx)
	:: mzn_inline =
	if mzn_array_access_known_valid(x, idx) then
		shackle_element_opt_root(x, idx)
	else
		shackle_element_opt(x, idx)
	endif;
function var opt $T: shackle_element_opt_dispatch(array [$$E] of var opt $T: x, var opt $$E: idx)
	:: mzn_inline =
	if mzn_array_access_known_valid(x, idx) then
		shackle_element_opt_root(x, idx)
	else
		shackle_element_opt(x, idx)
	endif;
function var opt $T: shackle_element_opt_dispatch(array [$E] of var opt $T: x, var $F: idx)
	:: mzn_inline =
	if mzn_array_access_known_valid(x, idx) then
		shackle_element_opt_root(x, idx)
	else
		shackle_element_opt(x, idx)
	endif;

% Helper to test whether indices all occur
test mzn_all_occurs($E) :: mzn_unreachable;
test mzn_all_occurs(opt $$E: i) = occurs(i);
test mzn_all_occurs(tuple(opt $$E): i) = occurs(i.1);
test mzn_all_occurs(tuple(opt $$E, opt $$F): i) = occurs(i.1) /\ occurs(i.2);
test mzn_all_occurs(tuple(opt $$E, opt $$F, opt $$G): i) =
	forall([occurs(i.1), occurs(i.2), occurs(i.3)]);
test mzn_all_occurs(tuple(opt $$E, opt $$F, opt $$G, opt $$H): i) =
	forall([occurs(i.1), occurs(i.2), occurs(i.3), occurs(i.4)]);
test mzn_all_occurs(tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I): i) =
	forall([occurs(i.1), occurs(i.2), occurs(i.3), occurs(i.4), occurs(i.5)]);
test mzn_all_occurs(tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I, opt $$J): i) =
	forall([occurs(i.1), occurs(i.2), occurs(i.3), occurs(i.4), occurs(i.5), occurs(i.6)]);
predicate mzn_all_occurs(var $E) :: mzn_unreachable;
predicate mzn_all_occurs(var opt $$E: i) = occurs(i);
predicate mzn_all_occurs(tuple(var opt $$E): i) = occurs(i.1);
predicate mzn_all_occurs(tuple(var opt $$E, var opt $$F): i) = occurs(i.1) /\ occurs(i.2);
predicate mzn_all_occurs(tuple(var opt $$E, var opt $$F, var opt $$G): i) =
	forall([occurs(i.1), occurs(i.2), occurs(i.3)]);
predicate mzn_all_occurs(tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H): i) =
	forall([occurs(i.1), occurs(i.2), occurs(i.3), occurs(i.4)]);
predicate mzn_all_occurs(
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I): i,
) = forall([occurs(i.1), occurs(i.2), occurs(i.3), occurs(i.4), occurs(i.5)]);
predicate mzn_all_occurs(
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I, var opt $$J): i,
) = forall([occurs(i.1), occurs(i.2), occurs(i.3), occurs(i.4), occurs(i.5), occurs(i.6)]);

% Helper to test whether indices are all absent
test mzn_all_absent($E) :: mzn_unreachable;
test mzn_all_absent(opt $$E: i) = absent(i);
test mzn_all_absent(tuple(opt $$E): i) = absent(i.1);
test mzn_all_absent(tuple(opt $$E, opt $$F): i) = absent(i.1) /\ absent(i.2);
test mzn_all_absent(tuple(opt $$E, opt $$F, opt $$G): i) =
	forall([absent(i.1), absent(i.2), absent(i.3)]);
test mzn_all_absent(tuple(opt $$E, opt $$F, opt $$G, opt $$H): i) =
	forall([absent(i.1), absent(i.2), absent(i.3), absent(i.4)]);
test mzn_all_absent(tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I): i) =
	forall([absent(i.1), absent(i.2), absent(i.3), absent(i.4), absent(i.5)]);
test mzn_all_absent(tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I, opt $$J): i) =
	forall([absent(i.1), absent(i.2), absent(i.3), absent(i.4), absent(i.5), absent(i.6)]);
predicate mzn_all_absent(var $E) :: mzn_unreachable;
predicate mzn_all_absent(var opt $$E: i) = absent(i);
predicate mzn_all_absent(tuple(var opt $$E): i) = absent(i.1);
predicate mzn_all_absent(tuple(var opt $$E, var opt $$F): i) = absent(i.1) /\ absent(i.2);
predicate mzn_all_absent(tuple(var opt $$E, var opt $$F, var opt $$G): i) =
	forall([absent(i.1), absent(i.2), absent(i.3)]);
predicate mzn_all_absent(tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H): i) =
	forall([absent(i.1), absent(i.2), absent(i.3), absent(i.4)]);
predicate mzn_all_absent(
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I): i,
) = forall([absent(i.1), absent(i.2), absent(i.3), absent(i.4), absent(i.5)]);
predicate mzn_all_absent(
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I, var opt $$J): i,
) = forall([absent(i.1), absent(i.2), absent(i.3), absent(i.4), absent(i.5), absent(i.6)]);

% Helper to deopt indices
function $E: mzn_deopt_index(array [$E] of any $T: x, $F: idx) :: mzn_unreachable;
function $$E: mzn_deopt_index(array [$$E] of any $T: x, opt $$E: idx) = mzn_destruct_opt(idx).2;
function tuple($$E, $$F): mzn_deopt_index(
	array [$$E, $$F] of any $T: x,
	tuple(opt $$E, opt $$F): idx,
) = (mzn_destruct_opt(idx.1).2, mzn_destruct_opt(idx.2).2);
function tuple($$E, $$F, $$G): mzn_deopt_index(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(opt $$E, opt $$F, opt $$G): idx,
) = (mzn_destruct_opt(idx.1).2, mzn_destruct_opt(idx.2).2, mzn_destruct_opt(idx.3).2);
function tuple($$E, $$F, $$G, $$H): mzn_deopt_index(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H): idx,
) = (
	mzn_destruct_opt(idx.1).2,
	mzn_destruct_opt(idx.2).2,
	mzn_destruct_opt(idx.3).2,
	mzn_destruct_opt(idx.4).2,
);
function tuple($$E, $$F, $$G, $$H, $$I): mzn_deopt_index(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I): idx,
) = (
	mzn_destruct_opt(idx.1).2,
	mzn_destruct_opt(idx.2).2,
	mzn_destruct_opt(idx.3).2,
	mzn_destruct_opt(idx.4).2,
	mzn_destruct_opt(idx.5).2,
);
function tuple($$E, $$F, $$G, $$H, $$I, $$J): mzn_deopt_index(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I, opt $$J): idx,
) = (
	mzn_destruct_opt(idx.1).2,
	mzn_destruct_opt(idx.2).2,
	mzn_destruct_opt(idx.3).2,
	mzn_destruct_opt(idx.4).2,
	mzn_destruct_opt(idx.5).2,
	mzn_destruct_opt(idx.6).2,
);
function var $E: mzn_deopt_index(array [$E] of any $T: x, var $F: idx) :: mzn_unreachable;
function var $$E: mzn_deopt_index(array [$$E] of any $T: x, var opt $$E: idx) = deopt(idx);
function tuple(var $$E, var $$F): mzn_deopt_index(
	array [$$E, $$F] of any $T: x,
	tuple(var opt $$E, var opt $$F): idx,
) = (mzn_destruct_opt(idx.1).2, mzn_destruct_opt(idx.2).2);
function tuple(var $$E, var $$F, var $$G): mzn_deopt_index(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G): idx,
) = (mzn_destruct_opt(idx.1).2, mzn_destruct_opt(idx.2).2, mzn_destruct_opt(idx.3).2);
function tuple(var $$E, var $$F, var $$G, var $$H): mzn_deopt_index(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H): idx,
) = (
	mzn_destruct_opt(idx.1).2,
	mzn_destruct_opt(idx.2).2,
	mzn_destruct_opt(idx.3).2,
	mzn_destruct_opt(idx.4).2,
);
function tuple(var $$E, var $$F, var $$G, var $$H, var $$I): mzn_deopt_index(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I): idx,
) = (
	mzn_destruct_opt(idx.1).2,
	mzn_destruct_opt(idx.2).2,
	mzn_destruct_opt(idx.3).2,
	mzn_destruct_opt(idx.4).2,
	mzn_destruct_opt(idx.5).2,
);
function tuple(var $$E, var $$F, var $$G, var $$H, var $$I, var $$J): mzn_deopt_index(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I, var opt $$J): idx,
) = (
	mzn_destruct_opt(idx.1).2,
	mzn_destruct_opt(idx.2).2,
	mzn_destruct_opt(idx.3).2,
	mzn_destruct_opt(idx.4).2,
	mzn_destruct_opt(idx.5).2,
	mzn_destruct_opt(idx.6).2,
);

function opt $T: shackle_element_opt(array [$$E] of $T: x, opt $$E: idx) =
	if occurs(idx) then mzn_element_internal(x, deopt(idx)) else <> endif;
function opt $T: shackle_element_opt(array [$E] of $T: x, $F: idx) =
	if mzn_all_occurs(idx) then mzn_element_internal(x, mzn_deopt_index(x, idx)) else <> endif;
function opt $T: shackle_element_opt(array [$$E] of opt $T: x, opt $$E: idx) =
	if occurs(idx) then mzn_element_internal(x, deopt(idx)) else <> endif;
function opt $T: shackle_element_opt(array [$E] of opt $T: x, $F: idx) =
	if mzn_all_occurs(idx) then mzn_element_internal(x, mzn_deopt_index(x, idx)) else <> endif;

function var opt $T: shackle_element_opt_root(array [$$E] of var $T: x, var opt $$E: idx) =
	if occurs(idx) then shackle_element(x, deopt(idx)) else <> endif;
function var opt $T: shackle_element_opt(array [$$E] of var $T: x, var opt $$E: idx) =
	if length(x) = 0 then
		let { constraint absent(idx) } in <>
	elseif occurs(idx) then
		shackle_element(x, deopt(idx))
	else
		<>
	endif;
function var opt $T: shackle_element_opt(array [$E] of var $T: x, var $F: idx) =
	if length(x) = 0 then
		let { constraint mzn_all_absent(idx) } in <>
	elseif mzn_all_occurs(idx) then
		shackle_element(x, mzn_deopt_index(x, idx))
	else
		<>
	endif;
function var opt $T: shackle_element_opt(array [$$E] of var opt $T: x, var opt $$E: idx) =
	if length(x) = 0 then
		let { constraint absent(idx) } in <>
	elseif mzn_can_extend_array_opt(x, idx) then
		mzn_construct_opt(
			shackle_element(mzn_destruct_array_opt(mzn_extend_array_opt(x)), deopt(idx)),
		)
	elseif occurs(idx) then
		mzn_construct_opt(shackle_element(mzn_destruct_array_opt(x), deopt(idx)))
	else
		<>
	endif;
function var opt $T: shackle_element_opt(array [$E] of var opt $T: x, var $F: idx) =
	if length(x) = 0 then
		let { constraint mzn_all_absent(idx) } in <>
	elseif mzn_can_extend_array_opt(x, idx) then
		mzn_construct_opt(
			shackle_element(
				mzn_destruct_array_opt(mzn_extend_array_opt(x)),
				mzn_deopt_index(x, idx),
			),
		)
	elseif mzn_all_occurs(idx) then
		mzn_construct_opt(shackle_element(mzn_destruct_array_opt(x), mzn_deopt_index(x, idx)))
	else
		<>
	endif;
function var opt $T: shackle_element_opt_root(array [$E] of var $T: x, var $F: idx) =
	if mzn_all_occurs(idx) then shackle_element_root(x, mzn_deopt_index(x, idx)) else <> endif;
function var opt $T: shackle_element_opt_root(array [$$E] of var opt $T: x, var opt $$E: idx) =
	if mzn_can_extend_array_opt(x, idx) then
		mzn_construct_opt(
			shackle_element_root(mzn_destruct_array_opt(mzn_extend_array_opt(x)), deopt(idx)),
		)
	elseif occurs(idx) then
		mzn_construct_opt(shackle_element_root(mzn_destruct_array_opt(x), mzn_deopt_index(x, idx)))
	else
		<>
	endif;
function var opt $T: shackle_element_opt_root(array [$E] of var opt $T: x, var $F: idx) =
	if mzn_can_extend_array_opt(x, idx) then
		mzn_construct_opt(
			shackle_element_root(
				mzn_destruct_array_opt(mzn_extend_array_opt(x)),
				mzn_deopt_index(x, idx),
			),
		)
	elseif mzn_all_occurs(idx) then
		mzn_construct_opt(shackle_element_root(mzn_destruct_array_opt(x), mzn_deopt_index(x, idx)))
	else
		<>
	endif;

% Helper to test if it is cheap to extend the array to handle absent indices
test mzn_can_extend_array_opt(array [$E] of any $T: x, var $F: idx) :: mzn_unreachable;
test mzn_can_extend_array_opt(array [$$E] of any $T: x, var opt $$E: idx) =
	enum2int(min(index_set(x))) = 1 /\ not had_zero(idx);
test mzn_can_extend_array_opt(array [$$E, $$F] of any $T: x, tuple(var opt $$E, var opt $$F): idx) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		enum2int(min(idx_set.1)) = 1,
		enum2int(min(idx_set.2)) = 1,
		not had_zero(idx.1),
		not had_zero(idx.2),
	]);
test mzn_can_extend_array_opt(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		enum2int(min(idx_set.1)) = 1,
		enum2int(min(idx_set.2)) = 1,
		enum2int(min(idx_set.3)) = 1,
		not had_zero(idx.1),
		not had_zero(idx.2),
		not had_zero(idx.3),
	]);
test mzn_can_extend_array_opt(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		enum2int(min(idx_set.1)) = 1,
		enum2int(min(idx_set.2)) = 1,
		enum2int(min(idx_set.3)) = 1,
		enum2int(min(idx_set.4)) = 1,
		not had_zero(idx.1),
		not had_zero(idx.2),
		not had_zero(idx.3),
		not had_zero(idx.4),
	]);
test mzn_can_extend_array_opt(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		enum2int(min(idx_set.1)) = 1,
		enum2int(min(idx_set.2)) = 1,
		enum2int(min(idx_set.3)) = 1,
		enum2int(min(idx_set.4)) = 1,
		enum2int(min(idx_set.5)) = 1,
		not had_zero(idx.1),
		not had_zero(idx.2),
		not had_zero(idx.3),
		not had_zero(idx.4),
		not had_zero(idx.5),
	]);
test mzn_can_extend_array_opt(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I, var opt $$J): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		enum2int(min(idx_set.1)) = 1,
		enum2int(min(idx_set.2)) = 1,
		enum2int(min(idx_set.3)) = 1,
		enum2int(min(idx_set.4)) = 1,
		enum2int(min(idx_set.5)) = 1,
		enum2int(min(idx_set.6)) = 1,
		not had_zero(idx.1),
		not had_zero(idx.2),
		not had_zero(idx.3),
		not had_zero(idx.4),
		not had_zero(idx.5),
		not had_zero(idx.6),
	]);

% Helper to extend arrays to include <> at index zero
function array [$E] of any $T: mzn_extend_array_opt(array [$E] of any $T: x) :: mzn_unreachable;
function array [$$E] of any $T: mzn_extend_array_opt(array [$$E] of any $T: x) =
	array1d(
		to_enum_internal(enum_of(index_set(x)), 0)..max(index_set(x)),
		[
			if i = 0 then <> else mzn_element_internal(x, i) endif |
				i in 0..enum2int(max(index_set(x))),
		],
	);
function array [$$E, $$F] of any $T: mzn_extend_array_opt(array [$$E, $$F] of any $T: x) =
	let {
		any: idx_set = index_sets(x);
	} in array2d(
		to_enum_internal(enum_of(idx_set.1), 0)..max(idx_set.1),
		to_enum_internal(enum_of(idx_set.2), 0)..max(idx_set.2),
		[
			if i = 0 \/ j = 0 then <> else mzn_element_internal(x, (i, j)) endif |
				i in 0..enum2int(max(idx_set.1)),
				j in 0..enum2int(max(idx_set.2)),
		],
	);
function array [$$E, $$F, $$G] of any $T: mzn_extend_array_opt(array [$$E, $$F, $$G] of any $T: x) =
	let {
		any: idx_set = index_sets(x);
	} in array3d(
		to_enum_internal(enum_of(idx_set.1), 0)..max(idx_set.1),
		to_enum_internal(enum_of(idx_set.2), 0)..max(idx_set.2),
		to_enum_internal(enum_of(idx_set.3), 0)..max(idx_set.3),
		[
			if i = 0 \/ j = 0 \/ k = 0 then <> else mzn_element_internal(x, (i, j, k)) endif |
				i in 0..enum2int(max(idx_set.1)),
				j in 0..enum2int(max(idx_set.2)),
				k in 0..enum2int(max(idx_set.3)),
		],
	);
function array [$$E, $$F, $$G, $$H] of any $T: mzn_extend_array_opt(
	array [$$E, $$F, $$G, $$H] of any $T: x,
) =
	let {
		any: idx_set = index_sets(x);
	} in array4d(
		to_enum_internal(enum_of(idx_set.1), 0)..max(idx_set.1),
		to_enum_internal(enum_of(idx_set.2), 0)..max(idx_set.2),
		to_enum_internal(enum_of(idx_set.3), 0)..max(idx_set.3),
		to_enum_internal(enum_of(idx_set.4), 0)..max(idx_set.4),
		[
			if i = 0 \/ j = 0 \/ k = 0 \/ l = 0 then
				<>
			else
				mzn_element_internal(x, (i, j, k, l))
			endif |
				i in 0..enum2int(max(idx_set.1)),
				j in 0..enum2int(max(idx_set.2)),
				k in 0..enum2int(max(idx_set.3)),
				l in 0..enum2int(max(idx_set.4)),
		],
	);
function array [$$E, $$F, $$G, $$H, $$I] of any $T: mzn_extend_array_opt(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
) =
	let {
		any: idx_set = index_sets(x);
	} in array5d(
		to_enum_internal(enum_of(idx_set.1), 0)..max(idx_set.1),
		to_enum_internal(enum_of(idx_set.2), 0)..max(idx_set.2),
		to_enum_internal(enum_of(idx_set.3), 0)..max(idx_set.3),
		to_enum_internal(enum_of(idx_set.4), 0)..max(idx_set.4),
		to_enum_internal(enum_of(idx_set.5), 0)..max(idx_set.5),
		[
			if i = 0 \/ j = 0 \/ k = 0 \/ l = 0 \/ m = 0 then
				<>
			else
				mzn_element_internal(x, (i, j, k, l, m))
			endif |
				i in 0..enum2int(max(idx_set.1)),
				j in 0..enum2int(max(idx_set.2)),
				k in 0..enum2int(max(idx_set.3)),
				l in 0..enum2int(max(idx_set.4)),
				m in 0..enum2int(max(idx_set.5)),
		],
	);
function array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: mzn_extend_array_opt(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
) =
	let {
		any: idx_set = index_sets(x);
	} in array6d(
		to_enum_internal(enum_of(idx_set.1), 0)..max(idx_set.1),
		to_enum_internal(enum_of(idx_set.2), 0)..max(idx_set.2),
		to_enum_internal(enum_of(idx_set.3), 0)..max(idx_set.3),
		to_enum_internal(enum_of(idx_set.4), 0)..max(idx_set.4),
		to_enum_internal(enum_of(idx_set.5), 0)..max(idx_set.5),
		to_enum_internal(enum_of(idx_set.6), 0)..max(idx_set.6),
		[
			if i = 0 \/ j = 0 \/ k = 0 \/ l = 0 \/ m = 0 \/ n = 0 then
				<>
			else
				mzn_element_internal(x, (i, j, k, l, m, n))
			endif |
				i in 0..enum2int(max(idx_set.1)),
				j in 0..enum2int(max(idx_set.2)),
				k in 0..enum2int(max(idx_set.3)),
				l in 0..enum2int(max(idx_set.4)),
				m in 0..enum2int(max(idx_set.5)),
				n in 0..enum2int(max(idx_set.6)),
		],
	);

% Par optional index validity check
test mzn_array_access_valid(array [$$E] of any $T: x, opt $$E: idx) =
	absent(idx) \/ mzn_array_access_valid(deopt(idx), index_set(x), 0);
test mzn_array_access_valid(array [$$E] of any $T: x, tuple(opt $$E): idx) =
	mzn_array_access_valid(x, idx.1);
test mzn_array_access_valid(array [$$E, $$F] of any $T: x, tuple(opt $$E, opt $$F): idx) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		absent(idx.1) \/ mzn_array_access_valid(mzn_destruct_opt(idx.1).2, idx_set.1, 1),
		absent(idx.2) \/ mzn_array_access_valid(mzn_destruct_opt(idx.2).2, idx_set.2, 2),
	]);
test mzn_array_access_valid(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(opt $$E, opt $$F, opt $$G): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		absent(idx.1) \/ mzn_array_access_valid(mzn_destruct_opt(idx.1).2, idx_set.1, 1),
		absent(idx.2) \/ mzn_array_access_valid(mzn_destruct_opt(idx.2).2, idx_set.2, 2),
		absent(idx.3) \/ mzn_array_access_valid(mzn_destruct_opt(idx.3).2, idx_set.3, 3),
	]);
test mzn_array_access_valid(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		absent(idx.1) \/ mzn_array_access_valid(mzn_destruct_opt(idx.1).2, idx_set.1, 1),
		absent(idx.2) \/ mzn_array_access_valid(mzn_destruct_opt(idx.2).2, idx_set.2, 2),
		absent(idx.3) \/ mzn_array_access_valid(mzn_destruct_opt(idx.3).2, idx_set.3, 3),
		absent(idx.4) \/ mzn_array_access_valid(mzn_destruct_opt(idx.4).2, idx_set.4, 4),
	]);
test mzn_array_access_valid(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		absent(idx.1) \/ mzn_array_access_valid(mzn_destruct_opt(idx.1).2, idx_set.1, 1),
		absent(idx.2) \/ mzn_array_access_valid(mzn_destruct_opt(idx.2).2, idx_set.2, 2),
		absent(idx.3) \/ mzn_array_access_valid(mzn_destruct_opt(idx.3).2, idx_set.3, 3),
		absent(idx.4) \/ mzn_array_access_valid(mzn_destruct_opt(idx.4).2, idx_set.4, 4),
		absent(idx.5) \/ mzn_array_access_valid(mzn_destruct_opt(idx.5).2, idx_set.5, 5),
	]);
test mzn_array_access_valid(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(opt $$E, opt $$F, opt $$G, opt $$H, opt $$I, opt $$J): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		absent(idx.1) \/ mzn_array_access_valid(mzn_destruct_opt(idx.1).2, idx_set.1, 1),
		absent(idx.2) \/ mzn_array_access_valid(mzn_destruct_opt(idx.2).2, idx_set.2, 2),
		absent(idx.3) \/ mzn_array_access_valid(mzn_destruct_opt(idx.3).2, idx_set.3, 3),
		absent(idx.4) \/ mzn_array_access_valid(mzn_destruct_opt(idx.4).2, idx_set.4, 4),
		absent(idx.5) \/ mzn_array_access_valid(mzn_destruct_opt(idx.5).2, idx_set.5, 5),
		absent(idx.6) \/ mzn_array_access_valid(mzn_destruct_opt(idx.6).2, idx_set.6, 6),
	]);

test mzn_array_access_known_valid(array [$$E] of any $T: x, var opt $$E: idx) =
	has_bounds(idx) /\ lb(idx)..ub(idx) subset index_set(x);
test mzn_array_access_known_valid(array [$$E] of any $T: x, tuple(var opt $$E): idx) =
	mzn_array_access_known_valid(x, idx.1);
test mzn_array_access_known_valid(
	array [$$E, $$F] of any $T: x,
	tuple(var opt $$E, var opt $$F): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		is_fixed(absent(idx.1)) /\ fix(absent(idx.1)) \/
			has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		is_fixed(absent(idx.2)) /\ fix(absent(idx.2)) \/
			has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
	]);
test mzn_array_access_known_valid(
	array [$$E, $$F, $$G] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		is_fixed(absent(idx.1)) /\ fix(absent(idx.1)) \/
			has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		is_fixed(absent(idx.2)) /\ fix(absent(idx.2)) \/
			has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
		is_fixed(absent(idx.3)) /\ fix(absent(idx.3)) \/
			has_bounds(idx.3) /\ lb(idx.3)..ub(idx.3) subset idx_set.3,
	]);
test mzn_array_access_known_valid(
	array [$$E, $$F, $$G, $$H] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		is_fixed(absent(idx.1)) /\ fix(absent(idx.1)) \/
			has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		is_fixed(absent(idx.2)) /\ fix(absent(idx.2)) \/
			has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
		is_fixed(absent(idx.3)) /\ fix(absent(idx.3)) \/
			has_bounds(idx.3) /\ lb(idx.3)..ub(idx.3) subset idx_set.3,
		is_fixed(absent(idx.4)) /\ fix(absent(idx.4)) \/
			has_bounds(idx.4) /\ lb(idx.4)..ub(idx.4) subset idx_set.4,
	]);
test mzn_array_access_known_valid(
	array [$$E, $$F, $$G, $$H, $$I] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		is_fixed(absent(idx.1)) /\ fix(absent(idx.1)) \/
			has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		is_fixed(absent(idx.2)) /\ fix(absent(idx.2)) \/
			has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
		is_fixed(absent(idx.3)) /\ fix(absent(idx.3)) \/
			has_bounds(idx.3) /\ lb(idx.3)..ub(idx.3) subset idx_set.3,
		is_fixed(absent(idx.4)) /\ fix(absent(idx.4)) \/
			has_bounds(idx.4) /\ lb(idx.4)..ub(idx.4) subset idx_set.4,
		is_fixed(absent(idx.5)) /\ fix(absent(idx.5)) \/
			has_bounds(idx.5) /\ lb(idx.5)..ub(idx.5) subset idx_set.5,
	]);
test mzn_array_access_known_valid(
	array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: x,
	tuple(var opt $$E, var opt $$F, var opt $$G, var opt $$H, var opt $$I, var opt $$J): idx,
) =
	let {
		any: idx_set = index_sets(x);
	} in forall([
		is_fixed(absent(idx.1)) /\ fix(absent(idx.1)) \/
			has_bounds(idx.1) /\ lb(idx.1)..ub(idx.1) subset idx_set.1,
		is_fixed(absent(idx.2)) /\ fix(absent(idx.2)) \/
			has_bounds(idx.2) /\ lb(idx.2)..ub(idx.2) subset idx_set.2,
		is_fixed(absent(idx.3)) /\ fix(absent(idx.3)) \/
			has_bounds(idx.3) /\ lb(idx.3)..ub(idx.3) subset idx_set.3,
		is_fixed(absent(idx.4)) /\ fix(absent(idx.4)) \/
			has_bounds(idx.4) /\ lb(idx.4)..ub(idx.4) subset idx_set.4,
		is_fixed(absent(idx.5)) /\ fix(absent(idx.5)) \/
			has_bounds(idx.5) /\ lb(idx.5)..ub(idx.5) subset idx_set.5,
		is_fixed(absent(idx.6)) /\ fix(absent(idx.6)) \/
			has_bounds(idx.6) /\ lb(idx.6)..ub(idx.6) subset idx_set.6,
	]);
