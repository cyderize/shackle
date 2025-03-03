
/** @group stdlib.builtins.array Return index sets of the array \a x */
function tuple(set of $$E): index_sets(array [$$E] of any $U: x) = mzn_builtin("index_sets", x);

/** @group stdlib.builtins.array Return index sets of the array \a x */
function tuple(set of $$E, set of $$F): index_sets(
	array [$$E, $$F] of any $U: x,
) = mzn_builtin("index_sets", x);

/** @group stdlib.builtins.array Return index sets of the array \a x */
function tuple(set of $$E, set of $$F, set of $$G): index_sets(
	array [$$E, $$F, $$G] of any $U: x,
) = mzn_builtin("index_sets", x);

/** @group stdlib.builtins.array Return index sets of the array \a x */
function tuple(set of $$E, set of $$F, set of $$G, set of $$H): index_sets(
	array [$$E, $$F, $$G, $$H] of any $U: x,
) = mzn_builtin("index_sets", x);

/** @group stdlib.builtins.array Return index sets of the array \a x */
function tuple(
	set of $$E,
	set of $$F,
	set of $$G,
	set of $$H,
	set of $$I,
): index_sets(array [$$E, $$F, $$G, $$H, $$I] of any $U: x) = mzn_builtin("index_sets", x);

/** @group stdlib.builtins.array Return index sets of the array \a x */
function tuple(
	set of $$E,
	set of $$F,
	set of $$G,
	set of $$H,
	set of $$I,
	set of $$J,
): index_sets(array [$$E, $$F, $$G, $$H, $$I, $$J] of any $U: x) = mzn_builtin("index_sets", x);

function array [$$E] of any $T: mzn_array_kd(tuple(set of $$E): idx, array[$U] of any $T: x) = mzn_builtin("array_kd", idx, x);
function array [$$E, $$F] of any $T: mzn_array_kd(tuple(set of $$E, set of $$F): idx, array[$U] of any $T: x) = mzn_builtin("array_kd", idx, x);
function array [$$E, $$F, $$G] of any $T: mzn_array_kd(tuple(set of $$E, set of $$F, set of $$G): idx, array[$U] of any $T: x) = mzn_builtin("array_kd", idx, x);
function array [$$E, $$F, $$G, $$H, ] of any $T: mzn_array_kd(tuple(set of $$E, set of $$F, set of $$G, set of $$H): idx, array[$U] of any $T: x) = mzn_builtin("array_kd", idx, x);
function array [$$E, $$F, $$G, $$H, $$I] of any $T: mzn_array_kd(tuple(set of $$E, set of $$F, set of $$G, set of $$H, set of $$I): idx, array[$U] of any $T: x) = mzn_builtin("array_kd", idx, x);
function array [$$E, $$F, $$G, $$H, $$I, $$J] of any $T: mzn_array_kd(tuple(set of $$E, set of $$F, set of $$G, set of $$H, set of $$I, set of $$J): idx, array[$U] of any $T: x) = mzn_builtin("array_kd", idx, x);

function var set of int: mzn_array2set(array [int] of var int: x) :: promise_total =
	if length(x) = 0 then
		{}
	else
		let {
			set of int: D = dom_array(x);
			constraint
				assert(
					min(D) > -infinity /\ max(D) < infinity,
					"array2set needs finite bounds on argument array",
				);
			var set of D: y;
			constraint fzn_array_int_union(x, y);
		} in y
	endif;

function var set of int: mzn_array2set(array [int] of var opt int: x)
	:: promise_total =
	if length(x) = 0 then
		{}
	else
		let {
			set of int: D = dom_array_occurring(x);
			constraint
				assert(
					min(D) > -infinity /\ max(D) < infinity,
					"array2set needs finite bounds on argument array",
				);
			var set of D: y;
			constraint fzn_array_opt_int_union(x, y);
		} in y
	endif;

function set of int: mzn_array2set(array [int] of int: x) = mzn_builtin("array2set", x);

function set of int: mzn_array2set(array [int] of opt int: x) = {
	deopt(x[i]) |
		i in index_set(x) where occurs(x[i]),
};

function set of bool: mzn_array2set(array [int] of bool: x) =
	let {
		bool: f = exists (b in x) (not b);
		bool: t = exists (b in x) (b);
	} in if f /\ t then
		{false, true}
	elseif f then
		{false}
	elseif t then
		{true}
	else
		{}
	endif;

function set of float: mzn_array2set(array [int] of float: x) = mzn_builtin("array2set", x);

function set of $$E: mzn_array2set(array [int] of $$E: x) :: mzn_unreachable;
