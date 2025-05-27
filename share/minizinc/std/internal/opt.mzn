function opt $T: mzn_construct_opt(tuple(bool, $T): x);
function var opt $T: mzn_construct_opt(tuple(var bool, var $T): x);

function array [$U] of opt $T: mzn_construct_array_opt(array [$U] of tuple(bool, $T): x);
function array [$U] of var opt $T: mzn_construct_array_opt(
	array [$U] of tuple(var bool, var $T): x,
);

function tuple(bool, $T): mzn_destruct_opt(opt $T: x);
function tuple(var bool, var $T): mzn_destruct_opt(var opt $T: x);

function array [$U] of tuple(bool, $T): mzn_destruct_array_opt(array [$U] of opt $T: x);
function array [$U] of tuple(var bool, var $T): mzn_destruct_array_opt(array [$U] of var opt $T: x);

predicate mzn_opt_channel(var opt int: x, set of int: s) =
	let {
		any: (bx, dx) = mzn_destruct_opt(x);
	} in if 0 in s then not bx -> dx = 0 else not bx <-> dx = 0 endif;
predicate mzn_opt_channel(var opt float: x, set of float: s) =
	let {
		any: (bx, dx) = mzn_destruct_opt(x);
	} in if 0.0 in s then not bx -> dx = 0.0 else not bx <-> dx = 0.0 endif;
function set of int: mzn_opt_domain(set of int: s) = s union {0};
function set of float: mzn_opt_domain(set of float: s) = s union {0.0};

annotation mzn_opt_bool;
