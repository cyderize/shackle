% Definitions to keep verbatim for compatibility with old MiniZinc

function $T: 'default'(opt $T: x, $T: y);
function opt $T: 'default'(opt $T: x, opt $T: y);
function var $T: 'default'(var opt $T: x, var $T: y);
function var opt $T: 'default'(var opt $T: x, var opt $T: y);
function array [$U] of $T: 'default'(array [$U] of $T: x, array [$U] of $T: y);
function array [$U] of opt $T: 'default'(
	array [$U] of opt $T: x,
	array [$U] of opt $T: y,
);
function array [$U] of var $T: 'default'(
	array [$U] of var $T: x,
	array [$U] of var $T: y,
);
function array [$U] of var opt $T: 'default'(
	array [$U] of var opt $T: x,
	array [$U] of var opt $T: y,
);

function ann: mzn_internal_warm_start(array [$X] of var bool: x, array [$X] of bool: v) = 
  assert(
    index_sets_agree(x, v),
    "warm_start: x and v must have identical index sets",
    warm_start(array1d(x), array1d(v)) :: mzn_internal_representation
  );

function ann: mzn_internal_warm_start(array [$X] of var $$E: x, array [$X] of $$E: v) = 
  assert(
    index_sets_agree(x, v),
    "warm_start: x and v must have identical index sets",
    warm_start(array1d(enum2int(x)), array1d(enum2int(v))) :: mzn_internal_representation
  );

function ann: mzn_internal_warm_start(array [$X] of var float: x, array [$X] of float: v) = 
  assert(
    index_sets_agree(x, v),
    "warm_start: x and v must have identical index sets",
    warm_start(array1d(x), array1d(v)) :: mzn_internal_representation
  );

function ann: mzn_internal_warm_start(array [$X] of var set of $$E: x, array [$X] of set of $$E: v) = 
  assert(
    index_sets_agree(x, v),
    "warm_start: x and v must have identical index sets",
    warm_start(array1d(enum2int(x)), array1d(enum2int(v))) :: mzn_internal_representation
  );


function any $T: mzn_element_internal(array [int] of any $T: x, int: index) =
	x[index];
function any $T: mzn_element_internal(
	array [int] of any $T: x,
	tuple(int): index,
) = x[index.1];
function any $T: mzn_element_internal(
	array [int, int] of any $T: x,
	tuple(int, int): indices,
) = x[indices.1, indices.2];
function any $T: mzn_element_internal(
	array [int, int, int] of any $T: x,
	tuple(int, int, int): indices,
) = x[indices.1, indices.2, indices.3];
function any $T: mzn_element_internal(
	array [int, int, int, int] of any $T: x,
	tuple(int, int, int, int): indices,
) = x[indices.1, indices.2, indices.3, indices.4];
function any $T: mzn_element_internal(
	array [int, int, int, int, int] of any $T: x,
	tuple(int, int, int, int, int): indices,
) = x[indices.1, indices.2, indices.3, indices.4, indices.5];
function any $T: mzn_element_internal(
	array [int, int, int, int, int, int] of any $T: x,
	tuple(int, int, int, int, int, int): indices,
) = x[indices.1, indices.2, indices.3, indices.4, indices.5, indices.6];

