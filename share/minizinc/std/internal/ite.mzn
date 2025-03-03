function var int: if_then_else_root(
	array [int] of var bool: c,
	array [int] of int: x
) = let {
  var lb_array(x)..ub_array(x): y;
  constraint fzn_if_then_else_int(c, x, y);
} in y;
function var int: if_then_else(
	array [int] of var bool: c,
	array [int] of int: x
) = if_then_else_root(c, x);

function var int: if_then_else_root(
	array [int] of var bool: c,
	array [int] of var int: x,
) = let {
  var lb_array(x)..ub_array(x): y;
  constraint fzn_if_then_else_var_int(c, x, y);
} in y;
function var int: if_then_else(
	array [int] of var bool: c,
	array [int] of var int: x,
) = if_then_else_root(c, x);

function var bool: if_then_else_root(
	array [int] of var bool: c,
	array [int] of bool: x,
) = let {
  var bool: y;
  constraint fzn_if_then_else_bool(c, x, y);
} in y;
function var bool: if_then_else(
	array [int] of var bool: c,
	array [int] of bool: x,
) = if_then_else_root(c, x);

function var bool: if_then_else_root(
	array [int] of var bool: c,
	array [int] of var bool: x,
) = let {
  var bool: y;
  constraint fzn_if_then_else_var_bool(c, x, y);
} in y;
function var bool: if_then_else(
	array [int] of var bool: c,
	array [int] of var bool: x,
) = if_then_else_root(c, x);

function var float: if_then_else_root(
	array [int] of var bool: c,
	array [int] of float: x,
) = let {
  var lb_array(x)..ub_array(x): y;
  constraint fzn_if_then_else_float(c, x, y);
} in y;
function var float: if_then_else(
	array [int] of var bool: c,
	array [int] of float: x,
) = if_then_else_root(c, x);

function var float: if_then_else_root(
	array [int] of var bool: c,
	array [int] of var float: x,
) = let {
  var lb_array(x)..ub_array(x): y;
  constraint fzn_if_then_else_var_float(c, x, y);
} in y;
function var float: if_then_else(
	array [int] of var bool: c,
	array [int] of var float: x,
) = if_then_else_root(c, x);

function var set of int: if_then_else_root(
	array [int] of var bool: c,
	array [int] of set of int: x,
) = let {
  var set of min(ub_array(x))..max(ub_array(x)): y;
  constraint fzn_if_then_else_set(c, x, y);
} in y;
function var set of int: if_then_else(
	array [int] of var bool: c,
	array [int] of set of int: x,
) = if_then_else_root(c, x);

function var set of int: if_then_else_root(
	array [int] of var bool: c,
	array [int] of var set of int: x,
) = let {
  var set of min(ub_array(x))..max(ub_array(x)): y;
  constraint fzn_if_then_else_var_set(c, x, y);
} in y;
function var set of int: if_then_else(
	array [int] of var bool: c,
	array [int] of var set of int: x,
) = if_then_else_root(c, x);
