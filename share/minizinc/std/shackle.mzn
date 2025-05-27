include "stdlib.mzn";
include "solver_redefinitions.mzn";

include "internal/annotation.mzn";
include "internal/array.mzn";
include "internal/debug.mzn";
include "internal/compare.mzn";
include "internal/domain.mzn";
include "internal/element.mzn";
include "internal/element_opt.mzn";
include "internal/enum.mzn";
include "internal/ite.mzn";
include "internal/logic.mzn";
include "internal/opt.mzn";
include "internal/partiality.mzn";
include "internal/set.mzn";
include "internal/slice.mzn";
include "internal/string.mzn";

/** @group stdlib.builtins.compare Return if \a x is equal to \a y */
function var bool: '='(var $T: x, $T: y) :: mzn_internal_representation :: promise_commutative;
/** @group stdlib.builtins.compare Return if \a x is equal to \a y */
function var bool: '='($T: x, var $T: y) :: mzn_internal_representation :: promise_commutative;

/** @group stdlib.builtins.compare Return if \a x is not equal to \a y */
function var bool: '!='($T: x, var $T: y) :: mzn_internal_representation :: promise_commutative;
/** @group stdlib.builtins.compare Return if \a x is not equal to \a y */
function var bool: '!='(var $T: x, $T: y) :: mzn_internal_representation :: promise_commutative;

function bool: forall(array [int] of bool: x) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_forall_par", x);
function var bool: forall(array [int] of var bool: x) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_forall_var", x);
function bool: exists(array [int] of bool: x) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_exists_par", x);
function var bool: exists(array [int] of var bool: x) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_exists_var", x);

predicate count_eq(array [$X] of var opt bool: x, var bool: y, var int: c) =
	count_eq([bool2int(i) default 2 | i in x], y, c);
