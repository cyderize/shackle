include "stdlib.mzn";
include "solver_redefinitions.mzn";

include "internal/annotation.mzn";
include "internal/array.mzn";
include "internal/coercion.mzn";
include "internal/debug.mzn";
include "internal/domain.mzn";
include "internal/element.mzn";
include "internal/element_opt.mzn";
include "internal/enum.mzn";
include "internal/ite.mzn";
include "internal/opt.mzn";
include "internal/partiality.mzn";
include "internal/reflect.mzn";
include "internal/slice.mzn";
include "internal/string.mzn";

/** @group stdlib.builtins.compare Return if \a x is equal to \a y */
function var bool: '='(var $T: x, $T: y) :: mzn_internal_representation;
/** @group stdlib.builtins.compare Return if \a x is equal to \a y */
function var bool: '='($T: x, var $T: y) :: mzn_internal_representation;

/** @group stdlib.builtins.compare Return if \a x is not equal to \a y */
function var bool: '!='($T: x, var $T: y) :: mzn_internal_representation;
/** @group stdlib.builtins.compare Return if \a x is not equal to \a y */
function var bool: '!='(var $T: x, $T: y) :: mzn_internal_representation;

predicate count_eq(array [$X] of var opt bool: x, var bool: y, var int: c) =
	count_eq([if occurs(i) then bool2int(deopt(i)) else 2 endif | i in x], y, c);
