include "stdlib.mzn";
include "solver_redefinitions.mzn";

include "internal/annotation.mzn";
include "internal/array.mzn";
include "internal/debug.mzn";
include "internal/domain.mzn";
include "internal/element.mzn";
include "internal/element_opt.mzn";
include "internal/enum.mzn";
include "internal/ite.mzn";
include "internal/opt.mzn";
include "internal/partiality.mzn";
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
