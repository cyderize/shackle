/** @group stdlib.builtins.compare Return if \a x is equal to \a y */
function var bool: '='(var $T: x, $T: y) :: mzn_internal_representation :: promise_commutative;
/** @group stdlib.builtins.compare Return if \a x is equal to \a y */
function var bool: '='($T: x, var $T: y) :: mzn_internal_representation :: promise_commutative;

/** @group stdlib.builtins.compare Return if \a x is not equal to \a y */
function var bool: '!='($T: x, var $T: y) :: mzn_internal_representation :: promise_commutative;
/** @group stdlib.builtins.compare Return if \a x is not equal to \a y */
function var bool: '!='(var $T: x, $T: y) :: mzn_internal_representation :: promise_commutative;
