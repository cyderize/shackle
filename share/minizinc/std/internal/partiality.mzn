% Helper for totalising functions
function var $T: mzn_totalise(var bool: defined, var $T: value, $T: 'default') =
	let { constraint defined } in if defined then value else 'default' endif;
