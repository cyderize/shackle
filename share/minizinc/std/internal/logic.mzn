function bool: '/\'(bool: x, bool: y) :: mzn_inline_call_by_name = mzn_builtin("mzn_and_par", x, y);
function var bool: '/\'(var bool: x, var bool: y) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_and_var", x, y);

function bool: '\/'(bool: x, bool: y) :: mzn_inline_call_by_name = mzn_builtin("mzn_or_par", x, y);
function var bool: '\/'(var bool: x, var bool: y) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_or_var", x, y);

function bool: 'not'(bool: x) :: mzn_inline_call_by_name = mzn_builtin("mzn_not_par", x);
function var bool: 'not'(var bool: x) :: mzn_inline_call_by_name = mzn_builtin("mzn_not_var", x);

function bool: forall(array [$T] of bool: x) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_forall_par", x);
function var bool: forall(array [$T] of var bool: x) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_forall_var", x);

function bool: exists(array [$T] of bool: x) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_exists_par", x);
function var bool: exists(array [$T] of var bool: x) :: mzn_inline_call_by_name =
	mzn_builtin("mzn_exists_var", x);
