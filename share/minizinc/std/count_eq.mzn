include "fzn_count_eq.mzn";
include "fzn_count_eq_par.mzn";
include "fzn_count_eq_reif.mzn";
include "fzn_count_eq_par_reif.mzn";

/** @group globals.counting
  Constrains \a c to be the number of occurrences of \a y in \a x. 
*/
predicate count_eq(array[$X] of var $$E: x, var $$E: y, var int: c) =
    fzn_count_eq(erase_enum(array1d(x)),erase_enum(y),c);

/** @group globals.counting
  Constrains \a c to be the number of occurrences of \a y in \a x. 
*/
predicate count_eq(array[$X] of var $$E: x, $$E: y, int: c) =
    fzn_count_eq_par(erase_enum(array1d(x)),erase_enum(y),c);

predicate count_eq(array[$X] of $$E: x, $$E: y, int: c) =
    c=count_eq(x,y);

/** @group globals.counting
  Returns the number of occurrences of \a y in \a x. 
*/
function var int: count_eq(array[$X] of var $$E: x, var $$E: y) ::promise_total =
  let { var 0..length(x): c; constraint fzn_count_eq(erase_enum(array1d(x)),erase_enum(y),c); } in c;

function int: count_eq(array[$X] of $$E: x, $$E: y) =
              sum(v in array1d(x))(v = y);

%-----------------------------------------------------------------------------%
