var bool: p = true;
var bool: q = let { constraint p } in true;
var 1..3: x;
function var int: foo(var int: v) = v div 2 + 1;
constraint x = foo(x);
solve maximize x + 2;
