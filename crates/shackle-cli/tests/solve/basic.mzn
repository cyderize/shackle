var 1..10: x;
var 1..10: y;
constraint x + y = 15;
output ["x = \(x);\ny = \(y);\n"];
solve satisfy;
