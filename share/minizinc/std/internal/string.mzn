function string: mzn_show_tuple_access(string: x, int: i) = x ++ ".\(i)";
function string: mzn_show_record_access(string: x, string: f) = x ++ "." ++ f;

function string: mzn_show_array_access(string: x, $$E: i) = x ++ "[" ++ show(i) ++ "]";
function string: mzn_show_array_access(string: x, tuple($$E): i) = x ++ "[" ++ show(i.1) ++ "]";
function string: mzn_show_array_access(string: x, tuple($$E, $$F): i) =
	x ++ "[" ++ show(i.1) ++ "," ++ show(i.2) ++ "]";
function string: mzn_show_array_access(string: x, tuple($$E, $$F, $$G): i) =
	x ++ "[" ++ show(i.1) ++ "," ++ show(i.2) ++ "," ++ show(i.3) ++ "]";
function string: mzn_show_array_access(string: x, tuple($$E, $$F, $$G, $$H): i) =
	x ++ "[" ++ show(i.1) ++ "," ++ show(i.2) ++ "," ++ show(i.3) ++ "," ++ show(i.4) ++ "]";
function string: mzn_show_array_access(string: x, tuple($$E, $$F, $$G, $$H, $$I): i) =
	x ++
		"[" ++
		show(i.1) ++
		"," ++
		show(i.2) ++
		"," ++
		show(i.3) ++
		"," ++
		show(i.4) ++
		"," ++
		show(i.5) ++
		"]";
function string: mzn_show_array_access(string: x, tuple($$E, $$F, $$G, $$H, $$I, $$J): i) =
	x ++
		"[" ++
		show(i.1) ++
		"," ++
		show(i.2) ++
		"," ++
		show(i.3) ++
		"," ++
		show(i.4) ++
		"," ++
		show(i.5) ++
		"," ++
		show(i.6) ++
		"]";
