//! Code formatting for MiniZinc

use format::Format;
pub use options::MiniZincFormatOptions;
use shackle_diagnostics::{Result, SourceFile};
use shackle_syntax::{InputLang, cst, minizinc::MznModel};

use crate::format::MiniZincFormatter;

pub(crate) mod container;
pub(crate) mod expression;
pub(crate) mod format;
pub(crate) mod ir;
pub(crate) mod item;
pub(crate) mod options;
pub(crate) mod pattern;
pub(crate) mod types;

#[cfg(feature = "fancy")]
mod fancy;
#[cfg(feature = "fancy")]
pub use fancy::*;

/// Format the given source code
pub fn format(source: &SourceFile, options: &MiniZincFormatOptions) -> Result<String> {
	let cst = cst::Cst::new(source.contents(), InputLang::MiniZinc);
	cst.check(source)?;
	let model = MznModel::new(cst);
	Ok(MiniZincFormatter::new(source.contents(), &model, options).format())
}

/// Format the given source code from a string
pub fn format_str(source: impl AsRef<str>, options: &MiniZincFormatOptions) -> Result<String> {
	let source = SourceFile::unnamed(source.as_ref().to_owned());
	format(&source, options)
}

/// Get IR for debugging
pub fn format_debug(source: &SourceFile, options: &MiniZincFormatOptions) -> Result<String> {
	let cst = cst::Cst::new(source.contents(), InputLang::MiniZinc);
	cst.check(source)?;
	let model = MznModel::new(cst);
	let mut formatter = MiniZincFormatter::new(source.contents(), &model, options);
	Ok(format!("{:#?}", model.format(&mut formatter)))
}

/// Get IR for debugging from a string
pub fn format_str_debug(
	source: impl AsRef<str>,
	options: &MiniZincFormatOptions,
) -> Result<String> {
	let source = SourceFile::unnamed(source.as_ref().to_owned());
	format_debug(&source, options)
}

#[cfg(test)]
mod tests {
	use expect_test::expect;
	use glob as _;
	use pretty_assertions as _;
	use shackle_hir as _;

	use super::*;

	#[test]
	fn test_format() {
		let actual = format_str(
			r#"
			a = albatross + bonobo + cassowary;
			c = albatross + bonobo + cassowary + dinosaur + elephant + frog + giraffe + hyena + iguana + jaguar + kangaroo + llama;
			c = [albatross, bonobo, cassowary];
			d = [albatross, bonobo, cassowary, dinosaur, elephant, frog, giraffe, hyena, iguana, jaguar, kangaroo, llama];
			e = {albatross, bonobo, cassowary};
			f = {albatross, bonobo, cassowary, dinosaur, elephant, frog, giraffe, hyena, iguana, jaguar, kangaroo, llama};
			g = [abacus + banana | abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana)];
			h = [(i, j): albatross * (bonobo + cassowary) + dinosaur * (elephant + frog) + giraffe * (hyena + iguana) + jaguar + kangaroo + llama | abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana), abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana), abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana)];
			i = {abacus + banana | abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana)};
			j = {albatross * (bonobo + cassowary) + dinosaur * (elephant + frog) + giraffe * (hyena + iguana) + jaguar + kangaroo + llama | abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana), abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana), abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana)};
			k = [| a, b, c, d |];
			l = [| a, b | c, d |];
			m = [| a: b: | i: c, d | j: e, f |];
			"#,
			&Default::default(),
		);
		let expected = expect![[r#"
    a = albatross + bonobo + cassowary;
    c =
    	albatross +
    		bonobo +
    		cassowary +
    		dinosaur +
    		elephant +
    		frog +
    		giraffe +
    		hyena +
    		iguana +
    		jaguar +
    		kangaroo +
    		llama;
    c = [albatross, bonobo, cassowary];
    d = [
    	albatross,
    	bonobo,
    	cassowary,
    	dinosaur,
    	elephant,
    	frog,
    	giraffe,
    	hyena,
    	iguana,
    	jaguar,
    	kangaroo,
    	llama,
    ];
    e = {albatross, bonobo, cassowary};
    f = {
    	albatross,
    	bonobo,
    	cassowary,
    	dinosaur,
    	elephant,
    	frog,
    	giraffe,
    	hyena,
    	iguana,
    	jaguar,
    	kangaroo,
    	llama,
    };
    g = [abacus + banana | abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana)];
    h = [
    	(i, j): albatross * (bonobo + cassowary) +
    		dinosaur * (elephant + frog) +
    		giraffe * (hyena + iguana) +
    		jaguar +
    		kangaroo +
    		llama |
    		abacus in foo(1, 2, 3),
    		banana in bar(1, 2, 3) where qux(abacus, banana),
    		abacus in foo(1, 2, 3),
    		banana in bar(1, 2, 3) where qux(abacus, banana),
    		abacus in foo(1, 2, 3),
    		banana in bar(1, 2, 3) where qux(abacus, banana),
    ];
    i = {abacus + banana | abacus in foo(1, 2, 3), banana in bar(1, 2, 3) where qux(abacus, banana)};
    j = {
    	albatross * (bonobo + cassowary) +
    		dinosaur * (elephant + frog) +
    		giraffe * (hyena + iguana) +
    		jaguar +
    		kangaroo +
    		llama |
    		abacus in foo(1, 2, 3),
    		banana in bar(1, 2, 3) where qux(abacus, banana),
    		abacus in foo(1, 2, 3),
    		banana in bar(1, 2, 3) where qux(abacus, banana),
    		abacus in foo(1, 2, 3),
    		banana in bar(1, 2, 3) where qux(abacus, banana),
    };
    k = [| a, b, c, d |];
    l = [|
    	a, b |
    	c, d
    |];
    m = [|
    	a: b: |
    	i: c, d |
    	j: e, f
    |];
"#]];
		expected.assert_eq(&actual.unwrap());
	}

	#[test]
	fn test_parentheses() {
		let actual = format_str(
			r#"
			a = (1 + (2 * 3) - 4) + 5;
			b = (1 + 2) * 3 - (4 + 5);
			c = -2 * 3;
			d = -((2 * 3));
			"#,
			&MiniZincFormatOptions {
				keep_parentheses: false,
				..Default::default()
			},
		);
		let expected = expect![[r#"
    a = 1 + 2 * 3 - 4 + 5;
    b = (1 + 2) * 3 - (4 + 5);
    c = -2 * 3;
    d = -(2 * 3);
"#]];
		expected.assert_eq(&actual.unwrap());
	}

	#[test]
	fn test_keep_parentheses() {
		let actual = format_str(
			r#"
			a = (1 + (2 * 3) - 4) + 5;
			b = -((2 * 3));
			"#,
			&MiniZincFormatOptions {
				keep_parentheses: true,
				..Default::default()
			},
		);
		let expected = expect![[r#"
    a = (1 + (2 * 3) - 4) + 5;
    b = -(2 * 3);
"#]];
		expected.assert_eq(&actual.unwrap());
	}

	#[test]
	fn test_format_types() {
		let actual = format_str(
			r#"
			int: x;
			var float: y;
			var opt 1..3:z;
			tuple(var 1..3,2..4,int): a;
			record(var 1..3: a, var int:b): b;
			"#,
			&Default::default(),
		);
		let expected = expect![[r#"
    int: x;
    var float: y;
    var opt 1..3: z;
    tuple(var 1..3, 2..4, int): a;
    record(var 1..3: a, var int: b): b;
"#]];
		expected.assert_eq(&actual.unwrap());
	}

	#[test]
	fn test_format_extras() {
		let actual = format_str(
			r#"


			% Foo
			/* hello */
			int: x = (1 /* foo */ + /* bar */ 2) + 3 % hello
			;


			/* hello */ /* hello */


			% bar
			int /* foo */:y= 3;
			/* world */

			/* one */
			% Hello
			"#,
			&MiniZincFormatOptions {
				keep_parentheses: false,
				..Default::default()
			},
		);
		let expected = expect![[r#"
    % Foo
    /* hello */
    int: x =
    	1 + /* foo */
    		/* bar */
    		2 +
    		3; % hello

    /* hello */
    /* hello */

    % bar
    int: y = 3; /* foo */
    /* world */

    /* one */
    % Hello
"#]];
		expected.assert_eq(&actual.unwrap());
	}

	#[test]
	fn test_format_extras_2() {
		let actual = format_str(
			r#"
			% Foo
			int: y;
			"#,
			&Default::default(),
		);
		let expected = expect![[r#"
    % Foo
    int: y;
"#]];
		expected.assert_eq(&actual.unwrap());
	}

	#[test]
	fn test_format_comprehension() {
		let actual = format_str(
			"constraint [a_really_long_word_here_which_overflows_a_really_long_word_here_which_overflows | j in 1..max(country)];",
			&Default::default(),
		);
		let expected = expect![[r#"
    constraint [
    	a_really_long_word_here_which_overflows_a_really_long_word_here_which_overflows |
    		j in 1..max(country),
    ];
"#]];
		expected.assert_eq(&actual.unwrap());
	}

	#[test]
	fn test_attach_comments_infix() {
		let actual = format_str(
			r#"
			any: x =
				albatross+ % a
				/* b */ bonobo+ % b
				cassowary+ /* c */
				dinosaur+ /* d */ % d
				elephant+
				frog+
				giraffe+
				hyena+
				iguana+
				jaguar+
				kangaroo+
				llama;
			"#,
			&Default::default(),
		);
		let expected = expect![[r#"
    any: x =
    	albatross + % a
    		/* b */
    		bonobo + % b
    		cassowary + /* c */
    		dinosaur + /* d */ % d
    		elephant +
    		frog +
    		giraffe +
    		hyena +
    		iguana +
    		jaguar +
    		kangaroo +
    		llama;
"#]];
		expected.assert_eq(&actual.unwrap());
	}

	#[test]
	fn test_debug_format() {
		let actual = format_str_debug(
			r#"
			int: x = 1;
			"#,
			&Default::default(),
		);
		let expected = expect![[r#"
    Element::sequence(
        [
            Element::sequence(
                [
                    Element::sequence(
                        [
                            Element::text(
                                "int",
                            ),
                            Element::text(
                                ": ",
                            ),
                            Element::text(
                                "x",
                            ),
                            Element::sequence(
                                [],
                            ),
                            Element::text(
                                " =",
                            ),
                            Element::group(
                                Element::indent(
                                    Element::sequence(
                                        [
                                            Element::sequence(
                                                [
                                                    Element::if_broken(
                                                        Element::line_break(),
                                                    ),
                                                    Element::if_unbroken(
                                                        Element::text(
                                                            " ",
                                                        ),
                                                    ),
                                                ],
                                            ),
                                            Element::text(
                                                "1",
                                            ),
                                        ],
                                    ),
                                ),
                            ),
                        ],
                    ),
                    Element::text(
                        ";",
                    ),
                ],
            ),
            Element::line_break(),
        ],
    )"#]];
		expected.assert_eq(&actual.unwrap());
	}
}
