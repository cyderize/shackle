//! AST representation of Eprime Expressions

use super::{BooleanLiteral, Domain, Infinity, IntegerLiteral, MatrixLiteral, StringLiteral};
use crate::ast::{
	AstNode, Children, ast_enum, ast_node, child_with_field_name, children_with_field_name,
	optional_child_with_field_name,
};

ast_enum!(
	/// Expression
	Expression,
	"boolean_literal" => BooleanLiteral,
	"integer_literal" => IntegerLiteral,
	"string_literal" => StringLiteral,
	"matrix_literal" => MatrixLiteral,
	"infinity" => Infinity,
	"call" => Call,
	"identifier" => Identifier,
	"indexed_access" => ArrayAccess,
	"infix_operator" => InfixOperator,
	"prefix_operator" => PrefixOperator,
	"unary_set_constructor" => UnarySetConstructor,
	"quantification" => Quantification,
	"matrix_comprehension" => MatrixComprehension,
	"absolute_operator" => AbsoluteOperator,
	"set_constructor" => SetConstructor,
	"parenthesised_expression" => "expression" // Turn parenthesised_expression into Expression node
);

ast_node!(
	/// Call
	Call,
	function,
	arguments
);

impl<'tree> Call<'tree> {
	/// Get the name of this call
	pub fn function(&self) -> Identifier<'tree> {
		child_with_field_name(self, "function")
	}

	/// Get the arguments of this call
	pub fn arguments(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "argument")
	}
}

ast_node!(
	/// Identifier
	Identifier
);

impl<'tree> Identifier<'tree> {
	/// Get the name of this identifier
	pub fn name<'a>(&self, source: &'a str) -> &'a str {
		self.cst_text(source)
	}
}

ast_node!(
	/// Indexed Access
	ArrayAccess,
	collection,
	indices
);

impl<'tree> ArrayAccess<'tree> {
	/// Get the collection of this indexed access
	pub fn collection(&self) -> Expression<'tree> {
		child_with_field_name(self, "collection")
	}

	/// Get the index of this indexed access
	pub fn indices(&self) -> Children<'tree, ArrayIndex<'tree>> {
		children_with_field_name(self, "index")
	}
}

ast_enum!(
	/// Array Index
	ArrayIndex,
	".." => IndexSlice, // This might be bad
	_ => Expression,
);

ast_node!(
	/// Slicing operator for indexed array access
	IndexSlice,
	operator,
);

impl<'tree> IndexSlice<'tree> {
	/// Get the name of this array slice
	pub fn operator(&self) -> &str {
		self.cst_kind()
	}
}

ast_node!(
	/// Infix Operator
	InfixOperator,
	operator,
	left,
	right
);

impl<'tree> InfixOperator<'tree> {
	/// Get the operator of this infix operator
	pub fn operator(&self) -> Operator<'tree> {
		child_with_field_name(self, "operator")
	}

	/// Get the left expression of this infix operator
	pub fn left(&self) -> Expression<'tree> {
		child_with_field_name(self, "left")
	}

	/// Get the right expression of this infix operator
	pub fn right(&self) -> Expression<'tree> {
		child_with_field_name(self, "right")
	}
}

ast_node!(
	/// Prefix Operator
	PrefixOperator,
	operator,
	operand
);

impl<'tree> PrefixOperator<'tree> {
	/// Get the operator of this prefix operator
	pub fn operator(&self) -> Operator<'tree> {
		child_with_field_name(self, "operator")
	}

	/// Get the operand of this prefix operator
	pub fn operand(&self) -> Expression<'tree> {
		child_with_field_name(self, "operand")
	}
}

ast_node!(
	/// Prefix Operator
	UnarySetConstructor,
	operator,
	operand
);

impl<'tree> UnarySetConstructor<'tree> {
	/// Get the operator of this unary operator
	pub fn operator(&self) -> Operator<'tree> {
		child_with_field_name(self, "operator")
	}

	/// Get the operand of this unary operator
	pub fn operand(&self) -> Expression<'tree> {
		child_with_field_name(self, "operand")
	}
}

ast_node!(
	/// An operator node
	Operator,
	name,
);

impl<'tree> Operator<'tree> {
	/// The name of the operator
	pub fn name(&self) -> &str {
		self.cst_kind()
	}
}

ast_node!(
	/// Quantification
	Quantification,
	function,
	generator,
	template,
);

impl<'tree> Quantification<'tree> {
	/// Get the function of this quantification
	pub fn function(&self) -> Identifier<'tree> {
		child_with_field_name(self, "function")
	}

	/// Get the generator of this quantification
	pub fn generator(&self) -> Generator<'tree> {
		child_with_field_name(self, "generator")
	}

	/// Get the template of this quantification
	pub fn template(&self) -> Expression<'tree> {
		child_with_field_name(self, "template")
	}
}

ast_node!(
	/// Generator
	Generator,
	names,
	collection,
);

impl<'tree> Generator<'tree> {
	/// Get the name of this generator
	pub fn names(&self) -> Children<'tree, Identifier<'tree>> {
		children_with_field_name(self, "name")
	}

	/// Get the collection of this generator
	pub fn collection(&self) -> Domain<'tree> {
		child_with_field_name(self, "collection")
	}
}

ast_node!(
	/// Matrix Comprehension
	MatrixComprehension,
	template,
	generators,
	conditions,
	indices
);

impl<'tree> MatrixComprehension<'tree> {
	/// Get the template of this matrix comprehension
	pub fn template(&self) -> Expression<'tree> {
		child_with_field_name(self, "template")
	}

	/// Get the generators of this matrix comprehension
	pub fn generators(&self) -> Children<'tree, Generator<'tree>> {
		children_with_field_name(self, "generator")
	}

	/// Get the conditions of this matrix comprehension
	pub fn conditions(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "condition")
	}

	/// Get the index of this matrix comprehension
	pub fn indices(&self) -> Option<Domain<'tree>> {
		optional_child_with_field_name(self, "index")
	}
}

ast_node!(
	/// Absolute operator
	AbsoluteOperator,
	operand,
);

impl<'tree> AbsoluteOperator<'tree> {
	/// Get the operand of this absolute operator
	pub fn operand(&self) -> Expression<'tree> {
		child_with_field_name(self, "operand")
	}
}

ast_node!(
	/// Infix Operator
	SetConstructor,
	operator,
	left,
	right
);

impl<'tree> SetConstructor<'tree> {
	/// Get the operator of this set operator
	pub fn operator(&self) -> Operator<'tree> {
		child_with_field_name(self, "operator")
	}

	/// Get the left expression of this set operator
	pub fn left(&self) -> Expression<'tree> {
		child_with_field_name(self, "left")
	}

	/// Get the right expression of this set operator
	pub fn right(&self) -> Expression<'tree> {
		child_with_field_name(self, "right")
	}
}

#[cfg(test)]
mod tests {
	use expect_test::expect;

	use crate::ast::tests::check_ast_eprime;

	#[test]
	fn test_call() {
		check_ast_eprime(
			"letting simple = toVec(X,Y)",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: Call(
                            Call {
                                cst_kind: "call",
                                function: Identifier {
                                    cst_kind: "identifier",
                                },
                                arguments: [
                                    Identifier(
                                        Identifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                    Identifier(
                                        Identifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                ],
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_indexed_access() {
		check_ast_eprime(
			r#"
            letting single = M[i]
            letting slice = Ms[..]
            "#,
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: ArrayAccess(
                            ArrayAccess {
                                cst_kind: "indexed_access",
                                collection: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                                indices: [
                                    Expression(
                                        Identifier(
                                            Identifier {
                                                cst_kind: "identifier",
                                            },
                                        ),
                                    ),
                                ],
                            },
                        ),
                        domain: None,
                    },
                ),
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: ArrayAccess(
                            ArrayAccess {
                                cst_kind: "indexed_access",
                                collection: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                                indices: [
                                    IndexSlice(
                                        IndexSlice {
                                            cst_kind: "..",
                                            operator: "..",
                                        },
                                    ),
                                ],
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_infix_operator() {
		check_ast_eprime(
			r#"
            letting different = x != y
            letting smallerlex = x <lex y
            letting and = x /\ y
            letting equiv = x <=> y
            letting exponent = x ** y
            "#,
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: InfixOperator(
                            InfixOperator {
                                cst_kind: "infix_operator",
                                operator: Operator {
                                    cst_kind: "!=",
                                    name: "!=",
                                },
                                left: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                                right: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: InfixOperator(
                            InfixOperator {
                                cst_kind: "infix_operator",
                                operator: Operator {
                                    cst_kind: "<lex",
                                    name: "<lex",
                                },
                                left: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                                right: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: InfixOperator(
                            InfixOperator {
                                cst_kind: "infix_operator",
                                operator: Operator {
                                    cst_kind: "/\\",
                                    name: "/\\",
                                },
                                left: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                                right: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: InfixOperator(
                            InfixOperator {
                                cst_kind: "infix_operator",
                                operator: Operator {
                                    cst_kind: "<=>",
                                    name: "<=>",
                                },
                                left: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                                right: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: InfixOperator(
                            InfixOperator {
                                cst_kind: "infix_operator",
                                operator: Operator {
                                    cst_kind: "**",
                                    name: "**",
                                },
                                left: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                                right: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_prefix_operator() {
		check_ast_eprime(
			r#"
            letting negative_ident = -x
            letting negated_bool = !true
            "#,
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: PrefixOperator(
                            PrefixOperator {
                                cst_kind: "prefix_operator",
                                operator: Operator {
                                    cst_kind: "-",
                                    name: "-",
                                },
                                operand: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: PrefixOperator(
                            PrefixOperator {
                                cst_kind: "prefix_operator",
                                operator: Operator {
                                    cst_kind: "!",
                                    name: "!",
                                },
                                operand: BooleanLiteral(
                                    BooleanLiteral {
                                        cst_kind: "boolean_literal",
                                        value: true,
                                    },
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_quantification() {
		check_ast_eprime(
			"letting expr = exists i,j : int(1..3) . x[i] = i",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: Quantification(
                            Quantification {
                                cst_kind: "quantification",
                                function: Identifier {
                                    cst_kind: "identifier",
                                },
                                generator: Generator {
                                    cst_kind: "generator",
                                    names: [
                                        Identifier {
                                            cst_kind: "identifier",
                                        },
                                        Identifier {
                                            cst_kind: "identifier",
                                        },
                                    ],
                                    collection: IntegerDomain(
                                        IntegerDomain {
                                            cst_kind: "integer_domain",
                                            domain: [
                                                SetConstructor(
                                                    SetConstructor {
                                                        cst_kind: "set_constructor",
                                                        operator: Operator {
                                                            cst_kind: "..",
                                                            name: "..",
                                                        },
                                                        left: IntegerLiteral(
                                                            IntegerLiteral {
                                                                cst_kind: "integer_literal",
                                                            },
                                                        ),
                                                        right: IntegerLiteral(
                                                            IntegerLiteral {
                                                                cst_kind: "integer_literal",
                                                            },
                                                        ),
                                                    },
                                                ),
                                            ],
                                        },
                                    ),
                                },
                                template: InfixOperator(
                                    InfixOperator {
                                        cst_kind: "infix_operator",
                                        operator: Operator {
                                            cst_kind: "=",
                                            name: "=",
                                        },
                                        left: ArrayAccess(
                                            ArrayAccess {
                                                cst_kind: "indexed_access",
                                                collection: Identifier(
                                                    Identifier {
                                                        cst_kind: "identifier",
                                                    },
                                                ),
                                                indices: [
                                                    Expression(
                                                        Identifier(
                                                            Identifier {
                                                                cst_kind: "identifier",
                                                            },
                                                        ),
                                                    ),
                                                ],
                                            },
                                        ),
                                        right: Identifier(
                                            Identifier {
                                                cst_kind: "identifier",
                                            },
                                        ),
                                    },
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_matrix_comprehension() {
		check_ast_eprime(
			"letting indexed = [ i+j | i: int(1..3), j : int(1..3), i<j ; int(7..) ]",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: MatrixComprehension(
                            MatrixComprehension {
                                cst_kind: "matrix_comprehension",
                                template: InfixOperator(
                                    InfixOperator {
                                        cst_kind: "infix_operator",
                                        operator: Operator {
                                            cst_kind: "+",
                                            name: "+",
                                        },
                                        left: Identifier(
                                            Identifier {
                                                cst_kind: "identifier",
                                            },
                                        ),
                                        right: Identifier(
                                            Identifier {
                                                cst_kind: "identifier",
                                            },
                                        ),
                                    },
                                ),
                                generators: [
                                    Generator {
                                        cst_kind: "generator",
                                        names: [
                                            Identifier {
                                                cst_kind: "identifier",
                                            },
                                        ],
                                        collection: IntegerDomain(
                                            IntegerDomain {
                                                cst_kind: "integer_domain",
                                                domain: [
                                                    SetConstructor(
                                                        SetConstructor {
                                                            cst_kind: "set_constructor",
                                                            operator: Operator {
                                                                cst_kind: "..",
                                                                name: "..",
                                                            },
                                                            left: IntegerLiteral(
                                                                IntegerLiteral {
                                                                    cst_kind: "integer_literal",
                                                                },
                                                            ),
                                                            right: IntegerLiteral(
                                                                IntegerLiteral {
                                                                    cst_kind: "integer_literal",
                                                                },
                                                            ),
                                                        },
                                                    ),
                                                ],
                                            },
                                        ),
                                    },
                                    Generator {
                                        cst_kind: "generator",
                                        names: [
                                            Identifier {
                                                cst_kind: "identifier",
                                            },
                                        ],
                                        collection: IntegerDomain(
                                            IntegerDomain {
                                                cst_kind: "integer_domain",
                                                domain: [
                                                    SetConstructor(
                                                        SetConstructor {
                                                            cst_kind: "set_constructor",
                                                            operator: Operator {
                                                                cst_kind: "..",
                                                                name: "..",
                                                            },
                                                            left: IntegerLiteral(
                                                                IntegerLiteral {
                                                                    cst_kind: "integer_literal",
                                                                },
                                                            ),
                                                            right: IntegerLiteral(
                                                                IntegerLiteral {
                                                                    cst_kind: "integer_literal",
                                                                },
                                                            ),
                                                        },
                                                    ),
                                                ],
                                            },
                                        ),
                                    },
                                ],
                                conditions: [
                                    InfixOperator(
                                        InfixOperator {
                                            cst_kind: "infix_operator",
                                            operator: Operator {
                                                cst_kind: "<",
                                                name: "<",
                                            },
                                            left: Identifier(
                                                Identifier {
                                                    cst_kind: "identifier",
                                                },
                                            ),
                                            right: Identifier(
                                                Identifier {
                                                    cst_kind: "identifier",
                                                },
                                            ),
                                        },
                                    ),
                                ],
                                indices: Some(
                                    IntegerDomain(
                                        IntegerDomain {
                                            cst_kind: "integer_domain",
                                            domain: [
                                                UnarySetConstructor(
                                                    UnarySetConstructor {
                                                        cst_kind: "unary_set_constructor",
                                                        operator: Operator {
                                                            cst_kind: "..o",
                                                            name: "..o",
                                                        },
                                                        operand: IntegerLiteral(
                                                            IntegerLiteral {
                                                                cst_kind: "integer_literal",
                                                            },
                                                        ),
                                                    },
                                                ),
                                            ],
                                        },
                                    ),
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_absolute() {
		check_ast_eprime(
			"letting absolute = | x |",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: AbsoluteOperator(
                            AbsoluteOperator {
                                cst_kind: "absolute_operator",
                                operand: Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_parenthesis() {
		check_ast_eprime(
			"letting x = ( y )",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                ConstDefinition(
                    ConstDefinition {
                        cst_kind: "const_def",
                        name: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        definition: Identifier(
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]],
		)
	}
}
