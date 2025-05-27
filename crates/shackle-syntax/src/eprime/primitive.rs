//! AST representation of primitive values

use super::{Domain, Expression};
use crate::ast::{
	AstNode, Children, ast_node, children_with_field_name, decode_string_literal,
	optional_child_with_field_name,
};

ast_node!(
	/// Integer literal
	IntegerLiteral
);

impl<'tree> IntegerLiteral<'tree> {
	/// Get the value of this integer literal
	pub fn value(&self, source: &str) -> i64 {
		self.cst_text(source).parse().unwrap()
	}
}

ast_node!(
	/// Boolean literal
	BooleanLiteral,
	value
);

impl<'tree> BooleanLiteral<'tree> {
	/// Get the value of this boolean literal
	pub fn value(&self) -> bool {
		match self.cst_node().child(0).unwrap().kind() {
			"true" => true,
			"false" => false,
			_ => unreachable!(),
		}
	}
}

ast_node!(
	/// String literal (without interpolation)
	StringLiteral
);

impl<'tree> StringLiteral<'tree> {
	/// Get the value of this string literal
	pub fn value(&self, source: &str) -> String {
		decode_string_literal(self.cst_node(), source)
	}
}

ast_node!(
	/// Matrix Literal
	MatrixLiteral,
	members,
	index
);

impl<'tree> MatrixLiteral<'tree> {
	/// Get the members of this matrix literal
	pub fn members(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "member")
	}

	/// Get the index of this matrix literal
	pub fn index(&self) -> Option<Domain<'tree>> {
		optional_child_with_field_name(self, "index")
	}
}

ast_node!(
	/// Infinity literal
	Infinity,
);

#[cfg(test)]
mod tests {
	use expect_test::expect;

	use crate::ast::tests::check_ast_eprime;

	#[test]
	fn test_integer_literal() {
		check_ast_eprime(
			"letting one be 1",
			expect!([r#"
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
                        definition: IntegerLiteral(
                            IntegerLiteral {
                                cst_kind: "integer_literal",
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]),
		);
	}

	#[test]
	fn test_infinity_literal() {
		check_ast_eprime(
			"letting inf be infinity",
			expect!([r#"
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
                        definition: Infinity(
                            Infinity {
                                cst_kind: "infinity",
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]),
		);
	}

	#[test]
	fn test_boolean_literal() {
		check_ast_eprime(
			"letting T = true",
			expect!([r#"
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
                        definition: BooleanLiteral(
                            BooleanLiteral {
                                cst_kind: "boolean_literal",
                                value: true,
                            },
                        ),
                        domain: None,
                    },
                ),
            ],
        },
    )
"#]),
		);
	}

	#[test]
	fn test_string_literal() {
		check_ast_eprime(
			r#"letting s = "foo""#,
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
                        definition: StringLiteral(
                            StringLiteral {
                                cst_kind: "string_literal",
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

	#[test]
	fn test_matrix_literal() {
		check_ast_eprime(
			"letting cmatrix: matrix indexed by [ int(1..2), int(1..4) ] of int(1..10) = [ [2,8,5,1], [3,7,9,4] ]",
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
                        definition: MatrixLiteral(
                            MatrixLiteral {
                                cst_kind: "matrix_literal",
                                members: [
                                    MatrixLiteral(
                                        MatrixLiteral {
                                            cst_kind: "matrix_literal",
                                            members: [
                                                IntegerLiteral(
                                                    IntegerLiteral {
                                                        cst_kind: "integer_literal",
                                                    },
                                                ),
                                                IntegerLiteral(
                                                    IntegerLiteral {
                                                        cst_kind: "integer_literal",
                                                    },
                                                ),
                                                IntegerLiteral(
                                                    IntegerLiteral {
                                                        cst_kind: "integer_literal",
                                                    },
                                                ),
                                                IntegerLiteral(
                                                    IntegerLiteral {
                                                        cst_kind: "integer_literal",
                                                    },
                                                ),
                                            ],
                                            index: None,
                                        },
                                    ),
                                    MatrixLiteral(
                                        MatrixLiteral {
                                            cst_kind: "matrix_literal",
                                            members: [
                                                IntegerLiteral(
                                                    IntegerLiteral {
                                                        cst_kind: "integer_literal",
                                                    },
                                                ),
                                                IntegerLiteral(
                                                    IntegerLiteral {
                                                        cst_kind: "integer_literal",
                                                    },
                                                ),
                                                IntegerLiteral(
                                                    IntegerLiteral {
                                                        cst_kind: "integer_literal",
                                                    },
                                                ),
                                                IntegerLiteral(
                                                    IntegerLiteral {
                                                        cst_kind: "integer_literal",
                                                    },
                                                ),
                                            ],
                                            index: None,
                                        },
                                    ),
                                ],
                                index: None,
                            },
                        ),
                        domain: Some(
                            MatrixDomain(
                                MatrixDomain {
                                    cst_kind: "matrix_domain",
                                    indexes: [
                                        IntegerDomain(
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
                                        IntegerDomain(
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
                                    ],
                                    base: IntegerDomain(
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
                            ),
                        ),
                    },
                ),
            ],
        },
    )
"#]],
		)
	}
}
