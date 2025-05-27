//! AST representation of Eprime items

use super::{Domain, Expression, Identifier, MatrixLiteral};
use crate::ast::{
	AstNode, Children, ast_enum, ast_node, child_with_field_name, children_with_field_name,
	optional_child_with_field_name,
};

ast_enum!(
	/// Item
	Item,
	"param_decl" => ParamDeclaration,
	"const_def" => ConstDefinition,
	"domain_alias" => DomainAlias,
	"decision_decl" => DecisionDeclaration,
	"objective" => Solve,
	"branching" => Branching,
	"heuristic" => Heuristic,
	"constraint" => Constraint,
	"output" => Output,
);

ast_node!(
	/// Parameter Declaration
	ParamDeclaration,
	names,
	domain,
	wheres,
);

impl<'tree> ParamDeclaration<'tree> {
	/// Get variable being declared
	pub fn names(&self) -> Children<'tree, Identifier<'tree>> {
		children_with_field_name(self, "name")
	}

	/// Domain of variable
	pub fn domain(&self) -> Domain<'tree> {
		child_with_field_name(self, "domain")
	}

	/// Where clauses
	pub fn wheres(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "where")
	}
}

ast_node!(
	/// Constant Definition
	ConstDefinition,
	name,
	definition,
	domain,
);

impl<'tree> ConstDefinition<'tree> {
	/// Get constant being declared
	pub fn name(&self) -> Expression<'tree> {
		child_with_field_name(self, "name")
	}

	/// Definition of constant
	pub fn definition(&self) -> Expression<'tree> {
		child_with_field_name(self, "definition")
	}

	/// Optional domain of constant
	pub fn domain(&self) -> Option<Domain<'tree>> {
		optional_child_with_field_name(self, "domain")
	}
}

ast_node!(
	/// Domain Alias
	DomainAlias,
	name,
	definition,
);

impl<'tree> DomainAlias<'tree> {
	/// Get alias being declared
	pub fn name(&self) -> Identifier<'tree> {
		child_with_field_name(self, "name")
	}

	/// Definition of alias
	pub fn definition(&self) -> Domain<'tree> {
		child_with_field_name(self, "definition")
	}
}

ast_node!(
	/// Decision Declaration
	DecisionDeclaration,
	names,
	domain,
);

impl<'tree> DecisionDeclaration<'tree> {
	/// Get variables being declared
	pub fn names(&self) -> Children<'tree, Identifier<'tree>> {
		children_with_field_name(self, "name")
	}

	/// Domain of decision
	pub fn domain(&self) -> Domain<'tree> {
		child_with_field_name(self, "domain")
	}
}

ast_node!(
	/// Objective
	Solve,
	goal,
);

impl<'tree> Solve<'tree> {
	/// Get objective strategy
	pub fn goal(&self) -> Goal<'tree> {
		match self.cst_node().child_with_field_name("strategy").kind() {
			"minimising" => Goal::Minimising(Expression::new(
				self.cst_node().child_with_field_name("objective_expr"),
			)),
			"maximising" => Goal::Maximising(Expression::new(
				self.cst_node().child_with_field_name("objective_expr"),
			)),
			_ => unreachable!(),
		}
	}
}

/// Solve goal
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Goal<'tree> {
	/// Default Satisifaction Constraint
	Satisfy,
	/// Minimising Objective
	Minimising(Expression<'tree>),
	/// Maximising Objective
	Maximising(Expression<'tree>),
}

impl<'tree> Goal<'tree> {
	/// Get objective expression if there is one
	pub fn objective(&self) -> Option<Expression<'tree>> {
		match self {
			Goal::Minimising(e) => Some(e.clone()),
			Goal::Maximising(e) => Some(e.clone()),
			_ => None,
		}
	}
}

ast_node!(
	/// Branching
	Branching,
	branching_array,
);

impl<'tree> Branching<'tree> {
	/// Get branching expressions
	pub fn branching_array(&self) -> MatrixLiteral<'tree> {
		child_with_field_name(self, "branching_array")
	}
}

ast_node!(
	/// Heuristic
	Heuristic,
	heuristic,
);

impl<'tree> Heuristic<'tree> {
	/// Get heuristic expression
	pub fn heuristic(&self) -> Option<HeuristicType<'tree>> {
		optional_child_with_field_name(self, "heuristic")
	}
}

ast_node!(
	/// Heuristic Type
	HeuristicType,
	name,
);

impl<'tree> HeuristicType<'tree> {
	/// Get heuristic name
	pub fn name(&self) -> &str {
		self.cst_kind()
	}
}

ast_node!(
	/// Constraint
	Constraint,
	expressions,
);

impl<'tree> Constraint<'tree> {
	/// Get constraint expressions
	pub fn expressions(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "expression")
	}
}

ast_node!(
	/// Output
	Output,
	expression,
);

impl<'tree> Output<'tree> {
	/// Get output expressions
	pub fn expression(&self) -> Expression<'tree> {
		child_with_field_name(self, "expression")
	}
}

#[cfg(test)]
mod tests {
	use expect_test::expect;

	use crate::ast::tests::check_ast_eprime;

	#[test]
	fn test_const_definition() {
		check_ast_eprime(
			r#"
                letting x = 10
                letting x be 10
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
                        definition: IntegerLiteral(
                            IntegerLiteral {
                                cst_kind: "integer_literal",
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
"#]],
		);
	}

	#[test]
	fn test_param_declaration() {
		check_ast_eprime(
			r#"
                given x: int(1..10)
                given y: int(1..10)
                    where y < x
            "#,
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                ParamDeclaration(
                    ParamDeclaration {
                        cst_kind: "param_decl",
                        names: [
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ],
                        domain: IntegerDomain(
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
                        wheres: [],
                    },
                ),
                ParamDeclaration(
                    ParamDeclaration {
                        cst_kind: "param_decl",
                        names: [
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ],
                        domain: IntegerDomain(
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
                        wheres: [
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
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_domain_alias() {
		check_ast_eprime(
			"letting INDEX be domain int(1..c*n)",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                DomainAlias(
                    DomainAlias {
                        cst_kind: "domain_alias",
                        name: Identifier {
                            cst_kind: "identifier",
                        },
                        definition: IntegerDomain(
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
                                            right: InfixOperator(
                                                InfixOperator {
                                                    cst_kind: "infix_operator",
                                                    operator: Operator {
                                                        cst_kind: "*",
                                                        name: "*",
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
                                        },
                                    ),
                                ],
                            },
                        ),
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_decision_declaration() {
		check_ast_eprime(
			"find x : int(1..10)",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                DecisionDeclaration(
                    DecisionDeclaration {
                        cst_kind: "decision_decl",
                        names: [
                            Identifier {
                                cst_kind: "identifier",
                            },
                        ],
                        domain: IntegerDomain(
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
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_objective() {
		check_ast_eprime(
			"minimising x",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                Solve(
                    Solve {
                        cst_kind: "objective",
                        goal: Minimising(
                            Identifier(
                                Identifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                    },
                ),
            ],
        },
    )
"#]],
		);
	}

	#[test]
	fn test_heuristic() {
		check_ast_eprime(
			"heuristic static",
			expect![[r#"
                EPrimeModel(
                    Model {
                        items: [
                            Heuristic(
                                Heuristic {
                                    cst_kind: "heuristic",
                                    heuristic: Some(
                                        HeuristicType {
                                            cst_kind: "static",
                                            name: "static",
                                        },
                                    ),
                                },
                            ),
                        ],
                    },
                )
            "#]],
		)
	}

	#[test]
	fn test_branching() {
		check_ast_eprime(
			"branching on [x]",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                Branching(
                    Branching {
                        cst_kind: "branching",
                        branching_array: MatrixLiteral {
                            cst_kind: "matrix_literal",
                            members: [
                                Identifier(
                                    Identifier {
                                        cst_kind: "identifier",
                                    },
                                ),
                            ],
                            index: None,
                        },
                    },
                ),
            ],
        },
    )
"#]],
		)
	}

	#[test]
	fn test_constraint() {
		check_ast_eprime(
			"such that x, y",
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                Constraint(
                    Constraint {
                        cst_kind: "constraint",
                        expressions: [
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
            ],
        },
    )
"#]],
		)
	}

	#[test]
	fn test_output() {
		check_ast_eprime(
			r#"showing ["foo"]"#,
			expect![[r#"
    EPrimeModel(
        Model {
            items: [
                Output(
                    Output {
                        cst_kind: "output",
                        expression: MatrixLiteral(
                            MatrixLiteral {
                                cst_kind: "matrix_literal",
                                members: [
                                    StringLiteral(
                                        StringLiteral {
                                            cst_kind: "string_literal",
                                        },
                                    ),
                                ],
                                index: None,
                            },
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
