//! AST Representation for containers

use super::{Children, Expression, Identifier, Pattern};
use crate::ast::{
	AstNode, ast_enum, ast_node, child_with_field_name, children_with_field_name,
	optional_child_with_field_name,
};

ast_node!(
	/// Tuple literal
	TupleLiteral,
	members,
);

impl<'tree> TupleLiteral<'tree> {
	/// Get the values in this tuple literal
	pub fn members(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "member")
	}
}

ast_node!(
	/// Record literal
	RecordLiteral,
	members,
);

impl<'tree> RecordLiteral<'tree> {
	/// Get the values in this record literal
	pub fn members(&self) -> Children<'tree, RecordLiteralMember<'tree>> {
		children_with_field_name(self, "member")
	}
}

ast_node!(
	/// Record literal key-value pair
	RecordLiteralMember,
	name,
	value
);

impl<'tree> RecordLiteralMember<'tree> {
	/// Get the name of this member
	pub fn name(&self) -> Identifier<'tree> {
		child_with_field_name(self, "name")
	}

	/// Get the value of this member
	pub fn value(&self) -> Expression<'tree> {
		child_with_field_name(self, "value")
	}
}

ast_node!(
	/// Set literal
	SetLiteral,
	members
);

impl<'tree> SetLiteral<'tree> {
	/// Get the values in this set literal
	pub fn members(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "member")
	}
}

ast_node!(
	/// Array literal
	ArrayLiteral,
	members
);

impl<'tree> ArrayLiteral<'tree> {
	/// Get the members of this array literal
	pub fn members(&self) -> Children<'tree, ArrayMember<'tree>> {
		children_with_field_name(self, "member")
	}
}

ast_enum!(
	/// A member of an array literal. Only a member written with an index
	/// (`[3: x, y]`) gets a node of its own; a plain one is its value.
	ArrayMember,
	"array_literal_member" => Indexed(ArrayLiteralMember),
	_ => Value(Expression)
);

impl<'tree> ArrayMember<'tree> {
	/// Get the indices for this member, if it was written with any
	pub fn indices(&self) -> Option<Expression<'tree>> {
		match self {
			ArrayMember::Indexed(m) => Some(m.indices()),
			ArrayMember::Value(_) => None,
		}
	}

	/// Get the value of this member
	pub fn value(&self) -> Expression<'tree> {
		match self {
			ArrayMember::Indexed(m) => m.value(),
			ArrayMember::Value(v) => v.clone(),
		}
	}
}

ast_node!(
	/// Array literal member written with an index (indices and value)
	ArrayLiteralMember,
	indices,
	value
);

impl<'tree> ArrayLiteralMember<'tree> {
	/// Get the indices for this member
	pub fn indices(&self) -> Expression<'tree> {
		child_with_field_name(self, "index")
	}

	/// Get the value of this member
	pub fn value(&self) -> Expression<'tree> {
		child_with_field_name(self, "value")
	}
}

ast_node!(
	/// 2D array literal
	ArrayLiteral2D,
	column_indices,
	rows
);

impl<'tree> ArrayLiteral2D<'tree> {
	/// Get the column indices if any
	pub fn column_indices(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "column_index")
	}

	/// Get the rows in this 2D array literal
	pub fn rows(&self) -> Children<'tree, ArrayLiteral2DRow<'tree>> {
		children_with_field_name(self, "row")
	}
}

ast_node!(
	/// 2D array literal row
	ArrayLiteral2DRow,
	index,
	members
);

impl<'tree> ArrayLiteral2DRow<'tree> {
	/// Get the row index if present
	pub fn index(&self) -> Option<Expression<'tree>> {
		optional_child_with_field_name(self, "index")
	}

	/// Get the values in this 2D array literal row
	pub fn members(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "member")
	}
}

ast_node!(
	/// 3D array literal
	ArrayLiteral3D,
	slices
);

impl<'tree> ArrayLiteral3D<'tree> {
	/// Get the slices in this 3D array literal
	pub fn slices(&self) -> Children<'tree, ArrayLiteral3DSlice<'tree>> {
		children_with_field_name(self, "slice")
	}
}

ast_node!(
	/// 3D array literal slice
	ArrayLiteral3DSlice,
	rows
);

impl<'tree> ArrayLiteral3DSlice<'tree> {
	/// Get the rows in this slice
	pub fn rows(&self) -> Children<'tree, ArrayLiteral3DRow<'tree>> {
		children_with_field_name(self, "row")
	}
}

ast_node!(
	/// 3D array literal row
	ArrayLiteral3DRow,
	members
);

impl<'tree> ArrayLiteral3DRow<'tree> {
	/// Get the values in this row
	pub fn members(&self) -> Children<'tree, Expression<'tree>> {
		children_with_field_name(self, "member")
	}
}

ast_node!(
	/// Array access
	ArrayAccess,
	collection,
	indices
);

impl<'tree> ArrayAccess<'tree> {
	/// The array being indexed
	pub fn collection(&self) -> Expression<'tree> {
		child_with_field_name(self, "collection")
	}

	/// Get the indices
	pub fn indices(&self) -> Children<'tree, ArrayIndex<'tree>> {
		children_with_field_name(self, "index")
	}
}

ast_enum!(
	/// Array index (could be `..` or an expression)
	ArrayIndex,
	".." | "<.." | "<..<" | "..<" => IndexSlice,
	_ => Expression
);

ast_node!(
	/// Array index slice
	IndexSlice,
	operator,
);

impl<'tree> IndexSlice<'tree> {
	/// Get the operator
	pub fn operator(&self) -> &str {
		self.cst_node().kind()
	}
}

ast_node!(
	/// Array comprehension
	ArrayComprehension,
	indices,
	template,
	generators
);

impl<'tree> ArrayComprehension<'tree> {
	/// The indices for the body of this comprehension
	pub fn indices(&self) -> Option<Expression<'tree>> {
		optional_child_with_field_name(self, "index")
	}

	/// The body of this comprehension
	pub fn template(&self) -> Expression<'tree> {
		child_with_field_name(self, "template")
	}

	/// The generators for this comprehension
	pub fn generators(&self) -> Children<'tree, Generator<'tree>> {
		children_with_field_name(self, "generator")
	}
}

ast_node!(
	/// Set comprehension
	SetComprehension,
	template,
	generators
);

impl<'tree> SetComprehension<'tree> {
	/// The body of this comprehension
	pub fn template(&self) -> Expression<'tree> {
		child_with_field_name(self, "template")
	}

	/// The generators for this comprehension
	pub fn generators(&self) -> Children<'tree, Generator<'tree>> {
		children_with_field_name(self, "generator")
	}
}

ast_enum!(
	/// Generator for a comprehension
	Generator,
	"generator" => IteratorGenerator,
	"assignment_generator" => AssignmentGenerator
);

ast_node!(
	/// Generator for a comprehension
	IteratorGenerator,
	patterns,
	collection,
	where_clause
);

impl<'tree> IteratorGenerator<'tree> {
	/// Patterns (variable names)
	pub fn patterns(&self) -> Children<'tree, Pattern<'tree>> {
		children_with_field_name(self, "name")
	}

	/// Expression being iterated over
	pub fn collection(&self) -> Expression<'tree> {
		child_with_field_name(self, "collection")
	}

	/// Where clause constraining iteration
	pub fn where_clause(&self) -> Option<Expression<'tree>> {
		optional_child_with_field_name(self, "where")
	}
}

ast_node!(
	/// Assignment generator for a comprehension
	AssignmentGenerator,
	pattern,
	value,
	where_clause
);

impl<'tree> AssignmentGenerator<'tree> {
	/// Pattern (variable name)
	pub fn pattern(&self) -> Pattern<'tree> {
		child_with_field_name(self, "name")
	}

	/// Expression being iterated over
	pub fn value(&self) -> Expression<'tree> {
		child_with_field_name(self, "value")
	}

	/// Where clause constraining iteration
	pub fn where_clause(&self) -> Option<Expression<'tree>> {
		optional_child_with_field_name(self, "where")
	}
}

#[cfg(test)]
mod tests {
	use expect_test::expect;

	use crate::ast::tests::*;

	#[test]
	fn test_tuple_literal() {
		check_ast(
			r#"
		x = (1, 2);
		y = (1, (2, 3));
		"#,
			expect!([r#"
    MznModel(
        Model {
            items: [
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: TupleLiteral(
                            TupleLiteral {
                                cst_kind: "tuple_literal",
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
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: TupleLiteral(
                            TupleLiteral {
                                cst_kind: "tuple_literal",
                                members: [
                                    IntegerLiteral(
                                        IntegerLiteral {
                                            cst_kind: "integer_literal",
                                        },
                                    ),
                                    TupleLiteral(
                                        TupleLiteral {
                                            cst_kind: "tuple_literal",
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
                                            ],
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
"#]),
		);
	}

	#[test]
	fn test_record_literal() {
		check_ast(
			r#"
		x = (a: 1, b: 2);
		y = (a: 1, b: (c: 2, d: 3));
		"#,
			expect!([r#"
    MznModel(
        Model {
            items: [
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: RecordLiteral(
                            RecordLiteral {
                                cst_kind: "record_literal",
                                members: [
                                    RecordLiteralMember {
                                        cst_kind: "record_member",
                                        name: UnquotedIdentifier(
                                            UnquotedIdentifier {
                                                cst_kind: "identifier",
                                            },
                                        ),
                                        value: IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    },
                                    RecordLiteralMember {
                                        cst_kind: "record_member",
                                        name: UnquotedIdentifier(
                                            UnquotedIdentifier {
                                                cst_kind: "identifier",
                                            },
                                        ),
                                        value: IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    },
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: RecordLiteral(
                            RecordLiteral {
                                cst_kind: "record_literal",
                                members: [
                                    RecordLiteralMember {
                                        cst_kind: "record_member",
                                        name: UnquotedIdentifier(
                                            UnquotedIdentifier {
                                                cst_kind: "identifier",
                                            },
                                        ),
                                        value: IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    },
                                    RecordLiteralMember {
                                        cst_kind: "record_member",
                                        name: UnquotedIdentifier(
                                            UnquotedIdentifier {
                                                cst_kind: "identifier",
                                            },
                                        ),
                                        value: RecordLiteral(
                                            RecordLiteral {
                                                cst_kind: "record_literal",
                                                members: [
                                                    RecordLiteralMember {
                                                        cst_kind: "record_member",
                                                        name: UnquotedIdentifier(
                                                            UnquotedIdentifier {
                                                                cst_kind: "identifier",
                                                            },
                                                        ),
                                                        value: IntegerLiteral(
                                                            IntegerLiteral {
                                                                cst_kind: "integer_literal",
                                                            },
                                                        ),
                                                    },
                                                    RecordLiteralMember {
                                                        cst_kind: "record_member",
                                                        name: UnquotedIdentifier(
                                                            UnquotedIdentifier {
                                                                cst_kind: "identifier",
                                                            },
                                                        ),
                                                        value: IntegerLiteral(
                                                            IntegerLiteral {
                                                                cst_kind: "integer_literal",
                                                            },
                                                        ),
                                                    },
                                                ],
                                            },
                                        ),
                                    },
                                ],
                            },
                        ),
                    },
                ),
            ],
        },
    )
"#]),
		);
	}

	#[test]
	fn test_set_literal() {
		check_ast(
			"x = {1, 2};",
			expect!([r#"
    MznModel(
        Model {
            items: [
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: SetLiteral(
                            SetLiteral {
                                cst_kind: "set_literal",
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
                                ],
                            },
                        ),
                    },
                ),
            ],
        },
    )
"#]),
		)
	}

	#[test]
	fn test_array_literal() {
		check_ast(
			r#"
		x = [1, 3];
		y = [2: 1, 3];
		z = [0: 1, 1: 3];
		w = [(1, 1): 1, (1, 2): 3];
		"#,
			expect!([r#"
    MznModel(
        Model {
            items: [
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayLiteral(
                            ArrayLiteral {
                                cst_kind: "array_literal",
                                members: [
                                    Value(
                                        IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    ),
                                    Value(
                                        IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    ),
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayLiteral(
                            ArrayLiteral {
                                cst_kind: "array_literal",
                                members: [
                                    Indexed(
                                        ArrayLiteralMember {
                                            cst_kind: "array_literal_member",
                                            indices: IntegerLiteral(
                                                IntegerLiteral {
                                                    cst_kind: "integer_literal",
                                                },
                                            ),
                                            value: IntegerLiteral(
                                                IntegerLiteral {
                                                    cst_kind: "integer_literal",
                                                },
                                            ),
                                        },
                                    ),
                                    Value(
                                        IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    ),
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayLiteral(
                            ArrayLiteral {
                                cst_kind: "array_literal",
                                members: [
                                    Indexed(
                                        ArrayLiteralMember {
                                            cst_kind: "array_literal_member",
                                            indices: IntegerLiteral(
                                                IntegerLiteral {
                                                    cst_kind: "integer_literal",
                                                },
                                            ),
                                            value: IntegerLiteral(
                                                IntegerLiteral {
                                                    cst_kind: "integer_literal",
                                                },
                                            ),
                                        },
                                    ),
                                    Indexed(
                                        ArrayLiteralMember {
                                            cst_kind: "array_literal_member",
                                            indices: IntegerLiteral(
                                                IntegerLiteral {
                                                    cst_kind: "integer_literal",
                                                },
                                            ),
                                            value: IntegerLiteral(
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
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayLiteral(
                            ArrayLiteral {
                                cst_kind: "array_literal",
                                members: [
                                    Indexed(
                                        ArrayLiteralMember {
                                            cst_kind: "array_literal_member",
                                            indices: TupleLiteral(
                                                TupleLiteral {
                                                    cst_kind: "tuple_literal",
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
                                                    ],
                                                },
                                            ),
                                            value: IntegerLiteral(
                                                IntegerLiteral {
                                                    cst_kind: "integer_literal",
                                                },
                                            ),
                                        },
                                    ),
                                    Indexed(
                                        ArrayLiteralMember {
                                            cst_kind: "array_literal_member",
                                            indices: TupleLiteral(
                                                TupleLiteral {
                                                    cst_kind: "tuple_literal",
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
                                                    ],
                                                },
                                            ),
                                            value: IntegerLiteral(
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
"#]),
		);
	}

	#[test]
	fn test_2d_array_literal() {
		check_ast(
			r#"
		x = [| 1, 2
		     | 3, 4 |];
		y = [| 1: 2:
		     | 1, 2 |];
		z = [|    1: 2: |
		     | 1: 1, 2 |];
		w = [| 1: 1, 2 |];
		"#,
			expect!([r#"
    MznModel(
        Model {
            items: [
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayLiteral2D(
                            ArrayLiteral2D {
                                cst_kind: "array_literal_2d",
                                column_indices: [],
                                rows: [
                                    ArrayLiteral2DRow {
                                        cst_kind: "array_literal_2d_row",
                                        index: None,
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
                                        ],
                                    },
                                    ArrayLiteral2DRow {
                                        cst_kind: "array_literal_2d_row",
                                        index: None,
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
                                        ],
                                    },
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayLiteral2D(
                            ArrayLiteral2D {
                                cst_kind: "array_literal_2d",
                                column_indices: [
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
                                rows: [
                                    ArrayLiteral2DRow {
                                        cst_kind: "array_literal_2d_row",
                                        index: None,
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
                                        ],
                                    },
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayLiteral2D(
                            ArrayLiteral2D {
                                cst_kind: "array_literal_2d",
                                column_indices: [
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
                                rows: [
                                    ArrayLiteral2DRow {
                                        cst_kind: "array_literal_2d_row",
                                        index: Some(
                                            IntegerLiteral(
                                                IntegerLiteral {
                                                    cst_kind: "integer_literal",
                                                },
                                            ),
                                        ),
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
                                        ],
                                    },
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayLiteral2D(
                            ArrayLiteral2D {
                                cst_kind: "array_literal_2d",
                                column_indices: [],
                                rows: [
                                    ArrayLiteral2DRow {
                                        cst_kind: "array_literal_2d_row",
                                        index: Some(
                                            IntegerLiteral(
                                                IntegerLiteral {
                                                    cst_kind: "integer_literal",
                                                },
                                            ),
                                        ),
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
                                        ],
                                    },
                                ],
                            },
                        ),
                    },
                ),
            ],
        },
    )
"#]),
		);
	}

	#[test]
	fn test_array_access() {
		check_ast(
			r#"
		x = foo[1];
		y = foo[1, 2];
		z = foo[1, .., 3..];
		"#,
			expect!([r#"
    MznModel(
        Model {
            items: [
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayAccess(
                            ArrayAccess {
                                cst_kind: "indexed_access",
                                collection: Identifier(
                                    UnquotedIdentifier(
                                        UnquotedIdentifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                ),
                                indices: [
                                    Expression(
                                        IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    ),
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayAccess(
                            ArrayAccess {
                                cst_kind: "indexed_access",
                                collection: Identifier(
                                    UnquotedIdentifier(
                                        UnquotedIdentifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                ),
                                indices: [
                                    Expression(
                                        IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    ),
                                    Expression(
                                        IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    ),
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayAccess(
                            ArrayAccess {
                                cst_kind: "indexed_access",
                                collection: Identifier(
                                    UnquotedIdentifier(
                                        UnquotedIdentifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                ),
                                indices: [
                                    Expression(
                                        IntegerLiteral(
                                            IntegerLiteral {
                                                cst_kind: "integer_literal",
                                            },
                                        ),
                                    ),
                                    IndexSlice(
                                        IndexSlice {
                                            cst_kind: "..",
                                            operator: "..",
                                        },
                                    ),
                                    Expression(
                                        PostfixOperator(
                                            PostfixOperator {
                                                cst_kind: "postfix_operator",
                                                operand: IntegerLiteral(
                                                    IntegerLiteral {
                                                        cst_kind: "integer_literal",
                                                    },
                                                ),
                                                operator: Operator {
                                                    cst_kind: "..",
                                                    name: "..",
                                                },
                                            },
                                        ),
                                    ),
                                ],
                            },
                        ),
                    },
                ),
            ],
        },
    )
"#]),
		);
	}

	#[test]
	fn test_array_comprehension() {
		check_ast(
			r#"
		x = [1 | i in s];
		y = [i: v | i in 1..3, j in s where i < j];
		z = [(i, j): v | i, j in s]
		a = [j | i in s, j = i + 1];
		"#,
			expect!([r#"
    MznModel(
        Model {
            items: [
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayComprehension(
                            ArrayComprehension {
                                cst_kind: "array_comprehension",
                                indices: None,
                                template: IntegerLiteral(
                                    IntegerLiteral {
                                        cst_kind: "integer_literal",
                                    },
                                ),
                                generators: [
                                    IteratorGenerator(
                                        IteratorGenerator {
                                            cst_kind: "generator",
                                            patterns: [
                                                Identifier(
                                                    UnquotedIdentifier(
                                                        UnquotedIdentifier {
                                                            cst_kind: "identifier",
                                                        },
                                                    ),
                                                ),
                                            ],
                                            collection: Identifier(
                                                UnquotedIdentifier(
                                                    UnquotedIdentifier {
                                                        cst_kind: "identifier",
                                                    },
                                                ),
                                            ),
                                            where_clause: None,
                                        },
                                    ),
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: ArrayComprehension(
                            ArrayComprehension {
                                cst_kind: "array_comprehension",
                                indices: Some(
                                    Identifier(
                                        UnquotedIdentifier(
                                            UnquotedIdentifier {
                                                cst_kind: "identifier",
                                            },
                                        ),
                                    ),
                                ),
                                template: Identifier(
                                    UnquotedIdentifier(
                                        UnquotedIdentifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                ),
                                generators: [
                                    IteratorGenerator(
                                        IteratorGenerator {
                                            cst_kind: "generator",
                                            patterns: [
                                                Identifier(
                                                    UnquotedIdentifier(
                                                        UnquotedIdentifier {
                                                            cst_kind: "identifier",
                                                        },
                                                    ),
                                                ),
                                            ],
                                            collection: InfixOperator(
                                                InfixOperator {
                                                    cst_kind: "infix_operator",
                                                    left: IntegerLiteral(
                                                        IntegerLiteral {
                                                            cst_kind: "integer_literal",
                                                        },
                                                    ),
                                                    operator: Operator {
                                                        cst_kind: "..",
                                                        name: "..",
                                                    },
                                                    right: IntegerLiteral(
                                                        IntegerLiteral {
                                                            cst_kind: "integer_literal",
                                                        },
                                                    ),
                                                },
                                            ),
                                            where_clause: None,
                                        },
                                    ),
                                    IteratorGenerator(
                                        IteratorGenerator {
                                            cst_kind: "generator",
                                            patterns: [
                                                Identifier(
                                                    UnquotedIdentifier(
                                                        UnquotedIdentifier {
                                                            cst_kind: "identifier",
                                                        },
                                                    ),
                                                ),
                                            ],
                                            collection: Identifier(
                                                UnquotedIdentifier(
                                                    UnquotedIdentifier {
                                                        cst_kind: "identifier",
                                                    },
                                                ),
                                            ),
                                            where_clause: Some(
                                                InfixOperator(
                                                    InfixOperator {
                                                        cst_kind: "infix_operator",
                                                        left: Identifier(
                                                            UnquotedIdentifier(
                                                                UnquotedIdentifier {
                                                                    cst_kind: "identifier",
                                                                },
                                                            ),
                                                        ),
                                                        operator: Operator {
                                                            cst_kind: "<",
                                                            name: "<",
                                                        },
                                                        right: Identifier(
                                                            UnquotedIdentifier(
                                                                UnquotedIdentifier {
                                                                    cst_kind: "identifier",
                                                                },
                                                            ),
                                                        ),
                                                    },
                                                ),
                                            ),
                                        },
                                    ),
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: InfixOperator(
                            InfixOperator {
                                cst_kind: "infix_operator",
                                left: ArrayComprehension(
                                    ArrayComprehension {
                                        cst_kind: "array_comprehension",
                                        indices: Some(
                                            TupleLiteral(
                                                TupleLiteral {
                                                    cst_kind: "tuple_literal",
                                                    members: [
                                                        Identifier(
                                                            UnquotedIdentifier(
                                                                UnquotedIdentifier {
                                                                    cst_kind: "identifier",
                                                                },
                                                            ),
                                                        ),
                                                        Identifier(
                                                            UnquotedIdentifier(
                                                                UnquotedIdentifier {
                                                                    cst_kind: "identifier",
                                                                },
                                                            ),
                                                        ),
                                                    ],
                                                },
                                            ),
                                        ),
                                        template: Identifier(
                                            UnquotedIdentifier(
                                                UnquotedIdentifier {
                                                    cst_kind: "identifier",
                                                },
                                            ),
                                        ),
                                        generators: [
                                            IteratorGenerator(
                                                IteratorGenerator {
                                                    cst_kind: "generator",
                                                    patterns: [
                                                        Identifier(
                                                            UnquotedIdentifier(
                                                                UnquotedIdentifier {
                                                                    cst_kind: "identifier",
                                                                },
                                                            ),
                                                        ),
                                                        Identifier(
                                                            UnquotedIdentifier(
                                                                UnquotedIdentifier {
                                                                    cst_kind: "identifier",
                                                                },
                                                            ),
                                                        ),
                                                    ],
                                                    collection: Identifier(
                                                        UnquotedIdentifier(
                                                            UnquotedIdentifier {
                                                                cst_kind: "identifier",
                                                            },
                                                        ),
                                                    ),
                                                    where_clause: None,
                                                },
                                            ),
                                        ],
                                    },
                                ),
                                operator: Operator {
                                    cst_kind: "=",
                                    name: "=",
                                },
                                right: ArrayComprehension(
                                    ArrayComprehension {
                                        cst_kind: "array_comprehension",
                                        indices: None,
                                        template: Identifier(
                                            UnquotedIdentifier(
                                                UnquotedIdentifier {
                                                    cst_kind: "identifier",
                                                },
                                            ),
                                        ),
                                        generators: [
                                            IteratorGenerator(
                                                IteratorGenerator {
                                                    cst_kind: "generator",
                                                    patterns: [
                                                        Identifier(
                                                            UnquotedIdentifier(
                                                                UnquotedIdentifier {
                                                                    cst_kind: "identifier",
                                                                },
                                                            ),
                                                        ),
                                                    ],
                                                    collection: Identifier(
                                                        UnquotedIdentifier(
                                                            UnquotedIdentifier {
                                                                cst_kind: "identifier",
                                                            },
                                                        ),
                                                    ),
                                                    where_clause: None,
                                                },
                                            ),
                                            AssignmentGenerator(
                                                AssignmentGenerator {
                                                    cst_kind: "assignment_generator",
                                                    pattern: Identifier(
                                                        UnquotedIdentifier(
                                                            UnquotedIdentifier {
                                                                cst_kind: "identifier",
                                                            },
                                                        ),
                                                    ),
                                                    value: InfixOperator(
                                                        InfixOperator {
                                                            cst_kind: "infix_operator",
                                                            left: Identifier(
                                                                UnquotedIdentifier(
                                                                    UnquotedIdentifier {
                                                                        cst_kind: "identifier",
                                                                    },
                                                                ),
                                                            ),
                                                            operator: Operator {
                                                                cst_kind: "+",
                                                                name: "+",
                                                            },
                                                            right: IntegerLiteral(
                                                                IntegerLiteral {
                                                                    cst_kind: "integer_literal",
                                                                },
                                                            ),
                                                        },
                                                    ),
                                                    where_clause: None,
                                                },
                                            ),
                                        ],
                                    },
                                ),
                            },
                        ),
                    },
                ),
            ],
        },
    )
"#]),
		);
	}

	#[test]
	fn test_set_comprehension() {
		check_ast(
			r#"
		x = {v | i in s};
		y = {v | i in 1..3, j in s where i < j};
		z = {v | i, j in s};
		a = {j | i in s, j = i + 1};
		"#,
			expect!([r#"
    MznModel(
        Model {
            items: [
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: SetComprehension(
                            SetComprehension {
                                cst_kind: "set_comprehension",
                                template: Identifier(
                                    UnquotedIdentifier(
                                        UnquotedIdentifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                ),
                                generators: [
                                    IteratorGenerator(
                                        IteratorGenerator {
                                            cst_kind: "generator",
                                            patterns: [
                                                Identifier(
                                                    UnquotedIdentifier(
                                                        UnquotedIdentifier {
                                                            cst_kind: "identifier",
                                                        },
                                                    ),
                                                ),
                                            ],
                                            collection: Identifier(
                                                UnquotedIdentifier(
                                                    UnquotedIdentifier {
                                                        cst_kind: "identifier",
                                                    },
                                                ),
                                            ),
                                            where_clause: None,
                                        },
                                    ),
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: SetComprehension(
                            SetComprehension {
                                cst_kind: "set_comprehension",
                                template: Identifier(
                                    UnquotedIdentifier(
                                        UnquotedIdentifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                ),
                                generators: [
                                    IteratorGenerator(
                                        IteratorGenerator {
                                            cst_kind: "generator",
                                            patterns: [
                                                Identifier(
                                                    UnquotedIdentifier(
                                                        UnquotedIdentifier {
                                                            cst_kind: "identifier",
                                                        },
                                                    ),
                                                ),
                                            ],
                                            collection: InfixOperator(
                                                InfixOperator {
                                                    cst_kind: "infix_operator",
                                                    left: IntegerLiteral(
                                                        IntegerLiteral {
                                                            cst_kind: "integer_literal",
                                                        },
                                                    ),
                                                    operator: Operator {
                                                        cst_kind: "..",
                                                        name: "..",
                                                    },
                                                    right: IntegerLiteral(
                                                        IntegerLiteral {
                                                            cst_kind: "integer_literal",
                                                        },
                                                    ),
                                                },
                                            ),
                                            where_clause: None,
                                        },
                                    ),
                                    IteratorGenerator(
                                        IteratorGenerator {
                                            cst_kind: "generator",
                                            patterns: [
                                                Identifier(
                                                    UnquotedIdentifier(
                                                        UnquotedIdentifier {
                                                            cst_kind: "identifier",
                                                        },
                                                    ),
                                                ),
                                            ],
                                            collection: Identifier(
                                                UnquotedIdentifier(
                                                    UnquotedIdentifier {
                                                        cst_kind: "identifier",
                                                    },
                                                ),
                                            ),
                                            where_clause: Some(
                                                InfixOperator(
                                                    InfixOperator {
                                                        cst_kind: "infix_operator",
                                                        left: Identifier(
                                                            UnquotedIdentifier(
                                                                UnquotedIdentifier {
                                                                    cst_kind: "identifier",
                                                                },
                                                            ),
                                                        ),
                                                        operator: Operator {
                                                            cst_kind: "<",
                                                            name: "<",
                                                        },
                                                        right: Identifier(
                                                            UnquotedIdentifier(
                                                                UnquotedIdentifier {
                                                                    cst_kind: "identifier",
                                                                },
                                                            ),
                                                        ),
                                                    },
                                                ),
                                            ),
                                        },
                                    ),
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: SetComprehension(
                            SetComprehension {
                                cst_kind: "set_comprehension",
                                template: Identifier(
                                    UnquotedIdentifier(
                                        UnquotedIdentifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                ),
                                generators: [
                                    IteratorGenerator(
                                        IteratorGenerator {
                                            cst_kind: "generator",
                                            patterns: [
                                                Identifier(
                                                    UnquotedIdentifier(
                                                        UnquotedIdentifier {
                                                            cst_kind: "identifier",
                                                        },
                                                    ),
                                                ),
                                                Identifier(
                                                    UnquotedIdentifier(
                                                        UnquotedIdentifier {
                                                            cst_kind: "identifier",
                                                        },
                                                    ),
                                                ),
                                            ],
                                            collection: Identifier(
                                                UnquotedIdentifier(
                                                    UnquotedIdentifier {
                                                        cst_kind: "identifier",
                                                    },
                                                ),
                                            ),
                                            where_clause: None,
                                        },
                                    ),
                                ],
                            },
                        ),
                    },
                ),
                Assignment(
                    Assignment {
                        cst_kind: "assignment",
                        assignee: Identifier(
                            UnquotedIdentifier(
                                UnquotedIdentifier {
                                    cst_kind: "identifier",
                                },
                            ),
                        ),
                        definition: SetComprehension(
                            SetComprehension {
                                cst_kind: "set_comprehension",
                                template: Identifier(
                                    UnquotedIdentifier(
                                        UnquotedIdentifier {
                                            cst_kind: "identifier",
                                        },
                                    ),
                                ),
                                generators: [
                                    IteratorGenerator(
                                        IteratorGenerator {
                                            cst_kind: "generator",
                                            patterns: [
                                                Identifier(
                                                    UnquotedIdentifier(
                                                        UnquotedIdentifier {
                                                            cst_kind: "identifier",
                                                        },
                                                    ),
                                                ),
                                            ],
                                            collection: Identifier(
                                                UnquotedIdentifier(
                                                    UnquotedIdentifier {
                                                        cst_kind: "identifier",
                                                    },
                                                ),
                                            ),
                                            where_clause: None,
                                        },
                                    ),
                                    AssignmentGenerator(
                                        AssignmentGenerator {
                                            cst_kind: "assignment_generator",
                                            pattern: Identifier(
                                                UnquotedIdentifier(
                                                    UnquotedIdentifier {
                                                        cst_kind: "identifier",
                                                    },
                                                ),
                                            ),
                                            value: InfixOperator(
                                                InfixOperator {
                                                    cst_kind: "infix_operator",
                                                    left: Identifier(
                                                        UnquotedIdentifier(
                                                            UnquotedIdentifier {
                                                                cst_kind: "identifier",
                                                            },
                                                        ),
                                                    ),
                                                    operator: Operator {
                                                        cst_kind: "+",
                                                        name: "+",
                                                    },
                                                    right: IntegerLiteral(
                                                        IntegerLiteral {
                                                            cst_kind: "integer_literal",
                                                        },
                                                    ),
                                                },
                                            ),
                                            where_clause: None,
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
"#]),
		);
	}
}
