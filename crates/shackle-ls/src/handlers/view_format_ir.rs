use lsp_server::ResponseError;
use lsp_types::TextDocumentPositionParams;
use shackle_fmt::{MiniZincFormatOptions, format_debug};
use shackle_hir::{db::CompilerDatabase, input::ModelFile};

use crate::{db::LanguageServerContext, dispatch::RequestHandler, extensions::ViewFormatIr};

#[derive(Debug)]
pub(crate) struct ViewFormatIrHandler;

impl RequestHandler<ViewFormatIr, ModelFile> for ViewFormatIrHandler {
	fn prepare(
		db: &mut impl LanguageServerContext,
		params: TextDocumentPositionParams,
	) -> Result<ModelFile, ResponseError> {
		db.set_active_file_from_document(&params.text_document)
	}

	fn execute(db: &CompilerDatabase, model_ref: ModelFile) -> Result<String, ResponseError> {
		Ok(format_debug(
			&model_ref.source_file(db),
			&MiniZincFormatOptions::default(),
		)
		.unwrap_or_else(|e| format!("Failed to format: {}", e)))
	}
}

#[cfg(test)]
mod tests {
	use std::str::FromStr;

	use expect_test::expect;
	use lsp_types::Uri;

	use super::ViewFormatIrHandler;
	use crate::handlers::tests::test_handler_display;

	#[test]
	fn test_view_format_ir() {
		test_handler_display::<ViewFormatIrHandler, _, _>(
			r#"
      int: x   = (1 + 2) + 3 % foo
      ;

      % bar
			"#,
			false,
			lsp_types::TextDocumentPositionParams {
				text_document: lsp_types::TextDocumentIdentifier {
					uri: Uri::from_str("file:///test.mzn").unwrap(),
				},
				position: lsp_types::Position {
					line: 0,
					character: 0,
				},
			},
			expect!([r#"
    Element::sequence(
        [
            Element::sequence(
                [
                    Element::sequence(
                        [],
                    ),
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
                                                    Element::group(
                                                        Element::sequence(
                                                            [
                                                                Element::sequence(
                                                                    [
                                                                        Element::text(
                                                                            "(",
                                                                        ),
                                                                        Element::group(
                                                                            Element::sequence(
                                                                                [
                                                                                    Element::indent(
                                                                                        Element::sequence(
                                                                                            [
                                                                                                Element::if_broken(
                                                                                                    Element::line_break(),
                                                                                                ),
                                                                                                Element::group(
                                                                                                    Element::sequence(
                                                                                                        [
                                                                                                            Element::text(
                                                                                                                "1",
                                                                                                            ),
                                                                                                            Element::indent(
                                                                                                                Element::sequence(
                                                                                                                    [
                                                                                                                        Element::text(
                                                                                                                            " ",
                                                                                                                        ),
                                                                                                                        Element::text(
                                                                                                                            "+",
                                                                                                                        ),
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
                                                                                                                            "2",
                                                                                                                        ),
                                                                                                                    ],
                                                                                                                ),
                                                                                                            ),
                                                                                                        ],
                                                                                                    ),
                                                                                                ),
                                                                                            ],
                                                                                        ),
                                                                                    ),
                                                                                    Element::if_broken(
                                                                                        Element::line_break(),
                                                                                    ),
                                                                                ],
                                                                            ),
                                                                        ),
                                                                        Element::text(
                                                                            ")",
                                                                        ),
                                                                    ],
                                                                ),
                                                                Element::indent(
                                                                    Element::sequence(
                                                                        [
                                                                            Element::text(
                                                                                " ",
                                                                            ),
                                                                            Element::text(
                                                                                "+",
                                                                            ),
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
                                                                                "3",
                                                                            ),
                                                                        ],
                                                                    ),
                                                                ),
                                                            ],
                                                        ),
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
                    Element::sequence(
                        [
                            Element::break_parent(),
                            Element::line_suffix(
                                " % foo",
                            ),
                            Element::line_break(),
                            Element::line_break(),
                            Element::text(
                                "% bar",
                            ),
                        ],
                    ),
                ],
            ),
            Element::line_break(),
        ],
    )"#]),
		)
	}
}
