use lsp_server::ResponseError;
use lsp_types::{DocumentFormattingParams, Position, TextEdit, request::Formatting};
use shackle_fmt::{MiniZincFormatOptions, format};
use shackle_hir::{CompilerDatabase, input::ModelFile};

use crate::{db::LanguageServerContext, dispatch::RequestHandler};

#[derive(Debug)]
pub(crate) struct FormatHandler;

impl RequestHandler<Formatting, (ModelFile, MiniZincFormatOptions)> for FormatHandler {
	fn prepare(
		db: &mut impl LanguageServerContext,
		params: DocumentFormattingParams,
	) -> Result<(ModelFile, MiniZincFormatOptions), ResponseError> {
		Ok((
			db.set_active_file_from_document(&params.text_document)?,
			MiniZincFormatOptions {
				use_tabs: !params.options.insert_spaces,
				indent_size: params.options.tab_size as usize,
				..Default::default()
			},
		))
	}

	fn execute(
		db: &CompilerDatabase,
		(model_ref, options): (ModelFile, MiniZincFormatOptions),
	) -> Result<Option<Vec<TextEdit>>, ResponseError> {
		let Ok(formatted) = format(&model_ref.source_file(db), &options) else {
			return Ok(None);
		};

		let end = model_ref
			.ast(db)
			.ast(db)
			.cst()
			.root()
			.as_ref()
			.end_position();
		Ok(Some(vec![TextEdit {
			range: lsp_types::Range {
				end: Position::new(end.row as u32, end.column as u32),
				..Default::default()
			},
			new_text: formatted,
		}]))
	}
}

#[cfg(test)]
mod tests {
	use std::str::FromStr;

	use expect_test::expect;
	use lsp_types::Uri;

	use super::FormatHandler;
	use crate::handlers::tests::test_handler;

	#[test]
	fn test_format() {
		test_handler::<FormatHandler, _, _>(
			r#"
int: x   = (1 + 2) + 3 % foo
;

% bar
			"#,
			false,
			lsp_types::DocumentFormattingParams {
				text_document: lsp_types::TextDocumentIdentifier {
					uri: Uri::from_str("file:///test.mzn").unwrap(),
				},
				options: lsp_types::FormattingOptions {
					tab_size: 4,
					insert_spaces: false,
					properties: Default::default(),
					trim_trailing_whitespace: None,
					insert_final_newline: None,
					trim_final_newlines: None,
				},
				work_done_progress_params: lsp_types::WorkDoneProgressParams {
					work_done_token: None,
				},
			},
			expect!([r#"
    {
      "Ok": [
        {
          "range": {
            "start": {
              "line": 0,
              "character": 0
            },
            "end": {
              "line": 5,
              "character": 3
            }
          },
          "newText": "int: x = (1 + 2) + 3; % foo\n\n% bar\n"
        }
      ]
    }"#]),
		)
	}
}
