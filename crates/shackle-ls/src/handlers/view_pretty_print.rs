use lsp_server::ResponseError;
use lsp_types::TextDocumentPositionParams;
use shackle_fmt::{MiniZincFormatOptions, format_str};
use shackle_hir::{db::CompilerDatabase, input::ModelFile, run_hir_phase};
use shackle_thir::{db::final_thir, pretty_print::PrettyPrinter};

use crate::{db::LanguageServerContext, dispatch::RequestHandler, extensions::ViewPrettyPrint};

#[derive(Debug)]
pub(crate) struct ViewPrettyPrintHandler;

impl RequestHandler<ViewPrettyPrint, ModelFile> for ViewPrettyPrintHandler {
	fn prepare(
		db: &mut impl LanguageServerContext,
		params: TextDocumentPositionParams,
	) -> Result<ModelFile, ResponseError> {
		db.set_active_file_from_document(&params.text_document)
	}

	fn execute(db: &CompilerDatabase, _: ModelFile) -> Result<String, ResponseError> {
		let result = run_hir_phase(db);
		if result.errors.is_empty() {
			let thir = match final_thir(db) {
				Ok(m) => m,
				Err(e) => return Ok(format!("%: THIR error: {}", e)),
			};
			let printer = PrettyPrinter::new(db, thir);
			let text = printer.pretty_print();
			if let Ok(f) = format_str(
				&text,
				&MiniZincFormatOptions {
					keep_parentheses: false,
					..Default::default()
				},
			) {
				return Ok(f);
			}
			Ok(format!("% Failed to format parsed text:\n{}", text))
		} else {
			Ok("% Errors present.".to_owned())
		}
	}
}
