use lsp_server::ResponseError;
use lsp_types::TextDocumentPositionParams;
use shackle_fmt::{MiniZincFormatOptions, format_str};
use shackle_hir::{db::CompilerDatabase, input::ModelFile, run_hir_phase};
use shackle_mir::{lower::lower_from_thir, pretty_print::PrettyPrinter};
use shackle_thir::db::final_thir;

use crate::{db::LanguageServerContext, dispatch::RequestHandler, extensions::ViewMir};

#[derive(Debug)]
pub(crate) struct ViewMirHandler;

impl RequestHandler<ViewMir, ModelFile> for ViewMirHandler {
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
			let mir = lower_from_thir(db, thir);
			let printer = PrettyPrinter::new(&mir);
			let text = printer.print_model(db);
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
