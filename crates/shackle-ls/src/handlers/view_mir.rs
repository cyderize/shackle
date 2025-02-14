use lsp_server::ResponseError;
use lsp_types::TextDocumentPositionParams;
use shackle_compiler::{
	db::CompilerDatabase,
	file::ModelRef,
	hir::db::Hir,
	mir::{db::Mir, pretty_print::PrettyPrinter},
};
use shackle_fmt::{format, MiniZincFormatOptions};

use crate::{db::LanguageServerContext, dispatch::RequestHandler, extensions::ViewMir};

#[derive(Debug)]
pub struct ViewMirHandler;

impl RequestHandler<ViewMir, ModelRef> for ViewMirHandler {
	fn prepare(
		db: &mut impl LanguageServerContext,
		params: TextDocumentPositionParams,
	) -> Result<ModelRef, ResponseError> {
		db.set_active_file_from_document(&params.text_document)
	}

	fn execute(db: &CompilerDatabase, _: ModelRef) -> Result<String, ResponseError> {
		let errors = db.all_errors();
		if errors.is_empty() {
			let mir = match db.model_mir() {
				Ok(m) => m,
				Err(e) => return Ok(format!("%: Error: {}", e)),
			};
			let text = PrettyPrinter::print_model(db, &mir);
			if let Ok(f) = format(&text, &MiniZincFormatOptions::default()) {
				return Ok(f);
			}
			Ok(format!("% Failed to format parsed text:\n{}", text))
		} else {
			Ok("% Errors present.".to_owned())
		}
	}
}
