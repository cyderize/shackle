use lsp_server::ResponseError;
use lsp_types::TextDocumentPositionParams;
use shackle_hir::{db::CompilerDatabase, input::ModelFile};

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

	fn execute(_db: &CompilerDatabase, _: ModelFile) -> Result<String, ResponseError> {
		todo!()
		// let errors = db.all_errors();
		// if errors.is_empty() {
		// 	let mir = match db.model_mir() {
		// 		Ok(m) => m,
		// 		Err(e) => return Ok(format!("%: Error: {}", e)),
		// 	};
		// 	let text = PrettyPrinter::print_model(db, &mir);
		// 	if let Ok(f) = format(&text, &MiniZincFormatOptions::default()) {
		// 		return Ok(f);
		// 	}
		// 	Ok(format!("% Failed to format parsed text:\n{}", text))
		// } else {
		// 	Ok("% Errors present.".to_owned())
		// }
	}
}
