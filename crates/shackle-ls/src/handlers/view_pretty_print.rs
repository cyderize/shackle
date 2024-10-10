use std::sync::Arc;

use lsp_server::ResponseError;
use lsp_types::TextDocumentPositionParams;
use shackle_compiler::{
	db::{CompilerDatabase, FileReader, Inputs},
	file::{InputFile, InputLang, ModelRef},
	hir::db::Hir,
	syntax::{ast::ConstraintModel, db::SourceParser},
	thir::{db::Thir, pretty_print::PrettyPrinter},
};
use shackle_fmt::{format_model, MiniZincFormatOptions};

use crate::{db::LanguageServerContext, dispatch::RequestHandler, extensions::ViewPrettyPrint};

#[derive(Debug)]
pub struct ViewPrettyPrintHandler;

impl RequestHandler<ViewPrettyPrint, ModelRef> for ViewPrettyPrintHandler {
	fn prepare(
		db: &mut impl LanguageServerContext,
		params: TextDocumentPositionParams,
	) -> Result<ModelRef, ResponseError> {
		db.set_active_file_from_document(&params.text_document)
	}

	fn execute(db: &CompilerDatabase, _: ModelRef) -> Result<String, ResponseError> {
		let errors = db.all_errors();
		if errors.is_empty() {
			let thir = match db.final_thir() {
				Ok(m) => m,
				Err(e) => return Ok(format!("%: THIR error: {}", e)),
			};
			let printer = PrettyPrinter::new(db, &thir);
			let text = printer.pretty_print();
			let mut c = CompilerDatabase::default();
			c.set_input_files(Arc::new(vec![InputFile::String(text, InputLang::MiniZinc)]));
			match c.ast(c.input_file_refs()[0]) {
				Ok(ConstraintModel::MznModel(model)) => {
					let formatted = format_model(&model, &MiniZincFormatOptions::default());
					if let Some(f) = formatted {
						return Ok(f);
					}
				}
				Err(_) => (),
				_ => unreachable!(),
			}
			Ok("% Failed to format parsed text".to_owned())
		} else {
			Ok("% Errors present.".to_owned())
		}
	}
}
