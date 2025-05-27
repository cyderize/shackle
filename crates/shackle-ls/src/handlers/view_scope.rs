use std::fmt::Write;

use lsp_server::ResponseError;
use lsp_types::{Position, TextDocumentPositionParams};
use shackle_hir::{db::CompilerDatabase, input::ModelFile, source::find_expression};

use crate::{
	db::LanguageServerContext, dispatch::RequestHandler, extensions::ViewScope,
	utils::position_to_byte_offset,
};

#[derive(Debug)]
pub(crate) struct ViewScopeHandler;

impl RequestHandler<ViewScope, (ModelFile, Position)> for ViewScopeHandler {
	fn prepare(
		db: &mut impl LanguageServerContext,
		params: TextDocumentPositionParams,
	) -> Result<(ModelFile, Position), ResponseError> {
		let model_ref = db.set_active_file_from_document(&params.text_document)?;
		Ok((model_ref, params.position))
	}

	fn execute(
		db: &CompilerDatabase,
		(model_ref, start): (ModelFile, Position),
	) -> Result<String, ResponseError> {
		let Some(byte_offset) = position_to_byte_offset(&model_ref.contents(db), start) else {
			return Ok("Invalid position.".to_owned());
		};

		// let line = Point {
		// 	row: start.row,
		// 	column: 0,
		// };

		let Some(e) = find_expression(db, model_ref, byte_offset) else {
			return Ok("Not an expression.".to_owned());
		};
		let expr = e.expression(db);
		let scopes = e.item(db).scope(db);
		let mut fns = Vec::new();
		let mut vars = Vec::new();
		for (i, r) in scopes.functions_in_scope(db, expr) {
			fns.push(format!("{} ({} overloads)", i.pretty_print(db), r.len()));
		}
		for (i, _) in scopes.variables_in_scope(db, expr) {
			vars.push(i.pretty_print(db));
		}
		fns.sort();
		vars.sort();
		let mut out = String::new();
		writeln!(&mut out, "Scope for current expression:").unwrap();
		writeln!(&mut out, "  Functions:",).unwrap();
		for f in fns {
			writeln!(&mut out, "    {}", f).unwrap();
		}
		writeln!(&mut out, "  Variables:",).unwrap();
		for v in vars {
			writeln!(&mut out, "    {}", v).unwrap();
		}
		Ok(out)
	}
}

#[cfg(test)]
mod tests {
	use std::str::FromStr;

	use expect_test::expect;
	use lsp_types::Uri;

	use super::ViewScopeHandler;
	use crate::handlers::tests::test_handler_display;

	#[test]
	fn test_view_scope() {
		test_handler_display::<ViewScopeHandler, _, _>(
			r#"
int: a = let { int: b = 1; } in 1;
int: c = let { int: d = 1; } in z;
			"#,
			true,
			lsp_types::TextDocumentPositionParams {
				text_document: lsp_types::TextDocumentIdentifier {
					uri: Uri::from_str("file:///test.mzn").unwrap(),
				},
				position: lsp_types::Position {
					line: 2,
					character: 32,
				},
			},
			expect!([r#"
    Scope for current expression:
      Functions:
      Variables:
        a
        c
        d
"#]),
		)
	}
}
