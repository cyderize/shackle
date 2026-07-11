use std::panic::AssertUnwindSafe;

use lsp_server::ResponseError;
use lsp_types::{
	CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, request::Completion,
};
use shackle_hir::{
	CompilerDatabase, Expression, PatternTy,
	db::Setter,
	input::{InlineModelFile, InputFiles, ModelFile},
	source::find_expression,
};
use shackle_syntax::minizinc::pretty_print_identifier;
use shackle_ty::TyData;

use crate::{db::LanguageServerContext, dispatch::RequestHandler, utils::position_to_byte_offset};

#[derive(Debug)]
pub(crate) struct CompletionsHandler;

const COMPLETION_PLACEHOLDER: &str = "__shackle_completion_placeholder";

pub(crate) enum CompletionData {
	Original {
		model: ModelFile,
		byte_offset: usize,
	},
	Synthetic {
		db: AssertUnwindSafe<Box<CompilerDatabase>>,
		model: ModelFile,
		byte_offset: usize,
	},
}

impl RequestHandler<Completion, CompletionData> for CompletionsHandler {
	fn prepare(
		db: &mut impl LanguageServerContext,
		params: CompletionParams,
	) -> Result<CompletionData, ResponseError> {
		let model =
			db.set_active_file_from_document(&params.text_document_position.text_document)?;
		let contents = model.contents(&**db);
		let cursor = position_to_byte_offset(&contents, params.text_document_position.position)
			.ok_or_else(|| ResponseError {
				code: lsp_server::ErrorCode::InvalidParams as i32,
				message: "Invalid position".to_owned(),
				data: None,
			})?;

		if let Some(byte_offset) = cursor.checked_sub(1)
			&& find_expression(&**db, model, byte_offset).is_some()
		{
			return Ok(CompletionData::Original { model, byte_offset });
		}

		let base_directory = model.base_directory(&**db);
		let mut completion_db = db.new_scratch_database();
		let mut completion_contents = contents.to_string();
		completion_contents.insert_str(cursor, COMPLETION_PLACEHOLDER);
		let completion_inline =
			InlineModelFile::new(&completion_db, completion_contents, model.language(&**db));
		let _ = completion_inline
			.set_base_directory(&mut completion_db)
			.to(base_directory);
		let completion_model = completion_inline.into();
		let _ = InputFiles::get(&completion_db)
			.set_files(&mut completion_db)
			.to(vec![completion_model]);

		Ok(CompletionData::Synthetic {
			db: AssertUnwindSafe(Box::new(completion_db)),
			model: completion_model,
			byte_offset: cursor,
		})
	}

	fn execute(
		db: &CompilerDatabase,
		data: CompletionData,
	) -> Result<Option<CompletionResponse>, ResponseError> {
		Ok(match data {
			CompletionData::Original { model, byte_offset } => {
				completions_at(db, model, byte_offset)
			}
			CompletionData::Synthetic {
				db,
				model,
				byte_offset,
			} => completions_at(&db.0, model, byte_offset),
		})
	}
}

fn completions_at(
	db: &CompilerDatabase,
	model_ref: ModelFile,
	byte_offset: usize,
) -> Option<CompletionResponse> {
	let expression = find_expression(db, model_ref, byte_offset)?;
	let item = expression.item(db);
	let types = item.types(db);
	let data = item.data(db);
	let structure = match &data[expression.expression(db)] {
		Expression::TupleAccess(ta) => Some(ta.tuple),
		Expression::RecordAccess(ra) => Some(ra.record),
		_ => None,
	};
	if let Some(e) = structure {
		// Give completions for tuple/record access
		let completions = match types[e].lookup(db) {
			TyData::Tuple(_, fs) => fs
				.iter()
				.enumerate()
				.map(|(i, t)| CompletionItem {
					label: format!("{}", i + 1),
					kind: Some(CompletionItemKind::FIELD),
					detail: Some(t.pretty_print(db)),
					..Default::default()
				})
				.collect(),
			TyData::Record(_, fs) => fs
				.iter()
				.map(|(i, t)| CompletionItem {
					label: pretty_print_identifier(i.lookup(db)),
					kind: Some(CompletionItemKind::FIELD),
					detail: Some(t.pretty_print(db)),
					..Default::default()
				})
				.collect(),
			TyData::Array { element, .. } => match element.lookup(db) {
				TyData::Tuple(_, fs) => fs
					.iter()
					.enumerate()
					.map(|(i, t)| CompletionItem {
						label: format!("{}", i + 1),
						kind: Some(CompletionItemKind::FIELD),
						detail: Some(t.pretty_print(db)),
						..Default::default()
					})
					.collect(),
				TyData::Record(_, fs) => fs
					.iter()
					.map(|(i, t)| CompletionItem {
						label: pretty_print_identifier(i.lookup(db)),
						kind: Some(CompletionItemKind::FIELD),
						detail: Some(t.pretty_print(db)),
						..Default::default()
					})
					.collect(),
				_ => vec![],
			},
			_ => vec![],
		};
		return Some(CompletionResponse::Array(completions));
	}

	// Give completions for identifiers in scope
	let scope = item.scope(db);
	let mut completions = Vec::new();
	for (i, ps) in scope.functions_in_scope(db, expression.expression(db)) {
		let p = ps.first().unwrap();
		let mut additional_overloads = ps.len() - 1;
		let types = p.item(db).types(db);
		match &types[p.pattern(db)] {
			PatternTy::Function(f)
			| PatternTy::AnnotationConstructor(f)
			| PatternTy::AnnotationDestructure(f) => completions.push(CompletionItem {
				label: i.pretty_print(db),
				kind: Some(CompletionItemKind::FUNCTION),
				detail: Some(if additional_overloads == 0 {
					f.overload.pretty_print_item(db, i)
				} else if additional_overloads == 1 {
					format!("{} + 1 overload", f.overload.pretty_print_item(db, i),)
				} else {
					format!(
						"{} + {} overloads",
						f.overload.pretty_print_item(db, i),
						additional_overloads,
					)
				}),
				..Default::default()
			}),
			PatternTy::EnumConstructor(ec) => {
				let func = &ec[0];
				additional_overloads += ec.len() - 1;
				completions.push(CompletionItem {
					label: i.pretty_print(db),
					kind: Some(CompletionItemKind::ENUM_MEMBER),
					detail: Some(if additional_overloads == 0 {
						func.overload.pretty_print_item(db, i)
					} else if additional_overloads == 1 {
						format!("{} + 1 overload", func.overload.pretty_print_item(db, i),)
					} else {
						format!(
							"{} + {} overloads",
							func.overload.pretty_print_item(db, i),
							additional_overloads,
						)
					}),
					..Default::default()
				});
			}
			PatternTy::EnumDestructure(ec) => {
				let func = &ec[0];
				additional_overloads += ec.len() - 1;
				completions.push(CompletionItem {
					label: i.pretty_print(db),
					kind: Some(CompletionItemKind::ENUM_MEMBER),
					detail: Some(if additional_overloads == 0 {
						func.overload.pretty_print_item(db, i)
					} else if additional_overloads == 1 {
						format!("{} + 1 overload", func.overload.pretty_print_item(db, i),)
					} else {
						format!(
							"{} + {} overloads",
							func.overload.pretty_print_item(db, i),
							additional_overloads,
						)
					}),
					..Default::default()
				});
			}
			_ => (),
		}
	}
	for (i, p) in scope.variables_in_scope(db, expression.expression(db)) {
		let types = p.item(db).types(db);
		match types[p.pattern(db)] {
			PatternTy::Variable(ty) | PatternTy::Argument(ty) => completions.push(CompletionItem {
				label: i.pretty_print(db),
				kind: Some(CompletionItemKind::VARIABLE),
				detail: Some(ty.pretty_print(db)),
				..Default::default()
			}),
			PatternTy::Enum(ty) => completions.push(CompletionItem {
				label: i.pretty_print(db),
				kind: Some(CompletionItemKind::ENUM),
				detail: Some(ty.pretty_print(db)),
				..Default::default()
			}),
			PatternTy::EnumAtom(ty) => completions.push(CompletionItem {
				label: i.pretty_print(db),
				kind: Some(CompletionItemKind::ENUM_MEMBER),
				detail: Some(ty.pretty_print(db)),
				..Default::default()
			}),
			PatternTy::AnnotationAtom => completions.push(CompletionItem {
				label: i.pretty_print(db),
				kind: Some(CompletionItemKind::CONSTANT),
				detail: Some("ann".to_owned()),
				..Default::default()
			}),
			_ => (),
		}
	}
	Some(CompletionResponse::Array(completions))
}

#[cfg(test)]
mod tests {
	use std::str::FromStr;

	use expect_test::expect;
	use lsp_types::{CompletionResponse, Uri};

	use super::CompletionsHandler;
	use crate::handlers::tests::{run_handler, test_handler};

	fn completion_labels(
		model: &str,
		position: lsp_types::Position,
		no_stdlib: bool,
	) -> Vec<String> {
		let response = run_handler::<CompletionsHandler, lsp_types::request::Completion, _>(
			model,
			no_stdlib,
			lsp_types::CompletionParams {
				context: None,
				partial_result_params: lsp_types::PartialResultParams {
					partial_result_token: None,
				},
				work_done_progress_params: lsp_types::WorkDoneProgressParams {
					work_done_token: None,
				},
				text_document_position: lsp_types::TextDocumentPositionParams {
					text_document: lsp_types::TextDocumentIdentifier {
						uri: Uri::from_str("file:///test.mzn").unwrap(),
					},
					position,
				},
			},
		)
		.unwrap()
		.unwrap();
		match response {
			CompletionResponse::Array(items) => items.into_iter().map(|item| item.label).collect(),
			CompletionResponse::List(list) => {
				list.items.into_iter().map(|item| item.label).collect()
			}
		}
	}

	#[test]
	fn test_completions() {
		test_handler::<CompletionsHandler, _, _>(
			r#"
enum Foo = {A, B};
Foo: hello;
any: y = he
			"#,
			true,
			lsp_types::CompletionParams {
				context: None,
				partial_result_params: lsp_types::PartialResultParams {
					partial_result_token: None,
				},
				work_done_progress_params: lsp_types::WorkDoneProgressParams {
					work_done_token: None,
				},
				text_document_position: lsp_types::TextDocumentPositionParams {
					text_document: lsp_types::TextDocumentIdentifier {
						uri: Uri::from_str("file:///test.mzn").unwrap(),
					},
					position: lsp_types::Position {
						line: 3,
						character: 11,
					},
				},
			},
			expect!([r#"
    {
      "Ok": [
        {
          "label": "A",
          "kind": 20,
          "detail": "Foo"
        },
        {
          "label": "B",
          "kind": 20,
          "detail": "Foo"
        },
        {
          "label": "Foo",
          "kind": 13,
          "detail": "set of Foo"
        },
        {
          "label": "hello",
          "kind": 6,
          "detail": "Foo"
        },
        {
          "label": "y",
          "kind": 6,
          "detail": "error"
        }
      ]
    }"#]),
		)
	}

	#[test]
	fn test_completions_for_missing_declaration_definition() {
		let labels = completion_labels(
			"int: existing;\nint: x = ",
			lsp_types::Position {
				line: 1,
				character: 9,
			},
			true,
		);
		assert!(labels.iter().any(|label| label == "existing"));
	}

	#[test]
	fn test_completions_for_missing_constraint_expression() {
		let labels = completion_labels(
			"bool: existing;\nconstraint ",
			lsp_types::Position {
				line: 1,
				character: 11,
			},
			false,
		);
		assert!(labels.iter().any(|label| label == "existing"));
		assert!(labels.iter().any(|label| label == "forall"));
	}
}
