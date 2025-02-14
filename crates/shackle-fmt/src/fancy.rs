use std::{
	fs::{read_to_string, write},
	path::{Path, PathBuf},
};

use console::{style, Style};
use glob::glob;
use miette::miette;
use shackle_diagnostics::{Error, FileError, MultipleErrors, Result, SourceFile};
use shackle_syntax::{cst::Cst, minizinc::MznModel};
use similar::{ChangeTag, TextDiff};
use tree_sitter::Parser;

use crate::{format::MiniZincFormatter, MiniZincFormatOptions};

/// Output diffs between formatted and original files
pub fn check_format<'a>(
	files: impl Iterator<Item = &'a PathBuf>,
	options: &MiniZincFormatOptions,
) -> miette::Result<()> {
	let todo = parse_files(files)?;

	log::info!("{}", style("Checking files...").bold().dim());
	let mut is_formatted = true;
	for (source, model) in todo {
		let path = source.path().unwrap();
		log::info!("  Checking {}", path.to_string_lossy());
		let formatted = MiniZincFormatter::new(&model, options).format();
		if source.contents() != &formatted {
			is_formatted = false;
			let diff = TextDiff::from_lines(source.contents(), &formatted);
			let mut udiff = diff.unified_diff();
			let filename = path.to_string_lossy();
			println!("--- {}", filename);
			println!("+++ {}", filename);
			for hunk in udiff.context_radius(5).iter_hunks() {
				for (idx, change) in hunk.iter_changes().enumerate() {
					if idx == 0 {
						println!("{}", hunk.header());
					}
					let style = match change.tag() {
						ChangeTag::Delete => Style::new().red(),
						ChangeTag::Insert => Style::new().green(),
						ChangeTag::Equal => Style::new(),
					};
					print!("{}{}", style.apply_to(change.tag()), style.apply_to(change));
					if !diff.newline_terminated() {
						println!();
					}
				}
			}
			println!();
		}
	}

	if is_formatted {
		log::info!("{}", style("Format check passed!").bold().dim());
		Ok(())
	} else {
		Err(miette!("Format check failed"))
	}
}

/// Format files in-place
pub fn format_files<'a>(
	files: impl Iterator<Item = &'a PathBuf>,
	options: &MiniZincFormatOptions,
) -> miette::Result<()> {
	let todo = parse_files(files)?;
	log::info!("{}", style("Formatting files...").bold().dim());
	let mut errors = Vec::new();
	for (source, model) in todo {
		let path = source.path().unwrap();
		log::info!("  Formatting {}", path.to_string_lossy());
		let formatted = MiniZincFormatter::new(&model, options).format();
		if source.contents() == &formatted {
			log::info!("    (Unchanged)");
		} else {
			match write(path, formatted) {
				Ok(_) => {}
				Err(err) => errors.push(
					FileError {
						file: path.to_path_buf(),
						message: err.to_string(),
						other: Vec::new(),
					}
					.into(),
				),
			}
		}
	}
	Error::try_from(errors)
		.map(|e| Err(e.into()))
		.unwrap_or_else(|_| {
			log::info!("{}", style("Done formatting files.").bold().dim());
			Ok(())
		})
}

fn parse_files<'a>(
	files: impl Iterator<Item = &'a PathBuf>,
) -> miette::Result<Vec<(SourceFile, MznModel)>> {
	let mut todo = Vec::new();
	let mut errors = Vec::new();
	let resolved_files = get_mzn_files(files)?;

	log::info!("{}", style("Parsing files...").bold().dim());
	for file in resolved_files {
		log::info!("  Parsing {}", file.to_string_lossy());
		let source = read_file(&file)?;
		match parse_model(source.clone()) {
			Ok(model) => {
				todo.push((source, model));
			}
			Err(err) => {
				errors.push(err);
			}
		}
	}
	log::info!("{}", style("Done parsing files.").bold().dim());
	Error::try_from(errors)
		.map(|e| Err(e.into()))
		.unwrap_or_else(|_| Ok(todo))
}

fn read_file(file: impl AsRef<Path>) -> Result<SourceFile> {
	let path = file.as_ref();
	let source = read_to_string(path).map_err(|err| FileError {
		file: path.to_path_buf(),
		message: err.to_string(),
		other: Vec::new(),
	})?;
	Ok(SourceFile::new(path.to_path_buf(), source))
}

fn parse_model(source: SourceFile) -> Result<MznModel> {
	let mut parser = Parser::new();
	parser
		.set_language(&tree_sitter_minizinc::language())
		.unwrap();
	let tree = parser.parse(source.contents().as_bytes(), None).unwrap();
	let cst = Cst::new(tree, source);
	cst.check()?;
	Ok(MznModel::new(cst))
}

fn get_mzn_files<'a>(files: impl Iterator<Item = &'a PathBuf>) -> Result<Vec<PathBuf>> {
	let mut result = Vec::new();
	let mut errors: Vec<Error> = Vec::new();
	for file in files {
		if !file.exists() {
			errors.push(
				FileError {
					file: file.clone(),
					message: "File not found".to_owned(),
					other: Vec::new(),
				}
				.into(),
			);
		} else if file.is_dir() {
			let pattern = format!("{}/**/*.mzn", file.to_string_lossy());
			let paths = glob(&pattern).unwrap_or_else(|err| {
				unreachable!("Glob pattern error: {} at {}:{}", pattern, err.pos, err.msg);
			});
			for p in paths {
				match p {
					Ok(path) => result.push(path),
					Err(err) => {
						errors.push(
							FileError {
								file: err.path().to_path_buf(),
								message: err.to_string(),
								other: Vec::new(),
							}
							.into(),
						);
					}
				}
			}
		} else if file.extension().map(|s| s == "mzn").unwrap_or(false) {
			result.push(file.clone());
		} else {
			errors.push(
				FileError {
					file: file.clone(),
					message: "Unsupported file type".to_owned(),
					other: Vec::new(),
				}
				.into(),
			);
		}
	}
	if errors.is_empty() {
		Ok(result)
	} else if errors.len() == 1 {
		Err(errors.pop().unwrap().into())
	} else {
		Err(MultipleErrors { errors }.into())
	}
}
