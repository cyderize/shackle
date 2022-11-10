//! Shackle library

#![warn(missing_docs)]
#![warn(unused_crate_dependencies, unused_extern_crates)]
#![warn(variant_size_differences)]

pub mod arena;
pub mod db;
pub mod error;
pub mod file;
pub mod hir;
mod legacy;
pub mod syntax;
pub mod thir;
pub mod ty;
pub mod utils;

use db::{CompilerDatabase, FileReader, Inputs};
use error::{FileError, MultipleErrors, ShackleError};
use file::{FileRefData, InputFile};
use serde_json::Map;
use tempfile::{Builder, NamedTempFile};

use std::{
	collections::BTreeMap,
	env,
	fmt::{self, Display},
	io::Write,
	ops::Range,
	path::{Path, PathBuf},
	sync::Arc,
	time::{Duration, Instant},
};

use crate::{
	hir::db::Hir,
	thir::{db::Thir, pretty_print::PrettyPrinter},
};

/// Shackle error type
pub type Error = ShackleError;
/// Result type for Shackle operations
pub type Result<T, E = Error> = std::result::Result<T, E>;

/// Parses a list of MiniZinc files given located using the Paths in the vector
pub fn parse_files(paths: Vec<&Path>) -> Result<()> {
	let now = Instant::now();
	let mut db = db::CompilerDatabase::new();
	db.set_input_files(Arc::new(
		paths
			.into_iter()
			.map(|p| InputFile::Path(p.to_owned()))
			.collect(),
	));
	let mut errors = (*db.all_diagnostics()).clone();
	eprintln!("Done in {}ms", now.elapsed().as_millis());
	if errors.is_empty() {
		// Can print THIR if there were no errors
		println!(
			"{}",
			PrettyPrinter::new(&db, &db.model_thir()).pretty_print()
		);
		Ok(())
	} else if errors.len() == 1 {
		Err(errors.pop().unwrap())
	} else {
		Err(MultipleErrors { errors }.into())
	}
}

/// Structure used to build a shackle model
#[derive(Default)]
pub struct Model {
	files: Vec<InputFile>,
	stdlib: Option<PathBuf>,
}

impl Model {
	/// Create a Model from the file at the given path
	pub fn from_file(path: PathBuf) -> Model {
		Model {
			files: vec![InputFile::Path(path)],
			stdlib: None,
		}
	}

	/// Create a Model from the given string
	pub fn from_string(m: String) -> Model {
		Model {
			files: vec![InputFile::ModelString(m)],
			stdlib: None,
		}
	}

	/// Compile current model into a Program that can be used by the Shackle interpreter
	pub fn compile(&self, slv: &Solver) -> Result<Program> {
		let mut db = db::CompilerDatabase::new();
		db.set_input_files(Arc::new(self.files.clone()));

		let mut search_dirs = Vec::new();
		// TODO: New STDLIB behaviour
		if let Some(path) = &self.stdlib {
			search_dirs.push(path.clone())
		} else if let Ok(pathstr) = env::var("MZN_STDLIB_DIR") {
			search_dirs.push(PathBuf::from(pathstr).join("std"))
		}
		db.set_search_directories(Arc::new(search_dirs));
		let errors = db.all_diagnostics();
		if !errors.is_empty() {
			if errors.len() == 1 {
				return Err(errors.last().unwrap().clone());
			}
			if errors.len() > 1 {
				return Err(MultipleErrors {
					errors: (*errors).clone(),
				}
				.into());
			}
		}
		let output = Builder::new()
			.suffix(".mzn")
			.tempfile()
			.map_err(|err| FileError {
				file: PathBuf::from("tempfile"),
				message: err.to_string(),
				other: Vec::new(),
			})?;
		for file in db
			.input_files()
			.iter()
			.enumerate()
			.filter_map(|(idx, f)| match f {
				InputFile::Path(p) => match p.extension() {
					Some(e) => {
						if e.to_str() == Some("mzn") {
							Some(db.intern_file_ref(FileRefData::InputFile(idx)))
						} else {
							None
						}
					}
					None => None,
				},
				InputFile::ModelString(_) => Some(db.intern_file_ref(FileRefData::InputFile(idx))),
				_ => None,
			}) {
			let contents = db.file_contents(file)?;
			output
				.as_file()
				.write_all(contents.as_bytes())
				.map_err(|e| FileError {
					file: PathBuf::from(output.path()),
					message: format!("{}", e),
					other: vec![],
				})?;
		}
		Ok(Program {
			db,
			slv: slv.clone(),
			code: output,
			enable_stats: false,
			time_limit: None,
		})
	}
}

/// Solver specification to compile and solve Model instances.
#[derive(Clone)]
pub struct Solver {
	// TODO: actual information (Load from solver configurations)
	/// Identifier of the solver
	ident: String,
}

impl Solver {
	/// Lookup a solver specification in default locations that best matches the given identifier
	pub fn lookup(ident: &str) -> Option<Solver> {
		Some(Solver {
			ident: ident.into(),
		})
	}
}

/// Structure to capture the result of succesful compilation of a Model object
pub struct Program {
	db: CompilerDatabase,
	slv: Solver,
	code: NamedTempFile,
	enable_stats: bool,
	time_limit: Option<Duration>,
}

/// Status of running and solving a Program
pub enum Status {
	/// No solutions exist
	Infeasible,
	/// A solution has been found
	Satisfied,
	/// A solution with the best possible objective value has been found
	Optimal,
	/// All possible solutions have been found
	AllSolutions,
	/// No result reached within the given limits
	Unknown,
	/// An error occurred
	Err(ShackleError),
}

/// Value types that can be part of a Solution
pub enum Value {
	/// Absence of an optional value
	Absent,
	/// Boolean
	Boolean(bool),
	/// Signed integer
	Integer(i64),
	/// Floating point
	Float(f64),
	/// String
	String(String),
	/// Identifier of a value of an enumerated type
	// FIXME this should probably have the actual structuring of enumerated types
	Enum(String),
	/// An array of values
	/// All values are of the same type
	Array(Vec<Range<i64>>, Vec<Value>),
	/// A set of values
	/// All values are of the same type and only occur once
	Set(Vec<Value>),
	/// A tuple of values
	Tuple(Vec<Value>),
	/// A record of values
	Record(BTreeMap<String, Value>),
}

impl Display for Value {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Value::Absent => write!(f, "<>"),
			Value::Boolean(v) => write!(f, "{}", v),
			Value::Integer(v) => write!(f, "{}", v),
			Value::Float(v) => write!(f, "{}", v),
			Value::String(v) => write!(f, "{:?}", v),
			Value::Enum(v) => write!(f, "{}", v),
			Value::Array(idx, v) => {
				let mut ii: Vec<i64> = idx.iter().map(|r| r.start).collect();
				let incr = |ii: &mut Vec<i64>| {
					let mut i = ii.len() - 1;
					ii[i] += 1;
					while !idx[i].contains(&ii[i]) && i > 0 {
						ii[i] = idx[i].start;
						i = i.wrapping_sub(1);
						ii[i] += 1;
					}
				};
				let mut first = true;
				write!(f, "[")?;
				for x in v {
					if !first {
						write!(f, ", ")?;
					}
					match &ii[..] {
						[i] => write!(f, "{i}: "),
						ii => {
							write!(f, "(")?;
							let mut tup_first = true;
							for i in ii {
								if !tup_first {
									write!(f, ",")?;
								}
								write!(f, "{i}")?;
								tup_first = false;
							}
							write!(f, "): ")
						}
					}?;
					write!(f, "{x}")?;
					first = false;
					incr(&mut ii);
				}
				write!(f, "]")
			}
			Value::Set(v) => {
				let mut first = true;
				write!(f, "{{")?;
				for x in v {
					if !first {
						write!(f, ", ")?;
					}
					write!(f, "{}", x)?;
					first = false;
				}
				write!(f, "}}")
			}
			Value::Tuple(v) => {
				let mut first = true;
				write!(f, "(")?;
				for x in v {
					if !first {
						write!(f, ", ")?;
					}
					write!(f, "{}", x)?;
					first = false;
				}
				write!(f, ")")
			}
			Value::Record(rec) => {
				let mut first = true;
				write!(f, "(")?;
				for (k, v) in rec {
					if !first {
						write!(f, ", ")?;
					}
					write!(f, "{}: {}", k, v)?;
					first = false;
				}
				write!(f, ")")
			}
		}
	}
}

/// Intermediate messages emitted by shackle in processing and solving a program
pub enum Message<'a> {
	/// (Intermediate) solution emitted in the process
	Solution(BTreeMap<String, Value>),
	/// Statistical information of the shackle or solving process
	Statistic(&'a Map<String, serde_json::Value>),
	/// Trace messages emitted during the shackle process
	Trace(&'a str),
	/// Warning messages emitted by shackle or the solver
	Warning(&'a str),
}

impl<'a> Display for Message<'a> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match self {
			Message::Solution(sol) => {
				for (name, val) in sol {
					writeln!(f, "{} = {};", name, val)?;
				}
				writeln!(f, "----------")
			}
			Message::Statistic(map) => {
				for (name, val) in *map {
					writeln!(f, "%%%mzn-stat: {}={}", name, val)?;
				}
				writeln!(f, "%%%mzn-stat-end")
			}
			Message::Trace(msg) => write!(f, "% mzn-trace: {}", msg),
			Message::Warning(msg) => write!(f, "% WARNING: {}", msg),
		}
	}
}

impl Program {
	/// Set whether messages containing statistical information regarding running the program should be sent
	pub fn with_statistics(mut self, stats: bool) -> Self {
		self.enable_stats = stats;
		self
	}
	/// Add the maximum duration that the run method is allowed to take before it will be canceled
	pub fn with_time_limit(mut self, dur: Duration) -> Self {
		self.time_limit = Some(dur);
		self
	}
}

#[cfg(test)]
mod tests {
	#[test]
	fn it_works() {
		let result = 2 + 2;
		assert_eq!(result, 4);
	}
}