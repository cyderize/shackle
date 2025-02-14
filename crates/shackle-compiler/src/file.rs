//! File-related functionality.
//!
//! `FileRef` is an interned data structure used to represent a pointer to a file (or inline string).

use std::{
	ops::Deref,
	panic::{RefUnwindSafe, UnwindSafe},
	path::{Path, PathBuf},
	sync::Arc,
};

use shackle_diagnostics::{FileError, SourceFile};
use shackle_syntax::InputLang;

use crate::db::FileReader;

/// Input files
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum InputFile {
	/// File from filesystem
	Path(PathBuf, InputLang),
	/// Inline model string
	String(String, InputLang),
}

impl InputFile {
	fn lang(&self) -> InputLang {
		match self {
			InputFile::Path(_, l) | InputFile::String(_, l) => *l,
		}
	}
}

/// Interned reference to an input file or external file
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct FileRef(salsa::InternId);

impl salsa::InternKey for FileRef {
	fn from_intern_id(id: salsa::InternId) -> Self {
		Self(id)
	}

	fn as_intern_id(&self) -> salsa::InternId {
		self.0
	}
}

impl FileRef {
	/// Create a new file reference for an external (included) file
	pub fn new(path: &Path, db: &dyn FileReader) -> Self {
		db.intern_file_ref(FileRefData::ExternalFile(path.to_owned()))
	}

	/// Get the file path if any
	pub fn path(&self, db: &dyn FileReader) -> Option<PathBuf> {
		match db.lookup_intern_file_ref(*self) {
			FileRefData::InputFile(i) => match db.input_files()[i] {
				InputFile::Path(ref p, _) => Some(p.clone()),
				_ => None,
			},
			FileRefData::ExternalFile(p) => Some(p),
		}
	}

	/// Get the input language of the file
	pub fn lang(&self, db: &dyn FileReader) -> InputLang {
		match db.lookup_intern_file_ref(*self) {
			FileRefData::InputFile(i) => match db.input_files()[i] {
				InputFile::Path(_, l) => l,
				InputFile::String(_, l) => l,
			},
			FileRefData::ExternalFile(p) => InputLang::from_path(&p),
		}
	}

	/// Get the contents of this file
	pub fn contents(&self, db: &dyn FileReader) -> Result<SourceFile, FileError> {
		db.file_contents(*self)
	}

	/// Pretty print file name for debugging
	pub fn pretty_print(&self, db: &dyn FileReader) -> String {
		self.path(db)
			.map(|p| p.to_string_lossy().to_string())
			.unwrap_or_else(|| "<unnamed file>".to_owned())
	}
}

/// Reference to an input file or external file
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum FileRefData {
	/// From input
	InputFile(usize),
	/// From external source (included file)
	ExternalFile(PathBuf),
}

/// Get `FileRef`s for all input files
pub fn input_file_refs(db: &dyn FileReader) -> Arc<Vec<FileRef>> {
	let size = db.input_files().len();
	Arc::new(
		(0..size)
			.map(|i| db.intern_file_ref(FileRefData::InputFile(i)))
			.collect(),
	)
}

/// Get the contents of a file
pub fn file_contents(db: &dyn FileReader, file: FileRef) -> Result<SourceFile, FileError> {
	match db.lookup_intern_file_ref(file) {
		FileRefData::InputFile(i) => match db.input_files()[i] {
			InputFile::Path(ref p, _) => {
				let h = db.get_file_handler();
				if !h.durable() {
					db.salsa_runtime()
						.report_synthetic_read(salsa::Durability::LOW);
				}
				h.read_file(p)
			}
			InputFile::String(ref s, _) => Ok(SourceFile::unnamed(s.clone())),
		},
		FileRefData::ExternalFile(p) => {
			let h = db.get_file_handler();
			if !h.durable() {
				db.salsa_runtime()
					.report_synthetic_read(salsa::Durability::LOW);
			}
			h.read_file(&p)
		}
	}
}

/// A reference to model file
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct ModelRef(FileRef);

impl From<FileRef> for ModelRef {
	fn from(r: FileRef) -> Self {
		Self(r)
	}
}

impl Deref for ModelRef {
	type Target = FileRef;

	fn deref(&self) -> &Self::Target {
		&self.0
	}
}

/// Get all input model files
pub fn input_models(db: &dyn FileReader) -> Arc<Vec<ModelRef>> {
	Arc::new(
		db.input_files()
			.iter()
			.enumerate()
			.filter_map(|(idx, f)| {
				if matches!(f.lang(), InputLang::MiniZinc | InputLang::EPrime) {
					Some(db.intern_file_ref(FileRefData::InputFile(idx)).into())
				} else {
					None
				}
			})
			.collect(),
	)
}

/// Trait for handling filesystem queries.
///
/// The `DefaultFileHandler` provides a default implementation which reads directly from the filesystem.
pub trait FileHandler: Send + UnwindSafe {
	/// Whether the results are durable (return false if file contents may change)
	fn durable(&self) -> bool {
		true
	}

	/// Read a file and return its contents.
	fn read_file(&self, path: &Path) -> Result<SourceFile, FileError>;

	/// Create a snapshot of the file handler
	fn snapshot(&self) -> Box<dyn FileHandler + RefUnwindSafe>;
}

/// Default file handler which reads from filesystem
#[derive(Clone, Debug)]
pub struct DefaultFileHandler;

impl FileHandler for DefaultFileHandler {
	fn read_file(&self, path: &Path) -> Result<SourceFile, FileError> {
		std::fs::read_to_string(path)
			.map(|contents| SourceFile::new(path.to_owned(), contents))
			.map_err(|err| FileError {
				file: path.to_path_buf(),
				message: err.to_string(),
				other: Vec::new(),
			})
	}

	fn snapshot(&self) -> Box<dyn FileHandler + RefUnwindSafe> {
		Box::new(self.clone())
	}
}
