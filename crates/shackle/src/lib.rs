//! Shackle library

#![warn(missing_docs)]
#![warn(unused_crate_dependencies, unused_extern_crates)]
#![warn(variant_size_differences)]

pub mod arena;
pub mod constants;
pub mod db;
pub mod diagnostics;
pub mod dzn;
pub mod file;
pub mod hir;
mod legacy;
pub mod mir;
pub mod refmap;
pub mod syntax;
pub mod thir;
pub mod ty;
pub mod utils;

use db::{CompilerDatabase, Inputs};
use diagnostics::{InternalError, ShackleError};
use file::InputFile;
use itertools::Itertools;
use rustc_hash::FxHashMap;
use serde_json::Map;
use ty::{Ty, TyData};

use std::{
	collections::BTreeMap,
	fmt::{self, Display},
	io::Write,
	ops::RangeInclusive,
	path::PathBuf,
	sync::Arc,
	time::Duration,
};

use crate::{
	hir::db::Hir,
	thir::{db::Thir, pretty_print::PrettyPrinter},
};

// Export OptType enumeration used in [`Type`]
pub use ty::OptType;

/// Shackle error type
pub type Error = ShackleError;
/// Result type for Shackle operations
pub type Result<T, E = Error> = std::result::Result<T, E>;

pub use diagnostics::Warning;

/// Structure used to build a shackle model
pub struct Model {
	db: CompilerDatabase,
}

impl Model {
	/// Create a Model from the file at the given path
	pub fn from_file(path: PathBuf) -> Model {
		let mut db = db::CompilerDatabase::default();
		db.set_input_files(Arc::new(vec![InputFile::Path(path)]));
		Model { db }
	}

	/// Create a Model from the given string
	pub fn from_string(m: String) -> Model {
		let mut db = db::CompilerDatabase::default();
		db.set_input_files(Arc::new(vec![InputFile::ModelString(m)]));
		Model { db }
	}

	/// Check whether a model contains any (non-runtime) errors
	pub fn check(&self, _slv: &Solver, _data: &[PathBuf], _complete: bool) -> Vec<Error> {
		// TODO: Check data files
		self.db
			.run_hir_phase()
			.map(|_| Vec::new())
			.unwrap_or_else(|e| e.iter().cloned().collect())
	}

	/// Compile current model into a Program that can be used by the Shackle interpreter
	pub fn compile(self, slv: &Solver) -> Result<Program> {
		let errors = self.check(slv, &[], false);
		if !errors.is_empty() {
			return Err(ShackleError::try_from(errors).unwrap());
		}
		let prg_model = self.db.final_thir()?;

		let input_types = self
			.db
			.input_type_map()
			.iter()
			.map(|(ident, ty)| match Type::from_compiler(&self.db, *ty) {
				Ok(nty) => Ok((ident.0.value(&self.db), nty)),
				Err(e) => Err(e),
			})
			.collect::<Result<FxHashMap<_, _>, _>>()?;

		let output_types = self
			.db
			.output_type_map()
			.iter()
			.map(|(ident, ty)| match Type::from_compiler(&self.db, *ty) {
				Ok(nty) => Ok((ident.0.value(&self.db), nty)),
				Err(e) => Err(e),
			})
			.collect::<Result<FxHashMap<_, _>, _>>()?;

		Ok(Program {
			db: self.db,
			slv: slv.clone(),
			code: prg_model,
			_input_types: input_types,
			_input_data: FxHashMap::default(),
			_output_types: output_types,
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
	// FIXME: CompilerDatabase should (probably) not be part of Program anymore
	db: CompilerDatabase,
	code: Arc<thir::Model>,
	slv: Solver,
	// Model instance data
	_input_types: FxHashMap<String, Type>,
	_input_data: FxHashMap<String, Value>,

	_output_types: FxHashMap<String, Type>,
	// run() options
	enable_stats: bool,
	time_limit: Option<Duration>,
}

/// Status of running and solving a Program
#[derive(Debug, Clone)]
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
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
	/// Absence of an optional value
	Absent,
	/// Infinity (+∞ or -∞)
	Infinity(Polarity),
	/// Boolean
	Boolean(bool),
	/// Signed integer
	Integer(i64),
	/// Floating point
	Float(f64),
	/// String
	String(String),
	/// Identifier of a value of an enumerated type
	Enum(EnumValue),
	/// An array of values
	/// All values are of the same type
	Array(Array),
	/// A set of values
	/// All values are of the same type and only occur once
	Set(Set),
	/// A tuple of values
	Tuple(Vec<Value>),
	/// A record of values
	Record(Record),
}

/// An type of the input or output of a Shackle model
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
	/// Boolean scalar
	Boolean(OptType),
	/// Integer scalar
	Integer(OptType),
	/// Float scalar
	Float(OptType),
	/// Enumerated type scalar
	Enum(OptType, Option<Arc<Enum>>),

	/// String scalar
	String(OptType),
	/// Annotation scalar
	Annotation(OptType),

	/// Array type
	Array {
		/// Whether the array is optional
		opt: OptType,
		/// Type used for indexing
		dim: Box<[Type]>,
		/// Type of the element
		element: Box<Type>,
	},
	/// Set type
	Set(OptType, Box<Type>),
	/// Tuple type
	Tuple(OptType, Box<[Type]>),
	/// Record type
	Record(OptType, Box<[(String, Type)]>),
}

impl Type {
	fn from_compiler(db: &CompilerDatabase, value: Ty) -> Result<Self, ShackleError> {
		let data = value.lookup(db);
		match data {
			TyData::Boolean(_, opt) => Ok(Type::Boolean(opt)),
			TyData::Integer(_, opt) => Ok(Type::Integer(opt)),
			TyData::Float(_, opt) => Ok(Type::Float(opt)),
			TyData::Enum(_, opt, _) => Ok(Type::Enum(opt, None)), // TODO: Fix None
			TyData::String(opt) => Ok(Type::String(opt)),
			TyData::Annotation(opt) => Ok(Type::Annotation(opt)),
			TyData::Array { opt, dim, element } => {
				let elem = Type::from_compiler(db, element)?;
				let index_conv = |nty| -> Result<Type, ShackleError> {
					match nty {
						TyData::Integer(ty::VarType::Par, OptType::NonOpt) => {
							Ok(Type::Integer(OptType::NonOpt))
						}
						TyData::Enum(ty::VarType::Par, OptType::NonOpt, _) => {
							Ok(Type::Enum(OptType::NonOpt, None)) // TODO: Fix None
						}
						_ => Err(InternalError::new(format!(
							"Unexpected index set type on user facing type {:?}",
							nty
						))
						.into()),
					}
				};
				let ndim = match dim.lookup(db) {
					TyData::Tuple(OptType::NonOpt, li) => li
						.iter()
						.map(|ty| index_conv(ty.lookup(db)))
						.collect::<Result<Vec<_>, _>>()?,
					x => {
						let nty = index_conv(x)?;
						vec![nty]
					}
				};
				Ok(Type::Array {
					opt,
					dim: ndim.into_boxed_slice(),
					element: Box::new(elem),
				})
			}
			TyData::Set(_, opt, elem) => {
				Ok(Type::Set(opt, Box::new(Type::from_compiler(db, elem)?)))
			}
			TyData::Tuple(opt, li) => Ok(Type::Tuple(
				opt,
				li.iter()
					.map(|ty| Type::from_compiler(db, *ty))
					.collect::<Result<Vec<_>, _>>()?
					.into_boxed_slice(),
			)),
			TyData::Record(opt, li) => Ok(Type::Record(
				opt,
				li.iter()
					.map(|(name, ty)| match Type::from_compiler(db, *ty) {
						Ok(nty) => Ok((name.value(db), nty)),
						Err(e) => Err(e),
					})
					.collect::<Result<Vec<_>, _>>()?
					.into_boxed_slice(),
			)),
			_ => Err(InternalError::new(format!(
				"Unable to create user facing type from {:?}",
				data
			))
			.into()),
		}
	}
}

/// Whether an value is negative or positive
///
/// For example, used for the constant infinity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Polarity {
	/// Positive
	Pos,
	/// Negative
	Neg,
}

impl Display for Value {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Value::Absent => write!(f, "<>"),
			Value::Infinity(p) => {
				if p == &Polarity::Neg {
					write!(f, "-")?;
				};
				write!(f, "∞")
			}
			Value::Boolean(v) => write!(f, "{v}"),
			Value::Integer(v) => write!(f, "{v}"),
			Value::Float(v) => write!(f, "{v}"),
			Value::String(v) => write!(f, "{:?}", v),
			Value::Enum(v) => write!(f, "{v}"),
			Value::Array(arr) => {
				write!(f, "{arr}")
			}
			Value::Set(v) => {
				write!(f, "{v}")
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
				write!(f, "{rec}")
			}
		}
	}
}

/// Representation of an (multidimensional) indexed array
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Array {
	indexes: Box<[Index]>,
	members: Box<[Value]>,
}

impl Array {
	/// Create a new array that contains the values in `elements` indexes by the given index sets
	pub fn new(indexes: Vec<Index>, elements: Vec<Value>) -> Self {
		assert_eq!(
			indexes.iter().map(|i| i.len()).product::<usize>(),
			elements.len(),
			"the size suggested by the index sets {} does not match the number of elements {}",
			indexes.iter().map(|i| i.len()).product::<usize>(),
			elements.len()
		);
		Self {
			indexes: indexes.into_boxed_slice(),
			members: elements.into_boxed_slice(),
		}
	}
}

impl std::ops::Index<&[Value]> for Array {
	type Output = Value;
	fn index(&self, index: &[Value]) -> &Self::Output {
		let mut idx = 0;
		let mut mult = 1;
		for (ii, ctx) in index.iter().zip_eq(self.indexes.iter()) {
			idx *= mult;
			match ctx {
				Index::Integer(r) => {
					if let Value::Integer(ii) = ii {
						assert!(
							r.contains(ii),
							"index out of bounds: the index set is {}..={} but the index is {ii}",
							r.start(),
							r.end()
						);
						idx += (ii - r.start()) as usize;
					} else {
						panic!("incorrect index type: using {ii} for an integer index")
					}
				}
				Index::Enum(e) => {
					if let Value::Enum(val) = ii {
						if e == &val.set {
							idx += val.val
						} else {
							panic!("incorrect index type: using value of type {} for an index of type {}", 
							if let Some(name) = &e.name {name.as_str()} else{"anonymous enum"},
							if let Some(name) = &val.set.name {name.as_str()} else{"anonymous enum"},)
						}
					} else {
						panic!(
							"incorrect index type: using {ii} for an index of type {}",
							if let Some(name) = &e.name {
								name.as_str()
							} else {
								"anonymous enum"
							}
						)
					}
				}
			}
			mult *= ctx.len();
		}
		&self.members[idx]
	}
}

impl Display for Array {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let it = self
			.indexes
			.iter()
			.map(|ii| ii.iter())
			.multi_cartesian_product()
			.zip_eq(self.members.iter());
		let mut first = true;
		write!(f, "[")?;
		for (ii, x) in it {
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
		}
		write!(f, "]")
	}
}

/// Representation of Array indexes
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Index {
	/// Closed integer range index
	Integer(RangeInclusive<i64>),
	/// Enumerated type used as an index
	Enum(Arc<Enum>),
}

impl Index {
	/// Returns the cardinality of the index set
	pub fn len(&self) -> usize {
		match self {
			Index::Integer(r) => {
				if r.is_empty() {
					0
				} else {
					(r.end() - r.start()) as usize + 1
				}
			}
			Index::Enum(e) => e.len(),
		}
	}

	/// Returns whether the index set contains any members
	pub fn is_empty(&self) -> bool {
		match &self {
			Index::Integer(r) => r.is_empty(),
			Index::Enum(e) => e.is_empty(),
		}
	}

	fn iter(&self) -> IndexIter {
		match self {
			Index::Integer(x) => IndexIter::Integer(x.clone()),
			Index::Enum(e) => IndexIter::Enum(e.clone(), 1..=e.len()),
		}
	}
}

#[derive(Debug, Clone)]
enum IndexIter {
	Integer(RangeInclusive<i64>),
	Enum(Arc<Enum>, RangeInclusive<usize>),
}

impl Iterator for IndexIter {
	type Item = Value;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			IndexIter::Integer(r) => r.next().map(Value::Integer),
			IndexIter::Enum(e, r) => r.next().map(|v| {
				Value::Enum(EnumValue {
					set: e.clone(),
					val: v,
				})
			}),
		}
	}
}

/// Member declaration of an enumerated type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
	name: Option<String>,
	constructors: Vec<(String, Option<Index>)>,
}

impl Enum {
	/// Returns the number of members of the enumerated type
	pub fn len(&self) -> usize {
		self.constructors
			.iter()
			.map(|(_, i)| if let Some(i) = i { i.len() } else { 1 })
			.sum()
	}

	/// Returns whether the enumerated type has any members
	pub fn is_empty(&self) -> bool {
		self.constructors.is_empty()
	}
}

/// Member declaration of an enumerated type
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumValue {
	set: Arc<Enum>,
	val: usize,
}

impl EnumValue {
	/// Create a new enumerated value that is not yet associated with an
	/// enumerated type
	///
	/// This method is intended for new enumerated values created by data
	/// parsers
	pub(crate) fn new_ident_member(ident: String) -> Self {
		let new_enum = Enum {
			name: None,
			constructors: vec![(ident, None)],
		};
		EnumValue {
			set: Arc::new(new_enum),
			val: 1,
		}
	}

	/// Create a new constructor based enumerated value that is not yet
	/// associated with an enumerated type
	///
	/// This method is intended for new enumerated values created by data
	/// parsers
	pub(crate) fn new_constructor_member(constructor: String, val: Value) -> EnumValue {
		let (index, val) = match val {
			Value::Integer(i) => (Index::Integer(i..=i), 1),
			Value::Enum(e) => (Index::Enum(e.set), e.val),
			_ => panic!(
				"constructing an enumerated value with an argument of unexpected type {}",
				val
			),
		};
		let new_enum = Enum {
			name: None,
			constructors: vec![(constructor, Some(index))],
		};
		EnumValue {
			set: Arc::new(new_enum),
			val,
		}
	}
	/// Create a new value of an anonymous enumerated type that is not yet
	/// associated with an enumerated type
	///
	/// This method is intended for new enumerated values created by data
	/// parsers
	pub(crate) fn new_anon_member(type_ident: String, i: usize) -> EnumValue {
		let new_enum = Enum {
			name: Some(type_ident),
			constructors: vec![("_".to_string(), Some(Index::Integer(1..=i as i64)))],
		};
		EnumValue {
			set: Arc::new(new_enum),
			val: i,
		}
	}

	/// Internal function used to find the constructor definition in the
	/// enumerated type and the position the value has within this constructor
	pub(crate) fn constructor_and_pos(&self) -> (&String, &Option<Index>, usize) {
		let mut i = self.val;
		let c = self
			.set
			.constructors
			.iter()
			.skip_while(|c| {
				let len = if let Some(ii) = &c.1 { ii.len() } else { 1 };
				if i > len {
					i -= len;
					true
				} else {
					false
				}
			})
			.take(1)
			.next()
			.unwrap();
		(&c.0, &c.1, i)
	}

	/// Returns the enumerated type to which this enumerated value belongs
	///
	/// ## Warning
	/// On parsed data the enumerated type might be a placeholder with only
	/// information required to fit the data to a `Program`.
	pub fn enum_type(&self) -> Arc<Enum> {
		self.set.clone()
	}

	/// Returns the name used to construct the value of the enumerated type
	///
	/// The method returns [`None`] if the enumerated type is anonymous
	pub fn constructor(&self) -> Option<&str> {
		let (c, _, _) = self.constructor_and_pos();
		if c == "_" {
			None
		} else {
			Some(c.as_str())
		}
	}

	/// Returns the argument used to construct the value of the enumerated type
	///
	/// This method resturns [`None`] if no argument was used to construct the
	/// value
	pub fn arg(&self) -> Option<Value> {
		let (_, index, i) = self.constructor_and_pos();
		match index {
			Some(Index::Enum(e)) => Some(Value::Enum(EnumValue {
				set: e.clone(),
				val: i,
			})),
			Some(Index::Integer(range)) => Some(Value::Integer(range.start() + i as i64 - 1)),
			None => {
				debug_assert!(i == 1);
				None
			}
		}
	}
}

impl Display for EnumValue {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self.constructor() {
			Some(constructor) => match self.arg() {
				Some(arg) => write!(f, "{constructor}({arg})"),
				None => write!(f, "{}", constructor),
			},
			None => write!(
				f,
				"to_enum({}, {})",
				if let Some(name) = &self.set.name {
					name.as_str()
				} else {
					"_"
				},
				self.arg().unwrap()
			),
		}
	}
}

/// Different representations used to represent sets in [`Value`]
#[derive(Debug, Clone, PartialEq)]
pub enum Set {
	/// List of (unique) Value elements
	SetList(Vec<Value>),
	/// Set that spans all members of an enumerated type
	Enum(Arc<Enum>),
	/// Sorted list of non-overlapping inclusive integer ranges
	IntRangeList(Vec<RangeInclusive<i64>>),
	/// Sorted list of non-overlapping inclusive floating point ranges
	FloatRangeList(Vec<RangeInclusive<f64>>),
}

impl Display for Set {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		match self {
			Set::SetList(v) => {
				if v.is_empty() {
					return write!(f, "∅");
				}
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
			Set::Enum(e) => {
				write!(
					f,
					"{}",
					if let Some(name) = &e.name {
						name.as_str()
					} else {
						"_"
					}
				)
			}
			Set::IntRangeList(ranges) => {
				if ranges.is_empty() || (ranges.len() == 1 && ranges.last().unwrap().is_empty()) {
					return write!(f, "∅");
				}
				let mut first = true;
				for range in ranges {
					if !first {
						write!(f, " union ")?;
					}
					write!(f, "{}..{}", range.start(), range.end())?;
					first = false;
				}
				Ok(())
			}
			Set::FloatRangeList(ranges) => {
				if ranges.is_empty() || (ranges.len() == 1 && ranges.last().unwrap().is_empty()) {
					return write!(f, "∅");
				}
				let mut first = true;
				for range in ranges {
					if !first {
						write!(f, " union ")?;
					}
					write!(f, "{}..{}", range.start(), range.end())?;
					first = false;
				}
				Ok(())
			}
		}
	}
}

/// A value of a record type
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Record {
	// fields are hidden to possibly replace inner implementation in the future
	fields: Vec<(Arc<String>, Value)>,
}

impl FromIterator<(Arc<String>, Value)> for Record {
	fn from_iter<T: IntoIterator<Item = (Arc<String>, Value)>>(iter: T) -> Self {
		let mut fields: Vec<(Arc<String>, Value)> = iter.into_iter().collect();
		fields.sort_by(|(k1, _), (k2, _)| k1.as_str().cmp(k2.as_str()));
		Self { fields }
	}
}
impl<'a> IntoIterator for &'a Record {
	type Item = &'a (Arc<String>, Value);
	type IntoIter = std::slice::Iter<'a, (Arc<String>, Value)>;

	#[inline]
	fn into_iter(self) -> Self::IntoIter {
		self.fields.iter()
	}
}
impl std::ops::Index<&str> for Record {
	type Output = Value;

	fn index(&self, index: &str) -> &Self::Output {
		for (k, v) in &self.fields {
			if k.as_str() == index {
				return v;
			}
		}
		panic!("no entry found for key");
	}
}

impl Display for Record {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let mut first = true;
		write!(f, "(")?;
		for (k, v) in &self.fields {
			if !first {
				write!(f, ", ")?;
			}
			write!(f, "{}: {}", *k, v)?;
			first = false;
		}
		write!(f, ")")
	}
}

/// Intermediate messages emitted by shackle in processing and solving a program
#[derive(Debug)]
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
	/// Output the [`Pogram`] using the given output interface, using the [`Write`] trait
	pub fn write<W: Write>(&self, out: &mut W) -> Result<(), std::io::Error> {
		let printer = PrettyPrinter::new_compat(&self.db, &self.code);
		out.write_all(printer.pretty_print().as_bytes())
	}
}

#[cfg(test)]
mod tests {}
