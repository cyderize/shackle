//! Low-level intermediate representation for the Shackle compiler.
//!
//! IR is imperative and designed to be easy to interpret.

use std::num::NonZeroU32;

use derive_more::{From, TryUnwrap, Unwrap};
#[cfg(test)]
use expect_test as _;
#[cfg(test)]
use salsa as _;
use shackle_hir::{self as _, BooleanLiteral, Db, FloatLiteral, IntegerLiteral};
pub use shackle_mir::{FreshVariable, SetType};
pub use shackle_thir::source::LineCharSpan;
use shackle_utils::arena::{Arena, ArenaIndex, ArenaMap};
pub mod lower;

/// A slot ID
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum SlotId {
	/// Absolute slot ID (for globals)
	Absolute(NonZeroU32),
	/// Relative to current frame
	Relative(NonZeroU32),
}

/// A string in the program
pub type StringId = ArenaIndex<String>;

/// Location in a source file
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Origin {
	/// The file in which the instruction originated
	pub file: StringId,
	/// The span
	pub span: LineCharSpan,
}

/// A program which can be executed
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Program {
	/// The number of slots in the program
	pub global_slot_count: usize,
	/// External inputs in deterministic order.
	pub inputs: Vec<Input>,
	/// The functions
	pub functions: Arena<Function>,
	/// The annotations
	pub annotations: Arena<Annotation>,
	/// The main entry point function
	pub main_entry_point: FunctionId,
	/// The output entry point function
	pub output_entry_point: FunctionId,
	/// Strings in the program
	pub strings: Arena<String>,
}

/// A named external input and its global slot.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Input {
	/// External input name.
	pub name: String,
	/// Absolute global slot used by the input.
	pub slot: SlotId,
}

impl Input {
	/// External input name.
	pub fn name(&self) -> &str {
		&self.name
	}
}

impl Program {
	/// Enumerate external inputs.
	pub fn inputs(&self) -> impl Iterator<Item = &Input> {
		self.inputs.iter()
	}

	/// Find an external input by name.
	pub fn input(&self, name: &str) -> Option<&Input> {
		self.inputs.iter().find(|input| input.name == name)
	}

	/// Get the IR of the program
	pub fn lower_from_mir(db: &dyn Db) -> Program {
		let mir = shackle_mir::Model::lower_from_thir(db);
		lower::lower_model(db, &mir)
	}
}

/// An instruction ID
pub type InstructionId = ArenaIndex<Instruction>;

/// An instruction
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction {
	/// Assign an expression to a slot
	Assign {
		/// Destination
		slot: SlotId,
		/// Expression to assign
		expression: Expression,
	},
	/// Post a constraint
	Constraint {
		/// The constraint to post
		expression: Expression,
	},
	/// Jump to a different instruction
	Jump {
		/// The target instruction ID
		target: InstructionId,
	},
	/// Jump to a different instruction if the condition is flase
	JumpIfFalse {
		/// The slot to check
		condition: Value,
		/// The target instruction ID to jump to if the slot is false
		target: InstructionId,
	},
	/// Push an iterator onto the stack
	IteratorStart {
		/// The value to iterate over
		value: Value,
	},
	/// Get the next value from the iterator on the stack and assign it to a slot
	///
	/// If the iterator is exhausted, jump to the `done` instruction ID and pop the iterator from the stack.
	IteratorNext {
		/// The slot to assign to
		slot: SlotId,
		/// The target instruction ID to jump to if the iterator is exhausted
		done: InstructionId,
	},
	/// Push a comprehension on to the stack
	ComprehensionStart,
	/// Append a value to the current comprehension
	ComprehensionAppend {
		/// Value to append
		value: Value,
	},
	/// Pop the current comprehension from the stack and assign it to a slot
	ComprehensionEnd {
		/// The slot to assign to
		slot: SlotId,
	},
	/// Return from a function
	Return {
		/// Expression to return
		expression: Expression,
	},
	/// Return no value from a function
	ReturnRoot,
	/// No operation
	NoOp,
}

/// A function
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function {
	/// Function name
	pub name: StringId,
	/// Number of slots
	pub slot_count: usize,
	/// Whether the function is ::promise_total
	pub promise_total: bool,
	/// Body of the function (if empty, the function is external)
	pub body: Arena<Instruction>,
	/// Location of instructions in the source file
	pub origins: ArenaMap<Instruction, Origin>,
}

/// A function ID
pub type FunctionId = ArenaIndex<Function>;

/// An expression
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression {
	/// A call
	Call(Call),
	/// An annotation call
	AnnotationCall(AnnotationCall),
	/// An interpreter intrinsic builtin
	Builtin(Builtin),
	/// A new fresh variable
	FreshVariable(FreshVariable<Value>),
	/// A value
	Value(Value),
}

/// A builtin whose arguments are IR values.
pub type Builtin<T = Box<Value>> = shackle_mir::builtins::Builtin<T>;

/// A call type
#[derive(Copy, Debug, Clone, PartialEq, Eq, Hash)]
pub enum CallType {
	/// Add call to NanoZinc
	Predicate,
	/// Evaluate call body immediately
	Function,
	/// Delayed call added to NanoZinc which is evaluated in future iteration
	Delayed,
}

/// A call
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Call {
	/// The function being called
	pub function: FunctionId,
	/// The type of call
	pub kind: CallType,
	/// The arguments
	pub arguments: Vec<Value>,
	/// The annotations
	pub annotations: Vec<Identifier>,
	/// Whether to forward annotations from the call site
	pub forward_annotations: bool,
}

/// An annotation call
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AnnotationCall {
	/// The annotation being called
	pub annotation: AnnotationId,
	/// The arguments
	pub arguments: Vec<Value>,
}

/// A value
#[derive(Clone, Debug, Hash, PartialEq, Eq, From, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum Value {
	/// A literal
	Literal(Literal),
	/// A tuple
	Tuple(Tuple),
	/// A set
	Set(Set),
	/// An array
	Array(Array),
	/// A tuple access
	TupleAccess(TupleAccess),
}

/// A tuple literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Tuple {
	/// Tuple members
	pub members: Vec<Literal>,
}

/// An array literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Array {
	/// Array literal members
	pub members: Vec<Literal>,
}

/// A set literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Set {
	/// Type of set literal
	pub kind: SetType,
	/// Set literal members
	pub members: Vec<Literal>,
}

/// A tuple field access
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TupleAccess {
	/// The tuple being accessed
	pub tuple: Identifier,
	/// The field
	pub field: IntegerLiteral,
}

/// A literal
#[derive(Clone, Debug, Hash, PartialEq, Eq, From, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum Literal {
	/// A boolean
	Boolean(BooleanLiteral),
	/// An integer
	Integer(IntegerLiteral),
	/// A floating point value
	Float(FloatLiteral),
	/// A string
	String(StringId),
	/// Infinity
	Infinity,
	/// Infinite integer set
	InfiniteIntSet,
	/// Infinite float set
	InfiniteFloatSet,
	/// An identifier
	Identifier(Identifier),
}

/// An identifier
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, From)]
pub enum Identifier {
	/// An identifier pointing to a slot
	Slot(SlotId),
	/// An identifier pointing to an annotation
	Annotation(AnnotationId),
}

/// An annotation item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Annotation {
	/// The name of the annotation
	pub name: StringId,
}

/// An annotation ID
pub type AnnotationId = ArenaIndex<Annotation>;
