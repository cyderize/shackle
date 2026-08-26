//! Low-level intermediate representation for the Shackle compiler.
//!
//! IR is imperative and designed to be easy to interpret.

use derive_more::{From, TryUnwrap, Unwrap};
#[cfg(test)]
use expect_test as _;
#[cfg(test)]
use salsa as _;
use shackle_hir::{self as _, BooleanLiteral, FloatLiteral, IntegerLiteral, StringLiteral};
use shackle_mir::{Name, liveness::SlotId};
use shackle_thir as _;
use shackle_utils::arena::{Arena, ArenaIndex};

pub mod lower;

/// A global slot
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GlobalSlot<'db> {
	/// The name of the global slot
	pub name: Option<Name<'db>>,
}

/// A program which can be executed
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Program<'db> {
	/// The global slots
	globals: Arena<GlobalSlot<'db>>,
	/// The functions
	functions: Arena<Function<'db>>,
	/// The annotations
	annotations: Arena<Annotation<'db>>,
	/// The main entry point function
	main_entry_point: FunctionId<'db>,
	/// The output entry point function
	output_entry_point: FunctionId<'db>,
}

/// A global slot ID
pub type GlobalSlotId<'db> = ArenaIndex<GlobalSlot<'db>>;

/// An instruction ID
pub type InstructionId<'db> = ArenaIndex<Instruction<'db>>;

/// An instruction
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Instruction<'db> {
	/// Assign an expression to a slot
	Assign {
		/// Destination
		slot: SlotId,
		/// Expression to assign
		expression: Expression<'db>,
	},
	/// Post a constraint
	Constraint {
		/// The constraint to post
		expression: Expression<'db>,
	},
	/// Jump to a different instruction
	Jump {
		/// The target instruction ID
		target: InstructionId<'db>,
	},
	/// Jump to a different instruction if the value in the slot is false
	JumpIfFalse {
		/// The slot to check
		slot: SlotId,
		/// The target instruction ID to jump to if the slot is false
		target: InstructionId<'db>,
	},
	/// Push an iterator onto the stack
	IteratorStart {
		/// The value to iterate over
		value: Value<'db>,
	},
	/// Get the next value from the iterator on the stack and assign it to a slot
	///
	/// If the iterator is exhausted, jump to the `done` instruction ID and pop the iterator from the stack.
	IteratorNext {
		/// The slot to assign to
		slot: SlotId,
		/// The target instruction ID to jump to if the iterator is exhausted
		done: InstructionId<'db>,
	},
}

/// A function
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function<'db> {
	/// Function name
	name: Name<'db>,
	/// Number of slots
	slot_count: usize,
	/// Body of the function (if empty, the function is external)
	body: Arena<Instruction<'db>>,
}

/// A function ID
pub type FunctionId<'db> = ArenaIndex<Function<'db>>;

/// An expression
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Expression<'db> {
	/// A call
	Call(Call<'db>),
	/// An annotation call
	AnnotationCall(AnnotationCall<'db>),
	/// An interpreter intrinsic builtin
	Builtin(Builtin<'db>),
	/// A value
	Value(Value<'db>),
}

/// A builtin whose arguments are IR values.
pub type Builtin<'db> = shackle_mir::builtins::Builtin<Box<Value<'db>>>;

/// A call
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Call<'db> {
	/// The function being called
	pub function: FunctionId<'db>,
	/// The arguments
	pub arguments: Vec<Value<'db>>,
}

/// An annotation call
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AnnotationCall<'db> {
	/// The annotation being called
	pub annotation: AnnotationId<'db>,
	/// The arguments
	pub arguments: Vec<Value<'db>>,
}

/// A value
#[derive(Clone, Debug, Hash, PartialEq, Eq, From, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum Value<'db> {
	/// A literal
	Literal(Literal<'db>),
	/// A tuple
	Tuple(Tuple<'db>),
	/// A set
	Set(Set<'db>),
	/// An array
	Array(Array<'db>),
	/// A tuple access
	TupleAccess(TupleAccess<'db>),
}

/// A tuple literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Tuple<'db> {
	/// Tuple members
	pub members: Vec<Literal<'db>>,
}

/// An array literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Array<'db> {
	/// Array literal members
	pub members: Vec<Literal<'db>>,
}

/// A set literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Set<'db> {
	/// Set literal members
	pub members: Vec<Literal<'db>>,
}

/// A tuple field access
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TupleAccess<'db> {
	/// The tuple being accessed
	pub tuple: Identifier<'db>,
	/// The field
	pub field: IntegerLiteral,
}

/// A literal
#[derive(Clone, Debug, Hash, PartialEq, Eq, From, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum Literal<'db> {
	/// A boolean
	Boolean(BooleanLiteral),
	/// An integer
	Integer(IntegerLiteral),
	/// A floating point value
	Float(FloatLiteral),
	/// A string
	String(StringLiteral<'db>),
	/// Infinity
	Infinity,
	/// Infinite integer set
	InfiniteIntSet,
	/// Infinite float set
	InfiniteFloatSet,
	/// An identifier
	Identifier(Identifier<'db>),
}

/// An identifier
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, From)]
pub enum Identifier<'db> {
	/// An identifier pointing to a slot
	Slot(SlotId),
	/// A global variable or parameter
	GlobalSlot(GlobalSlotId<'db>),
	/// An identifier pointing to an annotation
	Annotation(AnnotationId<'db>),
}

/// An annotation item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Annotation<'db> {
	/// The name of the annotation
	pub name: Name<'db>,
}

/// An annotation ID
pub type AnnotationId<'db> = ArenaIndex<Annotation<'db>>;
