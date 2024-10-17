//! Mid-level IR

pub mod db;
pub mod lower;
pub mod pretty_print;
pub mod ty;

pub mod builtins;

pub use builtins::Builtin;
use ty::Ty;

use crate::{
	hir::{BooleanLiteral, FloatLiteral, Identifier, IntegerLiteral, StringLiteral},
	thir::source::Origin,
	utils::{
		arena::{Arena, ArenaIndex},
		impl_enum_from,
	},
};

#[allow(dead_code)]
/// A mid-level IR program (MicroZinc)
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Model {
	/// Main entry point
	pub entrypoint: Expression,
	/// Annotation items
	pub annotations: Arena<Annotation>,
	/// Function items
	pub functions: Arena<Function>,
}

/// A parameter for a function or annotation
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parameter {
	/// The type of the parameter
	pub ty: Ty,
	/// The name of the parameter
	pub name: Identifier,
}

/// An annotation item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Annotation {
	/// Name of the annotation item
	pub name: Identifier,
	/// The parameters for the annotation item
	pub parameters: Vec<Parameter>,
}

/// A constraint item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Constraint {
	/// The expression which must hold
	pub expression: Expression,
	/// The annotations
	pub annotations: Vec<AnnotationRef>,
}

/// An annotation
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum AnnotationRef {
	/// Identifier for declaration with a RHS expression
	Identifier(Identifier),
	/// Direct reference to annotation definition
	Reference(AnnotationId),
}

/// The ID of an annotation item
pub type AnnotationId = ArenaIndex<Annotation>;

/// A declaration item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Declaration {
	/// The type of the variable
	pub ty: Ty,
	/// The domain of the variable
	pub domain: Option<Domain>,
	/// The name of the declaration
	pub name: Identifier,
	/// The RHS definition
	pub definition: Option<Expression>,
	/// The annotations
	pub annotations: Vec<AnnotationRef>,
}

/// A domain
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Domain {
	/// Identifier for domain
	Identifier(Identifier),
	/// Fully evaluated set domain
	Set(Set),
}

#[allow(dead_code)]
/// A function item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Function {
	/// The function name
	pub name: Identifier,
	/// The return type of the function
	pub ty: Ty,
	/// The function parameters
	pub parameters: Vec<Parameter>,
	/// The function body
	pub body: Option<Expression>,
}

/// An expression
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Expression {
	/// The expression data
	pub data: ExpressionData,
	/// The type of the expression
	pub ty: Ty,
	/// The origin of the expression
	pub origin: Origin,
}

impl Expression {
	/// Create a new expression
	pub fn new(data: impl Into<ExpressionData>, ty: Ty, origin: Origin) -> Self {
		Self {
			data: data.into(),
			ty,
			origin,
		}
	}

	/// Get whether this is a value
	pub fn is_value(&self) -> bool {
		matches!(self.data, ExpressionData::Value(_))
	}

	/// Convert into a value (panics if this isn't a value)
	pub fn into_value(self) -> Value {
		match self.data {
			ExpressionData::Value(v) => Value::new(v, self.ty, self.origin),
			_ => panic!("Not a value"),
		}
	}

	/// Get this identifier if it is one
	pub fn to_identifier(&self) -> Option<Identifier> {
		if let ExpressionData::Value(ValueData::Literal(LiteralData::Identifier(i))) = &self.data {
			Some(*i)
		} else {
			None
		}
	}
}

/// The expression data
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ExpressionData {
	/// A let expression
	Let(Let),
	/// A call
	Call(Call),
	/// An interpreter intrinsic builtin
	Builtin(Builtin),
	/// An if-then-else expression
	IfThenElse(IfThenElse),
	/// A comprehension
	Comprehension(Comprehension),
	/// A value
	Value(ValueData),
	/// A root-level forall
	ForAllRoot(Comprehension),
}

impl_enum_from!(ExpressionData::Let(Let));
impl_enum_from!(ExpressionData::Call(Call));
impl_enum_from!(ExpressionData::Builtin(Builtin));
impl_enum_from!(ExpressionData::IfThenElse(IfThenElse));
impl_enum_from!(ExpressionData::Comprehension(Comprehension));
impl_enum_from!(ExpressionData::Value(ValueData));

impl From<Value> for Expression {
	fn from(value: Value) -> Self {
		Self {
			data: value.data.into(),
			ty: value.ty,
			origin: value.origin,
		}
	}
}

impl From<Literal> for Expression {
	fn from(literal: Literal) -> Self {
		Value::into(literal.into())
	}
}

/// A let expression
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Let {
	/// Items in the let expression
	pub items: Vec<LetItem>,
	/// The result of the let expression
	pub result: LetResult,
}

/// The result of a let expression
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LetResult {
	/// Root let which must hold
	Root,
	/// Returns result
	Identifier(Identifier),
}

/// An item in a let expression
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LetItem {
	/// A constraint which will hold
	Constraint(Constraint),
	/// A declaration
	Declaration(Declaration),
}

/// A tuple literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Tuple {
	/// Tuple members
	pub members: Vec<Value>,
}

/// An array literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Array {
	/// Array literal members
	pub members: Vec<Value>,
}

/// A set literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Set {
	/// Set literal members
	pub members: Vec<Value>,
}

/// A tuple field access
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TupleAccess {
	/// The tuple being accessed
	pub tuple: Identifier,
	/// The field
	pub field: IntegerLiteral,
}

/// A call
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Call {
	/// The function being called
	pub function: Identifier,
	/// The arguments
	pub arguments: Vec<Value>,
}

/// An if-then-else expression
///
/// This only has an if-then and else branch, so may need to be nested
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct IfThenElse {
	/// The (par) condition
	pub condition: Box<Value>,
	/// The value if the condition holds
	pub then: Box<Expression>,
	/// The value if the condition doesn't hold
	pub else_expression: Box<Expression>,
}

/// A comprehension
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Comprehension {
	/// The generated expression
	pub expression: Box<Expression>,
	/// The generators
	pub generators: Vec<Generator>,
}

/// A generator in a comprehension
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Generator {
	/// An iterator such as `i, j in foo where bar`
	Iterator {
		/// The names of the iterators
		names: Vec<Identifier>,
		/// The collection being iterated over
		collection: Expression,
		/// The where clause
		where_clause: Option<Expression>,
	},
	/// An assignment such as `i = foo where bar`
	Assignment {
		/// The name of the assignment
		name: Identifier,
		/// The value of the assignment
		definition: Expression,
		/// The where clause
		where_clause: Option<Expression>,
	},
}

/// A literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Literal {
	/// The literal data
	pub data: LiteralData,
	/// The type of the literal
	pub ty: Ty,
	/// The origin of the literal
	pub origin: Origin,
}

impl Literal {
	/// Create a new literal
	pub fn new(data: impl Into<LiteralData>, ty: Ty, origin: Origin) -> Self {
		Self {
			data: data.into(),
			ty,
			origin,
		}
	}
}

/// The literal data
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum LiteralData {
	/// A boolean
	Boolean(BooleanLiteral),
	/// An integer
	Integer(IntegerLiteral),
	/// A floating point value
	Float(FloatLiteral),
	/// A string
	String(StringLiteral),
	/// Infinity
	Infinity,
	/// An identifier
	Identifier(Identifier),
}

impl_enum_from!(LiteralData::Boolean(BooleanLiteral));
impl_enum_from!(LiteralData::Integer(IntegerLiteral));
impl_enum_from!(LiteralData::Float(FloatLiteral));
impl_enum_from!(LiteralData::String(StringLiteral));
impl_enum_from!(LiteralData::Identifier(Identifier));

/// A value
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Value {
	/// The value
	pub data: ValueData,
	/// The type of the value
	pub ty: Ty,
	/// The origin of the value
	pub origin: Origin,
}

impl Value {
	/// Create a new value
	pub fn new(data: impl Into<ValueData>, ty: Ty, origin: Origin) -> Self {
		Self {
			data: data.into(),
			ty,
			origin,
		}
	}

	/// Get this identifier if it is one
	pub fn to_identifier(&self) -> Option<Identifier> {
		if let ValueData::Literal(LiteralData::Identifier(i)) = &self.data {
			Some(*i)
		} else {
			None
		}
	}
}

impl From<Literal> for Value {
	fn from(lit: Literal) -> Self {
		Self {
			data: lit.data.into(),
			ty: lit.ty,
			origin: lit.origin,
		}
	}
}

/// The value data
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ValueData {
	/// A literal
	Literal(LiteralData),
	/// A tuple
	Tuple(Tuple),
	/// A set
	Set(Set),
	/// An array
	Array(Array),
	/// A tuple access
	TupleAccess(TupleAccess),
}

impl_enum_from!(ValueData::Literal(LiteralData));
impl_enum_from!(ValueData::Tuple(Tuple));
impl_enum_from!(ValueData::Set(Set));
impl_enum_from!(ValueData::Array(Array));
impl_enum_from!(ValueData::TupleAccess(TupleAccess));
