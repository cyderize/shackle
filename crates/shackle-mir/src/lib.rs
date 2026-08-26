//! Mid-level IR

pub mod builtins;
pub mod liveness;
pub mod lower;
pub mod pretty_print;

use derive_more::{From, TryUnwrap, Unwrap};
pub use shackle_hir::Identifier as Name;
use shackle_hir::{BooleanLiteral, FloatLiteral, IntegerLiteral, StringLiteral};
use shackle_thir::source::Origin;
use shackle_ty::Ty;
use shackle_utils::{
	TypedIndex,
	arena::{Arena, ArenaIndex},
};

/// A builtin in MIR, whose arguments are boxed MIR values.
pub type Builtin<'db> = builtins::Builtin<Box<Value<'db>>>;

/// The ID of a declaration item
pub type DeclarationId<'db> = ArenaIndex<Declaration<'db>>;
/// The ID of a function item
pub type FunctionId<'db> = ArenaIndex<Function<'db>>;
/// The ID of a parameter
pub type ParameterId<'db> = ArenaIndex<Parameter<'db>>;
/// The ID of a global variable
pub type GlobalVarId<'db> = ArenaIndex<GlobalVar<'db>>;

#[allow(dead_code)]
/// A mid-level IR program (MicroZinc)
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Model<'db> {
	/// Annotation items
	pub annotations: Arena<Annotation<'db>>,
	/// Function items
	pub functions: Arena<Function<'db>>,
	/// Input parameters and global variables
	pub global_vars: Arena<GlobalVar<'db>>,
	/// Main entry point
	pub main_entrypoint: FunctionId<'db>,
	/// Output entry point
	pub output_entrypoint: FunctionId<'db>,
	/// Solve method
	pub solve: Solve<'db>,
}

/// Solve method
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Solve<'db> {
	/// Solve satisfy
	Satisfy,
	/// Minimize objective
	Minimize(GlobalVarId<'db>),
	/// Maximize objectice
	Maximize(GlobalVarId<'db>),
}

/// An identifier
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, From)]
pub enum Identifier<'db> {
	/// An identifier pointing to a declaration
	Declaration(DeclarationId<'db>),
	/// An identifier pointing to a parameter
	Parameter(ParameterId<'db>),
	/// A global variable or parameter
	GlobalVar(GlobalVarId<'db>),
	/// An identifier pointing to an annotation
	Annotation(AnnotationId<'db>),
}

/// A variable or parameter accessible from anywhere in the program
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GlobalVar<'db> {
	/// The type of the global
	pub ty: Ty<'db>,
	/// The name of the global
	pub name: Option<Name<'db>>,
}

/// A parameter for a function or annotation
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Parameter<'db> {
	/// The type of the parameter
	pub ty: Ty<'db>,
	/// The name of the parameter
	pub name: Option<Name<'db>>,
	/// The annotations on this parameter
	pub annotations: Vec<AnnotationId<'db>>,
}

/// An annotation item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Annotation<'db> {
	/// Name of the annotation item
	pub name: Name<'db>,
	/// The parameters for the annotation item
	pub parameters: Arena<Parameter<'db>>,
	/// The origin of the annotation item
	pub origin: Origin<'db>,
}

/// A constraint item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Constraint<'db> {
	/// The expression which must hold
	pub expression: Box<Expression<'db>>,
	/// The annotations
	pub annotations: Vec<Identifier<'db>>,
}

/// The ID of an annotation item
pub type AnnotationId<'db> = ArenaIndex<Annotation<'db>>;

/// A declaration item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Declaration<'db> {
	/// The type of the variable
	pub ty: Ty<'db>,
	/// The name of the declaration
	pub name: Option<Name<'db>>,
	/// The RHS definition
	pub definition: Expression<'db>,
	/// The annotations
	pub annotations: Vec<Identifier<'db>>,
}
/// An assignment to a global variable
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct GlobalAssignment<'db> {
	/// The global variable being assigned to
	pub global: GlobalVarId<'db>,
	/// The expression being assigned
	pub definition: Box<Expression<'db>>,
}

/// A let expression
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Let<'db> {
	/// Items in the let expression
	pub items: Vec<LetItem<'db>>,
	/// The result of the let expression
	pub result: LetResult<'db>,
	/// The origin of the expression
	pub origin: Origin<'db>,
}

/// The result of a let expression
#[derive(Clone, Debug, Hash, PartialEq, Eq, From)]
pub enum LetResult<'db> {
	/// Root let which must hold
	Root,
	/// Returns result
	Identifier(Identifier<'db>),
}

/// An item in a let expression
#[derive(Clone, Debug, Hash, PartialEq, Eq, From, TryUnwrap, Unwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum LetItem<'db> {
	/// A constraint which will hold
	Constraint(Constraint<'db>),
	/// A declaration
	Declaration(DeclarationId<'db>),
	/// An assignment to a global variable
	GlobalAssignment(GlobalAssignment<'db>),
}

#[allow(dead_code)]
/// A function item
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Function<'db> {
	/// The function name
	pub name: Name<'db>,
	/// The return type of the function
	pub ty: Ty<'db>,
	/// The function parameters
	pub parameters: Vec<ParameterId<'db>>,
	/// The function data
	pub data: FunctionData<'db>,
	/// The annotations on the function
	pub annotations: Vec<AnnotationId<'db>>,
	/// The function body
	pub body: Option<Let<'db>>,
	/// The origin of the function item
	pub origin: Origin<'db>,
}

/// A function item's data
#[derive(Clone, Default, Debug, Hash, PartialEq, Eq, TypedIndex)]
pub struct FunctionData<'db> {
	/// The parameters in the function
	#[index(ParameterId<'db>)]
	pub parameters: Arena<Parameter<'db>>,
	/// The declarations in the function
	#[index(DeclarationId<'db>)]
	pub declarations: Arena<Declaration<'db>>,
}

/// An expression
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Expression<'db> {
	/// The expression data
	pub data: ExpressionData<'db>,
	/// The type of the expression
	pub ty: Ty<'db>,
	/// The origin of the expression
	pub origin: Origin<'db>,
}

impl<'db> Expression<'db> {
	/// Create a new expression
	pub fn new(data: impl Into<ExpressionData<'db>>, ty: Ty<'db>, origin: Origin<'db>) -> Self {
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
	pub fn into_value(self) -> Value<'db> {
		match self.data {
			ExpressionData::Value(v) => Value::new(v, self.ty, self.origin),
			_ => panic!("Not a value"),
		}
	}

	/// Get whether this is a literal
	pub fn is_literal(&self) -> bool {
		matches!(self.data, ExpressionData::Value(ValueData::Literal(_)))
	}

	/// Convert into a literal (panics if this isn't a literal)
	pub fn into_literal(self) -> Literal<'db> {
		match self.data {
			ExpressionData::Value(ValueData::Literal(l)) => Literal::new(l, self.ty, self.origin),
			_ => panic!("Not a literal"),
		}
	}

	/// Get this identifier if it is one
	pub fn to_identifier(&self) -> Option<Identifier<'db>> {
		if let ExpressionData::Value(ValueData::Literal(LiteralData::Identifier(i))) = &self.data {
			Some(*i)
		} else {
			None
		}
	}
}

/// The expression data
#[derive(Clone, Debug, Hash, PartialEq, Eq, From, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum ExpressionData<'db> {
	/// A call
	Call(Call<'db>),
	/// An annotation call
	AnnotationCall(AnnotationCall<'db>),
	/// An interpreter intrinsic builtin
	Builtin(Builtin<'db>),
	/// An if-then-else expression
	IfThenElse(IfThenElse<'db>),
	/// A comprehension
	Comprehension(Comprehension<'db>),
	/// A comprehension iterator
	#[from(skip)]
	Iterator(ValueData<'db>),
	/// A value
	Value(ValueData<'db>),
}

impl<'db> From<Value<'db>> for Expression<'db> {
	fn from(value: Value<'db>) -> Self {
		Self {
			data: value.data.into(),
			ty: value.ty,
			origin: value.origin,
		}
	}
}

impl<'db> From<Literal<'db>> for Expression<'db> {
	fn from(literal: Literal<'db>) -> Self {
		Self {
			data: ExpressionData::Value(literal.data.into()),
			ty: literal.ty,
			origin: literal.origin,
		}
	}
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

/// An if-then-else expression
///
/// This only has an if-then and else branch, so may need to be nested
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct IfThenElse<'db> {
	/// The (par) condition
	pub condition: Box<Value<'db>>,
	/// The value if the condition holds
	pub then: Let<'db>,
	/// The value if the condition doesn't hold
	pub else_expression: Let<'db>,
}

/// A comprehension
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Comprehension<'db> {
	/// The generated value
	pub template: Let<'db>,
	/// The generators
	pub generators: Vec<Generator<'db>>,
}

/// The declarations comprising a comprehension iterator
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Generator<'db> {
	/// An iterator generator
	Iterator {
		/// THe declaration getting the value from the iterator
		element: DeclarationId<'db>,
	},
	/// An assignment generator
	Assignment {
		/// The declaration
		declaration: DeclarationId<'db>,
	},
	/// A constraint item
	Constraint(Constraint<'db>),
	/// A where clause
	Where(Value<'db>),
}

/// A literal
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Literal<'db> {
	/// The literal data
	pub data: LiteralData<'db>,
	/// The type of the literal
	pub ty: Ty<'db>,
	/// The origin of the literal
	pub origin: Origin<'db>,
}

impl<'db> Literal<'db> {
	/// Create a new value
	pub fn new(data: impl Into<LiteralData<'db>>, ty: Ty<'db>, origin: Origin<'db>) -> Self {
		Self {
			data: data.into(),
			ty,
			origin,
		}
	}

	/// Get this identifier if it is one
	pub fn to_identifier(&self) -> Option<Identifier<'db>> {
		if let LiteralData::Identifier(i) = &self.data {
			Some(*i)
		} else {
			None
		}
	}
}

/// A literal
#[derive(Clone, Debug, Hash, PartialEq, Eq, From, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum LiteralData<'db> {
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

impl<'db> From<DeclarationId<'db>> for LiteralData<'db> {
	fn from(value: DeclarationId<'db>) -> Self {
		LiteralData::Identifier(value.into())
	}
}

impl<'db> From<ParameterId<'db>> for LiteralData<'db> {
	fn from(value: ParameterId<'db>) -> Self {
		LiteralData::Identifier(value.into())
	}
}

impl<'db> From<AnnotationId<'db>> for LiteralData<'db> {
	fn from(value: AnnotationId<'db>) -> Self {
		LiteralData::Identifier(value.into())
	}
}

/// A value
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Value<'db> {
	/// The value
	pub data: ValueData<'db>,
	/// The type of the value
	pub ty: Ty<'db>,
	/// The origin of the value
	pub origin: Origin<'db>,
}

impl<'db> Value<'db> {
	/// Create a new value
	pub fn new(data: impl Into<ValueData<'db>>, ty: Ty<'db>, origin: Origin<'db>) -> Self {
		Self {
			data: data.into(),
			ty,
			origin,
		}
	}

	/// Get this identifier if it is one
	pub fn to_identifier(&self) -> Option<Identifier<'db>> {
		if let ValueData::Literal(LiteralData::Identifier(i)) = &self.data {
			Some(*i)
		} else {
			None
		}
	}

	/// Get whether this is a literal
	pub fn is_literal(&self) -> bool {
		matches!(self.data, ValueData::Literal(_))
	}

	/// Convert into a literal (panics if this isn't a literal)
	pub fn into_literal(self) -> Literal<'db> {
		match self.data {
			ValueData::Literal(l) => Literal::new(l, self.ty, self.origin),
			_ => panic!("Not a literal"),
		}
	}
}

impl<'db> From<Literal<'db>> for Value<'db> {
	fn from(literal: Literal<'db>) -> Self {
		Self {
			data: literal.data.into(),
			ty: literal.ty,
			origin: literal.origin,
		}
	}
}

/// The value data
#[derive(Clone, Debug, Hash, PartialEq, Eq, From, Unwrap, TryUnwrap)]
#[unwrap(ref)]
#[try_unwrap(ref)]
pub enum ValueData<'db> {
	/// A literal
	Literal(LiteralData<'db>),
	/// A tuple
	Tuple(Tuple<'db>),
	/// A set
	Set(Set<'db>),
	/// An array
	Array(Array<'db>),
	/// A tuple access
	TupleAccess(TupleAccess<'db>),
}
