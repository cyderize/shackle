//! Error types for the Shackle runtime
use miette::Diagnostic;
use thiserror::Error;

/// Error evaluating arithmetic operation
#[derive(Error, Debug, Diagnostic, PartialEq, Eq, Clone)]
#[error("Error evaluating arithmetic operation")]
#[diagnostic(code(shackle::arithmetic_error))]
pub struct ArithmeticError {
	/// The reason for the undefined operation
	pub reason: &'static str,
}

/// Invalid float set coercion
#[derive(Error, Debug, Diagnostic, PartialEq, Eq, Clone)]
#[error("Invalid float set coercion")]
#[diagnostic(code(shackle::float_set_coercion_error))]
pub struct FloatSetCoercionError {
	/// The reason for the error
	pub reason: &'static str,
}

/// Invalid index set
#[derive(Error, Debug, Diagnostic, PartialEq, Eq, Clone)]
#[error("Invalid index set")]
#[diagnostic(code(shackle::invalid_index_set))]
pub struct InvalidIndexSet {
	/// The reason for the error
	pub reason: &'static str,
}

/// Invalid value
#[derive(Error, Debug, Diagnostic, PartialEq, Eq, Clone)]
#[error("Invalid value")]
#[diagnostic(code(shackle::value_error))]
pub struct ValueError {
	/// The reason for the error
	pub reason: &'static str,
}

/// The reason interpretation stopped.
#[derive(Error, Debug, PartialEq, Eq, Clone)]
pub enum RuntimeErrorKind {
	/// The program explicitly aborted.
	#[error("Abort: {0}")]
	Abort(String),
	/// A constraint evaluated to false.
	#[error("Constraint evaluated to false")]
	ConstraintFalse,
	/// An arithmetic operation failed.
	#[error("Arithmetic error: {}", .0.reason)]
	Arithmetic(#[from] ArithmeticError),
	/// A float set coercion failed.
	#[error("Float set coercion error: {}", .0.reason)]
	FloatSetCoercion(#[from] FloatSetCoercionError),
	/// An index set was invalid.
	#[error("Invalid index set: {}", .0.reason)]
	InvalidIndexSet(#[from] InvalidIndexSet),
	/// A value was invalid.
	#[error("Invalid value: {}", .0.reason)]
	Value(#[from] ValueError),
	/// Distribution parameters were invalid.
	#[error("Invalid distribution parameters: {}", .0.reason)]
	InvalidDistribution(ValueError),
	/// The interpreter has already aborted.
	#[error("Interpreter has already aborted")]
	AlreadyAborted,
}

/// An owned source location in a runtime stack trace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RuntimeLocation {
	/// Source file name.
	pub file: String,
	/// Source span.
	pub span: shackle_ir::LineCharSpan,
}

impl std::fmt::Display for RuntimeLocation {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}:{}", self.file, self.span)
	}
}

/// An interpretation failure and its source locations, innermost first.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
#[error("{kind}")]
pub struct RuntimeError {
	/// The cause of the failure.
	#[source]
	pub kind: RuntimeErrorKind,
	/// The failing instruction followed by its callers.
	pub stack_trace: Vec<RuntimeLocation>,
}

impl RuntimeError {
	pub(crate) fn already_aborted() -> Self {
		Self {
			kind: RuntimeErrorKind::AlreadyAborted,
			stack_trace: Vec::new(),
		}
	}
}
