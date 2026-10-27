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
