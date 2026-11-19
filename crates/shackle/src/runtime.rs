//! Conversion of model data to values understood by the interpreter.
use shackle_runtime::{FloatVal, IntVal, Interpreter, Value as RuntimeValue};
use thiserror::Error;

use crate::{
	Program, Type, Value,
	value::{Polarity, Set},
};

/// Failure while writing a compiled program.
#[derive(Debug, Error)]
pub enum WriteError {
	/// Failed to write the output.
	#[error(transparent)]
	Io(#[from] std::io::Error),
	/// Input data could not be prepared.
	#[error(transparent)]
	Data(#[from] crate::Error),
	/// The interpreter failed, with source locations when available.
	#[error("{message}")]
	Runtime {
		/// Error and stack trace.
		message: String,
	},
	/// A value cannot yet be converted to the runtime representation.
	#[error("{0}")]
	Unsupported(String),
}

fn dummy(ty: &Type) -> Result<RuntimeValue, WriteError> {
	Ok(match ty {
		Type::Boolean(_) => false.into(),
		Type::Integer(_) => 0.into(),
		Type::Float(_) => 0.0.into(),
		Type::Enum(_, _) => 1.into(),
		Type::String(_) => "".into(),
		Type::Array { .. } => RuntimeValue::from_iter(std::iter::empty::<RuntimeValue>()),
		Type::Set(_, element) => match &**element {
			Type::Float(_) => {
				RuntimeValue::from_iter(std::iter::empty::<std::ops::RangeInclusive<FloatVal>>())
			}
			_ => RuntimeValue::from_iter(std::iter::empty::<std::ops::RangeInclusive<IntVal>>()),
		},
		Type::Tuple(_, fields) => fields
			.iter()
			.map(absent_dummy)
			.collect::<Result<Vec<_>, _>>()?
			.into_iter()
			.collect(),
		Type::Record(_, fields) => fields
			.iter()
			.map(|(_, ty)| absent_dummy(ty))
			.collect::<Result<Vec<_>, _>>()?
			.into_iter()
			.collect(),
		Type::Annotation(_) => {
			return Err(WriteError::Unsupported(
				"optional annotations are not supported by NanoZinc data conversion".into(),
			));
		}
	})
}

fn absent_dummy(ty: &Type) -> Result<RuntimeValue, WriteError> {
	let value = dummy(ty)?;
	Ok(if ty.is_opt() {
		[false.into(), value].into_iter().collect()
	} else {
		value
	})
}

fn index_value(value: Value) -> Result<i64, WriteError> {
	match value {
		Value::Integer(v) => Ok(v),
		Value::Enum(v) => i64::try_from(v.int_val())
			.map_err(|_| WriteError::Unsupported("enum index is too large".into())),
		_ => Err(WriteError::Unsupported("unsupported array index".into())),
	}
}

fn convert(value: &Value, ty: &Type) -> Result<RuntimeValue, WriteError> {
	if ty.is_opt() {
		if *value == Value::Absent {
			return absent_dummy(ty);
		}
		return Ok([true.into(), convert_nonopt(value, ty)?]
			.into_iter()
			.collect());
	}
	convert_nonopt(value, ty)
}

fn convert_nonopt(value: &Value, ty: &Type) -> Result<RuntimeValue, WriteError> {
	Ok(match (value, ty) {
		(Value::Boolean(v), Type::Boolean(_)) => (*v).into(),
		(Value::Integer(v), Type::Integer(_)) => (*v).into(),
		(Value::Float(v), Type::Float(_)) => (*v).into(),
		(Value::String(v), Type::String(_)) => v.as_ref().into(),
		(Value::Infinity(p), Type::Integer(_)) => match p {
			Polarity::Pos => IntVal::InfPos.into(),
			Polarity::Neg => IntVal::InfNeg.into(),
		},
		(Value::Infinity(p), Type::Float(_)) => match p {
			Polarity::Pos => f64::INFINITY.into(),
			Polarity::Neg => f64::NEG_INFINITY.into(),
		},
		(Value::Enum(v), Type::Enum(_, _)) => i64::try_from(v.int_val())
			.map_err(|_| WriteError::Unsupported("enum value is too large".into()))?
			.into(),
		(Value::Tuple(values), Type::Tuple(_, fields)) if values.len() == fields.len() => values
			.iter()
			.zip(fields.iter())
			.map(|(v, t)| convert(v, t))
			.collect::<Result<Vec<_>, _>>()?
			.into_iter()
			.collect(),
		(Value::Record(values), Type::Record(_, fields)) if values.len() == fields.len() => values
			.iter()
			.zip(fields.iter())
			.map(|((_, v), (_, t))| convert(v, t))
			.collect::<Result<Vec<_>, _>>()?
			.into_iter()
			.collect(),
		(Value::Array(array), Type::Array { element, .. }) => {
			let values: RuntimeValue = array
				.members
				.iter()
				.map(|v| convert(v, element))
				.collect::<Result<Vec<_>, _>>()?
				.into_iter()
				.collect();
			let dims = array
				.indices
				.iter()
				.map(|i| Ok((index_value(i.start())?, index_value(i.end())?)))
				.collect::<Result<Vec<_>, WriteError>>()?;
			if dims.is_empty() {
				values
			} else {
				values.with_dim(dims)
			}
		}
		(Value::Set(Set::Int(ranges)), Type::Set(_, _)) => ranges
			.iter()
			.map(|r| IntVal::from(*r.start())..=IntVal::from(*r.end()))
			.collect(),
		(Value::Set(Set::Float(ranges)), Type::Set(_, _)) => ranges
			.iter()
			.map(|r| FloatVal::from(*r.start())..=FloatVal::from(*r.end()))
			.collect(),
		(Value::Set(Set::Enum(ranges)), Type::Set(_, _)) => ranges
			.iter()
			.map(|r| {
				IntVal::from(r.start().int_val() as i64)..=IntVal::from(r.end().int_val() as i64)
			})
			.collect(),
		_ => {
			return Err(WriteError::Unsupported(format!(
				"unsupported NanoZinc input value for type {ty}"
			)));
		}
	})
}

impl Program {
	pub(crate) fn render_nanozinc(&self) -> Result<String, WriteError> {
		let mut missing: Vec<String> = self
			.input_types
			.iter()
			.filter(|(name, ty)| !ty.is_opt() && !self.input_data.contains_key(*name))
			.map(|(name, _)| name.to_string())
			.collect();
		missing.extend(
			self.legacy_enums
				.iter()
				.filter(|e| !e.is_defined())
				.map(|e| e.name().to_string()),
		);
		if !missing.is_empty() {
			missing.sort();
			let errors: Vec<crate::Error> = missing
				.into_iter()
				.map(|identifier| shackle_diagnostics::error::MissingData { identifier }.into())
				.collect();
			return Err(crate::Error::try_from(errors).unwrap().into());
		}
		let ir = shackle_ir::Program::lower_from_mir(&self.db);
		let mut interpreter = Interpreter::new(&ir);
		for input in ir.inputs() {
			let name = input.name();
			let val = if let Some(ty) = self.input_types.get(name) {
				convert(self.input_data.get(name).unwrap_or(&Value::Absent), ty)?
			} else if let Some(ty) = self.compiler_input_types.get(name) {
				if !ty.is_opt() {
					return Err(WriteError::Unsupported(format!(
						"missing compiler input {name}"
					)));
				}
				convert(&Value::Absent, ty)?
			} else if let Some(e) = self.enum_types.get(name) {
				if !e.is_defined() {
					return Err(WriteError::Unsupported(format!(
						"missing enum definition {name}"
					)));
				}
				let guard = e.lock();
				let mut names = Vec::new();
				for (ctor, args, _) in guard.iter() {
					if !args.is_empty() {
						return Err(WriteError::Unsupported(format!(
							"enum constructors with arguments are not yet supported for NanoZinc input {name}"
						)));
					}
					names.push(RuntimeValue::from(ctor.as_ref()));
				}
				names.into_iter().collect()
			} else {
				return Err(WriteError::Unsupported(format!(
					"unknown NanoZinc input {name}"
				)));
			};
			interpreter
				.set_input(name, val)
				.map_err(|e| WriteError::Unsupported(e.to_string()))?;
		}
		interpreter.run().map_err(|err| {
			let mut message = err.to_string();
			for location in &err.stack_trace {
				message.push_str(&format!("\n  at {location}"));
			}
			WriteError::Runtime { message }
		})?;
		Ok(interpreter.nanozinc_mut().print(&ir))
	}
}
