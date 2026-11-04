//! NanoZinc model
//!
//! Represents the state of the instance

use std::{
	fmt::{Write, format},
	sync::{
		Arc,
		atomic::{AtomicU64, Ordering},
	},
};

use rustc_hash::FxHashSet;

use crate::{DataView, Value, value::WeakValue};

/// A NanoZinc model, which consists of a list of items (variables and constraints).
#[derive(Debug, Default)]
pub struct NanoZinc {
	items: Vec<Item>,
	next_id: AtomicU64,
	pending: Vec<PendingNanoZinc>,
}

impl NanoZinc {
	/// Add a fresh boolean variable to the NanoZinc
	pub fn add_var_bool(&mut self, is_defined: bool) -> Value {
		let var = Value::new_bool_var(self.next_id.fetch_add(1, Ordering::Relaxed));
		self.items.push(Item::Variable(var.downgrade()));
		if is_defined {
			let _ = self
				.pending
				.last_mut()
				.unwrap()
				.variables_defined
				.insert(var.clone());
		}
		var
	}

	/// Add a fresh integer variable to the NanoZinc
	pub fn add_var_int(&mut self, domain: Value, is_defined: bool) -> Value {
		let var = Value::new_int_var(self.next_id.fetch_add(1, Ordering::Relaxed), domain);
		self.items.push(Item::Variable(var.downgrade()));
		if is_defined {
			let _ = self
				.pending
				.last_mut()
				.unwrap()
				.variables_defined
				.insert(var.clone());
		}
		var
	}

	/// Add a fresh float variable to the NanoZinc
	pub fn add_var_float(&mut self, domain: Value, is_defined: bool) -> Value {
		let var = Value::new_float_var(self.next_id.fetch_add(1, Ordering::Relaxed), domain);
		self.items.push(Item::Variable(var.downgrade()));

		if is_defined {
			let _ = self
				.pending
				.last_mut()
				.unwrap()
				.variables_defined
				.insert(var.clone());
		}
		var
	}

	/// Add a fresh int set variable to the NanoZinc
	pub fn add_var_set_of_int(&mut self, domain: Value, is_defined: bool) -> Value {
		let var = Value::new_int_set_var(self.next_id.fetch_add(1, Ordering::Relaxed), domain);
		self.items.push(Item::Variable(var.downgrade()));
		if is_defined {
			let _ = self
				.pending
				.last_mut()
				.unwrap()
				.variables_defined
				.insert(var.clone());
		}
		var
	}

	/// Add a constraint to the NanoZinc
	pub fn add_constraint(
		&mut self,
		function: shackle_ir::FunctionId,
		arguments: impl IntoIterator<Item = Value>,
		annotations: impl IntoIterator<Item = Value>,
	) {
		let arguments = arguments.into_iter().collect();
		let annotations = annotations.into_iter().collect();
		let constraint = Constraint {
			function,
			arguments,
			annotations,
		};

		if let Some(pending) = self.pending.last_mut() {
			pending.constraints.push(constraint);
		} else {
			self.items.push(Item::Constraint(constraint));
		}
	}

	/// Push a new pending NanoZinc context onto the stack
	pub fn push_pending(&mut self) {
		self.pending.push(PendingNanoZinc::default());
	}

	/// Pop the top pending NanoZinc context off the stack and apply it to the model
	pub fn pop_pending(&mut self) {
		let pending = self.pending.pop().unwrap();
		let cons = pending
			.constraints
			.into_iter()
			.map(|c| {
				let arguments = c
					.arguments
					.iter()
					.map(|arg| {
						if let DataView::Seq(vs) = arg.deref() {
							let mut had_defined = false;
							let args = vs
								.iter()
								.map(|v| {
									if pending.variables_defined.contains(v) {
										had_defined = true;
										ScalarArgument::DefinedVariable(v.downgrade())
									} else {
										ScalarArgument::Input(v.clone())
									}
								})
								.collect();
							return if had_defined {
								DefiningConstraintArgument::Array(args)
							} else {
								DefiningConstraintArgument::Scalar(ScalarArgument::Input(
									arg.clone(),
								))
							};
						}
						if pending.variables_defined.contains(arg) {
							DefiningConstraintArgument::Scalar(ScalarArgument::DefinedVariable(
								arg.downgrade(),
							))
						} else {
							DefiningConstraintArgument::Scalar(ScalarArgument::Input(arg.clone()))
						}
					})
					.collect();
				DefiningConstraint {
					function: c.function,
					arguments,
					annotations: c.annotations.clone(),
				}
			})
			.collect();
		let defining = DefiningConstraints {
			constraints: Some(Arc::new(cons)),
		};
		for v in pending.variables_defined {
			match v.deref() {
				DataView::BoolVar(mut var) => {
					var.set_defining_constraints(defining.clone());
				}
				DataView::IntVar(mut var) => {
					var.set_defining_constraints(defining.clone());
				}
				DataView::FloatVar(mut var) => {
					var.set_defining_constraints(defining.clone());
				}
				DataView::IntSetVar(mut var) => {
					var.set_defining_constraints(defining.clone());
				}
				_ => unreachable!("Not a variable"),
			}
		}
	}

	/// Iterate over the items in the model.
	pub fn items(&self) -> impl Iterator<Item = &Item> {
		self.items.iter()
	}

	/// Iterate over the items in the model mutably.
	pub fn items_mut(&mut self) -> impl Iterator<Item = &mut Item> {
		self.items.iter_mut()
	}

	/// Print the NanoZinc model as a string
	pub fn print(&mut self, program: &shackle_ir::Program) -> String {
		let mut s = String::new();
		for item in self.items() {
			match item {
				Item::Variable(var) => {
					let Some(value) = var.upgrade() else {
						continue;
					};

					let defining_constraints = match value.deref() {
						DataView::BoolVar(v) => {
							writeln!(
								&mut s,
								"var {}: _X_INTRODUCED_{};",
								v.domain()
									.map(|b| format!("{:?}", b))
									.unwrap_or_else(|| "bool".to_owned()),
								v.ident()
							)
							.unwrap();
							v.defining_constraints().clone()
						}
						DataView::IntVar(v) => {
							writeln!(
								&mut s,
								"var {}: _X_INTRODUCED_{};",
								v.domain()
									.map(|d| format!("{}", d.as_int_set()))
									.unwrap_or_else(|| "int".to_owned()),
								v.ident(),
							)
							.unwrap();
							v.defining_constraints().clone()
						}
						DataView::FloatVar(v) => {
							writeln!(
								&mut s,
								"var {}: _X_INTRODUCED_{};",
								v.domain()
									.map(|d| format!("{}", d.as_float_set()))
									.unwrap_or_else(|| "float".to_owned()),
								v.ident(),
							)
							.unwrap();
							v.defining_constraints().clone()
						}
						DataView::IntSetVar(v) => {
							writeln!(
								&mut s,
								"var set of {}: _X_INTRODUCED_{};",
								v.domain()
									.map(|d| format!("{}", d.as_int_set()))
									.unwrap_or_else(|| "int".to_owned()),
								v.ident(),
							)
							.unwrap();
							v.defining_constraints().clone()
						}
						_ => unreachable!(),
					};

					for constraint in defining_constraints.iter() {
						writeln!(
							&mut s,
							"⤷ constraint {}({}){};",
							program.strings[program.functions[constraint.function].name],
							constraint
								.arguments
								.iter()
								.map(|arg| self.print_value(program, arg.to_value()))
								.collect::<Vec<_>>()
								.join(", "),
							constraint
								.annotations
								.iter()
								.map(|ann| format!(
									" :: {}",
									self.print_value(program, ann.clone())
								))
								.collect::<Vec<_>>()
								.join("")
						)
						.unwrap();
					}
				}
				Item::Constraint(constraint) => writeln!(
					&mut s,
					"constraint {}({});",
					program.strings[program.functions[constraint.function].name],
					constraint
						.arguments
						.iter()
						.map(|arg| self.print_value(program, arg.clone()))
						.collect::<Vec<_>>()
						.join(", ")
				)
				.unwrap(),
			}
		}
		s
	}

	fn print_value(&self, program: &shackle_ir::Program, value: Value) -> String {
		match value.deref() {
			DataView::BoolVar(v) => format!("_X_INTRODUCED_{}", v.ident()),
			DataView::IntVar(v) => format!("_X_INTRODUCED_{}", v.ident()),
			DataView::FloatVar(v) => format!("_X_INTRODUCED_{}", v.ident()),
			DataView::IntSetVar(v) => format!("_X_INTRODUCED_{}", v.ident()),
			DataView::Annotation(a) => format!(
				"{}{}",
				program.strings[program.annotations[a.annotation].name],
				if a.arguments.is_empty() {
					"".to_owned()
				} else {
					format!(
						"({})",
						a.arguments
							.iter()
							.map(|arg| self.print_value(program, arg.clone()))
							.collect::<Vec<_>>()
							.join(", ")
					)
				}
			),
			DataView::Int(int_val) => format!("{}", int_val),
			DataView::Float(float_val) => format!("{}", float_val),
			DataView::Seq(seq_view) => format!(
				"[{}]",
				seq_view
					.iter()
					.map(|v| self.print_value(program, v.clone()))
					.collect::<Vec<_>>()
					.join(", ")
			),
			DataView::Set(set) => format!(
				"{{{}}}",
				set.values()
					.map(|v| self.print_value(program, v.clone()))
					.collect::<Vec<_>>()
					.join(", ")
			),
			DataView::Str(s) => format!("{:?}", s),
			DataView::IntSet(int_set_view) => format!("{}", int_set_view),
			DataView::FloatSet(float_set_view) => format!("{}", float_set_view),
		}
	}
}

/// A NanoZinc item, which can be either a variable or a constraint.
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
	/// A variable
	Variable(WeakValue),
	/// A top-level constraint
	Constraint(Constraint),
}

/// A constraint
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint<T = Value> {
	/// The function
	pub function: shackle_ir::FunctionId,
	/// The arguments
	pub arguments: Vec<T>,
	/// Annotations on this constraint
	pub annotations: Vec<Value>,
}

/// A constraint which defines a variable (or variables)
type DefiningConstraint = Constraint<DefiningConstraintArgument>;

/// Defining constraints for a variable (or variables)
///
/// Can be shared by multiple variables if they are all defined by these constraints
#[derive(Clone, Debug, Default, PartialEq)]
pub struct DefiningConstraints {
	constraints: Option<Arc<Vec<DefiningConstraint>>>,
}

impl<T: IntoIterator<Item = DefiningConstraint>> From<T> for DefiningConstraints {
	fn from(constraints: T) -> Self {
		Self {
			constraints: Some(Arc::new(constraints.into_iter().collect())),
		}
	}
}

impl DefiningConstraints {
	/// Iterate over the defining constraints (if any)
	fn iter(&self) -> impl Iterator<Item = &DefiningConstraint> {
		self.constraints
			.as_ref()
			.map(|arc| arc.iter())
			.into_iter()
			.flatten()
	}
}

/// Argument to a defining constraint
#[derive(Debug, Clone, PartialEq)]
enum DefiningConstraintArgument {
	/// A single argument which may be defined by the constraint
	///
	/// May also be a sequence of arguments which are all outputs
	Scalar(ScalarArgument),
	// An array of arguments for which
	Array(Vec<ScalarArgument>),
}

impl DefiningConstraintArgument {
	fn to_value(&self) -> Value {
		match self {
			DefiningConstraintArgument::Scalar(scalar) => scalar.to_value(),
			DefiningConstraintArgument::Array(array) => {
				let values: Vec<Value> = array.iter().map(|arg| arg.to_value()).collect();
				Value::from_iter(values)
			}
		}
	}
}

#[derive(Debug, Clone, PartialEq)]
enum ScalarArgument {
	/// Input value kept alive by this constraint
	Input(Value),
	/// Output variable defined by this constraint
	DefinedVariable(WeakValue),
}

impl ScalarArgument {
	fn to_value(&self) -> Value {
		match self {
			ScalarArgument::Input(v) => v.clone(),
			ScalarArgument::DefinedVariable(v) => v.upgrade().unwrap(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
struct PendingNanoZinc {
	variables_defined: FxHashSet<Value>,
	constraints: Vec<Constraint>,
}
