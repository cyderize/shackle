//! NanoZinc model
//!
//! Represents the state of the instance

use std::sync::atomic::{AtomicU64, Ordering};

use shackle_hir::Db;

use crate::{DataView, Value, value::WeakValue};

/// A NanoZinc model, which consists of a list of items (variables and constraints).
#[derive(Debug, Default)]
pub struct NanoZinc<'db> {
	items: Vec<Item<'db>>,
	next_id: AtomicU64,
}

impl<'db> NanoZinc<'db> {
	/// Add a fresh boolean variable to the NanoZinc
	pub fn add_var_bool(&mut self) -> Value {
		let var = Value::new_bool_var(self.next_id.fetch_add(1, Ordering::Relaxed));
		self.items.push(Item::Variable(var.downgrade()));
		var
	}

	/// Add a fresh integer variable to the NanoZinc
	pub fn add_var_int(&mut self, domain: Value) -> Value {
		let var = Value::new_int_var(self.next_id.fetch_add(1, Ordering::Relaxed), domain);
		self.items.push(Item::Variable(var.downgrade()));
		var
	}

	/// Add a fresh float variable to the NanoZinc
	pub fn add_var_float(&mut self, domain: Value) -> Value {
		let var = Value::new_float_var(self.next_id.fetch_add(1, Ordering::Relaxed), domain);
		self.items.push(Item::Variable(var.downgrade()));
		var
	}

	/// Add a fresh int set variable to the NanoZinc
	pub fn add_var_set_of_int(&mut self, domain: Value) -> Value {
		let var = Value::new_int_set_var(self.next_id.fetch_add(1, Ordering::Relaxed), domain);
		self.items.push(Item::Variable(var.downgrade()));
		var
	}

	/// Add a constraint to the NanoZinc
	pub fn add_constraint(
		&mut self,
		function: shackle_ir::FunctionId<'db>,
		arguments: impl IntoIterator<Item = Value>,
	) {
		let arguments = arguments.into_iter().map(Argument::Value).collect();
		let constraint = Constraint {
			function,
			arguments,
			annotations: Vec::new(),
		};
		self.items.push(Item::Constraint(constraint));
	}

	/// Add a defining constraint to the NanoZinc
	pub fn add_defining_constraint(
		&mut self,
		function: shackle_ir::FunctionId<'db>,
		arguments: impl IntoIterator<Item = Value>,
		variable: Value,
	) {
		let arguments = arguments
			.into_iter()
			.map(|arg| {
				if arg == variable {
					Argument::DefinedVariable(arg.downgrade())
				} else {
					Argument::Value(arg)
				}
			})
			.collect();
		let constraint = Constraint {
			function,
			arguments,
			annotations: Vec::new(),
		};
		self.items.push(Item::Constraint(constraint));
	}

	/// Remove dead items from the model.
	pub fn clean_up(&mut self) {
		self.items.retain(|item| match item {
			Item::Variable(var) => var.is_alive(),
			Item::Constraint(constraint) => constraint.arguments.iter().all(|arg| match arg {
				Argument::Value(_) => true,
				Argument::DefinedVariable(var) => var.is_alive(),
			}),
		});
	}

	/// Iterate over the items in the model.
	///
	/// Note that this will clean up dead items before returning the iterator.
	pub fn items(&mut self) -> impl Iterator<Item = &Item<'db>> {
		self.clean_up();
		self.items.iter()
	}

	/// Iterate over the items in the model mutably.
	///
	/// Note that this will clean up dead items before returning the iterator.
	pub fn items_mut(&mut self) -> impl Iterator<Item = &mut Item<'db>> {
		self.clean_up();
		self.items.iter_mut()
	}

	/// Print the NanoZinc model as a string
	pub fn print(&mut self, db: &'db dyn Db, program: &shackle_ir::Program<'db>) -> String {
		let mut s = String::new();
		for item in self.items() {
			let Some(line) = (|| {
				Some(match item {
					Item::Variable(var) => match var.upgrade()?.deref() {
						DataView::BoolVar(v) => {
							format!(
								"var {}: _X_INTRODUCED_{};\n",
								v.domain()
									.map(|b| format!("{:?}", b))
									.unwrap_or_else(|| "bool".to_owned()),
								v.ident()
							)
						}
						DataView::IntVar(v) => {
							format!(
								"var {}: _X_INTRODUCED_{};\n",
								v.domain()
									.map(|d| format!("{}", d.as_int_set()))
									.unwrap_or_else(|| "int".to_owned()),
								v.ident(),
							)
						}
						DataView::FloatVar(v) => format!(
							"var {}: _X_INTRODUCED_{};\n",
							v.domain()
								.map(|d| format!("{}", d.as_float_set()))
								.unwrap_or_else(|| "float".to_owned()),
							v.ident(),
						),
						DataView::IntSetVar(v) => format!(
							"var set of {}: _X_INTRODUCED_{};\n",
							v.domain()
								.map(|d| format!("{}", d.as_int_set()))
								.unwrap_or_else(|| "int".to_owned()),
							v.ident(),
						),
						_ => unreachable!(),
					},
					Item::Constraint(constraint) => {
						format!(
							"constraint {}({});\n",
							program.functions[constraint.function].name.pretty_print(db),
							constraint
								.arguments
								.iter()
								.map(|arg| {
									let value = match arg {
										Argument::Value(v) => v.clone(),
										Argument::DefinedVariable(wv) => wv.upgrade()?,
									};
									Some(match value.deref() {
										DataView::BoolVar(v) => {
											format!("_X_INTRODUCED_{}", v.ident())
										}
										DataView::IntVar(v) => {
											format!("_X_INTRODUCED_{}", v.ident())
										}
										DataView::FloatVar(v) => {
											format!("_X_INTRODUCED_{}", v.ident(),)
										}
										DataView::IntSetVar(v) => {
											format!("_X_INTRODUCED_{}", v.ident(),)
										}
										DataView::Int(i) => format!("{}\n", i),
										DataView::Float(f) => format!("{}\n", f),
										DataView::Str(s) => format!("{:?}", s),
										v => format!("{:?}", v),
									})
								})
								.collect::<Option<Vec<_>>>()?
								.join(", ")
						)
					}
				})
			})() else {
				continue;
			};
			s.push_str(&line);
		}
		s
	}
}

/// A NanoZinc item, which can be either a variable or a constraint.
#[derive(Debug, Clone, PartialEq)]
pub enum Item<'db> {
	/// A variable
	Variable(WeakValue),
	/// A top-level constraint
	Constraint(Constraint<'db>),
}

/// A constraint
#[derive(Debug, Clone, PartialEq)]
pub struct Constraint<'db> {
	function: shackle_ir::FunctionId<'db>,
	arguments: Vec<Argument>,
	annotations: Vec<Value>,
}

/// Argument to a constraint
#[derive(Debug, Clone, PartialEq)]
pub enum Argument {
	/// Value kept alive by this constraint
	Value(Value),
	/// Variable defined by this constraint
	DefinedVariable(WeakValue),
}
