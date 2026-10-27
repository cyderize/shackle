//! NanoZinc model
//!
//! Represents the state of the instance

use std::{
	ops::Deref,
	sync::atomic::{AtomicU64, Ordering},
};

use shackle_hir::Db;

use crate::{DataView, Value};

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
		self.items.push(Item::Variable(var.clone()));
		var
	}

	/// Add a fresh integer variable to the NanoZinc
	pub fn add_var_int(&mut self, domain: Value) -> Value {
		let var = Value::new_int_var(self.next_id.fetch_add(1, Ordering::Relaxed), domain);
		self.items.push(Item::Variable(var.clone()));
		var
	}

	/// Add a fresh float variable to the NanoZinc
	pub fn add_var_float(&mut self, domain: Value) -> Value {
		let var = Value::new_float_var(self.next_id.fetch_add(1, Ordering::Relaxed), domain);
		self.items.push(Item::Variable(var.clone()));
		var
	}

	/// Add a fresh int set variable to the NanoZinc
	pub fn add_var_set_of_int(&mut self, domain: Value) -> Value {
		let var = Value::new_int_set_var(self.next_id.fetch_add(1, Ordering::Relaxed), domain);
		self.items.push(Item::Variable(var.clone()));
		var
	}

	/// Print the NanoZinc model as a string
	pub fn print(&self, db: &'db dyn Db, program: &shackle_ir::Program<'db>) -> String {
		let mut s = String::new();
		for item in &self.items {
			match item {
				Item::Variable(var) => match var.deref() {
					DataView::BoolVar(v) => {
						s.push_str(&format!("var bool: _X_INTRODUCED_{};\n", todo!("ident")))
					}
					DataView::IntVar(_) => s.push_str(&format!(
						"var {}: _X_INTRODUCED_{};\n",
						todo!("domain"),
						todo!("ident")
					)),
					DataView::FloatVar(_) => s.push_str(&format!(
						"var {}: _X_INTRODUCED_{};\n",
						todo!("domain"),
						todo!("ident")
					)),
					DataView::IntSetVar(_) => s.push_str(&format!(
						"var set of {}: _X_INTRODUCED_{};\n",
						todo!("domain"),
						todo!("ident")
					)),
					_ => unreachable!(),
				},
				Item::Constraint(constraint) => {
					s.push_str(&format!(
						"constraint {}({});\n",
						program.functions[constraint.function].name.pretty_print(db),
						constraint
							.arguments
							.iter()
							.map(|arg| "todo".to_owned())
							.collect::<Vec<_>>()
							.join(", ")
					));
				}
			}
		}
		s
	}
}

/// A NanoZinc item, which can be either a variable or a constraint.
#[derive(Debug, Clone, PartialEq)]
pub enum Item<'db> {
	/// A variable
	Variable(Value),
	/// A top-level constraint
	Constraint(Constraint<'db>),
}

/// A constraint
#[derive(Debug, Clone, PartialEq)]
pub struct Constraint<'db> {
	function: shackle_ir::FunctionId<'db>,
	arguments: Vec<Value>,
	annotations: Vec<Value>,
}
