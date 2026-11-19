//! NanoZinc model
//!
//! Represents the state of the instance

use std::{
	fmt::Write,
	sync::{
		Arc, Weak,
		atomic::{AtomicU64, Ordering},
	},
};

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{DataView, Value, value::WeakValue};

/// A NanoZinc model, which consists of a list of items (variables and constraints).
#[derive(Debug, Default)]
pub struct NanoZinc {
	items: Vec<Item>,
	// A domain can represent a subsumed non-defining constraint. Keep that
	// variable alive until the restriction is known to be redundant.
	domain_roots: Vec<Value>,
	next_id: AtomicU64,
	pending: Vec<PendingNanoZinc>,
	solve: Solve,
}

impl NanoZinc {
	/// Retain a variable whose domain represents a removed constraint.
	///
	/// Register the root before removing the constraint. Repeated registrations
	/// of the same variable are idempotent.
	pub fn retain_domain(&mut self, variable: &Value) {
		let weak = variable.downgrade();
		assert!(
			self.items
				.iter()
				.any(|item| matches!(item, Item::Variable(v) if *v == weak)),
			"domain root must be a variable in this model"
		);
		if !self
			.domain_roots
			.iter()
			.any(|root| root.same_identity(variable))
		{
			self.domain_roots.push(variable.clone());
		}
	}

	/// Release a domain root once the restrictions it represents are redundant.
	///
	/// The caller is responsible for proving redundancy without relying on the
	/// restrictions being removed. Returns whether a root was present.
	pub fn release_domain(&mut self, variable: &Value) -> bool {
		if let Some(index) = self
			.domain_roots
			.iter()
			.position(|root| root.same_identity(variable))
		{
			let _ = self.domain_roots.swap_remove(index);
			true
		} else {
			false
		}
	}

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
			.map(|c| DefiningConstraint {
				function: c.function,
				arguments: c
					.arguments
					.iter()
					.map(|v| DefiningConstraintArgument::new(v, &pending.variables_defined))
					.collect(),
				annotations: c
					.annotations
					.iter()
					.map(|v| DefiningConstraintArgument::new(v, &pending.variables_defined))
					.collect(),
			})
			.collect();
		let defining = DefiningConstraints {
			constraints: Some(Arc::new(DefinitionGroup { constraints: cons })),
			weak: None,
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

	/// Get the solve item
	pub fn solve(&self) -> &Solve {
		&self.solve
	}

	/// Set the solve item for the model.
	pub fn set_solve(&mut self, solve: Solve) {
		self.solve = solve;
	}

	/// Print the NanoZinc model as a string
	pub fn print(&mut self, program: &shackle_ir::Program) -> String {
		let mut s = String::new();
		let mut constraints = String::new();
		let mut seen_groups = FxHashMap::default();
		let mut held_groups = Vec::new();
		for item in self.items() {
			match item {
				Item::Variable(var) => {
					let Some(value) = var.upgrade() else {
						continue;
					};

					let defining_constraints = match value.deref() {
						DataView::BoolVar(v) => {
							write!(
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
							write!(
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
							write!(
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
							write!(
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

					let Some(group) = defining_constraints.constraints.as_ref() else {
						writeln!(&mut s).unwrap();
						continue;
					};
					let group_ptr = Arc::as_ptr(group);
					let (group_id, is_new) = match seen_groups.get(&group_ptr) {
						Some(&id) => (id, false),
						None => {
							let id = seen_groups.len() + 1;
							let _ = seen_groups.insert(group_ptr, id);
							(id, true)
						}
					};
					writeln!(&mut s, " % defined by group {group_id}").unwrap();
					if !is_new {
						continue;
					}
					if !constraints.is_empty() {
						writeln!(&mut constraints).unwrap();
					}
					writeln!(&mut constraints, "% defining group {group_id}").unwrap();
					for constraint in defining_constraints.iter() {
						writeln!(
							&mut constraints,
							"constraint {}({}){};",
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
									self.print_value(program, ann.to_value())
								))
								.collect::<Vec<_>>()
								.join("")
						)
						.unwrap();
					}
					held_groups.push(defining_constraints);
				}
				Item::Constraint(constraint) => {
					if !constraints.is_empty() {
						writeln!(&mut constraints).unwrap();
					}
					writeln!(
						&mut constraints,
						"constraint {}({});",
						program.strings[program.functions[constraint.function].name],
						constraint
							.arguments
							.iter()
							.map(|arg| self.print_value(program, arg.clone()))
							.collect::<Vec<_>>()
							.join(", ")
					)
					.unwrap();
				}
			}
		}
		if !constraints.is_empty() {
			writeln!(&mut s).unwrap();
			s.push_str(&constraints);
		}

		if !s.is_empty() {
			writeln!(&mut s).unwrap();
		}
		writeln!(
			&mut s,
			"solve{} {};",
			self.solve
				.annotations()
				.iter()
				.map(|ann| format!(" :: {}", self.print_value(program, ann.clone())))
				.collect::<Vec<_>>()
				.join(""),
			match &self.solve {
				Solve::Maximize { objective, .. } =>
					format!("maximize {}", self.print_value(program, objective.clone())),
				Solve::Minimize { objective, .. } =>
					format!("minimize {}", self.print_value(program, objective.clone())),
				Solve::Satisfy { .. } => "satisfy".to_owned(),
			}
		)
		.unwrap();

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
#[derive(Debug, Clone, PartialEq)]
struct DefiningConstraint {
	function: shackle_ir::FunctionId,
	arguments: Vec<DefiningConstraintArgument>,
	annotations: Vec<DefiningConstraintArgument>,
}

/// The constraints shared by a set of defined variables.
#[derive(Debug)]
pub(crate) struct DefinitionGroup {
	constraints: Vec<DefiningConstraint>,
}

impl Drop for DefinitionGroup {
	fn drop(&mut self) {
		for constraint in &self.constraints {
			for argument in constraint.arguments.iter().chain(&constraint.annotations) {
				argument.cleanup_dead_outputs(self);
			}
		}
	}
}

/// Defining constraints for a variable (or variables).
#[derive(Clone, Debug, Default)]
pub struct DefiningConstraints {
	constraints: Option<Arc<DefinitionGroup>>,
	weak: Option<Weak<DefinitionGroup>>,
}

impl PartialEq for DefiningConstraints {
	fn eq(&self, other: &Self) -> bool {
		match (&self.constraints, &other.constraints) {
			(Some(a), Some(b)) => a.constraints == b.constraints,
			(None, None) => match (&self.weak, &other.weak) {
				(Some(a), Some(b)) => Weak::ptr_eq(a, b),
				(None, None) => true,
				_ => false,
			},
			_ => false,
		}
	}
}

impl<T: IntoIterator<Item = DefiningConstraint>> From<T> for DefiningConstraints {
	fn from(constraints: T) -> Self {
		Self {
			constraints: Some(Arc::new(DefinitionGroup {
				constraints: constraints.into_iter().collect(),
			})),
			weak: None,
		}
	}
}

impl DefiningConstraints {
	pub(crate) fn downgrade(&mut self) -> Option<Self> {
		let strong = self.constraints.take()?;
		self.weak = Some(Arc::downgrade(&strong));
		Some(Self {
			constraints: Some(strong),
			weak: None,
		})
	}

	pub(crate) fn restore(&mut self) -> bool {
		if self.constraints.is_some() {
			return true;
		}
		if let Some(strong) = self.weak.as_ref().and_then(Weak::upgrade) {
			self.constraints = Some(strong);
			self.weak = None;
			true
		} else {
			false
		}
	}

	pub(crate) fn is_weak_for(&self, group: *const DefinitionGroup) -> bool {
		self.weak
			.as_ref()
			.is_some_and(|weak| std::ptr::eq(weak.as_ptr(), group))
	}

	/// Iterate over the defining constraints (if any).
	fn iter(&self) -> impl Iterator<Item = &DefiningConstraint> {
		self.constraints
			.as_ref()
			.map(|arc| arc.constraints.iter())
			.into_iter()
			.flatten()
	}
}

/// A structured constraint value whose defined outputs are weak references.
#[derive(Debug, Clone, PartialEq)]
enum DefiningConstraintArgument {
	Input(Value),
	DefinedVariable(WeakValue),
	Seq(Vec<Self>),
	Annotation {
		annotation: shackle_ir::AnnotationId,
		arguments: Vec<Self>,
	},
}

impl DefiningConstraintArgument {
	fn new(value: &Value, outputs: &FxHashSet<Value>) -> Self {
		if outputs.iter().any(|output| output.same_identity(value)) {
			return Self::DefinedVariable(value.downgrade());
		}
		match value.deref() {
			DataView::Seq(seq) => Self::Seq(seq.iter().map(|v| Self::new(v, outputs)).collect()),
			DataView::Set(_) => panic!("discrete sets are not supported in NanoZinc"),
			DataView::Annotation(annotation) => Self::Annotation {
				annotation: annotation.annotation,
				arguments: annotation
					.arguments
					.iter()
					.map(|v| Self::new(v, outputs))
					.collect(),
			},
			_ => Self::Input(value.clone()),
		}
	}

	fn to_value(&self) -> Value {
		match self {
			Self::Input(value) => value.clone(),
			Self::DefinedVariable(value) => value
				.upgrade()
				.expect("live definition retains its outputs"),
			Self::Seq(values) => values.iter().map(Self::to_value).collect(),
			Self::Annotation {
				annotation,
				arguments,
			} => Value::new_annotation(*annotation, arguments.iter().map(Self::to_value)),
		}
	}

	fn cleanup_dead_outputs(&self, group: *const DefinitionGroup) {
		match self {
			Self::Input(_) => {}
			Self::DefinedVariable(value) => value.cleanup_dead_definition(group),
			Self::Seq(values) => {
				for value in values {
					value.cleanup_dead_outputs(group);
				}
			}
			Self::Annotation { arguments, .. } => {
				for value in arguments {
					value.cleanup_dead_outputs(group);
				}
			}
		}
	}
}

/// Solve item
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Solve {
	/// Maximization
	Maximize {
		/// Search annotations
		annotations: Vec<Value>,
		/// Objective
		objective: Value,
	},
	/// Minimization
	Minimize {
		/// Search annotations
		annotations: Vec<Value>,
		/// Objective
		objective: Value,
	},
	/// Satisfaction
	Satisfy {
		/// Search annotations
		annotations: Vec<Value>,
	},
}

impl Solve {
	/// Get the annotations
	pub fn annotations(&self) -> &[Value] {
		match self {
			Solve::Maximize { annotations, .. } => annotations,
			Solve::Minimize { annotations, .. } => annotations,
			Solve::Satisfy { annotations } => annotations,
		}
	}
}

impl Default for Solve {
	fn default() -> Self {
		Solve::Satisfy {
			annotations: Vec::new(),
		}
	}
}

#[derive(Debug, Clone, PartialEq, Default)]
struct PendingNanoZinc {
	variables_defined: FxHashSet<Value>,
	constraints: Vec<Constraint>,
}

#[cfg(test)]
mod tests {
	use expect_test::expect;

	use super::*;

	#[test]
	fn defining_constraint_sequences_are_one_based_and_one_dimensional() {
		let sequence: Value = (1..=4).map(Value::from).collect();
		let sequence = sequence.with_dim([(0, 1), (3, 4)]);
		let argument = DefiningConstraintArgument::new(&sequence, &FxHashSet::default());
		let reconstructed = argument.to_value();
		let DataView::Seq(seq) = reconstructed.deref() else {
			panic!("expected a sequence");
		};
		assert_eq!(seq.dims(), 1);
		assert_eq!(seq.dim(1), (1, 4));
		assert_eq!(
			seq.iter().cloned().collect::<Vec<_>>(),
			vec![1.into(), 2.into(), 3.into(), 4.into()]
		);
	}

	#[test]
	#[should_panic(expected = "discrete sets are not supported in NanoZinc")]
	fn defining_constraints_reject_discrete_sets() {
		let set = Value::new_discrete_set([Value::from("element")]);
		let _ = DefiningConstraintArgument::new(&set, &FxHashSet::default());
	}

	#[test]
	fn domain_roots_are_model_owned_and_idempotent() {
		let mut model = NanoZinc::default();
		let variable = model.add_var_bool(false);
		let weak = variable.downgrade();
		model.retain_domain(&variable);
		model.retain_domain(&variable);
		drop(variable);
		let value = weak.upgrade().expect("domain root retains the variable");
		assert!(model.release_domain(&value));
		assert!(!model.release_domain(&value));
		drop(value);
		assert!(weak.upgrade().is_none());
	}

	#[test]
	fn shared_definition_revives_outputs_and_releases_contents() {
		let mut model = NanoZinc::default();
		model.push_pending();
		let domain = Value::from("domain");
		let weak_domain = domain.downgrade();
		let values = [
			model.add_var_bool(true),
			model.add_var_int(domain.clone(), true),
			model.add_var_float(domain.clone(), true),
			model.add_var_set_of_int(domain.clone(), true),
		];
		let weak: Vec<_> = values.iter().map(Value::downgrade).collect();
		let input = Value::from("input");
		let weak_input = input.downgrade();
		model.add_constraint(
			shackle_ir::FunctionId::new(1),
			values.iter().cloned().chain([input.clone()]),
			[],
		);
		model.pop_pending();
		drop(input);
		drop(domain);
		let [survivor, a, b, c] = values;
		drop((a, b, c));
		for value in &weak[1..] {
			assert!(value.is_alive());
			let revived = value.upgrade().expect("live group retains its output");
			assert!(matches!(
				revived.deref(),
				DataView::IntVar(_) | DataView::FloatVar(_) | DataView::IntSetVar(_)
			));
		}
		assert!(weak_domain.is_alive());
		assert!(weak_input.is_alive());
		drop(survivor);
		assert!(weak.iter().all(|value| !value.is_alive()));
		assert!(!weak_input.is_alive());
		assert!(
			!weak_domain.is_alive(),
			"dead outputs release their domains despite model weak references"
		);
	}

	#[test]
	fn nested_outputs_and_annotations_do_not_form_cycles() {
		let mut model = NanoZinc::default();
		model.push_pending();
		let a = model.add_var_bool(true);
		let b = model.add_var_bool(true);
		let weak_a = a.downgrade();
		let weak_b = b.downgrade();
		let inner: Value = [b.clone()].into_iter().collect();
		let outer: Value = [inner].into_iter().collect();
		let annotation =
			Value::new_annotation(shackle_ir::AnnotationId::new(1), [a.clone(), b.clone()]);
		model.add_constraint(shackle_ir::FunctionId::new(1), [outer], [annotation]);
		model.pop_pending();
		drop(b);
		assert!(weak_b.upgrade().is_some());
		drop(a);
		assert!(!weak_a.is_alive());
		assert!(!weak_b.is_alive());
	}

	#[test]
	#[ignore = "definition groups with cyclic input dependencies are not yet collected"]
	fn cyclic_definition_groups_are_released() {
		let mut model = NanoZinc::default();
		model.push_pending();
		let outer = model.add_var_bool(true);
		model.push_pending();
		let inner = model.add_var_bool(true);
		let weak_outer = outer.downgrade();
		let weak_inner = inner.downgrade();
		model.add_constraint(
			shackle_ir::FunctionId::new(1),
			[outer.clone(), inner.clone()],
			[],
		);
		model.pop_pending();
		model.add_constraint(
			shackle_ir::FunctionId::new(1),
			[outer.clone(), inner.clone()],
			[],
		);
		model.pop_pending();
		drop((outer, inner, model));
		assert!(!weak_outer.is_alive());
		assert!(!weak_inner.is_alive());
	}

	#[test]
	fn model_drop_releases_domain_roots_but_not_external_handles() {
		let mut model = NanoZinc::default();
		model.push_pending();
		let a = model.add_var_bool(true);
		let b = model.add_var_bool(true);
		let weak_a = a.downgrade();
		let weak_b = b.downgrade();
		model.add_constraint(shackle_ir::FunctionId::new(1), [a.clone(), b.clone()], []);
		model.pop_pending();
		model.retain_domain(&a);
		drop(a);
		drop(model);
		assert!(weak_a.is_alive(), "external sibling retains the group");
		drop(b);
		assert!(!weak_a.is_alive());
		assert!(!weak_b.is_alive());
	}

	#[test]
	fn domain_root_keeps_shared_definition_and_can_be_removed() {
		let mut model = NanoZinc::default();
		model.push_pending();
		let a = model.add_var_bool(true);
		let b = model.add_var_bool(true);
		let weak_a = a.downgrade();
		let weak_b = b.downgrade();
		model.add_constraint(shackle_ir::FunctionId::new(1), [a.clone(), b.clone()], []);
		model.pop_pending();
		model.retain_domain(&a);
		drop((a, b));
		assert!(weak_b.is_alive());
		let rooted = weak_a.upgrade().unwrap();
		assert!(model.release_domain(&rooted));
		drop(rooted);
		assert!(!weak_a.is_alive());
		assert!(!weak_b.is_alive());
	}

	#[test]
	fn dropping_a_clone_under_variable_guard_does_not_deadlock() {
		let mut model = NanoZinc::default();
		let variable = model.add_var_bool(false);
		let clone = variable.clone();
		let guard = variable.as_bool_var();
		drop(clone);
		drop(guard);
	}

	#[test]
	fn concurrent_last_release_and_revival() {
		let mut model = NanoZinc::default();
		model.push_pending();
		let a = model.add_var_bool(true);
		let b = model.add_var_bool(true);
		let weak = b.downgrade();
		model.add_constraint(shackle_ir::FunctionId::new(1), [a.clone(), b.clone()], []);
		model.pop_pending();
		std::thread::scope(|scope| {
			for _ in 0..4 {
				let weak = weak.clone();
				let _ = scope.spawn(move || {
					for _ in 0..5_000 {
						drop(weak.upgrade().expect("live group retains output"));
					}
				});
			}
			drop(b);
		});
		drop(a);
		assert!(!weak.is_alive());
	}

	#[test]
	fn concurrent_group_destruction_and_weak_upgrades() {
		let mut model = NanoZinc::default();
		model.push_pending();
		let a = model.add_var_bool(true);
		let b = model.add_var_bool(true);
		let weak = b.downgrade();
		model.add_constraint(shackle_ir::FunctionId::new(1), [a.clone(), b.clone()], []);
		model.pop_pending();
		drop(b);
		std::thread::scope(|scope| {
			for _ in 0..4 {
				let weak = weak.clone();
				let _ = scope.spawn(move || {
					for _ in 0..5_000 {
						drop(weak.upgrade());
					}
				});
			}
			drop(a);
		});
		assert!(!weak.is_alive());
	}

	#[test]
	fn printing_declares_dormant_outputs_and_emits_group_once() {
		let function_id = shackle_ir::FunctionId::new(1);
		let mut program = shackle_ir::Program {
			global_slot_count: 0,
			functions: Default::default(),
			annotations: Default::default(),
			main_entry_point: function_id,
			output_entry_point: function_id,
			strings: Default::default(),
		};
		let name = program.strings.insert("f".to_owned());
		let _ = program.functions.insert(shackle_ir::Function {
			name,
			slot_count: 0,
			promise_total: false,
			body: Default::default(),
			origins: Default::default(),
		});
		let mut model = NanoZinc::default();
		model.push_pending();
		let a = model.add_var_bool(true);
		let b = model.add_var_bool(true);
		model.add_constraint(function_id, [a.clone(), b.clone()], []);
		model.pop_pending();
		model.push_pending();
		let c = model.add_var_bool(true);
		model.add_constraint(function_id, [c.clone()], []);
		model.pop_pending();
		drop(b);
		let expected = expect![[r#"
    var bool: _X_INTRODUCED_0; % defined by group 1
    var bool: _X_INTRODUCED_1; % defined by group 1
    var bool: _X_INTRODUCED_2; % defined by group 2

    % defining group 1
    constraint f(_X_INTRODUCED_0, _X_INTRODUCED_1);

    % defining group 2
    constraint f(_X_INTRODUCED_2);

    solve satisfy;
"#]];
		let actual = model.print(&program);
		expected.assert_eq(&actual);
		drop(a);
		drop(c);
	}
}
