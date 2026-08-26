//! Liveness analysis and stack slot assignment for MIR.
use std::num::NonZeroU32;

use shackle_utils::{
	arena::ArenaMap,
	hash::{Map, Set},
};

use crate::{
	Comprehension, Declaration, DeclarationId, ExpressionData, Function, Generator, Identifier,
	Let, LetItem, LetResult, LiteralData, ValueData,
};

#[derive(Debug, Clone, PartialEq, Eq)]
enum TodoItem<'a, 'db> {
	Declaration(DeclarationId<'db>),
	Expression(&'a ExpressionData<'db>),
	Value(&'a ValueData<'db>),
	Identifier(&'a Identifier<'db>),
	AddLive(DeclarationId<'db>),
	RemoveLive(DeclarationId<'db>),
	SwapStash,
	MergeStash,
	Comprehension {
		comprehension: &'a Comprehension<'db>,
		from_generator: Option<usize>,
	},
}

/// Determines declarations which are live at the same time and therefore interfere with each other.
#[derive(Debug, Clone)]
pub struct Liveness<'a, 'db> {
	function: &'a Function<'db>,
	todo: Vec<TodoItem<'a, 'db>>,
	interference: Set<(DeclarationId<'db>, DeclarationId<'db>)>,
	live: Set<DeclarationId<'db>>,
	stash: Vec<Set<DeclarationId<'db>>>,
}

impl<'a, 'db> Liveness<'a, 'db> {
	/// Create a new liveness analyser for the given function.
	pub fn new(function: &'a Function<'db>) -> Self {
		Self {
			function,
			todo: Vec::new(),
			interference: Set::new(),
			live: Set::new(),
			stash: Vec::new(),
		}
	}

	/// Run the analysis and return the interferences.
	pub fn analyse(mut self) -> Set<(DeclarationId<'db>, DeclarationId<'db>)> {
		let Some(body) = &self.function.body else {
			return Set::new();
		};
		self.push_let(body);
		while let Some(item) = self.todo.pop() {
			match item {
				TodoItem::Declaration(decl_id) => self.push_declaration(decl_id),
				TodoItem::Expression(expr) => self.push_expression(expr),
				TodoItem::Value(value) => self.push_value(value),
				TodoItem::Identifier(ident) => self.visit_identifier(*ident),
				TodoItem::AddLive(decl_id) => {
					self.add_live(decl_id);
				}
				TodoItem::RemoveLive(decl_id) => {
					let _ = self.live.remove(&decl_id);
				}
				TodoItem::SwapStash => {
					let stashed = self.stash.last_mut().expect("Stash should not be empty");
					std::mem::swap(&mut self.live, stashed);
				}
				TodoItem::MergeStash => {
					let stashed = self.stash.pop().expect("Stash should not be empty");
					self.live.extend(stashed);
				}
				TodoItem::Comprehension {
					comprehension,
					from_generator,
				} => {
					self.push_comprehension(comprehension, from_generator);
				}
			}
		}
		self.interference
	}

	fn visit_declaration(&mut self, idx: DeclarationId<'db>) {
		let _ = self.live.remove(&idx);
		let live = self.live.iter().copied().collect::<Vec<_>>();
		for live_decl in live {
			self.add_interference(idx, live_decl);
		}
	}

	fn add_interference(&mut self, a: DeclarationId<'db>, b: DeclarationId<'db>) {
		if a < b {
			let _ = self.interference.insert((a, b));
		} else {
			let _ = self.interference.insert((b, a));
		}
	}

	fn add_live(&mut self, declaration: DeclarationId<'db>) {
		if self.live.insert(declaration) {
			let live = self.live.iter().copied().collect::<Vec<_>>();
			for other in live {
				if declaration != other {
					self.add_interference(declaration, other);
				}
			}
		}
	}

	fn push_declaration(&mut self, idx: DeclarationId<'db>) {
		self.visit_declaration(idx);
		let decl = &self.function.data[idx];
		for ann in decl.annotations.iter() {
			self.todo.push(TodoItem::Identifier(ann));
		}
		if matches!(&decl.definition.data, ExpressionData::Comprehension(_)) {
			// The result is built in its own slot, so keep it live while
			// evaluating the comprehension, but not before this declaration.
			self.todo.push(TodoItem::RemoveLive(idx));
			self.todo.push(TodoItem::Expression(&decl.definition.data));
			self.todo.push(TodoItem::AddLive(idx));
		} else {
			self.todo.push(TodoItem::Expression(&decl.definition.data));
		}
	}

	fn push_let_item(&mut self, item: &'a LetItem<'db>) {
		match item {
			LetItem::Constraint(c) => {
				for ann in c.annotations.iter() {
					self.todo.push(TodoItem::Identifier(ann));
				}
				self.todo.push(TodoItem::Expression(&c.expression.data));
			}
			LetItem::Declaration(d) => self.todo.push(TodoItem::Declaration(*d)),
			LetItem::GlobalAssignment(g) => {
				self.todo.push(TodoItem::Expression(&g.definition.data));
			}
		}
	}

	fn push_let(&mut self, l: &'a Let<'db>) {
		for item in l.items.iter() {
			self.push_let_item(item);
		}
		if let LetResult::Identifier(ident) = &l.result {
			self.todo.push(TodoItem::Identifier(ident));
		}
	}

	fn push_comprehension(&mut self, c: &'a Comprehension<'db>, from_generator: Option<usize>) {
		let mut skip = 0;
		if let Some(f) = from_generator {
			let Generator::Iterator { element } = &c.generators[f] else {
				unreachable!("Should be an iterator")
			};
			// In the next iteration, the slot will be assigned the next value from the iterator,
			// but the RHS doesn't need to be evaluated again
			self.todo.push(TodoItem::RemoveLive(*element));
			skip = f + 1;
		}
		for g in c.generators.iter().skip(skip) {
			match g {
				Generator::Assignment { declaration } => {
					self.todo.push(TodoItem::Declaration(*declaration));
				}
				Generator::Iterator { element } => {
					self.todo.push(TodoItem::Declaration(*element));
				}
				Generator::Constraint(c) => {
					self.todo.push(TodoItem::Expression(&c.expression.data));
				}
				Generator::Where(w) => {
					self.todo.push(TodoItem::Value(&w.data));
				}
			}
		}
		self.push_let(&c.template);
	}

	fn push_expression(&mut self, expr: &'a ExpressionData<'db>) {
		match &expr {
			ExpressionData::Call(call) => {
				for arg in call.arguments.iter() {
					self.todo.push(TodoItem::Value(&arg.data));
				}
			}
			ExpressionData::AnnotationCall(annotation_call) => {
				for arg in annotation_call.arguments.iter() {
					self.todo.push(TodoItem::Value(&arg.data));
				}
			}
			ExpressionData::Builtin(builtin) => {
				for arg in builtin.arguments().iter() {
					self.todo.push(TodoItem::Value(&arg.data));
				}
			}
			ExpressionData::IfThenElse(if_then_else) => {
				self.push_value(&if_then_else.condition.data);
				self.todo.push(TodoItem::MergeStash);
				self.push_let(&if_then_else.then);
				self.todo.push(TodoItem::SwapStash);
				self.push_let(&if_then_else.else_expression);
				self.stash.push(self.live.clone());
			}
			ExpressionData::Comprehension(comprehension) => {
				self.push_comprehension(comprehension, None);
				for (i, g) in comprehension.generators.iter().enumerate() {
					if matches!(g, Generator::Iterator { .. }) {
						self.todo.push(TodoItem::Comprehension {
							comprehension,
							from_generator: Some(i),
						});
					}
				}
			}
			ExpressionData::Iterator(it) => {
				self.todo.push(TodoItem::Value(it));
			}
			ExpressionData::Value(value) => self.push_value(value),
		}
	}

	fn push_value(&mut self, value: &'a ValueData<'db>) {
		match value {
			ValueData::Literal(LiteralData::Identifier(ident)) => {
				self.todo.push(TodoItem::Identifier(ident));
			}
			ValueData::Tuple(tuple) => self.todo.extend(tuple.members.iter().filter_map(|v| {
				if let LiteralData::Identifier(ident) = &v.data {
					Some(TodoItem::Identifier(ident))
				} else {
					None
				}
			})),
			ValueData::Set(set) => self.todo.extend(set.members.iter().filter_map(|v| {
				if let LiteralData::Identifier(ident) = &v.data {
					Some(TodoItem::Identifier(ident))
				} else {
					None
				}
			})),
			ValueData::Array(array) => self.todo.extend(array.members.iter().filter_map(|v| {
				if let LiteralData::Identifier(ident) = &v.data {
					Some(TodoItem::Identifier(ident))
				} else {
					None
				}
			})),
			ValueData::TupleAccess(ta) => {
				self.todo.push(TodoItem::Identifier(&ta.tuple));
			}
			_ => (),
		}
	}

	fn visit_identifier(&mut self, ident: Identifier<'db>) {
		if let Identifier::Declaration(d) = ident {
			self.add_live(d);
		}
	}
}

/// A single stack slot allocated to a declaration or input parameter.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SlotId(u32);

impl std::fmt::Debug for SlotId {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "SlotId({})", self.0)
	}
}

/// Allocates stack slots for declarations based on the interferences.
#[derive(Debug, Clone)]
pub struct SlotAllocator<'a, 'db> {
	function: &'a Function<'db>,
	interferences: Map<DeclarationId<'db>, Vec<DeclarationId<'db>>>,
	ordered: Vec<DeclarationId<'db>>,
}

impl<'a, 'db> SlotAllocator<'a, 'db> {
	/// Create a new slot allocator from the given interference edges.
	pub fn new(
		function: &'a Function<'db>,
		interferences: &Set<(DeclarationId<'db>, DeclarationId<'db>)>,
	) -> Self {
		let mut map: Map<_, Vec<_>> = Map::new();
		for (a, b) in interferences.iter() {
			map.entry(*a).or_default().push(*b);
			map.entry(*b).or_default().push(*a);
		}
		Self {
			function,
			interferences: map,
			ordered: Vec::new(),
		}
	}

	/// Allocate stack slots for the declarations in the function.
	pub fn allocate_slots(mut self) -> ArenaMap<Declaration<'db>, SlotId> {
		self.max_card_search();
		self.greedy_slot_assignment()
	}

	fn max_card_search(&mut self) {
		let Some(body) = self.function.body.as_ref() else {
			return;
		};
		let one = NonZeroU32::new(1).unwrap();
		let mut weights =
			ArenaMap::from_iter(self.function.data.declarations.keys().map(|k| (k, one)));
		if let LetResult::Identifier(Identifier::Declaration(ret)) = body.result {
			// Force the return value to be the first slot
			let two = NonZeroU32::new(2).unwrap();
			weights.insert(ret, two);
		}
		loop {
			// TODO: Use a priority queue to avoid O(n) search for max weight
			let Some((d, _)) = weights.iter().map(|(d, w)| (d, *w)).max_by_key(|(_, w)| *w) else {
				return;
			};
			self.ordered.push(d);
			let _ = weights.remove(d);
			for n in self.interferences.get(&d).iter().flat_map(|ds| ds.iter()) {
				if let Some(nw) = weights.get_mut(*n) {
					*nw = nw.saturating_add(1);
				}
			}
		}
	}

	fn greedy_slot_assignment(&mut self) -> ArenaMap<Declaration<'db>, SlotId> {
		let mut slots = ArenaMap::new();
		for d in self.ordered.iter() {
			let mut used_slots: Set<u32> = Set::new();
			for n in self.interferences.get(d).iter().flat_map(|ds| ds.iter()) {
				if let Some(SlotId(slot)) = slots.get(*n) {
					let _ = used_slots.insert(*slot);
				}
			}
			let slot = (self.function.parameters.len() as u32..)
				.find(|s| !used_slots.contains(s))
				.expect("There should always be a free slot");
			slots.insert(*d, SlotId(slot));
		}
		slots
	}
}

#[cfg(test)]
mod tests {
	use expect_test::{Expect, expect};
	use salsa::Setter;
	use shackle_hir::{
		CompilerDatabase, Db,
		input::{CompilerSettings, InlineModelFile, InputFiles},
	};
	use shackle_syntax::InputLang;
	use shackle_thir::db::final_thir;
	use shackle_utils::{
		arena::ArenaMap,
		hash::{Map, Set},
	};

	use crate::{
		Declaration, DeclarationId, Function,
		liveness::{Liveness, SlotAllocator, SlotId},
		lower::lower_from_thir,
	};

	#[derive(Debug)]
	struct LivenessAndSlots {
		#[allow(dead_code, reason = "used for testing")]
		interferences: Vec<InterferenceEdge>,
		#[allow(dead_code, reason = "used for testing")]
		slots: Map<String, SlotId>,
	}

	impl LivenessAndSlots {
		fn new<'db>(
			db: &'db dyn Db,
			function: &Function<'db>,
			interferences: &Set<(DeclarationId<'db>, DeclarationId<'db>)>,
			slots: &ArenaMap<Declaration<'db>, SlotId>,
		) -> Self {
			let mut pretty_interferences = interferences
				.iter()
				.filter_map(|(a, b)| {
					Some(InterferenceEdge(
						function.data[*a].name?.pretty_print(db),
						function.data[*b].name?.pretty_print(db),
					))
				})
				.collect::<Vec<_>>();
			pretty_interferences.sort();
			let pretty_slots = slots
				.iter()
				.filter_map(|(d, s)| Some((function.data[d].name?.pretty_print(db), *s)))
				.collect();
			Self {
				interferences: pretty_interferences,
				slots: pretty_slots,
			}
		}
	}

	#[derive(PartialEq, Eq, PartialOrd, Ord)]
	struct InterferenceEdge(String, String);
	impl std::fmt::Debug for InterferenceEdge {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			write!(f, "({}, {})", self.0, self.1)
		}
	}

	fn check(model: &str, expected: Expect) {
		let mut db = CompilerDatabase::default();
		let model_file = InlineModelFile::new(&db, model.to_owned(), InputLang::MiniZinc).into();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let model = final_thir(&db).unwrap();
		let mir = lower_from_thir(&db, &model);
		let main = &mir.functions[mir.main_entrypoint];
		let interferences = Liveness::new(main).analyse();
		let slots = SlotAllocator::new(main, &interferences).allocate_slots();
		for (a, b) in interferences.iter() {
			assert_ne!(
				slots[*a], slots[*b],
				"Interfering declarations should not share a slot"
			);
		}
		let actual = LivenessAndSlots::new(&db, main, &interferences, &slots);
		expected.assert_debug_eq(&actual);
	}

	#[test]
	fn test_liveness_1() {
		check(
			r#"
			function int: foo(int: x, int: y) = 1;
			int: p = foo(1, 2);
			int: q = foo(p, 3);
			int: r = foo(p, 4);
			int: s = foo(q, 5);
			int: t = foo(r, 6);
			"#,
			expect![[r#"
    LivenessAndSlots {
        interferences: [
            (p, q),
            (q, r),
            (r, s),
        ],
        slots: {
            "p": SlotId(1),
            "q": SlotId(0),
            "r": SlotId(1),
            "s": SlotId(0),
            "t": SlotId(0),
        },
    }
"#]],
		);
	}

	#[test]
	fn test_liveness_2() {
		check(
			r#"
			function int: foo(int: x, int: y) = 1;
			function bool: branch_condition() = true;
			int: p = 1;
			int: q = 2;
			int: x =
				if branch_condition() then
					let {
						int: a = 3;
						int: b = foo(p, 4);
						int: c = foo(b, a);
					} in c
				else
					let {
						int: d = 5;
						int: e = foo(q, 6);
						int: f = foo(e, d);
					} in f
				endif;
			int: r = foo(x, 7);
			"#,
			expect![[r#"
    LivenessAndSlots {
        interferences: [
            (a, b),
            (d, e),
            (p, a),
            (p, q),
            (q, d),
        ],
        slots: {
            "a": SlotId(1),
            "b": SlotId(0),
            "c": SlotId(0),
            "d": SlotId(0),
            "e": SlotId(1),
            "f": SlotId(0),
            "p": SlotId(0),
            "q": SlotId(2),
            "r": SlotId(0),
            "x": SlotId(0),
        },
    }
"#]],
		);
	}

	#[test]
	fn test_liveness_3() {
		check(
			r#"
			function int: foo(int: x, int: y) = 1;
			function bool: bar(int: i) = true;
			int: p = 1;
			array [int] of int: a = [let { int: r = foo(i, p) } in r | i in {1, 2, 3} where bar(i)];
			"#,
			expect![[r#"
    LivenessAndSlots {
        interferences: [
            (i, a),
            (p, a),
            (p, i),
            (p, r),
            (r, a),
        ],
        slots: {
            "a": SlotId(0),
            "i": SlotId(3),
            "p": SlotId(2),
            "r": SlotId(1),
        },
    }
"#]],
		);
	}

	#[test]
	fn test_liveness_multiple_comprehension_generators() {
		check(
			r#"
			function int: foo(int: x, int: y) = 1;
			array [int] of int: a = [let { int: r = foo(i, j); } in r | i in {1, 2}, j in {3, 4}];
			"#,
			expect![[r#"
    LivenessAndSlots {
        interferences: [
            (i, a),
            (i, j),
            (i, r),
            (j, a),
            (r, a),
        ],
        slots: {
            "a": SlotId(0),
            "i": SlotId(2),
            "j": SlotId(1),
            "r": SlotId(1),
        },
    }
"#]],
		);
	}

	#[test]
	fn test_liveness_comprehension_complex() {
		check(
			r#"
			function int: foo(int: x, int: y) = 1;
			test bar(int: b) = true;
			int: p = 1;
			array [int] of int: a = [
				let { int: r = 1 } in r
			|
				i in {1, 2},
				c = bar(i) where c,
				u = 1,
				v = u,
				w = foo(v, 3),
				j in {i, p, u},
				d = bar(j) where d,
			];
			"#,
			expect![[r#"
    LivenessAndSlots {
        interferences: [
            (c, a),
            (d, a),
            (i, a),
            (i, c),
            (i, u),
            (i, v),
            (i, w),
            (j, a),
            (p, a),
            (p, c),
            (p, d),
            (p, i),
            (p, j),
            (p, r),
            (p, u),
            (p, v),
            (p, w),
            (r, a),
            (u, a),
            (u, v),
            (u, w),
            (v, a),
            (w, a),
        ],
        slots: {
            "a": SlotId(0),
            "c": SlotId(1),
            "d": SlotId(1),
            "i": SlotId(4),
            "j": SlotId(1),
            "p": SlotId(2),
            "r": SlotId(1),
            "u": SlotId(3),
            "v": SlotId(1),
            "w": SlotId(1),
        },
    }
"#]],
		);
	}

	#[test]
	fn test_slot_allocation_for_bodyless_function() {
		let mut db = CompilerDatabase::default();
		let model_file = InlineModelFile::new(
			&db,
			r#"
				int: value = 1;
			"#
			.to_owned(),
			InputLang::MiniZinc,
		)
		.into();
		let _ = CompilerSettings::get(&db)
			.set_ignore_stdlib(&mut db)
			.to(true);
		let _ = InputFiles::get(&db).set_files(&mut db).to(vec![model_file]);
		let model = final_thir(&db).unwrap();
		let mir = lower_from_thir(&db, &model);
		let mut function = mir.functions[mir.main_entrypoint].clone();
		function.body = None;
		function.data = Default::default();

		let interferences = Liveness::new(&function).analyse();
		let slots = SlotAllocator::new(&function, &interferences).allocate_slots();

		assert!(interferences.is_empty());
		assert!(slots.is_empty());
	}
}
