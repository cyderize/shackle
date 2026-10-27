//! Implementation of the stack
//!

use std::{
	fmt::Write,
	ops::{Index, IndexMut},
};

use shackle_hir::Db;

use crate::{Address, IntVal, Value};

/// The index of a slot in the stack
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct SlotIndex(usize);

impl SlotIndex {
	/// Return the (reserved) first slot index
	pub fn first() -> Self {
		Self(0)
	}
}

/// Trait for converting into a slot index
pub trait AsSlotIndex {
	/// Convert to a slot index
	fn as_slot_index<'db>(self, stack: &Stack<'db>) -> SlotIndex;
}

impl AsSlotIndex for SlotIndex {
	fn as_slot_index<'db>(self, _stack: &Stack<'db>) -> SlotIndex {
		self
	}
}

impl AsSlotIndex for shackle_ir::SlotId {
	fn as_slot_index<'db>(self, stack: &Stack<'db>) -> SlotIndex {
		match self {
			shackle_ir::SlotId::Absolute(a) => SlotIndex(u32::from(a) as usize),
			shackle_ir::SlotId::Relative(i) => {
				let frame_offset = stack.call_stack.last().map(|f| f.frame_offset).unwrap();
				SlotIndex(frame_offset + u32::from(i) as usize - 1)
			}
		}
	}
}

/// The stack of values and call frames
#[derive(Clone, Debug, PartialEq)]
pub struct Stack<'db> {
	/// The stack of values
	value_stack: Vec<Value>,
	/// The call stack of frames
	call_stack: Vec<CallFrameEntry<'db>>,
}

impl<'db> Stack<'db> {
	/// Create a new stack
	pub fn new(slot_count: usize) -> Self {
		let value = IntVal::Int(42).into();
		Self {
			value_stack: std::iter::repeat_n(value, slot_count + 1).collect(),
			call_stack: Vec::new(),
		}
	}

	/// Push a new call frame onto the call stack
	pub fn push_call_frame(&mut self, frame: CallFrame<'db, impl AsSlotIndex>, slot_count: usize) {
		let frame_offset = self.value_stack.len();
		let inner = CallFrame {
			return_address: frame.return_address,
			return_slot: frame.return_slot.as_slot_index(self),
			halt_on_return: frame.halt_on_return,
		};
		self.call_stack.push(CallFrameEntry {
			frame_offset,
			tail_call_count: 0,
			values_defined: Vec::new(),
			inner,
		});
		self.resize_call_frame(slot_count);
	}

	/// Resize the top call frame and increment the tail call count
	pub fn tail_call(&mut self, slot_count: usize) {
		self.resize_call_frame(slot_count);
		let frame = self.call_stack.last_mut().unwrap();
		frame.values_defined.clear();
		frame.tail_call_count += 1;
	}

	/// Resize the top call frame to have the given number of slots
	fn resize_call_frame(&mut self, slot_count: usize) {
		let frame_offset = self.call_stack.last().map(|f| f.frame_offset).unwrap();
		self.value_stack
			.resize(frame_offset + slot_count, IntVal::Int(42).into());
	}

	/// Pop the top call frame from the call stack
	pub fn pop_call_frame(&mut self) -> CallFrame<'db> {
		let frame = self.call_stack.pop().expect("Call stack is empty");
		self.value_stack
			.resize(frame.frame_offset, IntVal::Int(42).into());
		frame.inner
	}

	/// Get the number of call frames on the call stack
	pub fn call_frame_count(&self) -> usize {
		self.call_stack.len()
	}

	/// Add a value to the list of values defined in the current call frame
	pub fn add_value_defined(&mut self, value: Value) {
		self.call_stack
			.last_mut()
			.unwrap()
			.values_defined
			.push(value);
	}

	/// Get the values defined in the current call frame
	pub fn values_defined(&self) -> &[Value] {
		self.call_stack
			.last()
			.map(|f| f.values_defined.as_slice())
			.unwrap_or(&[])
	}

	/// Print the call stack to a string
	pub fn dump(&self, db: &'db dyn Db, program: &shackle_ir::Program<'db>) -> String {
		let mut s = String::new();
		for entry in self.call_stack.iter() {
			let origin = program.functions[entry.inner.return_address.function].origins
				[entry.inner.return_address.instruction];
			writeln!(&mut s, "{}", origin.pretty_print(db)).unwrap();
		}
		s
	}
}

impl<'db, T: AsSlotIndex> Index<T> for Stack<'db> {
	type Output = Value;

	fn index(&self, index: T) -> &Self::Output {
		&self.value_stack[index.as_slot_index(self).0]
	}
}

impl<'db, T: AsSlotIndex> IndexMut<T> for Stack<'db> {
	fn index_mut(&mut self, index: T) -> &mut Self::Output {
		let idx = index.as_slot_index(self);
		&mut self.value_stack[idx.0]
	}
}

/// Information about a call
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallFrame<'db, T = SlotIndex> {
	/// The address to return to after the call
	pub return_address: Address<'db>,
	/// The slot to write the return value to after the call
	pub return_slot: T,
	/// Whether to halt the interpreter when returning from this call
	pub halt_on_return: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CallFrameEntry<'db> {
	frame_offset: usize,
	tail_call_count: usize,
	values_defined: Vec<Value>,
	inner: CallFrame<'db, SlotIndex>,
}
