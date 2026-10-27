//! Implementation of the stack
//!

use std::ops::{Index, IndexMut};

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
				SlotIndex(frame_offset + u32::from(i) as usize)
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
	/// Push a new call frame onto the call stack
	pub fn push_call_frame(
		&mut self,
		return_address: Address<'db>,
		return_slot: impl AsSlotIndex,
		slot_count: usize,
	) {
		let frame_offset = self.value_stack.len();
		let frame = CallFrame {
			return_address,
			return_slot: return_slot.as_slot_index(self),
		};
		self.call_stack.push(CallFrameEntry {
			frame_offset,
			inner: frame,
		});
		self.resize_call_frame(slot_count);
	}

	/// Resize the top call frame to have the given number of slots
	pub fn resize_call_frame(&mut self, slot_count: usize) {
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
}

impl<'db> Default for Stack<'db> {
	fn default() -> Self {
		Self {
			value_stack: vec![IntVal::Int(42).into()],
			call_stack: Vec::new(),
		}
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
pub struct CallFrame<'db> {
	/// The address to return to after the call
	pub return_address: Address<'db>,
	/// The slot to write the return value to after the call
	pub return_slot: SlotIndex,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CallFrameEntry<'db> {
	frame_offset: usize,
	inner: CallFrame<'db>,
}
