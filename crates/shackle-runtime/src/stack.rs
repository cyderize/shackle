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
	fn as_slot_index(self, stack: &Stack) -> SlotIndex;
}

impl AsSlotIndex for SlotIndex {
	fn as_slot_index(self, _stack: &Stack) -> SlotIndex {
		self
	}
}

impl AsSlotIndex for shackle_ir::SlotId {
	fn as_slot_index(self, stack: &Stack) -> SlotIndex {
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
pub struct Stack {
	/// The stack of values
	value_stack: Vec<Value>,
	/// The call stack of frames
	call_stack: Vec<CallFrameEntry>,
}

impl Stack {
	/// Create a new stack
	pub fn new(slot_count: usize) -> Self {
		let value = IntVal::Int(42).into();
		Self {
			value_stack: std::iter::repeat_n(value, slot_count + 1).collect(),
			call_stack: Vec::new(),
		}
	}

	/// Push a new call frame onto the call stack
	pub fn push_call_frame(&mut self, frame: CallFrame<impl AsSlotIndex>, slot_count: usize) {
		let frame_offset = self.value_stack.len();
		let inner = CallFrame {
			return_address: frame.return_address,
			return_slot: frame.return_slot.as_slot_index(self),
			output_slot: frame.output_slot,
			halt_on_return: frame.halt_on_return,
			annotations: frame.annotations,
			nanozinc_pending_count: frame.nanozinc_pending_count,
		};
		self.call_stack.push(CallFrameEntry {
			frame_offset,
			tail_call_count: 0,
			inner,
		});
		self.resize_call_frame(slot_count);
	}

	/// Resize the top call frame and increment the tail call count
	pub fn tail_call(
		&mut self,
		slot_count: usize,
		was_total: bool,
		clear_annotations: bool,
		annotations: impl IntoIterator<Item = Value>,
	) {
		self.resize_call_frame(slot_count);
		let frame = self.call_stack.last_mut().unwrap();
		frame.tail_call_count += 1;
		if was_total {
			frame.inner.nanozinc_pending_count += 1;
		}
		if clear_annotations {
			frame.inner.annotations.clear();
		}
		frame.inner.annotations.extend(annotations);
	}

	/// Resize the top call frame to have the given number of slots
	fn resize_call_frame(&mut self, slot_count: usize) {
		let frame_offset = self.call_stack.last().map(|f| f.frame_offset).unwrap();
		self.value_stack
			.resize(frame_offset + slot_count, IntVal::Int(42).into());
	}

	/// Pop the top call frame from the call stack
	pub fn pop_call_frame(&mut self) -> CallFrame {
		let frame = self.call_stack.pop().expect("Call stack is empty");
		self.value_stack
			.resize(frame.frame_offset, IntVal::Int(42).into());
		frame.inner
	}

	/// Get the number of call frames on the call stack
	pub fn call_frame_count(&self) -> usize {
		self.call_stack.len()
	}

	/// Get the annotations on the current callframe
	pub fn annotations(&self) -> &[Value] {
		self.call_stack
			.last()
			.map(|f| f.inner.annotations.as_slice())
			.unwrap_or(&[])
	}

	/// Collect source locations for the failing instruction and its callers.
	pub fn trace(
		&self,
		program: &shackle_ir::Program,
		current_address: Address,
	) -> Vec<crate::error::RuntimeLocation> {
		let location = |address: Address| {
			let origin = &program.functions[address.function].origins[address.instruction];
			crate::error::RuntimeLocation {
				file: program.strings[origin.file].clone(),
				span: origin.span,
			}
		};
		let mut trace = vec![location(current_address)];
		for entry in self.call_stack.iter().rev() {
			if entry.inner.halt_on_return {
				continue;
			}
			let mut address = entry.inner.return_address;
			address.instruction = address.instruction.prev().unwrap_or(address.instruction);
			trace.push(location(address));
		}
		trace
	}
}

impl<T: AsSlotIndex> Index<T> for Stack {
	type Output = Value;

	fn index(&self, index: T) -> &Self::Output {
		&self.value_stack[index.as_slot_index(self).0]
	}
}

impl<T: AsSlotIndex> IndexMut<T> for Stack {
	fn index_mut(&mut self, index: T) -> &mut Self::Output {
		let idx = index.as_slot_index(self);
		&mut self.value_stack[idx.0]
	}
}

/// Information about a call
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CallFrame<T = SlotIndex> {
	/// The address to return to after the call
	pub return_address: Address,
	/// The slot to write the return value to after the call
	pub return_slot: T,
	/// Whether to mark the return as output
	pub output_slot: bool,
	/// Whether to halt the interpreter when returning from this call
	pub halt_on_return: bool,
	/// Annotations on the call
	pub annotations: Vec<Value>,
	/// Number of pending NanoZinc frames pushed
	pub nanozinc_pending_count: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct CallFrameEntry {
	frame_offset: usize,
	tail_call_count: usize,
	inner: CallFrame<SlotIndex>,
}
