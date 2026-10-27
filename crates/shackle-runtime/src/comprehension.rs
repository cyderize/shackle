//! State for evaluating comprehensions

use std::ops::RangeInclusive;

use rangelist::IntervalIterator;

use crate::{DataView, FloatVal, IntVal, Value};

/// Stores info about currently evaluated comprehensions

#[derive(Debug, Default)]
pub struct ComprehensionState {
	generators: Vec<GeneratorState>,
	comprehension_values: Vec<Value>,
	comprehension_start_indices: Vec<usize>,
}

impl ComprehensionState {
	/// Begin a comprehension
	pub fn start_comprehension(&mut self) {
		self.comprehension_start_indices
			.push(self.comprehension_values.len());
	}

	/// Append a value to the current comprehension
	pub fn append_comprehension_value(&mut self, value: Value) {
		self.comprehension_values.push(value);
	}

	/// Finish a comprehension and yield the container value
	pub fn end_comprehension(&mut self) -> Value {
		let idx = self.comprehension_start_indices.pop().unwrap();
		Value::from_iter(self.comprehension_values.drain(idx..))
	}

	/// Start a generator which iterates over the value
	pub fn start_generator(&mut self, value: Value) {
		let generator = match value.clone().deref() {
			DataView::Seq(seq) => {
				let indices = Box::new(seq.indices());
				GeneratorState::Seq { value, indices }
			}
			DataView::IntSet(set) => {
				assert!(set.is_finite(), "unable to iterate over an infinite set");
				let intervals = set
					.intervals()
					.map(|interval| {
						let (IntVal::Int(start), IntVal::Int(end)) =
							(*interval.start(), *interval.end())
						else {
							unreachable!()
						};
						start..=end
					})
					.collect::<Vec<_>>()
					.into_iter();
				GeneratorState::IntSet {
					intervals,
					current: None,
				}
			}
			DataView::FloatSet(set) => GeneratorState::FloatSet {
				values: set.values().collect::<Vec<_>>().into_iter(),
			},
			_ => unreachable!(),
		};
		self.generators.push(generator);
	}

	/// Get the next value from the current generator, or None if the generator is exhausted
	pub fn next_generator_value(&mut self) -> Option<Value> {
		if let Some(value) = self.generators.last_mut().unwrap().next() {
			Some(value)
		} else {
			let _ = self.generators.pop();
			None
		}
	}
}

/// The state of a generator, which is an iterator over values
pub enum GeneratorState {
	/// Sequence iterator
	Seq {
		/// The value being iterated over
		value: Value,
		/// The indices to iterate with
		indices: Box<dyn Iterator<Item = Vec<i64>>>,
	},
	/// Integer set iterator
	IntSet {
		/// The intervals to iterate over
		intervals: std::vec::IntoIter<RangeInclusive<i64>>,
		/// The current range being iterated over
		current: Option<RangeInclusive<i64>>,
	},
	/// Float set iterator
	FloatSet {
		/// The values to iterate over
		values: std::vec::IntoIter<FloatVal>,
	},
}

impl Iterator for GeneratorState {
	type Item = Value;

	fn next(&mut self) -> Option<Self::Item> {
		match self {
			GeneratorState::Seq { value, indices } => {
				let index = indices.next()?;
				let DataView::Seq(seq) = value.deref() else {
					unreachable!()
				};
				Some(seq[&index].clone())
			}
			GeneratorState::IntSet { intervals, current } => loop {
				if let Some(value) = current.as_mut().and_then(Iterator::next) {
					return Some(value.into());
				}
				*current = intervals.next();
				if current.is_none() {
					return None;
				}
			},
			GeneratorState::FloatSet { values } => values.next().map(Value::from),
		}
	}
}

impl std::fmt::Debug for GeneratorState {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_struct("GeneratorState").finish()
	}
}

#[cfg(test)]
mod tests {
	use super::GeneratorState;

	#[test]
	fn int_set_generator_advances_through_intervals() {
		let generator = GeneratorState::IntSet {
			intervals: vec![1..=3, 5..=6].into_iter(),
			current: None,
		};
		assert_eq!(
			generator.collect::<Vec<_>>(),
			vec![1.into(), 2.into(), 3.into(), 5.into(), 6.into()]
		);
	}
}
