#![allow(unsafe_op_in_unsafe_fn, reason = "occurs due to varlen derive macro")]

//! Value representation for the Shackle runtime.
mod bool_var;
mod float_var;
mod int_set_var;
mod int_var;
mod num;
mod seq;
mod set;

use std::{
	cmp::Ordering,
	fmt::Debug,
	hash::{Hash, Hasher},
	mem::MaybeUninit,
	num::{NonZeroU32, NonZeroU64},
	pin::Pin,
	ptr::NonNull,
	str::{from_utf8, from_utf8_unchecked},
	sync::{
		Mutex,
		atomic::{
			self, AtomicU8, AtomicU16,
			Ordering::{Acquire, Relaxed, Release},
		},
	},
};

use bilge::{
	Bitsized, TryFromBits, bitsize,
	prelude::{Integer, u2, u61},
};
use once_cell::sync::Lazy;
use seq::{InnerViewType, ViewType};
use varlen::{
	Initializer, Layout, VarLen, define_varlen,
	prelude::{ArrayInitializer, FromIterPrefix},
};

use crate::value::seq::{Pairs, SeqViewInner};
pub use crate::value::{
	bool_var::{BoolVar, BoolVarRef},
	float_var::{FloatVar, FloatVarRef},
	int_set_var::{IntSetVar, IntSetVarRef},
	int_var::{IntVar, IntVarRef},
	num::{FLOAT_INF_NEG, FLOAT_INF_POS, FloatVal, INT_INF_NEG, INT_INF_POS, IntVal},
	seq::{EMPTY_SEQ, SeqView},
	set::{
		FLOAT_SET_EMPTY, FLOAT_SET_INF, FloatSetView, INT_SET_EMPTY, INT_SET_INF, IntSetView, Set,
	},
};

#[bitsize(2)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, TryFromBits)]
enum RefType {
	Boxed = 0b00,
	Float = 0b1,
	Int = 0b10,
}

/// A value which is computed by the interpreter.
pub struct Value {
	raw: NonZeroU64,
}

impl Clone for Value {
	fn clone(&self) -> Self {
		if matches!(self.ref_ty(), RefType::Boxed) {
			let slf = self.get_pin();
			debug_assert!(slf.refs().ref_count.load(Relaxed) > 0);
			// Using a relaxed ordering is alright here, as knowledge of the
			// original reference prevents other threads from erroneously deleting
			// the object.
			//
			// As explained in the [Boost documentation][1], Increasing the
			// reference counter can always be done with memory_order_relaxed: New
			// references to an object can only be formed from an existing
			// reference, and passing an existing reference from one thread to
			// another must already provide any required synchronization.
			//
			// [1]: (www.boost.org/doc/libs/1_55_0/doc/html/atomic/usage_examples.html)
			let _ = increment_strong_count(&slf.refs().ref_count);
		}
		Self { raw: self.raw }
	}
}

impl Drop for Value {
	fn drop(&mut self) {
		if matches!(self.ref_ty(), RefType::Boxed) {
			unsafe { ValueStorage::release_strong(self.get_pin()) };
		}
	}
}

impl PartialEq for Value {
	fn eq(&self, other: &Self) -> bool {
		// Check whether bitwise identical
		if self.raw == other.raw {
			return true;
		}
		// Interned or boxed data would have been bitwise identical
		if !matches!(self.ref_ty(), RefType::Boxed)
			|| matches!(self.get_pin().ty, ValType::Int | ValType::Float)
		{
			// TODO: is this okay for Float??
			return false;
		}
		// Compare
		self.deref().eq(&other.deref())
	}
}
impl Eq for Value {}

impl Hash for Value {
	fn hash<H: Hasher>(&self, state: &mut H) {
		use DataView::*;

		match self.deref() {
			// Scalar equality is based on the tagged representation rather than
			// the decoded value, so preserve that distinction when hashing.
			Int(_) | Float(_) => self.raw.hash(state),
			Seq(value) => {
				2_u8.hash(state);
				value.hash(state);
			}
			Set(value) => {
				3_u8.hash(state);
				value.hash(state);
			}
			Str(value) => {
				4_u8.hash(state);
				value.hash(state);
			}
			IntSet(value) => {
				5_u8.hash(state);
				value.hash(state);
			}
			FloatSet(value) => {
				6_u8.hash(state);
				value.hash(state);
			}
			Annotation(value) => {
				7_u8.hash(state);
				value.hash(state);
			}
			BoolVar(value) => {
				8_u8.hash(state);
				value.ident().hash(state);
			}
			IntVar(value) => {
				9_u8.hash(state);
				value.ident().hash(state);
			}
			FloatVar(value) => {
				10_u8.hash(state);
				value.ident().hash(state);
			}
			IntSetVar(value) => {
				11_u8.hash(state);
				value.ident().hash(state);
			}
		}
	}
}

impl PartialOrd for Value {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(self.cmp(other))
	}
}

impl Ord for Value {
	fn cmp(&self, other: &Self) -> Ordering {
		use DataView::*;

		match (self.deref(), other.deref()) {
			(Int(lhs), Int(rhs)) => lhs.cmp(&rhs).then_with(|| self.raw.cmp(&other.raw)),
			(Float(lhs), Float(rhs)) => lhs.cmp(&rhs).then_with(|| self.raw.cmp(&other.raw)),
			(Seq(lhs), Seq(rhs)) => lhs.cmp(&rhs),
			(Set(lhs), Set(rhs)) => lhs.cmp(&rhs),
			(Str(lhs), Str(rhs)) => lhs.cmp(rhs),
			(IntSet(lhs), IntSet(rhs)) => lhs.cmp(&rhs),
			(FloatSet(lhs), FloatSet(rhs)) => lhs.cmp(&rhs),
			(Annotation(lhs), Annotation(rhs)) => lhs
				.ident
				.cmp(&rhs.ident)
				.then_with(|| lhs.arguments.cmp(rhs.arguments)),
			(BoolVar(lhs), BoolVar(rhs)) => lhs.ident().cmp(&rhs.ident()),
			(IntVar(lhs), IntVar(rhs)) => lhs.ident().cmp(&rhs.ident()),
			(FloatVar(lhs), FloatVar(rhs)) => lhs.ident().cmp(&rhs.ident()),
			(IntSetVar(lhs), IntSetVar(rhs)) => lhs.ident().cmp(&rhs.ident()),
			(lhs, rhs) => data_view_discriminant(&lhs).cmp(&data_view_discriminant(&rhs)),
		}
	}
}

fn data_view_discriminant(value: &DataView<'_>) -> u8 {
	match value {
		DataView::Int(_) => 0,
		DataView::Float(_) => 1,
		DataView::Seq(_) => 2,
		DataView::Set(_) => 3,
		DataView::Str(_) => 4,
		DataView::IntSet(_) => 5,
		DataView::FloatSet(_) => 6,
		DataView::Annotation(_) => 7,
		DataView::BoolVar(_) => 8,
		DataView::IntVar(_) => 9,
		DataView::FloatVar(_) => 10,
		DataView::IntSetVar(_) => 11,
	}
}

impl Debug for Value {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		self.deref().fmt(f)
	}
}

#[inline(never)]
#[cold]
fn allocation_overflow() -> ! {
	panic!("Allocation size overflow")
}

impl Value {
	const FLOAT_SIGN_BIT: u64 = 0b1 << 63;
	const FLOAT_TAG: u64 = 0b1;
	const INT_SIGN_BIT: u64 = 0b100;
	const INT_TAG: u64 = 0b10;
	/// Max integer value
	pub const MAX_INT: i64 = <u61 as Integer>::MAX.value() as i64;
	/// Min integer value
	pub const MIN_INT: i64 = -(Self::MAX_INT + 1);

	/// Determine the type of the reference based on bit-tags
	fn ref_ty(&self) -> RefType {
		// Note that the order in which the tags is checked is imporant. INT_TAG can
		// trigger on a unboxed float
		if self.raw.get() & Self::FLOAT_TAG == Self::FLOAT_TAG {
			RefType::Float // Unboxed Float
		} else if self.raw.get() & Self::INT_TAG == Self::INT_TAG {
			RefType::Int // Unboxed Int
		} else {
			RefType::Boxed // Boxed Value
		}
	}

	/// Get the value as a [DataView].
	pub fn deref(&self) -> DataView<'_> {
		match self.ref_ty() {
			RefType::Float => {
				let pos = (self.raw.get() & Value::FLOAT_SIGN_BIT) == 0;
				let value = self.raw.get() >> 1;
				const EXPONENT_MASK: u64 = 0x3FF << 52;
				let mut exponent = (value & EXPONENT_MASK) >> 52;
				if exponent != 0 {
					exponent += 512; // reconstruct original bias of 1023
				}
				const FRACTION_MASK: u64 = 0xFFFFFFFFFFFFF;
				let fraction = value & FRACTION_MASK;
				let mut value = fraction | (exponent << 52);
				if !pos {
					value |= Value::FLOAT_SIGN_BIT;
				}
				DataView::Float(f64::from_bits(value).into())
			}
			RefType::Int => {
				let pos = (self.raw.get() & Value::INT_SIGN_BIT) == 0;
				let val = (self.raw.get() >> 3) as i64;
				let val = if pos {
					val
				} else if val == 0 {
					Value::MIN_INT
				} else {
					-val
				};
				debug_assert!((Value::MIN_INT..=Value::MAX_INT).contains(&val));
				DataView::Int(IntVal::Int(val))
			}
			RefType::Boxed => unsafe {
				let v = &*self.get_ptr();
				match v.ty {
					ValType::Int => {
						let inf = v.refs().bytes[0] != 0;
						let val = v.refs().ints[0];
						DataView::Int(if inf {
							if val >= 0 {
								IntVal::InfPos
							} else {
								IntVal::InfNeg
							}
						} else {
							IntVal::Int(val)
						})
					}
					ValType::Float => {
						let val = v.refs().floats[0];
						DataView::Float(val)
					}
					ValType::Seq => DataView::Seq(SeqViewInner::Direct(v.refs().values).into()),
					ValType::Set => DataView::Set(Set {
						values: v.refs().values,
					}),
					ValType::Str => DataView::Str(from_utf8_unchecked(v.refs().bytes)),
					ValType::IntSet => DataView::IntSet(IntSetView {
						has_lb: v.len & 0b01 != 0,
						has_ub: v.len & 0b10 != 0,
						intervals: v.refs().ints,
					}),
					ValType::FloatSet => DataView::FloatSet(FloatSetView {
						intervals: v.refs().floats,
					}),
					ValType::Annotation => DataView::Annotation(AnnotationView {
						ident: v.refs().idents[0],
						arguments: v.refs().values,
					}),
					ValType::View => {
						let vty = ViewType::from_len(v.len);
						DataView::Seq(match vty.ty() {
							InnerViewType::Dim => SeqViewInner::WithDim {
								dims: Pairs::new(v.refs().ints),
								storage: &v.refs().values[0],
							}
							.into(),
							InnerViewType::Slice => {
								let slice_offset = vty.slice() as usize;
								SeqViewInner::Slice {
									dims: Pairs::new(&v.refs().ints[..slice_offset]),
									slice: Pairs::new(&v.refs().ints[slice_offset..]),
									storage: &v.refs().values[0],
								}
								.into()
							}
							InnerViewType::Transpose => SeqViewInner::Transposed {
								reloc: v.refs().ints,
								storage: &v.refs().values[0],
							}
							.into(),
							InnerViewType::Compact => SeqViewInner::Compressed {
								dims: Pairs::new(&v.refs().ints[1..]),
								repeat: v.refs().ints[0],
								storage: &v.refs().values[0],
							}
							.into(),
						})
					}
					ValType::BoolVar => {
						DataView::BoolVar(v.refs().bool_var[0].lock().unwrap().into())
					}
					ValType::IntVar => DataView::IntVar(v.refs().int_var[0].lock().unwrap().into()),
					ValType::FloatVar => {
						DataView::FloatVar(v.refs().float_var[0].lock().unwrap().into())
					}
					ValType::IntSetVar => {
						DataView::IntSetVar(v.refs().int_set_var[0].lock().unwrap().into())
					}
				}
			},
		}
	}

	/// Check whether the value is equal to a constant value.
	pub fn is_constant(&self, c: &'static Value) -> bool {
		self.raw == c.raw
	}

	fn new_box(init: impl Initializer<ValueStorage>) -> Value {
		// Note: This code is inspired by the initialisation code of varlen::VBox::new
		let layout = init
			.calculate_layout_cautious()
			.unwrap_or_else(|| allocation_overflow());
		let alloc_layout = std::alloc::Layout::from_size_align(layout.size(), ValueStorage::ALIGN)
			.unwrap_or_else(|_| allocation_overflow());
		unsafe {
			let p = std::alloc::alloc(alloc_layout) as *mut ValueStorage;
			let layout_size = layout.size();
			init.initialize(NonNull::new_unchecked(p), layout);
			// Retain one implicit weak reference for the lifetime of the strong
			// references. This prevents a weak drop from racing the last strong
			// drop to free the allocation.
			(*p).weak_count.store(1, Relaxed);
			debug_assert_eq!((*p).calculate_layout().size(), layout_size);
			let p = p as u64;
			debug_assert!(p & (0b11 << 62) == 0 && p != 0);
			Value {
				raw: NonZeroU64::new_unchecked(p << 2),
			}
		}
	}

	/// Create a string value
	pub fn new_str<I: ExactSizeIterator<Item = u8>, S: IntoIterator<IntoIter = I>>(s: S) -> Value {
		let s = s.into_iter();
		if s.len() == 0 {
			return EMPTY_STRING.clone();
		}
		let v = Self::new_box(value_storage::Init {
			ty: ValType::Str,
			len: s.len() as u32,
			ref_count: 1.into(),
			weak_count: 0.into(),
			values: InitEmpty,
			idents: InitEmpty,
			ints: InitEmpty,
			floats: InitEmpty,
			bool_var: InitEmpty,
			int_var: InitEmpty,
			float_var: InitEmpty,
			int_set_var: InitEmpty,
			bytes: FromIterPrefix(s),
		});
		debug_assert!(from_utf8(v.get_pin().refs().bytes).is_ok());
		v
	}

	/// Construct an annotation with the given identifier and arguments.
	pub fn new_annotation<I: ExactSizeIterator<Item = Value>, S: IntoIterator<IntoIter = I>>(
		ident: NonZeroU32,
		arguments: S,
	) -> Value {
		let arguments = arguments.into_iter();
		Self::new_box(value_storage::Init {
			ty: ValType::Annotation,
			len: arguments.len() as u32,
			ref_count: 1.into(),
			weak_count: 0.into(),
			values: FromIterPrefix(arguments),
			idents: FromIterPrefix([ident].into_iter()),
			ints: InitEmpty,
			floats: InitEmpty,
			bool_var: InitEmpty,
			int_var: InitEmpty,
			float_var: InitEmpty,
			int_set_var: InitEmpty,
			bytes: InitEmpty,
		})
	}

	/// Downgrade to a weak reference. The value must be boxed.
	pub fn downgrade(&self) -> WeakValue {
		assert_eq!(self.ref_ty(), RefType::Boxed);
		let slf = self.get_pin();
		increment_weak_count(&slf.refs().weak_count);
		WeakValue { raw: self.raw }
	}

	unsafe fn get_ptr(&self) -> *mut ValueStorage {
		debug_assert_eq!(self.ref_ty(), RefType::Boxed);
		(self.raw.get() >> 2) as *mut ValueStorage
	}

	fn get_pin(&self) -> Pin<&mut ValueStorage> {
		assert_eq!(self.ref_ty(), RefType::Boxed);
		unsafe { Pin::new_unchecked(&mut (*self.get_ptr())) }
	}

	/// Get value as a string
	pub fn as_str(&self) -> &str {
		let DataView::Str(s) = self.deref() else {
			panic!("expected value to be a string literal")
		};
		s
	}
}

/// The empty string value
pub static EMPTY_STRING: Lazy<Value> = Lazy::new(|| {
	Value::new_box(value_storage::Init {
		ty: ValType::Str,
		len: 0u32,
		ref_count: 1.into(),
		weak_count: 0.into(),
		values: InitEmpty,
		idents: InitEmpty,
		ints: InitEmpty,
		floats: InitEmpty,
		bool_var: InitEmpty,
		int_var: InitEmpty,
		float_var: InitEmpty,
		int_set_var: InitEmpty,
		bytes: InitEmpty,
	})
});

impl From<&str> for Value {
	fn from(value: &str) -> Self {
		Self::new_str(value.as_bytes().iter().copied())
	}
}
impl<'a> TryInto<&'a str> for &'a Value {
	type Error = ();

	fn try_into(self) -> Result<&'a str, Self::Error> {
		if let DataView::Str(s) = self.deref() {
			Ok(s)
		} else {
			todo!()
		}
	}
}

/// Used to access the data in a [Value]
#[derive(Debug, PartialEq)]
pub enum DataView<'a> {
	/// Integer value, which can be either a boxed integer or an unboxed integer.
	Int(IntVal),
	/// Floating point value, which can be either a boxed float or an unboxed float.
	Float(FloatVal),
	/// Sequence of values (tuple or array)
	Seq(SeqView<'a>),
	/// Finite set of non-range values.
	Set(Set<'a>),
	/// Character string
	Str(&'a str),
	/// Integer set
	IntSet(IntSetView<'a>),
	/// Floating point set
	FloatSet(FloatSetView<'a>),
	/// Annotation
	Annotation(AnnotationView<'a>),
	/// Boolean variable
	BoolVar(BoolVarRef<'a>),
	/// Integer variable
	IntVar(IntVarRef<'a>),
	/// Float variable
	FloatVar(FloatVarRef<'a>),
	/// Int set variable
	IntSetVar(IntSetVarRef<'a>),
}

/// A borrowed view of an annotation value.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AnnotationView<'a> {
	/// The annotation identifier.
	pub ident: NonZeroU32,
	/// The annotation arguments.
	pub arguments: &'a [Value],
}

macro_rules! define_var_ref {
	($v:ident, $r:ident) => {
		/// A reference to a variable
		#[derive(Debug)]
		pub struct $r<'a> {
			guard: std::sync::MutexGuard<'a, $v>,
		}

		impl PartialEq for $r<'_> {
			fn eq(&self, other: &Self) -> bool {
				*self.guard == *other.guard
			}
		}
		impl Eq for $r<'_> {}
		impl std::ops::Deref for $r<'_> {
			type Target = $v;

			fn deref(&self) -> &Self::Target {
				&self.guard
			}
		}
		impl std::ops::DerefMut for $r<'_> {
			fn deref_mut(&mut self) -> &mut Self::Target {
				&mut self.guard
			}
		}
		impl<'a> From<std::sync::MutexGuard<'a, $v>> for $r<'a> {
			fn from(guard: std::sync::MutexGuard<'a, $v>) -> Self {
				Self { guard }
			}
		}
	};
}
pub(crate) use define_var_ref;

#[bitsize(8)]
#[derive(Clone, PartialEq, Eq, TryFromBits)]
enum ValType {
	/// Boxed integers (cannot fit in 62 bits)
	Int,
	/// Boxed floats (cannot fit in 63 bits)
	Float,
	/// Types that can be represented as simple sequences of values: Tuples, 1-to-n arrays
	Seq,
	/// A finite set of non-range values.
	Set,
	/// An annotation identifier and its arguments
	Annotation,
	/// Character strings
	Str,
	/// View into an array
	View,
	/// Int Set (Range List)
	IntSet,
	/// Float Set (Range List)
	FloatSet,
	/// Boolean decision variable
	BoolVar,
	/// Integer decision variable
	IntVar,
	/// Floating point decision variable
	FloatVar,
	/// Set of integers decision variable
	IntSetVar,
}

#[allow(dead_code)] // attributes accessed through [`.refs()`]
#[define_varlen]
struct ValueStorage {
	/// Type of the value
	#[controls_layout]
	ty: ValType,
	/// Length of the value (if relevant)
	/// Seq -> number of values
	/// Str -> number of bytes
	/// IntSet -> lowest two bit whether value has lb/ub, rest is number of gaps
	/// FloatSet -> Number of intervals
	#[controls_layout]
	len: u32,
	// Number of values referencing this value
	ref_count: AtomicU16,
	// Number of weak references, including one implicit reference while there
	// are any strong references.
	weak_count: AtomicU8,
	#[varlen_array]
	idents: [NonZeroU32; match *ty {
		ValType::Annotation => 1,
		_ => 0,
	}],

	#[varlen_array]
	values: [Value; match *ty {
		ValType::Seq | ValType::Set | ValType::Annotation => *len as usize,
		ValType::View => 1,
		_ => 0,
	}],
	#[varlen_array]
	ints: [i64; match *ty {
		ValType::Int => 1,
		ValType::IntSet => ValueStorage::int_set_len(*len),
		ValType::View => ViewType::from_len(*len).int_len(),
		_ => 0,
	}],
	#[varlen_array]
	floats: [FloatVal; match *ty {
		ValType::Float => 1,
		ValType::FloatSet => *len as usize * 2,
		_ => 0,
	}],
	#[varlen_array]
	bool_var: [Mutex<BoolVar>; match *ty {
		ValType::BoolVar => 1,
		_ => 0,
	}],
	#[varlen_array]
	int_var: [Mutex<IntVar>; match *ty {
		ValType::IntVar => 1,
		_ => 0,
	}],
	#[varlen_array]
	float_var: [Mutex<FloatVar>; match *ty {
		ValType::FloatVar => 1,
		_ => 0,
	}],
	#[varlen_array]
	int_set_var: [Mutex<IntSetVar>; match *ty {
		ValType::IntSetVar => 1,
		_ => 0,
	}],
	#[varlen_array]
	bytes: [u8; match *ty {
		// Fully stored as bytes
		ValType::Str => *len as usize,
		// Stored infinity tag
		ValType::Int => 1,
		_ => 0,
	}],
}

impl ValueStorage {
	const fn int_set_len(len: u32) -> usize {
		let has_lb = (len & 0b01 != 0) as usize;
		let has_ub = (len & 0b10 != 0) as usize;
		((len >> 2) as usize * 2) + has_lb + has_ub
	}

	unsafe fn deinit(slf: Pin<&mut ValueStorage>) {
		match slf.ty {
			ValType::Seq | ValType::Set | ValType::Annotation => {
				// Replace all contained values with non-reference counted objects.
				for v in slf.muts().values {
					*v = 0.into();
				}
			}
			ValType::View => slf.muts().values[0] = 0.into(),
			ValType::IntVar => slf.muts().int_var[0].lock().unwrap().clear_domain(),
			ValType::FloatVar => slf.muts().float_var[0].lock().unwrap().clear_domain(),
			ValType::IntSetVar => slf.muts().int_set_var[0].lock().unwrap().clear_domain(),
			_ => {}
		}
	}

	unsafe fn drop(slf: Pin<&mut ValueStorage>) {
		let ptr: *mut ValueStorage = unsafe { Pin::get_unchecked_mut(slf) };
		let slf = unsafe { Pin::new_unchecked(&mut *ptr) };
		let layout = ValueStorage::calculate_layout(&slf);
		let alloc_layout = unsafe {
			std::alloc::Layout::from_size_align_unchecked(layout.size(), ValueStorage::ALIGN)
		};
		unsafe {
			ValueStorage::vdrop(slf, layout);
			std::alloc::dealloc(ptr as *mut u8, alloc_layout);
		}
	}

	unsafe fn release_weak(mut slf: Pin<&mut ValueStorage>) {
		let muts = slf.as_mut().muts();
		if muts.weak_count.fetch_sub(1, Release) == 1 {
			atomic::fence(Acquire);
			let needs_deinit = match muts.ty {
				ValType::BoolVar => muts.bool_var[0].lock().unwrap().binding_domain(),
				ValType::IntVar => muts.int_var[0].lock().unwrap().binding_domain(),
				ValType::FloatVar => muts.float_var[0].lock().unwrap().binding_domain(),
				ValType::IntSetVar => muts.int_set_var[0].lock().unwrap().binding_domain(),
				_ => false,
			};
			unsafe {
				if needs_deinit {
					ValueStorage::deinit(slf.as_mut());
				}
				ValueStorage::drop(slf)
			}
		}
	}

	unsafe fn release_strong(mut slf: Pin<&mut ValueStorage>) {
		debug_assert!(slf.refs().ref_count.load(Relaxed) > 0);
		let muts = slf.as_mut().muts();
		if muts.ref_count.fetch_sub(1, Release) == 1 {
			atomic::fence(Acquire);
			match muts.ty {
				ValType::BoolVar if muts.bool_var[0].lock().unwrap().binding_domain() => {}
				ValType::IntVar if muts.int_var[0].lock().unwrap().binding_domain() => {}
				ValType::FloatVar if muts.float_var[0].lock().unwrap().binding_domain() => {}
				ValType::IntSetVar if muts.int_set_var[0].lock().unwrap().binding_domain() => {}
				_ => {
					unsafe {
						ValueStorage::deinit(slf.as_mut());
					};
				}
			}

			unsafe {
				// Release implicit weak reference
				ValueStorage::release_weak(slf);
			}
		}
	}
}

struct InitEmpty;
unsafe impl<T> ArrayInitializer<T> for InitEmpty {
	fn initialize(self, dst: &mut [MaybeUninit<T>]) {
		assert_eq!(dst.len(), 0);
	}
}

fn increment_strong_count(counter: &AtomicU16) -> u16 {
	counter
		.fetch_update(Relaxed, Relaxed, |count| count.checked_add(1))
		.expect("strong reference count overflow")
}

fn increment_weak_count(counter: &AtomicU8) {
	let _ = counter
		.fetch_update(Relaxed, Relaxed, |count| count.checked_add(1))
		.expect("weak reference count overflow");
}

fn increment_strong_count_if_nonzero(counter: &AtomicU16) -> bool {
	let mut count = counter.load(Relaxed);
	loop {
		if count == 0 {
			return false;
		}
		let next = count
			.checked_add(1)
			.expect("strong reference count overflow");
		match counter.compare_exchange_weak(count, next, Acquire, Relaxed) {
			Ok(_) => return true,
			Err(actual) => count = actual,
		}
	}
}

/// Weak reference to a boxed value
pub struct WeakValue {
	raw: NonZeroU64,
}

impl WeakValue {
	/// Check whether the value is still alive.
	pub fn is_alive(&self) -> bool {
		let slf = self.get_pin();
		match slf.refs().ty {
			ValType::BoolVar if slf.refs().bool_var[0].lock().unwrap().binding_domain() => true,
			ValType::IntVar if slf.refs().int_var[0].lock().unwrap().binding_domain() => true,
			ValType::FloatVar if slf.refs().float_var[0].lock().unwrap().binding_domain() => true,
			ValType::IntSetVar if slf.refs().int_set_var[0].lock().unwrap().binding_domain() => {
				true
			}
			_ => slf.refs().ref_count.load(Relaxed) > 0,
		}
	}

	/// Upgrade to a strong reference, if the value is still alive.
	pub fn upgrade(&self) -> Option<Value> {
		let slf = self.get_pin();

		match slf.refs().ty {
			ValType::BoolVar => {
				let var = slf.refs().bool_var[0].lock().unwrap();
				if var.binding_domain() {
					if increment_strong_count(&slf.refs().ref_count) == 0 {
						increment_weak_count(&slf.refs().weak_count);
					}
					return Some(Value { raw: self.raw });
				}
			}
			ValType::IntVar => {
				let var = slf.refs().int_var[0].lock().unwrap();
				if var.binding_domain() {
					if increment_strong_count(&slf.refs().ref_count) == 0 {
						increment_weak_count(&slf.refs().weak_count);
					}
					return Some(Value { raw: self.raw });
				}
			}
			ValType::FloatVar => {
				let var = slf.refs().float_var[0].lock().unwrap();
				if var.binding_domain() {
					if increment_strong_count(&slf.refs().ref_count) == 0 {
						increment_weak_count(&slf.refs().weak_count);
					}
					return Some(Value { raw: self.raw });
				}
			}
			ValType::IntSetVar => {
				let var = slf.refs().int_set_var[0].lock().unwrap();
				if var.binding_domain() {
					if increment_strong_count(&slf.refs().ref_count) == 0 {
						increment_weak_count(&slf.refs().weak_count);
					}
					return Some(Value { raw: self.raw });
				}
			}
			_ => {}
		}

		if increment_strong_count_if_nonzero(&slf.refs().ref_count) {
			Some(Value { raw: self.raw })
		} else {
			None
		}
	}

	unsafe fn get_ptr(&self) -> *mut ValueStorage {
		(self.raw.get() >> 2) as *mut ValueStorage
	}

	fn get_pin(&self) -> Pin<&mut ValueStorage> {
		unsafe { Pin::new_unchecked(&mut (*self.get_ptr())) }
	}
}

impl PartialEq for WeakValue {
	fn eq(&self, other: &Self) -> bool {
		self.raw == other.raw
	}
}

impl Clone for WeakValue {
	fn clone(&self) -> Self {
		let slf = self.get_pin();
		debug_assert!(slf.refs().weak_count.load(Relaxed) > 0);
		increment_weak_count(&slf.refs().weak_count);
		Self { raw: self.raw }
	}
}

impl Drop for WeakValue {
	fn drop(&mut self) {
		let slf = self.get_pin();
		unsafe { ValueStorage::release_weak(slf) };
	}
}

impl Debug for WeakValue {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.debug_tuple("WeakValue").field(&self.upgrade()).finish()
	}
}

#[cfg(test)]
mod tests {
	use std::{
		collections::hash_map::DefaultHasher,
		hash::{Hash, Hasher},
	};

	use super::*;

	fn hash(value: &Value) -> u64 {
		let mut hasher = DefaultHasher::new();
		value.hash(&mut hasher);
		hasher.finish()
	}

	#[test]
	fn memory_guarantees() {
		assert_eq!(size_of::<Value>(), 8);
		assert_eq!(ValueStorage::ALIGN, 8);

		const BOX_BASE_BYTES: usize = size_of::<u64>();

		const S: &str = "123";
		let val_str = Value::from(S);
		assert_eq!(
			val_str.get_pin().calculate_layout().size(),
			BOX_BASE_BYTES + size_of_val(S.as_bytes())
		);
		let t: &[Value] = &[1.into(), 2.2.into()];
		let tup2: Value = t.iter().cloned().collect();
		assert_eq!(
			tup2.get_pin().calculate_layout().size(),
			BOX_BASE_BYTES + size_of_val(t)
		);
	}

	#[test]
	fn weak_value_upgrades_while_strong_value_exists() {
		let value = Value::from("weak");
		let weak = value.downgrade();
		assert_eq!(weak.get_pin().refs().ref_count.load(Relaxed), 1);

		let upgraded = weak.upgrade().expect("value should still be alive");
		assert_eq!(upgraded.deref(), DataView::Str("weak"));
		assert_eq!(weak.get_pin().refs().ref_count.load(Relaxed), 2);
	}

	#[test]
	fn weak_value_expires_and_releases_references() {
		let inner = Value::from("inner");
		let value: Value = [inner.clone()].into_iter().collect();
		let weak = value.downgrade();
		assert_eq!(inner.get_pin().refs().ref_count.load(Relaxed), 2);

		drop(value);
		assert!(weak.upgrade().is_none());
		// Dropping the final strong reference deinitializes contained Values even
		// though the allocation remains alive for `weak`.
		assert_eq!(inner.get_pin().refs().ref_count.load(Relaxed), 1);
	}

	#[test]
	fn weak_value_clone_and_drop_track_weak_references() {
		let value = Value::from("weak");
		let weak = value.downgrade();
		let weak_clone = weak.clone();
		assert_eq!(weak.get_pin().refs().weak_count.load(Relaxed), 3);

		drop(value);
		assert_eq!(weak.get_pin().refs().weak_count.load(Relaxed), 2);
		drop(weak_clone);
		assert_eq!(weak.get_pin().refs().weak_count.load(Relaxed), 1);
	}

	#[test]
	fn binding_domain_retains_variables_until_cleared() {
		let values = [
			Value::new_bool_var(0),
			Value::new_int_var(1, 1.into()),
			Value::new_float_var(2, 1.into()),
			Value::new_int_set_var(3, 1.into()),
		];
		for value in values {
			let weak = value.downgrade();
			match value.deref() {
				DataView::BoolVar(mut variable) => variable.set_binding_domain(true),
				DataView::IntVar(mut variable) => variable.set_binding_domain(true),
				DataView::FloatVar(mut variable) => variable.set_binding_domain(true),
				DataView::IntSetVar(mut variable) => variable.set_binding_domain(true),
				_ => unreachable!("Expected variable"),
			};

			assert_eq!(weak.get_pin().refs().ref_count.load(Relaxed), 1);
			assert_eq!(weak.get_pin().refs().weak_count.load(Relaxed), 2);
			drop(value);
			assert_eq!(weak.get_pin().refs().ref_count.load(Relaxed), 0);
			assert_eq!(weak.get_pin().refs().weak_count.load(Relaxed), 1);

			let retained_value = weak
				.upgrade()
				.expect("binding domain should retain the variable");
			assert_eq!(weak.get_pin().refs().ref_count.load(Relaxed), 1);
			assert_eq!(weak.get_pin().refs().weak_count.load(Relaxed), 2);

			match retained_value.deref() {
				DataView::BoolVar(mut retained) => retained.set_binding_domain(false),
				DataView::IntVar(mut retained) => retained.set_binding_domain(false),
				DataView::FloatVar(mut retained) => retained.set_binding_domain(false),
				DataView::IntSetVar(mut retained) => retained.set_binding_domain(false),
				_ => unreachable!("Expected variable"),
			};
			drop(retained_value);

			assert!(weak.upgrade().is_none());
			assert_eq!(weak.get_pin().refs().ref_count.load(Relaxed), 0);
			assert_eq!(weak.get_pin().refs().weak_count.load(Relaxed), 1);
		}
	}

	#[test]
	fn test_string_value() {
		let empty = Value::from("");
		assert_eq!(empty.deref(), DataView::Str(""));
		assert!(empty.is_constant(&EMPTY_STRING));

		let single = Value::from("1");
		assert_eq!(single.deref(), DataView::Str("1"));
		let double = Value::from("12");
		let double_str: &str = (&double).try_into().unwrap();
		assert_eq!(double_str, "12");

		let lorem = r#"Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
		Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
		Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur.
		Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."#;
		let vlorem = Value::from(lorem);
		assert_eq!(vlorem.deref(), DataView::Str(lorem));
	}

	#[test]
	fn non_range_set_value_deduplicates_members() {
		let string = Value::from("string");
		let tuple = Value::from_iter([Value::from(1), Value::from(2)]);
		let annotation = Value::new_annotation(NonZeroU32::new(7).unwrap(), [3.into()].into_iter());
		let set = Value::new_discrete_set([
			string.clone(),
			tuple.clone(),
			annotation.clone(),
			string.clone(),
			tuple.clone(),
		]);

		let DataView::Set(set) = set.deref() else {
			unreachable!()
		};
		assert_eq!(set.card(), 3.into());
		assert!(set.contains(&string));
		assert!(set.contains(&tuple));
		assert!(set.contains(&annotation));
		assert_eq!(
			Value::new_discrete_set([string.clone(), tuple.clone()]),
			Value::new_discrete_set([tuple, string])
		);

		let sorted = Value::new_discrete_set([Value::from(3), Value::from(1), Value::from(2)]);
		let DataView::Set(sorted) = sorted.deref() else {
			unreachable!()
		};
		assert_eq!(
			sorted.values().cloned().collect::<Vec<_>>(),
			vec![Value::from(1), Value::from(2), Value::from(3)]
		);
	}

	#[test]
	fn equal_values_have_equal_hashes() {
		let string = Value::from("string");
		assert_eq!(hash(&string), hash(&Value::from("string")));

		let left = Value::new_discrete_set([Value::from(1), Value::from("string")]);
		let right = Value::new_discrete_set([Value::from("string"), Value::from(1)]);
		assert_eq!(left, right);
		assert_eq!(hash(&left), hash(&right));
	}

	#[test]
	fn annotation_value() {
		let empty = Value::new_annotation(NonZeroU32::new(42).unwrap(), std::iter::empty());
		assert_eq!(
			empty.deref(),
			DataView::Annotation(AnnotationView {
				ident: NonZeroU32::new(42).unwrap(),
				arguments: &[],
			})
		);

		let annotation = Value::new_annotation(
			NonZeroU32::new(7).unwrap(),
			[1.into(), 2.5.into()].into_iter(),
		);
		assert_eq!(
			annotation.deref(),
			DataView::Annotation(AnnotationView {
				ident: NonZeroU32::new(7).unwrap(),
				arguments: &[1.into(), 2.5.into()],
			})
		);
	}

	#[test]
	fn decision_variables_are_distinct_and_droppable() {
		let bool_var = Value::new_bool_var(0);
		let other_bool_var = Value::new_bool_var(1);
		assert!(matches!(bool_var.deref(), DataView::BoolVar(_)));
		assert_eq!(bool_var, bool_var.clone());
		assert_ne!(bool_var, other_bool_var);

		let int_domain = Value::from_iter([IntVal::Int(1)..=3.into()]);
		let int_var = Value::new_int_var(2, int_domain);
		assert!(matches!(int_var.deref(), DataView::IntVar(_)));

		let float_domain = Value::from_iter([FloatVal::from(1.0)..=3.0.into()]);
		let float_var = Value::new_float_var(3, float_domain);
		assert!(matches!(float_var.deref(), DataView::FloatVar(_)));

		let set_domain = Value::from_iter([IntVal::Int(1)..=3.into()]);
		let int_set_var = Value::new_int_set_var(4, set_domain);
		assert!(matches!(int_set_var.deref(), DataView::IntSetVar(_)));
	}
}
