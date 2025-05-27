//! Miscellaneous utilities

use salsa::Database as Db;
pub use shackle_utils_derive::TypedIndex;

pub mod arena;
pub mod refmap;

// use std::fmt::Write;

// use salsa::InternKey;

// use crate::{db::InternedString, hir::db::Hir};

// /// Trait for pretty printing for debugging with a Salsa database
// pub trait DebugPrint<'a> {
// 	/// Type of database (e.g. `dyn Hir`)
// 	type Database: ?Sized + 'a;
// 	/// Pretty print to a string
// 	fn debug_print(&self, db: &Self::Database) -> String;
// }

// /// Replace debug printed `InternedString`s with their values
// pub fn debug_print_strings(db: &dyn Hir, s: &str) -> String {
// 	// Replace interned strings with values
// 	let mut o = String::new();
// 	for (i, x) in s.split("InternedString(").enumerate() {
// 		if i > 0 {
// 			if let Some(idx) = x.find(')') {
// 				let s = InternedString::from_intern_id((x[..idx]).parse::<u32>().unwrap().into())
// 					.value(db.upcast());
// 				write!(&mut o, "{:?}", s).unwrap();
// 				o.push_str(&x[idx + 1..]);
// 			} else {
// 				o.push_str(x);
// 			}
// 		} else {
// 			o.push_str(x);
// 		}
// 	}
// 	o
// }

/// Get levenshtein distance between two strings
pub fn levenshtein_distance(s: &str, t: &str) -> usize {
	let n = t.len();
	let mut dp0 = (0..=n).collect::<Vec<_>>();
	let mut dp1 = vec![0_usize; n + 1];
	for (i, s_i) in s.chars().enumerate() {
		dp1[0] = i + 1;
		for (j, t_j) in t.chars().enumerate() {
			let del = dp0[j + 1] + 1;
			let ins = dp1[j] + 1;
			let sub = if s_i == t_j { dp0[j] } else { dp0[j] + 1 };
			dp1[j + 1] = del.min(ins.min(sub));
		}
		std::mem::swap(&mut dp0, &mut dp1);
	}
	*dp0.last().unwrap()
}

/// Grow the stack if necessary to run the given function.
///
/// Useful for recursive calls which may overrun the stack otherwise.
#[inline]
pub fn maybe_grow_stack<R>(f: impl FnOnce() -> R) -> R {
	stacker::maybe_grow(64 * 1024, 1024 * 1024, f)
}

#[allow(missing_docs, reason = "Salsa generates code with missing docs")]
mod interned_string {
	use std::fmt::Display;

	use crate::Db;

	/// An interned string
	#[salsa::interned(debug)]
	pub struct InternedString {
		#[returns(ref)]
		value: String,
	}

	impl<'db> InternedString<'db> {
		/// Get the string value
		pub fn lookup(&self, db: &'db dyn Db) -> &'db str {
			self.value(db)
		}
	}

	impl<'db> Display for InternedString<'db> {
		fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
			salsa::with_attached_database(|db| self.value(db).fmt(f))
				.unwrap_or_else(|| write!(f, "<interned string>"))
		}
	}
}

pub use interned_string::InternedString;
