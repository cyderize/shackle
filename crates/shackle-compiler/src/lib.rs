//! Shackle internal compiler library.
//!
//! This library is considered internal and no stability guarantees are given.

#![warn(missing_docs)]
#![warn(unused_crate_dependencies, unused_extern_crates)]
#![warn(variant_size_differences)]
#![recursion_limit = "256"]

pub mod constants;
pub mod db;
pub mod file;
pub mod hir;
pub mod mir;
pub mod syntax;
pub mod thir;
pub mod ty;
pub mod utils;

pub use db::CompilerDatabase;
pub use shackle_diagnostics::Result;

#[cfg(test)]
mod tests {}
