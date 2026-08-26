//! Derive macros for the mid-level IR.

mod builtin;

/// Generate name-based construction and registry operations for a MIR builtin enum.
#[proc_macro_derive(MirBuiltin, attributes(name))]
pub fn derive_mir_builtin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	builtin::expand(input)
}
