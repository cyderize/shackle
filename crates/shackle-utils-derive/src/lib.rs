//! Derive macros for convenience

mod typed_index;

/// Derive indexing traits which index into struct fields based on a specific index type.
///
/// ```
/// use shackle_utils_derive::TypedIndex;
///
/// use std::collections::HashMap;
///
/// #[derive(TypedIndex)]
/// struct Foo {
///     #[index_mut(usize)]
///     vector: Vec<i32>,
///     #[index(&str)]
///     map: HashMap<String, i32>,
/// }
///
/// let foo = Foo {
///     vector: vec![1, 2, 3],
///     map: HashMap::from([("a".to_string(), 1), ("b".to_string(), 2)]),
/// };
///
/// assert_eq!(foo[0], 1);
/// assert_eq!(foo["a"], 1);
/// assert_eq!(foo["b"], 2);
/// ```
#[proc_macro_derive(TypedIndex, attributes(index, index_mut))]
pub fn derive_typed_index(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	typed_index::expand(input)
}
