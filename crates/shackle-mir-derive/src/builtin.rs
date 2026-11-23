//! MIR builtin derive macro.

use std::collections::HashMap;

use quote::{format_ident, quote};
use syn::{Data, DeriveInput, Error, Fields, Ident, Type, parse_macro_input};

struct Variant<'a> {
	ident: &'a Ident,
	name: Ident,
	fields: &'a Fields,
}

/// Expand the `MirBuiltin` derive macro.
pub(crate) fn expand(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	match expand_derive(&input) {
		Ok(tokens) => tokens.into(),
		Err(error) => error.into_compile_error().into(),
	}
}

fn expand_derive(input: &DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
	let enum_data = match &input.data {
		Data::Enum(data) => data,
		_ => {
			return Err(Error::new_spanned(
				input,
				"MirBuiltin can only be derived for enums",
			));
		}
	};
	let argument = input
		.generics
		.type_params()
		.next()
		.ok_or_else(|| Error::new_spanned(&input.generics, "MirBuiltin requires a type parameter"))?
		.ident
		.clone();
	if input.generics.params.len() != 1 {
		return Err(Error::new_spanned(
			&input.generics,
			"MirBuiltin requires exactly one type parameter",
		));
	}

	let mut variants = Vec::with_capacity(enum_data.variants.len());
	let mut keys = HashMap::new();
	for variant in &enum_data.variants {
		let mut names = variant
			.attrs
			.iter()
			.filter(|attr| attr.path().is_ident("name"));
		let attribute = names.next().ok_or_else(|| {
			Error::new_spanned(variant, "builtin variant requires a #[name(...)] attribute")
		})?;
		if let Some(extra) = names.next() {
			return Err(Error::new_spanned(
				extra,
				"duplicate #[name(...)] attribute",
			));
		}
		let name = attribute.parse_args::<Ident>()?;
		for field in variant.fields.iter() {
			if !is_argument(&field.ty, &argument) {
				return Err(Error::new_spanned(
					field,
					"builtin fields must have the builtin argument type",
				));
			}
		}
		let key = (name.to_string(), variant.fields.len());
		if let Some(previous) = keys.insert(key, &variant.ident) {
			return Err(Error::new_spanned(
				attribute,
				format!("builtin name and arity are also used by variant `{previous}`"),
			));
		}
		variants.push(Variant {
			ident: &variant.ident,
			name,
			fields: &variant.fields,
		});
	}

	let enum_ident = &input.ident;
	let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
	let new_arms = variants.iter().map(|variant| {
		let name = &variant.name;
		let arity = variant.fields.len();
		let builder = builder(enum_ident, variant);
		quote! {
			(candidate, #arity) if candidate == ids.functions.#name => {
				let mut arguments = ::core::mem::take(arguments).into_iter();
				Some(#builder)
			}
		}
	});
	let name_arms = variants.iter().map(|variant| {
		let pattern = variant_pattern(enum_ident, variant);
		let name = &variant.name;
		quote!(#pattern => ids.functions.#name)
	});
	let argument_arms = variants.iter().map(|variant| {
		let pattern = argument_pattern(enum_ident, variant);
		let values = argument_values(variant);
		quote!(#pattern => vec![#values])
	});
	let map_arms = variants.iter().map(|variant| {
		let pattern = argument_pattern(enum_ident, variant);
		let mapper = map_builder(enum_ident, variant);
		quote!(#pattern => #mapper)
	});
	let signatures = variants.iter().map(|variant| {
		let name = &variant.name;
		let arity = variant.fields.len();
		quote!((ids.functions.#name, #arity))
	});

	Ok(quote! {
		impl #impl_generics #enum_ident #ty_generics #where_clause {
			/// Transform every argument of this builtin.
			pub fn map<U>(&self, mut f: impl FnMut(&#argument) -> U) -> #enum_ident<U> {
				match self {
					#(#map_arms),*
				}
			}

			/// Get the name of the builtin (used for printing as MiniZinc).
			pub fn name<'db>(&self, db: &'db dyn shackle_hir::Db) -> shackle_hir::Identifier<'db> {
				let ids = shackle_hir::constants::IdentifierRegistry::lookup(db);
				match self {
					#(#name_arms),*
				}
			}

			/// Get arguments in declaration order.
			pub fn arguments(&self) -> Vec<&#argument> {
				match self {
					#(#argument_arms),*
				}
			}
		}

		impl<'db> #enum_ident<Box<crate::Value<'db>>> {
			/// Construct the builtin with the given MiniZinc name and arguments.
			///
			/// The arguments are consumed when the builtin is recognized and left unchanged
			/// when it is not.
			pub fn new(
				db: &'db dyn shackle_hir::Db,
				name: shackle_hir::Identifier<'db>,
				arguments: &mut Vec<crate::Value<'db>>,
			) -> Option<Self> {
				let ids = shackle_hir::constants::IdentifierRegistry::lookup(db);
				match (name, arguments.len()) {
					#(#new_arms),*,
					_ => None,
				}
			}

			#[cfg(test)]
			pub(crate) fn signatures(
				db: &'db dyn shackle_hir::Db,
			) -> Vec<(shackle_hir::Identifier<'db>, usize)> {
				let ids = shackle_hir::constants::IdentifierRegistry::lookup(db);
				vec![#(#signatures),*]
			}
		}
	})
}

fn is_argument(ty: &Type, argument: &Ident) -> bool {
	matches!(ty, Type::Path(path) if path.qself.is_none() && path.path.is_ident(argument))
}

fn bindings(variant: &Variant<'_>) -> Vec<Ident> {
	variant
		.fields
		.iter()
		.enumerate()
		.map(|(index, field)| {
			field
				.ident
				.clone()
				.unwrap_or_else(|| format_ident!("field_{index}"))
		})
		.collect()
}

fn variant_pattern(enum_ident: &Ident, variant: &Variant<'_>) -> proc_macro2::TokenStream {
	let ident = variant.ident;
	match variant.fields {
		Fields::Unit => quote!(#enum_ident::#ident),
		Fields::Unnamed(_) => quote!(#enum_ident::#ident(..)),
		Fields::Named(_) => quote!(#enum_ident::#ident { .. }),
	}
}

fn builder(enum_ident: &Ident, variant: &Variant<'_>) -> proc_macro2::TokenStream {
	let ident = variant.ident;
	let values = variant.fields.iter().map(|_| {
		quote!(Box::new(
			arguments.next().expect("builtin arity was checked")
		))
	});
	let field_names = variant
		.fields
		.iter()
		.map(|field| field.ident.as_ref().unwrap());
	match variant.fields {
		Fields::Unit => quote!(#enum_ident::#ident),
		Fields::Unnamed(_) => quote!(#enum_ident::#ident(#(#values),*)),
		Fields::Named(_) => quote!(#enum_ident::#ident { #(#field_names: #values),* }),
	}
}

fn argument_pattern(enum_ident: &Ident, variant: &Variant<'_>) -> proc_macro2::TokenStream {
	let ident = variant.ident;
	let bindings = bindings(variant);
	match variant.fields {
		Fields::Unit => quote!(#enum_ident::#ident),
		Fields::Unnamed(_) => quote!(#enum_ident::#ident(#(#bindings),*)),
		Fields::Named(_) => quote!(#enum_ident::#ident { #(#bindings),* }),
	}
}

fn argument_values(variant: &Variant<'_>) -> proc_macro2::TokenStream {
	let bindings = bindings(variant);
	quote!(#(#bindings),*)
}

fn map_builder(enum_ident: &Ident, variant: &Variant<'_>) -> proc_macro2::TokenStream {
	let ident = variant.ident;
	let bindings = bindings(variant);
	let values = bindings.iter().map(|binding| quote!(f(#binding)));
	let field_names = variant
		.fields
		.iter()
		.map(|field| field.ident.as_ref().unwrap());
	match variant.fields {
		Fields::Unit => quote!(#enum_ident::#ident),
		Fields::Unnamed(_) => quote!(#enum_ident::#ident(#(#values),*)),
		Fields::Named(_) => quote!(#enum_ident::#ident { #(#field_names: #values),* }),
	}
}

#[cfg(test)]
mod tests {
	use syn::{DeriveInput, parse_quote};

	use super::expand_derive;

	#[test]
	fn accepts_supported_variant_shapes() {
		let input: DeriveInput = parse_quote! {
			enum Builtin<T> {
				#[name(zero)]
				Zero,
				#[name(one)]
				One(T),
				#[name(two)]
				Two { left: T, right: T },
			}
		};
		assert!(expand_derive(&input).is_ok());
	}

	#[test]
	fn requires_a_name_for_every_variant() {
		let input: DeriveInput = parse_quote! {
			enum Builtin<T> {
				Missing(T),
			}
		};
		let error = expand_derive(&input).unwrap_err();
		assert!(error.to_string().contains("requires a #[name(...)]"));
	}

	#[test]
	fn rejects_duplicate_name_and_arity() {
		let input: DeriveInput = parse_quote! {
			enum Builtin<T> {
				#[name(same)]
				First(T),
				#[name(same)]
				Second { value: T },
			}
		};
		let error = expand_derive(&input).unwrap_err();
		assert!(error.to_string().contains("also used by variant `First`"));
	}

	#[test]
	fn rejects_unsupported_field_types() {
		let input: DeriveInput = parse_quote! {
			enum Builtin<T> {
				#[name(wrong)]
				Wrong(String),
			}
		};
		let error = expand_derive(&input).unwrap_err();
		assert!(error.to_string().contains("builtin argument type"));
	}
}
