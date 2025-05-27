//! Typed index derive macro

use quote::{ToTokens, quote};
use syn::{Data, DeriveInput, Field, Fields, Index, Type, parse_macro_input, spanned::Spanned};

/// Expand the `TypedIndex` derive macro
pub(crate) fn expand(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
	let input = parse_macro_input!(input as DeriveInput);
	let name = input.ident;

	let expand = |arg: (usize, &Field)| {
		let (n, f) = arg;
		let field_ty = f.ty.clone();
		let field_name = f
			.ident
			.as_ref()
			.map(|i| i.to_token_stream())
			.unwrap_or_else(|| Index::from(n).into_token_stream());

		let impls = f.attrs.iter().filter_map(|attr| {
			let index_mut = attr.path().is_ident("index_mut");
			if !index_mut && !attr.path().is_ident("index") {
				return None;
			}

			let mut index_ty = match attr.parse_args::<Type>() {
				Ok(ty) => ty,
				Err(e) => return Some(e.into_compile_error()),
			};

			let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();
			let mut generics = input.generics.clone();
			let impl_generics = match &mut index_ty {
				Type::Reference(tr) if tr.lifetime.is_none() => {
					generics
						.params
						.push(syn::parse_quote! { 'typed_index_lifetime });
					tr.lifetime = Some(syn::Lifetime::new("'typed_index_lifetime", tr.span()));
					generics.split_for_impl().0
				}
				_ => impl_generics,
			};

			let impl_index = quote! {
				impl #impl_generics ::core::ops::Index<#index_ty> for #name #ty_generics #where_clause {
					type Output = <#field_ty as ::core::ops::Index<#index_ty>>::Output;

					fn index(&self, index: #index_ty) -> &Self::Output {
						&self.#field_name[index]
					}
				}
			};

			if index_mut {
				Some(quote! {
					#impl_index

					impl #impl_generics ::core::ops::IndexMut<#index_ty> for #name #ty_generics #where_clause {
						fn index_mut(&mut self, index: #index_ty) -> &mut Self::Output {
							&mut self.#field_name[index]
						}
					}
				})
			} else {
				Some(impl_index)
			}
		});

		quote! { #(#impls)* }
	};

	let impls = match input.data {
		Data::Struct(ref data) => match data.fields {
			Fields::Named(ref fields) => fields.named.iter().enumerate().map(expand),
			Fields::Unnamed(ref fields) => fields.unnamed.iter().enumerate().map(expand),
			Fields::Unit => panic!("Deriving index not supported for unit structs"),
		},
		Data::Enum(_) | Data::Union(_) => panic!("Deriving index only supported for structs"),
	};

	let expanded = quote! {
		#(#impls)*
	};

	proc_macro::TokenStream::from(expanded)
}
