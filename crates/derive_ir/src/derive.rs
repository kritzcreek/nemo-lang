use proc_macro2::TokenStream;
use quote::quote;
use syn::{Data, DeriveInput, Fields};

pub(crate) fn derive_ir_builder(ast: DeriveInput) -> TokenStream {
    let mut ts = TokenStream::new();
    let type_name = &ast.ident;

    match ast.data {
        Data::Struct(s) => {
            let Fields::Named(fields) = s.fields else {
                panic!("Expected a struct with named fields");
            };
            let builder = generate_field_builders(
                fields,
                type_name,
                type_name.to_string() + "Builder",
                type_name.to_string(),
            );
            ts.extend(builder);
        }
        Data::Enum(e) => {
            for variant in e.variants {
                let Fields::Named(fields) = variant.fields else {
                    continue;
                };
                let variant_name = &variant.ident;
                let builder = generate_field_builders(
                    fields,
                    type_name,
                    variant_name.to_string() + "Builder",
                    format!("{type_name}::{variant_name}"),
                );
                ts.extend(builder);
            }
        }
        Data::Union(_) => todo!(),
    }

    ts
}

fn generate_field_builders(
    fields: syn::FieldsNamed,
    type_name: &syn::Ident,
    builder_name: String,
    ctor: String,
) -> TokenStream {
    let mut setters = vec![];
    let mut builder_fields = vec![];
    let mut build_fields = vec![];
    for field in fields.named {
        let Some((setter, field, build)) = generate_setter(field) else {
            continue;
        };
        builder_fields.push(field);
        setters.push(setter);
        build_fields.push(build);
    }
    let builder_name = syn::parse_str::<syn::Type>(&builder_name).unwrap();
    let builder_struct = quote! {
      #[derive(Default)]
      pub(crate) struct #builder_name {
        #(#builder_fields)*
      }
    };
    let ctor = syn::parse_str::<syn::Path>(&ctor).unwrap();
    let build_fn = quote! {
        pub(crate) fn build(self) -> Option<#type_name> {
            Some(#ctor { #(#build_fields)* })
        }
    };

    let impl_block = quote! {
        impl #builder_name {
            #(#setters)*
            #build_fn
        }
    };
    let mut ts = TokenStream::new();
    ts.extend(builder_struct);
    ts.extend(impl_block);
    ts
}

fn generate_setter(field: syn::Field) -> Option<(TokenStream, TokenStream, TokenStream)> {
    let id = field.ident?;
    let ty = field.ty;
    let setter: TokenStream;
    let field: TokenStream;
    let build: TokenStream;
    if let Some(elem_ty) = parse_vec_ty(&ty) {
        setter = quote! {
          pub(crate) fn #id(&mut self, elem: Option<#elem_ty>) -> &mut Self {
            if let Some(elem) = elem {
              self.#id.push(elem);
            }
            self
          }
        };
        field = quote! {
          #id: #ty,
        };
        build = quote! {
            #id: self.#id,
        };
    } else if let Some(elem_ty) = parse_option_ty(&ty) {
        setter = quote! {
          pub(crate) fn #id(&mut self, #id: Option<#elem_ty>) -> &mut Self {
            self.#id = #id;
            self
          }
        };
        field = quote! {
          #id: Option<#elem_ty>,
        };
        build = quote! {
            #id: self.#id,
        };
    } else {
        setter = quote! {
          pub(crate) fn #id(&mut self, #id: Option<#ty>) -> &mut Self {
            self.#id = #id;
            self
          }
        };
        field = quote! {
          #id: Option<#ty>,
        };
        build = quote! {
            #id: self.#id?,
        };
    };
    Some((setter, field, build))
}

fn parse_vec_ty(ty: &syn::Type) -> Option<&syn::Type> {
    parse_unary_ty(ty, "Vec")
}

fn parse_option_ty(ty: &syn::Type) -> Option<&syn::Type> {
    parse_unary_ty(ty, "Option")
}

fn parse_unary_ty<'a>(ty: &'a syn::Type, name: &str) -> Option<&'a syn::Type> {
    let syn::Type::Path(path) = ty else {
        return None;
    };
    let segments = &path.path.segments;
    if segments.len() != 1 {
        return None;
    };
    let segment = &segments[0];
    if segment.ident.to_string().as_str() != name {
        return None;
    }
    let syn::PathArguments::AngleBracketed(args) = &segment.arguments else {
        return None;
    };
    if args.args.len() != 1 {
        return None;
    }
    let syn::GenericArgument::Type(elem_ty) = &args.args[0] else {
        return None;
    };
    Some(elem_ty)
}
