use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DeriveInput, Fields, GenericArgument,
    Path, PathArguments, Type, TypePath,
};

type OptionalField = bool;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let builder_name = Ident::new(&format!("{}Builder", name), Span::call_site());

    let fields_data = fields_data(&input.data);
    let struct_fields = struct_fields(&fields_data);
    let init_fields = init_fields(&fields_data);
    let setter_methods = setter_methods(&fields_data);
    let required_fields_are_some = required_fields_are_some(&fields_data);
    let unwrapped_fields = unwrapped_fields(&fields_data);

    let expanded = quote! {
        use std::error::Error;

        pub struct #builder_name {
            #(#struct_fields),*
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#init_fields),*
                }
            }
        }

        impl #builder_name {
            #(#setter_methods)*

            pub fn build(&mut self) -> Result<#name, Box<dyn Error>> {
                if self.required_fields_are_some() {
                    Ok(#name {
                        #(#unwrapped_fields),*
                    })
                } else {
                    Err("One or more fields is missing".into())
                }
            }

            fn required_fields_are_some(&self) -> bool {
                #(#required_fields_are_some)&&*
            }
        }
    };
    println!("{}", expanded.to_string());

    TokenStream::from(expanded)
}

fn required_fields_are_some(
    data: &Vec<(&Option<Ident>, &Type, OptionalField)>,
) -> Vec<proc_macro2::TokenStream> {
    data.iter()
        .filter(|(_, _, optional)| *optional == false)
        .map(|(name, _, _)| {
            quote! {
                self.#name.is_some()
            }
        })
        .collect::<Vec<_>>()
}

fn unwrapped_fields(
    data: &Vec<(&Option<Ident>, &Type, OptionalField)>,
) -> Vec<proc_macro2::TokenStream> {
    data.iter()
        .map(|(name, _, optional)| {
            if *optional {
                quote! {
                    #name: self.#name.take()
                }
            } else {
                quote! {
                    #name: self.#name.take().unwrap()
                }
            }
        })
        .collect::<Vec<_>>()
}

fn setter_methods(
    data: &Vec<(&Option<Ident>, &Type, OptionalField)>,
) -> Vec<proc_macro2::TokenStream> {
    data.iter()
        .map(|(name, ty, _)| {
            quote! {
                fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        })
        .collect::<Vec<_>>()
}

fn struct_fields(
    data: &Vec<(&Option<Ident>, &Type, OptionalField)>,
) -> Vec<proc_macro2::TokenStream> {
    data.iter()
        .map(|(name, ty, _)| {
            quote! {
                #name: Option<#ty>
            }
        })
        .collect::<Vec<_>>()
}

fn init_fields(
    data: &Vec<(&Option<Ident>, &Type, OptionalField)>,
) -> Vec<proc_macro2::TokenStream> {
    data.iter()
        .map(|(name, _, _)| {
            quote! {
                #name: None
            }
        })
        .collect::<Vec<_>>()
}

fn fields_data(data: &Data) -> Vec<(&Option<Ident>, &Type, OptionalField)> {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => fields
                .named
                .iter()
                .map(|f| match f.ty {
                    Type::Path(TypePath {
                        qself: None,
                        path:
                            Path {
                                segments: ref segs, ..
                            },
                    }) => {
                        if segs[0].ident.to_string() == String::from("Option") {
                            let option_args = &segs[0].arguments;
                            match option_args {
                                PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                    args,
                                    ..
                                }) => {
                                    let gen_args = &args[0];
                                    match gen_args {
                                        GenericArgument::Type(ty) => (&f.ident, ty, true),
                                        _ => (&f.ident, &f.ty, false),
                                    }
                                }
                                _ => (&f.ident, &f.ty, false),
                            }
                        } else {
                            (&f.ident, &f.ty, false)
                        }
                    }
                    _ => (&f.ident, &f.ty, false),
                })
                .collect::<Vec<_>>(),
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}
