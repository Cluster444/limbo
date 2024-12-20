extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_attribute]
pub fn opcodes(_args: TokenStream, code: TokenStream) -> TokenStream {
    let input = parse_macro_input!(code as DeriveInput);
    let name = &input.ident;

    let variants = match &input.data {
        syn::Data::Enum(v) => &v.variants,
        _ => panic!("opcode macro only supports enums"),
    };

    let variant_names = variants.iter().map(|variant| {
        let variant_name = &variant.ident;
        let name_str = variant_name.to_string();

        quote! {
            #name::#variant_name{..} => #name_str
        }
    });

    let output = quote! {
        #input

        impl #name {
            pub fn get_name(&self) -> &str {
                match self {
                    #(#variant_names,)*
                }
            }
        }
    };

    TokenStream::from(output)
}

// #[proc_macro_attribute]
// pub fn opcodes(_attr: TokenStream, item: TokenStream) -> TokenStream {
//     let input = parse_macro_input!(item as DeriveInput);

//     let name = &input.ident;
//     let variants = match &input.data {
//         syn::Data::Enum(v) => &v.variants,
//         _ => panic!("opcode macro only supports enums"),
//     };

//     let variant_names = variants.iter().map(|variant| {
//         let variant_name = &variant.ident;
//         let name_str = variant_name.to_string();

//         quote! {
//             #name::#variant_name { .. } => #name_str.to_string()
//         }
//     });

//     let variant_descriptions = variants.iter().map(|variant| {
//         let variant_name = &variant.ident;
//         let desc = find_description_attr(&variant.attrs)
//             .unwrap_or_else(|| String::from("No Description"));
//         quote! {
//             #name::#variant_name { .. } => #desc.to_string()
//         }
//     });

//     let expanded = quote! {
//         #input

//         impl #name {
//             pub fn get_name(&self) -> String {
//                 match self {
//                     #(#variant_names,)*
//                 }
//             }

//             pub fn get_description(&self) -> String {
//                 match self {
//                     #(#variant_descriptions,)*
//                 }
//             }
//         }
//     };

//     TokenStream::from(expanded)
// }

// #[proc_macro_attribute]
// pub fn description(_attr: TokenStream, item: TokenStream) -> TokenStream {
//     let input: TokenStream = item.clone();
//     let attr_str = attr.to_string();

//     let desc = if attr_str.is_empty() {
//         "No description provided".to_string()
//     } else {
//         attr_str.trim_matches("").to_string()
//     };
// }
// let desc = variant.attrs.iter()
//     .find(|attr| attr.path().is_ident("description"))
//     .map(|attr| {
//         attr.parse_args::<syn::LitStr>()
//             .expect("description attribute requires a string literal")
//             .value()
//     })

// fn find_description_attr(attrs: &[Attribute]) -> Option<String> {
//     for attr in attrs {
//         if attr.path().is_ident("description") {
//             if let Ok(Meta::NameValue(MetaNameValue { value: Lit::Str(lit_str), ..})) = attr.parse_meta() {
//                 return Some(lit_str.value());
//             }
//         }
//     }
//     None
// }
