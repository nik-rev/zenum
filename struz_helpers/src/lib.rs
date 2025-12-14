use proc_macro::TokenStream;

/// Does literally nothing. Only exists so you won't get a compiler error when writing `#[stren]
#[proc_macro_derive(dummy, attributes(stren))]
pub fn dummy(_: TokenStream) -> TokenStream {
    TokenStream::new()
}
