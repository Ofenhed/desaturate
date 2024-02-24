use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote_spanned, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    Ident, Lifetime, Token,
};

pub(crate) fn default<T: Default>() -> T {
    T::default()
}

mod input_function;
mod transformer;
mod visitors;
use crate::{input_function::*, transformer::*};

#[derive(Default)]
struct Asyncable {
    debug_dump: Option<Span>,
    lifetime: Option<Lifetime>,
    make_blocking: bool,
    make_async: bool,
}

impl Parse for Asyncable {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        struct Setting {
            name: Ident,
            value: Option<(Token![=], syn::Lit)>,
        }
        impl Setting {
            fn span(&self) -> Span {
                let span = self.name.span();
                if let Some((token, lit)) = &self.value {
                    span.join(token.span()).unwrap().join(lit.span()).unwrap()
                } else {
                    span
                }
            }
        }
        impl Parse for Setting {
            fn parse(input: ParseStream) -> syn::Result<Self> {
                let name = input.parse()?;
                let value = if input.peek(Token![=]) {
                    Some((input.parse()?, input.parse()?))
                } else {
                    None
                };
                Ok(Self { name, value })
            }
        }
        let mut result = Asyncable {
            make_blocking: cfg!(feature = "generate-blocking"),
            make_async: cfg!(feature = "generate-async"),
            ..Asyncable::default()
        };
        let mut errors: Vec<syn::Error> = vec![];
        Punctuated::<Setting, Token![,]>::parse_terminated(input)?
            .iter()
            .for_each(|setting| match setting {
                setting @ Setting { name, value: None } if name == "debug_dump" => {
                    result.debug_dump = Some(setting.span())
                }
                Setting {
                    name,
                    value: Some((_eq, syn::Lit::Str(lifetime))),
                } if name == "lifetime" => match lifetime.parse() {
                    Ok(lifetime) => result.lifetime = lifetime,
                    Err(e) => errors.push(e),
                },
                invalid => errors.push(syn::Error::new(invalid.span(), "Invalid setting")),
            });
        let errors = errors
            .into_iter()
            .fold(Option::<syn::Error>::None, |prev, err| {
                Some(prev.map_or(err.clone(), move |mut old_err| {
                    old_err.combine(err);
                    old_err
                }))
            });
        if let Some(errors) = errors {
            Err(errors)
        } else {
            Ok(result)
        }
    }
}

impl Asyncable {
    fn from_attributes(input: TokenStream2) -> syn::Result<Self> {
        parse2(input)
    }
}

struct PrintFunctionState<'a, 'b: 'a> {
    state: &'a FunctionState<'b>,
}

impl ToTokens for PrintFunctionState<'_, '_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let PrintFunctionState {
            state:
                state @ FunctionState {
                    options:
                        Asyncable {
                            make_blocking,
                            make_async,
                            ..
                        },
                    function:
                        AsyncFunction {
                            visibility,
                            constness,
                            _asyncness,
                            unsafety,
                            fn_token,
                            ident,
                            generics: _, // replaced with new_generics()
                            paren_token,
                            inputs: _, // replaced with simple_input_variables()
                            output: _, // replaced with new_return_type_tokens()
                            where_clause,
                            body,
                            identities: _,
                        },
                    ..
                },
        } = self;
        visibility.to_tokens(tokens);
        constness.to_tokens(tokens);
        unsafety.to_tokens(tokens);
        fn_token.to_tokens(tokens);
        ident.to_tokens(tokens);
        state.new_generics().to_tokens(tokens);
        paren_token.surround(tokens, |tokens| {
            state.simple_input_variables().to_tokens(tokens)
        });
        state.new_return_type_tokens().to_tokens(tokens);
        where_clause.to_tokens(tokens);
        body.brace_token.surround(tokens, |tokens| {
            if *make_blocking && *make_async {
                let async_let = state.async_let_statement();
                let blocking_let = state.blocking_let_statement();
                let async_var = state.async_name();
                let blocking_var = state.blocking_name();
                let args_var = state.simple_input_variables_tuple();
                // TODO: Add parantesis to args
                quote_spanned!{body.span()=>
                    #async_let;
                    #blocking_let;
                    ::desaturate::IntoDesaturatedWith::desaturate_with(#async_var, #args_var, #blocking_var)
                }.to_tokens(tokens);
            } else if *make_blocking {
                let blocking_body = state.blocking_function_body();
                let warning = format!("Tried to await Desaturated from {} when desaturate wasn't compiled with \"async\"", state.function.ident);
                quote_spanned!{body.span()=>
                    ::desaturate::IntoDesaturated::desaturate(async { unreachable!(#warning) }, move || #blocking_body)
                }.to_tokens(tokens);
            } else if *make_async {
                let async_body = &state.body;
                let warning = format!("Tried to call Desaturated from {} when desaturate wasn't compiled with \"blocking\"", state.function.ident);
                quote_spanned!{body.span()=>
                    ::desaturate::IntoDesaturated::desaturate(async move #async_body, || unreachable!(#warning))
                }.to_tokens(tokens);
            } else {
                let async_warning = format!("Tried to await Desaturated from {} when desaturate wasn't compiled with \"async\"", state.function.ident);
                let blocking_warning = format!("Tried to call Desaturated from {} when desaturate wasn't compiled with \"blocking\"", state.function.ident);
                quote_spanned!{body.span()=>
                    ::desaturate::IntoDesaturated::desaturate(async { unreachable!(#async_warning) }, || unreachable!(#blocking_warning))
                }.to_tokens(tokens);
            }
        });
    }
}

impl Asyncable {
    fn desaturate(&self, item: TokenStream2) -> syn::Result<TokenStream2> {
        let function: AsyncFunction = parse2(item)?;
        let state = FunctionState::new(self, &function);
        let result = PrintFunctionState { state: &state }.into_token_stream();
        if self.debug_dump.is_some() {
            eprintln!("{result}");
        }
        Ok(result)
    }
}

#[proc_macro_attribute]
pub fn desaturate(attr: TokenStream, item: TokenStream) -> TokenStream {
    match Asyncable::from_attributes(attr.into()) {
        Ok(handler) => handler
            .desaturate(item.into())
            .unwrap_or_else(syn::Error::into_compile_error)
            .into(),
        Err(e) => e.into_compile_error().into(),
    }
}

#[cfg(test)]
mod tests {
    use super::Asyncable;
    use quote::quote;
    #[test]
    fn only_async_unchanged() -> syn::Result<()> {
        let handler = Asyncable {
            lifetime: None,
            debug_dump: Some(proc_macro2::Span::call_site()),
            make_async: true,
            make_blocking: false,
        };
        let function = quote! {
            pub async fn do_something(other: i32) -> i32 {
                do_something().await?;
                call_somewhere().await;
                other * 2
            }
        };
        let expected = function.clone();
        let result = handler.desaturate(function)?;
        assert_eq!(format!("{result}"), format!("{expected}"));
        Ok(())
    }
}
