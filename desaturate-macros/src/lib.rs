use std::{cell::OnceCell, collections::HashSet, ops::Deref, sync::Mutex};

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2, TokenTree};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    braced, bracketed, parenthesized,
    parse::{Parse, ParseStream},
    parse2, parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
    token,
    visit::{self, Visit},
    visit_mut::{self, VisitMut},
    AngleBracketedGenericArguments, Block, Expr, ExprLet, ExprPath, Field, FnArg, GenericArgument,
    GenericParam, Generics, Ident, Lifetime, Result, ReturnType, Token, Type, TypeImplTrait,
    TypeParamBound, Visibility, WhereClause,
};

fn default<T: Default>() -> T {
    T::default()
}

#[cfg_attr(feature = "debug", derive(Debug))]
struct AsyncFunction {
    visibility: Visibility,
    constness: Option<Token![const]>,
    asyncness: Token![async],
    unsafety: Option<Token![unsafe]>,
    fn_token: Token![fn],
    ident: Ident,
    generics: Generics,
    paren_token: token::Paren,
    inputs: Punctuated<FnArg, Token![,]>,
    output: ReturnType,
    where_clause: Option<WhereClause>,
    body: Block,
    identities: Mutex<HashSet<String>>,
}

#[derive(Default)]
struct IdentityVisitor(HashSet<String>);

impl<'ast> Visit<'ast> for IdentityVisitor {
    fn visit_ident(&mut self, ident: &'ast Ident) {
        self.0.insert(format!("{}", ident));
        visit::visit_ident(self, ident);
    }
}

struct SelfReplacer(Ident);
impl VisitMut for SelfReplacer {
    fn visit_item_struct_mut(&mut self, _: &mut syn::ItemStruct) {}
    fn visit_ident_mut(&mut self, ident: &mut Ident) {
        eprintln!("Found identifier {ident}");
    }
}

// TODO: Add RenameSelf: VisitMut
// TODO: Add ReplaceAwait: VisitMut

impl Parse for AsyncFunction {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let args;
        let function = Self {
            visibility: input.parse()?,
            constness: input.parse()?,
            asyncness: input.parse()?,
            unsafety: input.parse()?,
            fn_token: input.parse()?,
            ident: input.parse()?,
            generics: input.parse()?,
            paren_token: parenthesized!(args in input),
            inputs: Punctuated::parse_terminated(&args)?,
            output: input.parse()?,
            where_clause: input.parse()?,
            body: input.parse()?,
            identities: default(),
        };
        let mut visitor = IdentityVisitor::default();
        visitor.visit_generics(&function.generics);
        for arg in function.inputs.iter() {
            visitor.visit_fn_arg(arg);
        }
        *function.identities.lock().unwrap() = visitor.0;
        Ok(function)
    }
}

impl AsyncFunction {
    fn new_ident(&self, ident: Ident) -> Ident {
        let mut identities = self.identities.lock().unwrap();
        let new_ident = if identities.contains(&format!("{ident}")) {
            let mut counter = 0;
            loop {
                counter += 1;
                let name = format!("{ident}{counter}");
                if !identities.contains(&name) {
                    break Ident::new(&name, ident.span());
                }
            }
        } else {
            ident
        };
        identities.insert(format!("{new_ident}"));
        new_ident
    }
}

struct FunctionState<'a> {
    function: &'a AsyncFunction,
    old_return_type: OnceCell<ReturnType>,
    new_return_type: OnceCell<syn::Result<ReturnType>>,
    async_name: OnceCell<Ident>,
    async_let_statement: OnceCell<ExprLet>,
    blocking_name: OnceCell<Ident>,
    blocking_let_statement: OnceCell<ExprLet>,
    self_new_name: OnceCell<Option<Ident>>,
    input_variables: OnceCell<Punctuated<syn::Pat, Token![,]>>,
    function_arguments_without_self: OnceCell<Punctuated<syn::Pat, Token![,]>>,
    input_variables_without_self: OnceCell<Punctuated<syn::Pat, Token![,]>>,
    body_without_self: OnceCell<Block>,
    blocking_body_without_self: OnceCell<Block>,
}

impl<'a> From<&'a AsyncFunction> for FunctionState<'a> {
    fn from(function: &'a AsyncFunction) -> Self {
        Self {
            function,
            old_return_type: default(),
            new_return_type: default(),
            async_name: default(),
            async_let_statement: default(),
            blocking_name: default(),
            blocking_let_statement: default(),
            self_new_name: default(),
            input_variables: default(),
            input_variables_without_self: default(),
            function_arguments_without_self: default(),
            body_without_self: default(),
            blocking_body_without_self: default(),
        }
    }
}

impl Deref for FunctionState<'_> {
    type Target = AsyncFunction;

    fn deref(&self) -> &AsyncFunction {
        self.function
    }
}

impl FunctionState<'_> {
    fn old_return_type(&self) -> &ReturnType {
        self.old_return_type.get_or_init(|| {
            if let ReturnType::Type(arrow, output) = self.output.clone() {
                ReturnType::Type(arrow, output)
            } else {
                ReturnType::Default
            }
        })
    }
    fn new_return_type(&self) -> &syn::Result<ReturnType> {
        self.new_return_type.get_or_init(|| {
            let output_type = self.old_return_type();
            let (default_paren, default_elems);
            let (arrow, output_type) = if let ReturnType::Type(arrow, output) = output_type {
                if let Err(e) = match &**output {
                    Type::BareFn(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    Type::ImplTrait(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    Type::Infer(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    Type::Macro(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    Type::Verbatim(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    _ => Ok(())
                } {
                    return Err(e);
                }
                (arrow, &**output)
            } else {
                default_paren = <Token![->]>::default();
                default_elems = Type::Tuple(syn::TypeTuple {
                    paren_token: default(),
                    elems: default(),
                });
                (&default_paren, &default_elems)
            };
            let mut bounds = Punctuated::new();
            bounds.push(TypeParamBound::Trait(parse2(quote_spanned!{output_type.span()=>::core::future::IntoFuture<Output=#output_type>})?));
            bounds.push(TypeParamBound::Trait(parse2(quote_spanned!{output_type.span()=>::desaturate::Syncable<#output_type>})?));
            for lifetime in self.generics.lifetimes() {
                bounds.push_value(TypeParamBound::Lifetime(lifetime.lifetime.clone()));
            }
            let return_type = ReturnType::Type(*arrow, Box::new(TypeImplTrait {
                impl_token: parse2(quote_spanned!{output_type.span() => impl}).unwrap(),
                bounds,
            }.into()));
            Ok(return_type)
        })
    }
    fn self_new_name(&self) -> Option<&Ident> {
        self.self_new_name
            .get_or_init(|| {
                self.inputs
                    .first()
                    .map(|first| {
                        if let FnArg::Receiver(_) = first {
                            Some(self.new_ident(Ident::new("selfish", first.span())))
                        } else {
                            None
                        }
                    })
                    .flatten()
            })
            .as_ref()
    }
    fn input_variables(&self) -> &Punctuated<syn::Pat, Token![,]> {
        self.input_variables.get_or_init(|| {
            self.inputs
                .iter()
                .map(|variable| match variable {
                    //FnArg::Receiver(syn::Receiver {
                    //    reference: Some((and_token, _lifetime)),
                    //    mutability,
                    //    self_token,
                    //    ..
                    //}) => syn::Pat::Reference(syn::PatReference {
                    //    attrs: default(),
                    //    and_token: *and_token,
                    //    mutability: *mutability,
                    //    pat: Box::new(syn::Pat::Verbatim(self_token.into_token_stream())),
                    //}),
                    FnArg::Receiver(syn::Receiver { self_token, .. }) => {
                        syn::Pat::Verbatim(self_token.into_token_stream())
                    }
                    FnArg::Typed(typed) => *typed.pat.clone(),
                })
                .collect()
        })
    }
    fn input_variables_without_self(&self) -> &Punctuated<syn::Pat, Token![,]> {
        if let Some(new_self_name) = self.self_new_name() {
            self.input_variables_without_self.get_or_init(|| {
                self.inputs
                    .iter()
                    .map(|variable| match variable {
                        FnArg::Receiver(_) => syn::Pat::Verbatim(new_self_name.into_token_stream()),
                        FnArg::Typed(typed) => *typed.pat.clone(),
                    })
                    .collect()
            })
        } else {
            self.input_variables()
        }
    }
    fn body_without_self(&self) -> &Block {
        if let Some(new_self_name) = self.self_new_name() {
            self.body_without_self.get_or_init(|| {
                let mut body = self.body.clone();
                let mut visitor = SelfReplacer(new_self_name.clone());
                visitor.visit_block_mut(&mut body);
                body
            })
        } else {
            &self.body
        }
    }
    fn function_arguments(&self) -> &Punctuated<Ident, Token![,]> {
        todo!("function arguments")
    }
    fn function_arguments_without_self(&self) -> &Punctuated<syn::Pat, Token![,]> {
        self.input_variables()
    }
    fn blocking_body_without_self(&self) -> &Block {
        self.blocking_body_without_self.get_or_init(|| {
            struct AsyncStripper;
            impl VisitMut for AsyncStripper {
                fn visit_expr_async_mut(&mut self, _: &mut syn::ExprAsync) {}
                fn visit_expr_await_mut(&mut self, awaited: &mut syn::ExprAwait) {
                    eprintln!("Search failed!");
                    visit_mut::visit_expr_await_mut(self, awaited)
                }
                fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
                    match expr {
                        syn::Expr::Await(syn::ExprAwait {
                            attrs,
                            base,
                            dot_token,
                            await_token,
                        }) => {
                            *expr = syn::Expr::Call(syn::ExprCall {
                                attrs: attrs.clone(),
                                func: Box::new(syn::Expr::Path(syn::ExprPath {
                                    attrs: vec![],
                                    qself: None,
                                    path: syn::Path {
                                        leading_colon: Some(default()),
                                        segments: ["desaturate", "Blocking", "call"]
                                            .into_iter()
                                            .map(|segment| syn::PathSegment {
                                                ident: Ident::new(segment, await_token.span()),
                                                arguments: default(),
                                            })
                                            .collect(),
                                    },
                                })),
                                paren_token: default(),
                                args: [*base.clone()].into_iter().collect(),
                            });
                        }
                        _ => (),
                    }
                    visit_mut::visit_expr_mut(self, expr)
                }
            }
            let mut visitor = AsyncStripper;
            let mut body = self.body_without_self().clone();
            visitor.visit_block_mut(&mut body);
            body
        })
    }
    fn async_function_body(&self) -> &Block {
        self.body_without_self()
    }
    fn async_name(&self) -> &Ident {
        self.async_name
            .get_or_init(|| self.new_ident(Ident::new("async_function", Span::mixed_site())))
    }
    fn async_let_statement(&self) -> &ExprLet {
        self.async_let_statement.get_or_init(|| ExprLet {
            attrs: default(),
            let_token: default(),
            pat: Box::new(syn::Pat::Verbatim(self.async_name().into_token_stream())),
            eq_token: default(),
            expr: Box::new(Expr::Closure(syn::ExprClosure {
                attrs: default(),
                lifetimes: default(),
                constness: default(),
                movability: default(),
                asyncness: default(),
                capture: default(),
                or1_token: default(),
                inputs: self.function_arguments_without_self().clone(),
                or2_token: default(),
                output: ReturnType::Default,
                body: Box::new(Expr::Async(syn::ExprAsync {
                    attrs: default(),
                    async_token: default(),
                    capture: Some(default()),
                    block: self.async_function_body().clone(),
                })),
            })),
        })
    }
    fn blocking_name(&self) -> &Ident {
        self.blocking_name
            .get_or_init(|| self.new_ident(Ident::new("blocking_function", Span::mixed_site())))
    }
    fn blocking_let_statement(&self) -> &ExprLet {
        self.blocking_let_statement.get_or_init(|| ExprLet {
            attrs: default(),
            let_token: default(),
            pat: Box::new(syn::Pat::Verbatim(self.blocking_name().into_token_stream())),
            eq_token: default(),
            expr: Box::new(Expr::Closure(syn::ExprClosure {
                attrs: default(),
                lifetimes: default(),
                constness: default(),
                movability: default(),
                asyncness: default(),
                capture: default(),
                or1_token: default(),
                inputs: self.function_arguments_without_self().clone(),
                or2_token: default(),
                output: ReturnType::Default,
                body: Box::new(syn::Expr::Block(syn::ExprBlock {
                    attrs: default(),
                    label: default(),
                    block: self.blocking_body_without_self().clone(),
                })),
            })),
        })
    }
}

struct Asyncable {
    attrs: TokenStream2,
    make_sync: bool,
    make_async: bool,
}

struct PrintFunctionState<'a, 'b: 'a> {
    state: &'a FunctionState<'b>,
    make_async: bool,
    make_blocking: bool,
}

impl ToTokens for PrintFunctionState<'_, '_> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let PrintFunctionState {
            state:
                state @ FunctionState {
                    function:
                        AsyncFunction {
                            visibility,
                            constness,
                            asyncness,
                            unsafety,
                            fn_token,
                            ident,
                            generics,
                            paren_token,
                            inputs,
                            output,
                            where_clause,
                            body,
                            identities,
                        },
                    ..
                },
            make_async,
            make_blocking,
        } = self;
        visibility.to_tokens(tokens);
        constness.to_tokens(tokens);
        if !*make_blocking && *make_async {
            asyncness.to_tokens(tokens);
        }
        unsafety.to_tokens(tokens);
        fn_token.to_tokens(tokens);
        ident.to_tokens(tokens);
        generics.to_tokens(tokens);
        paren_token.surround(tokens, |tokens| inputs.to_tokens(tokens));
        output.to_tokens(tokens);
        where_clause.to_tokens(tokens);
        body.brace_token.surround(tokens, |tokens| {
            if *make_blocking && *make_async {
                let async_let = state.async_let_statement();
                let blocking_let = state.blocking_let_statement();
                let async_var = state.async_name();
                let blocking_var = state.blocking_name();
                // TODO: Add parantesis to args
                let args = state.input_variables();
                quote_spanned!{body.span()=>
                    #async_let;
                    #blocking_let;
                    ::desaturate::IntoDesaturatedWith::desaturate_with(#async_var, #args, #blocking_var)
                }.to_tokens(tokens);
            } else if *make_blocking {
                let blocking_let = state.blocking_let_statement();
                let blocking_var = state.blocking_name();
                quote_spanned!{body.span()=>
                    #blocking_let;
                    #blocking_var
                }.to_tokens(tokens);
            } else if *make_async {
                body.stmts.iter().for_each(|x| x.to_tokens(tokens));
            } else {
                unreachable!()
            }
        });
    }
}

impl Asyncable {
    fn create(attrs: impl Into<TokenStream2>) -> Self {
        let attrs: TokenStream2 = attrs.into();
        Asyncable {
            attrs,
            make_sync: cfg!(feature = "generate-blocking"),
            make_async: cfg!(feature = "generate-async"),
        }
    }
    fn desaturate(&self, item: TokenStream2) -> syn::Result<TokenStream2> {
        match (self.make_async, self.make_sync) {
            (false, false) => Err(syn::Error::new(self.attrs.span(), "desaturate-macros requires one of 'generate-async' or 'generate-non-async' features to be active"))?,
            (true, false) => Ok(item),
            (make_async, make_sync) => {
                let function: AsyncFunction = parse2(item)?;
                let state: FunctionState = (&function).into();
                Ok(PrintFunctionState {
                    state: &state,
                    make_async,
                    make_blocking: make_sync,
                }.into_token_stream())
            },
        }
    }
}

#[proc_macro_attribute]
pub fn desaturate(attr: TokenStream, item: TokenStream) -> TokenStream {
    let handler = Asyncable::create(attr);
    handler
        .desaturate(item.into())
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

#[cfg(test)]
mod tests {
    use super::{default, Asyncable};
    use quote::quote;
    #[test]
    fn async_unchanged() -> syn::Result<()> {
        let handler = Asyncable {
            attrs: default(),
            make_async: true,
            make_sync: false,
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
    #[test]
    fn double_gets_complex() -> syn::Result<()> {
        let handler = Asyncable {
            attrs: default(),
            make_async: true,
            make_sync: true,
        };
        let function = quote! {
            pub async fn do_something(&self, other: i32) -> i32 {
                do_something().await?;
                self.hello();
                call_somewhere().await;
                other * 2
            }
        };
        let expected = function.clone();
        let result = handler.desaturate(function)?;
        eprintln!("{result}");
        assert!(false, "Show result");
        Ok(())
    }
}
