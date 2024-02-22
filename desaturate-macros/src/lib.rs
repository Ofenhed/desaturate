use std::{cell::OnceCell, collections::HashSet, ops::Deref, sync::Mutex};

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote_spanned, ToTokens};
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    parse2,
    punctuated::Punctuated,
    spanned::Spanned,
    token,
    visit::{self, Visit},
    visit_mut::{self, VisitMut},
    Block, Expr, ExprLet, FnArg, GenericArgument, GenericParam, Generics, Ident, Lifetime,
    ReturnType, Token, Type, TypeImplTrait, TypeParamBound, Visibility, WhereClause,
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

struct SelfReplacer<'a>(&'a str);
impl VisitMut for SelfReplacer<'_> {
    fn visit_item_struct_mut(&mut self, _: &mut syn::ItemStruct) {}
    fn visit_item_enum_mut(&mut self, _: &mut syn::ItemEnum) {}
    fn visit_item_impl_mut(&mut self, _: &mut syn::ItemImpl) {}
    fn visit_item_trait_mut(&mut self, _: &mut syn::ItemTrait) {}
    fn visit_ident_mut(&mut self, ident: &mut Ident) {
        if ident == "self" {
            *ident = Ident::new(self.0, ident.span())
        } else {
            visit_mut::visit_ident_mut(self, ident);
        }
    }
}

struct DefaultLifetime<'a>(&'a Lifetime);
impl VisitMut for DefaultLifetime<'_> {
    fn visit_type_reference_mut(&mut self, data: &mut syn::TypeReference) {
        if let elided_lifetime @ None = &mut data.lifetime {
            *elided_lifetime = Some(self.0.clone());
        }
        visit_mut::visit_type_reference_mut(self, data);
    }
    fn visit_receiver_mut(&mut self, data: &mut syn::Receiver) {
        if let Some((_, elided_lifetime @ None)) = &mut data.reference {
            *elided_lifetime = Some(self.0.clone());
        }
        visit_mut::visit_receiver_mut(self, data);
    }
}

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
            let mut counter = 1;
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
    args_tuple_type: OnceCell<Punctuated<Type, Token![,]>>,
    async_let_statement: OnceCell<ExprLet>,
    async_name: OnceCell<Ident>,
    blocking_body_without_self: OnceCell<Block>,
    blocking_let_statement: OnceCell<ExprLet>,
    blocking_name: OnceCell<Ident>,
    body_without_self: OnceCell<Block>,
    desaturated_lifetime: OnceCell<Option<syn::Lifetime>>,
    function_arguments_without_self: OnceCell<Punctuated<syn::Pat, Token![,]>>,
    input_variables: OnceCell<Punctuated<syn::Pat, Token![,]>>,
    new_generics: OnceCell<Generics>,
    new_return_type: OnceCell<syn::Result<ReturnType>>,
    old_return_type: OnceCell<ReturnType>,
    self_new_name: OnceCell<Option<Ident>>,
    simple_input_variables: OnceCell<Punctuated<syn::FnArg, Token![,]>>,
    simple_input_variables_tuple: OnceCell<syn::ExprTuple>,
}

impl<'a> From<&'a AsyncFunction> for FunctionState<'a> {
    fn from(function: &'a AsyncFunction) -> Self {
        Self {
            function,
            args_tuple_type: default(),
            async_let_statement: default(),
            async_name: default(),
            blocking_body_without_self: default(),
            blocking_let_statement: default(),
            blocking_name: default(),
            body_without_self: default(),
            desaturated_lifetime: default(),
            function_arguments_without_self: default(),
            input_variables: default(),
            new_generics: default(),
            new_return_type: default(),
            old_return_type: default(),
            self_new_name: default(),
            simple_input_variables: default(),
            simple_input_variables_tuple: default(),
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
    fn desaturated_lifetime(&self) -> Option<&syn::Lifetime> {
        self.desaturated_lifetime
            .get_or_init(|| {
                self.inputs
                    .iter()
                    .find(|x| match x {
                        FnArg::Receiver(syn::Receiver {
                            reference: Some(_), ..
                        }) => true,
                        FnArg::Typed(syn::PatType { ty, .. }) => {
                            matches!(**ty, syn::Type::Reference(_))
                        }
                        _ => false,
                    })
                    .map(|_| syn::Lifetime {
                        apostrophe: self.generics.span(),
                        ident: self.new_ident(Ident::new("desaturated", self.generics.span())),
                    })
            })
            .as_ref()
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
                    _ => Ok(()),
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
            if let Some(desaturated_lifetime) = self.desaturated_lifetime() {
                bounds.push(TypeParamBound::Lifetime(desaturated_lifetime.clone()));
            }
            bounds.push(TypeParamBound::Trait(syn::TraitBound {
                paren_token: default(),
                modifier: syn::TraitBoundModifier::None,
                lifetimes: default(),
                path: syn::Path {
                    leading_colon: Some(default()),
                    segments: [
                        syn::PathSegment {
                            ident: Ident::new("desaturate", Span::call_site()),
                            arguments: default(),
                        },
                        syn::PathSegment {
                            ident: Ident::new("Desaturated", Span::call_site()),
                            arguments: syn::PathArguments::AngleBracketed(
                                syn::AngleBracketedGenericArguments {
                                    colon2_token: default(),
                                    lt_token: default(),
                                    args: [GenericArgument::Type(output_type.clone())]
                                        .into_iter()
                                        .collect(),
                                    gt_token: default(),
                                },
                            ),
                        },
                    ]
                    .into_iter()
                    .collect(),
                },
            }));
            let return_type = ReturnType::Type(
                *arrow,
                Box::new(
                    TypeImplTrait {
                        impl_token: parse2(quote_spanned! {output_type.span() => impl}).unwrap(),
                        bounds,
                    }
                    .into(),
                ),
            );
            Ok(return_type)
        })
    }
    fn new_generics(&self) -> &Generics {
        self.new_generics.get_or_init(|| {
            let mut generics = self.generics.clone();
            if let Some(new_lifetime) = self.desaturated_lifetime() {
                generics
                    .lifetimes_mut()
                    .for_each(|lifetime| lifetime.bounds.push(new_lifetime.clone()));
                generics.params.insert(
                    0,
                    GenericParam::Lifetime(syn::LifetimeParam {
                        attrs: default(),
                        lifetime: new_lifetime.clone(),
                        colon_token: default(),
                        bounds: default(),
                    }),
                );
            }
            generics
        })
    }
    fn new_return_type_tokens(&self) -> TokenStream2 {
        match self.new_return_type() {
            Ok(v) => v.into_token_stream(),
            Err(e) => e.to_compile_error(),
        }
    }
    fn self_new_name(&self) -> Option<&Ident> {
        self.self_new_name
            .get_or_init(|| {
                self.inputs
                    .first()
                    .map(|first| {
                        if let FnArg::Receiver(syn::Receiver { self_token, .. }) = first {
                            Some(self.new_ident(Ident::new("selfish", self_token.span())))
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
                    FnArg::Receiver(syn::Receiver {
                        self_token,
                        attrs,
                        reference: _,
                        mutability: _,
                        colon_token: _,
                        ty: _,
                    }) => syn::Pat::Ident(syn::PatIdent {
                        attrs: attrs.clone(),
                        by_ref: default(),
                        mutability: default(),
                        ident: Ident::new("self", self_token.span()),
                        subpat: default(),
                    }),
                    FnArg::Typed(typed) => *typed.pat.clone(),
                })
                .collect()
        })
    }
    fn simple_input_variables(&self) -> &Punctuated<syn::FnArg, Token![,]> {
        self.simple_input_variables.get_or_init(|| {
            let mut variables = self.function.inputs.clone();
            variables.iter_mut().for_each(|variable| match variable {
                FnArg::Typed(typed) => {
                    let syn::PatType {
                        attrs,
                        pat,
                        colon_token,
                        ty,
                    } = typed;
                    *typed = syn::PatType {
                        attrs: attrs.clone(),
                        pat: Box::new(syn::Pat::Ident(syn::PatIdent {
                            ident: self.new_ident(Ident::new("argument", pat.span())),
                            attrs: default(),
                            by_ref: default(),
                            mutability: default(),
                            subpat: default(),
                        })),
                        colon_token: colon_token.clone(),
                        ty: ty.clone(),
                    };
                }
                _ => (),
            });
            if let Some(desaturated_lifetime) = self.desaturated_lifetime() {
                let mut replacer = DefaultLifetime(desaturated_lifetime);
                variables
                    .iter_mut()
                    .for_each(|arg| replacer.visit_fn_arg_mut(arg));
            }
            variables
        })
    }
    fn simple_input_variables_tuple(&self) -> &syn::ExprTuple {
        self.simple_input_variables_tuple.get_or_init(|| {
            let variables = self
                .simple_input_variables()
                .iter()
                .map(|variable| match variable {
                    FnArg::Receiver(syn::Receiver {
                        attrs, self_token, ..
                    }) => syn::Expr::Path(syn::ExprPath {
                        attrs: attrs.clone(),
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: [syn::PathSegment {
                                ident: Ident::new("self", self_token.span()),
                                arguments: syn::PathArguments::None,
                            }]
                            .into_iter()
                            .collect(),
                        },
                    }),
                    FnArg::Typed(syn::PatType { pat, .. }) => match &**pat {
                        syn::Pat::Ident(syn::PatIdent { attrs, ident, .. }) => {
                            syn::Expr::Path(syn::ExprPath {
                                attrs: attrs.clone(),
                                qself: default(),
                                path: syn::Path {
                                    leading_colon: None,
                                    segments: [syn::PathSegment {
                                        ident: ident.clone(),
                                        arguments: syn::PathArguments::None,
                                    }]
                                    .into_iter()
                                    .collect(),
                                },
                            })
                        }
                        _ => unreachable!("simple_input_variables will only use the PatIdent"),
                    },
                })
                .collect();
            syn::ExprTuple {
                attrs: default(),
                paren_token: default(),
                elems: variables,
            }
        })
    }
    fn args_tuple_type(&self) -> &Punctuated<Type, Token![,]> {
        self.args_tuple_type.get_or_init(|| {
            fn into_type<'f, 'a: 'f>(this: &'a FunctionState) -> impl Fn(&'f mut Type) {
                |input| match input {
                    Type::Array(syn::TypeArray { elem, .. }) => into_type(this)(elem),
                    Type::Group(syn::TypeGroup { elem, .. }) => into_type(this)(elem),
                    Type::Paren(syn::TypeParen { elem, .. }) => into_type(this)(elem),
                    Type::Reference(type_ref @ syn::TypeReference { .. }) => {
                        match &mut type_ref.lifetime {
                            none @ None => {
                                *none = this.desaturated_lifetime().cloned();
                            }
                            Some(_) => (),
                        }
                        into_type(this)(&mut type_ref.elem)
                    }
                    Type::Slice(syn::TypeSlice { elem, .. }) => into_type(this)(elem),
                    Type::Tuple(syn::TypeTuple { elems, .. }) => {
                        elems.iter_mut().for_each(into_type(this))
                    }
                    _ => (),
                }
            }
            self.inputs
                .iter()
                .map(|input| match input {
                    // TODO: Check attributes
                    FnArg::Receiver(syn::Receiver {
                        attrs: _, // TODO: Can we respect this attribute?
                        reference: Some((and, lifetime)),
                        mutability,
                        self_token,
                        ..
                    }) => Type::Reference(syn::TypeReference {
                        and_token: *and,
                        lifetime: lifetime
                            .as_ref()
                            .or_else(|| self.desaturated_lifetime())
                            .cloned(),
                        mutability: mutability.clone(),
                        elem: Box::new(Type::Path(syn::TypePath {
                            qself: default(),
                            path: syn::Path {
                                leading_colon: default(),
                                segments: [syn::PathSegment {
                                    ident: Ident::new("Self", self_token.span()),
                                    arguments: default(),
                                }]
                                .into_iter()
                                .collect(),
                            },
                        })),
                    }),
                    FnArg::Receiver(syn::Receiver {
                        attrs: _, // TODO: Can we respect this attribute?
                        self_token,
                        ..
                    }) => Type::Path(syn::TypePath {
                        qself: default(),
                        path: syn::Path {
                            leading_colon: default(),
                            segments: [syn::PathSegment {
                                ident: Ident::new("Self", self_token.span()),
                                arguments: default(),
                            }]
                            .into_iter()
                            .collect(),
                        },
                    }),
                    FnArg::Typed(syn::PatType { ty, .. }) => {
                        let mut ty = *ty.clone();
                        into_type(self)(&mut ty);
                        ty
                    }
                })
                .collect()
        })
    }
    fn body_without_self(&self) -> &Block {
        if let Some(new_self_name) = self.self_new_name() {
            self.body_without_self.get_or_init(|| {
                let mut body = self.body.clone();
                let new_self_name = format!("{new_self_name}");
                let mut visitor = SelfReplacer(&new_self_name);
                visitor.visit_block_mut(&mut body);
                body
            })
        } else {
            &self.body
        }
    }
    fn function_arguments_without_self(&self) -> &Punctuated<syn::Pat, Token![,]> {
        if let Some(new_self_name) = self.self_new_name() {
            self.function_arguments_without_self.get_or_init(|| {
                let mut inputs = self.input_variables().clone();
                let new_self_name = format!("{new_self_name}");
                let mut visitor = SelfReplacer(&new_self_name);
                inputs.iter_mut().for_each(|pat| visitor.visit_pat_mut(pat));
                inputs
            })
        } else {
            self.input_variables()
        }
    }
    fn function_arguments_tuple(&self) -> syn::Pat {
        syn::Pat::Tuple(syn::PatTuple {
            attrs: default(),
            paren_token: default(),
            elems: self.function_arguments_without_self().clone(),
        })
    }
    fn blocking_body_without_self(&self) -> &Block {
        self.blocking_body_without_self.get_or_init(|| {
            struct AsyncStripper;
            impl VisitMut for AsyncStripper {
                fn visit_expr_async_mut(&mut self, _: &mut syn::ExprAsync) {}
                fn visit_expr_await_mut(&mut self, awaited: &mut syn::ExprAwait) {
                    visit_mut::visit_expr_await_mut(self, awaited)
                }
                fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
                    match expr {
                        syn::Expr::Await(syn::ExprAwait {
                            attrs,
                            base,
                            dot_token: _,
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
                inputs: [syn::Pat::Type(syn::PatType {
                    attrs: default(),
                    pat: Box::new(self.function_arguments_tuple()),
                    colon_token: default(),
                    ty: Box::new(syn::Type::Tuple(syn::TypeTuple {
                        paren_token: default(),
                        elems: self.args_tuple_type().clone(),
                    })),
                })]
                .into_iter()
                .collect(),
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
                inputs: [syn::Pat::Type(syn::PatType {
                    attrs: default(),
                    pat: Box::new(self.function_arguments_tuple()),
                    colon_token: default(),
                    ty: Box::new(syn::Type::Tuple(syn::TypeTuple {
                        paren_token: default(),
                        elems: self.args_tuple_type().clone(),
                    })),
                })]
                .into_iter()
                .collect(),
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
    //fn args_name(&self) -> &Ident {
    //    self.args_name.get_or_init(|| self.new_ident(Ident::new("args", self.body.span())))
    //}
    //fn args_let_statement(&self) -> &ExprLet {
    //    self.args_let_statement.get_or_init(|| ExprLet {
    //        attrs: default(),
    //        let_token: default(),
    //        pat: Box::new(syn::Pat::Type(syn::PatType {
    //            attrs: default(),
    //            pat: Box::new(syn::Pat::Ident(syn::PatIdent {
    //                attrs: default(),
    //                by_ref: default(),
    //                mutability: default(),
    //                ident: self.args_name().clone(),
    //                subpat: default(),
    //            })),
    //            colon_token: default(),
    //            ty: Box::new(syn::Type::Tuple(syn::TypeTuple {
    //                paren_token: default(),
    //                elems: self.args_tuple_type().clone(),
    //            })),
    //        })),
    //        eq_token: default(),
    //        expr: Box::new(syn::Expr::Tuple(self.simple_input_variables_tuple().clone())),
    //    })
    //}
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
                eprintln!("Generating async({make_async}) and blocking({make_sync})");
                let function: AsyncFunction = parse2(item)?;
                let state: FunctionState = (&function).into();
                let result = PrintFunctionState {
                    state: &state,
                    make_async,
                    make_blocking: make_sync,
                }.into_token_stream();
                eprintln!("Rendered code {result}");
                Ok(result)
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
    fn only_async_unchanged() -> syn::Result<()> {
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
}
