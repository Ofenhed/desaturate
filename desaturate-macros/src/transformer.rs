use proc_macro2::{Span, TokenStream as TokenStream2};
use std::{cell::OnceCell, ops::Deref};

use quote::{quote_spanned, ToTokens};
use syn::{
    parse2, punctuated::Punctuated, spanned::Spanned, visit_mut::VisitMut, Block, Expr, FnArg,
    GenericArgument, Generics, Ident, Lifetime, Pat, Result, ReturnType, Token, Type,
};

use crate::{
    default,
    visitors::{AsyncStripper, DefaultLifetime, MacroFilter, SelfReplacer},
    AsyncFunction, Asyncable,
};

pub(crate) struct FunctionState<'a> {
    pub function: &'a AsyncFunction,
    pub options: &'a Asyncable,
    args_tuple_type: OnceCell<Punctuated<Type, Token![,]>>,
    async_let_statement: OnceCell<syn::ExprLet>,
    async_name: OnceCell<Ident>,
    blocking_body_without_self: OnceCell<Block>,
    blocking_function_body: OnceCell<Block>,
    blocking_let_statement: OnceCell<syn::ExprLet>,
    blocking_name: OnceCell<Ident>,
    async_body_without_self: OnceCell<Block>,
    async_body: OnceCell<Block>,
    desaturated_lifetime: OnceCell<Option<Lifetime>>,
    function_arguments_without_self: OnceCell<Punctuated<Pat, Token![,]>>,
    input_variables: OnceCell<Punctuated<Pat, Token![,]>>,
    new_generics: OnceCell<Generics>,
    new_return_type: OnceCell<Result<ReturnType>>,
    old_return_type: OnceCell<ReturnType>,
    self_new_name: OnceCell<Option<Ident>>,
    simple_input_variables: OnceCell<Punctuated<syn::FnArg, Token![,]>>,
    simple_input_variables_tuple: OnceCell<syn::ExprTuple>,
}

impl<'a> FunctionState<'a> {
    pub fn new(options: &'a Asyncable, function: &'a AsyncFunction) -> Self {
        Self {
            function,
            options,
            args_tuple_type: default(),
            async_let_statement: default(),
            async_name: default(),
            blocking_body_without_self: default(),
            blocking_function_body: default(),
            blocking_let_statement: default(),
            blocking_name: default(),
            async_body_without_self: default(),
            async_body: default(),
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
    pub fn old_return_type(&self) -> &ReturnType {
        self.old_return_type.get_or_init(|| {
            if let ReturnType::Type(arrow, output) = self.output.clone() {
                ReturnType::Type(arrow, output)
            } else {
                ReturnType::Default
            }
        })
    }
    pub fn manual_lifetime(&self) -> Option<&Lifetime> {
        self.options.lifetime.as_ref()
    }
    pub fn desaturated_lifetime(&self) -> Option<&Lifetime> {
        // TODO: Rewrite this to use a Visitor instead
        self.manual_lifetime().or_else(|| {
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
                        .map(|_| Lifetime {
                            apostrophe: self.generics.span(),
                            ident: self.new_ident(Ident::new("desaturated", self.generics.span())),
                        })
                })
                .as_ref()
        })
    }
    pub fn new_return_type(&self) -> &Result<ReturnType> {
        self.new_return_type.get_or_init(|| {
            let output_type = self.old_return_type();
            let (default_paren, default_elems);
            let (arrow, output_type) = if let ReturnType::Type(arrow, output) = output_type {
                match &**output {
                    Type::BareFn(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    Type::ImplTrait(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    Type::Infer(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    Type::Macro(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    Type::Verbatim(x) => Err(syn::Error::new(x.span(), "Unsupported return type")),
                    _ => Ok(()),
                }?;
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
                if self.manual_lifetime().is_none() {
                    bounds.push(syn::TypeParamBound::Lifetime(desaturated_lifetime.clone()));
                }
            }
            let output_type = {
                let mut output = output_type.clone();
                if let Some(desaturated_lifetime) = self.desaturated_lifetime() {
                    let mut set_default = DefaultLifetime(desaturated_lifetime);
                    set_default.visit_type_mut(&mut output);
                }
                output
            };
            bounds.push(syn::TypeParamBound::Trait(syn::TraitBound {
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
                    syn::TypeImplTrait {
                        impl_token: parse2(quote_spanned! {output_type.span() => impl}).unwrap(),
                        bounds,
                    }
                    .into(),
                ),
            );
            Ok(return_type)
        })
    }
    pub fn new_generics(&self) -> &Generics {
        self.new_generics.get_or_init(|| {
            let mut generics = self.generics.clone();
            if self.manual_lifetime().is_some() {
                return generics;
            }
            if let Some(new_lifetime) = self.desaturated_lifetime() {
                generics
                    .lifetimes_mut()
                    .for_each(|lifetime| lifetime.bounds.push(new_lifetime.clone()));
                generics.params.insert(
                    0,
                    syn::GenericParam::Lifetime(syn::LifetimeParam {
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
    pub fn new_return_type_tokens(&self) -> TokenStream2 {
        match self.new_return_type() {
            Ok(v) => v.into_token_stream(),
            Err(e) => e.to_compile_error(),
        }
    }
    pub fn self_new_name(&self) -> Option<&Ident> {
        self.self_new_name
            .get_or_init(|| {
                self.inputs.first().and_then(|first| {
                    if let FnArg::Receiver(syn::Receiver { self_token, .. }) = first {
                        Some(self.new_ident(Ident::new("selfish", self_token.span())))
                    } else {
                        None
                    }
                })
            })
            .as_ref()
    }
    pub fn input_variables(&self) -> &Punctuated<Pat, Token![,]> {
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
                    }) => Pat::Ident(syn::PatIdent {
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
    pub fn simple_input_variables(&self) -> &Punctuated<syn::FnArg, Token![,]> {
        self.simple_input_variables.get_or_init(|| {
            let mut variables = self.function.inputs.clone();
            variables.iter_mut().for_each(|variable| {
                if let FnArg::Typed(typed) = variable {
                    let syn::PatType {
                        attrs,
                        pat,
                        colon_token,
                        ty,
                    } = typed;
                    *typed = syn::PatType {
                        attrs: attrs.clone(),
                        pat: Box::new(Pat::Ident(syn::PatIdent {
                            ident: self.new_ident(Ident::new("argument", pat.span())),
                            attrs: default(),
                            by_ref: default(),
                            mutability: default(),
                            subpat: default(),
                        })),
                        colon_token: *colon_token,
                        ty: ty.clone(),
                    };
                }
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
    pub fn simple_input_variables_tuple(&self) -> &syn::ExprTuple {
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
                        Pat::Ident(syn::PatIdent { attrs, ident, .. }) => {
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
    pub fn args_tuple_type(&self) -> &Punctuated<Type, Token![,]> {
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
                        mutability: *mutability,
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
    pub fn async_body(&self) -> &Block {
        self.async_body.get_or_init(|| {
            let mut body = self.body.clone();
            MacroFilter {
                remove: &self.options.only_blocking_attr.iter().collect::<Vec<_>>(),
                strip: &self.options.only_async_attr.iter().collect::<Vec<_>>(),
            }
            .visit_block_mut(&mut body);
            body
        })
    }
    pub fn async_body_without_self(&self) -> &Block {
        if let Some(self_new_name) = self.self_new_name() {
            self.async_body_without_self.get_or_init(|| {
                let mut body = self.async_body().clone();
                SelfReplacer(self_new_name).visit_block_mut(&mut body);
                body
            })
        } else {
            self.async_body()
        }
    }
    pub fn function_arguments_without_self(&self) -> &Punctuated<Pat, Token![,]> {
        if let Some(new_self_name) = self.self_new_name() {
            self.function_arguments_without_self.get_or_init(|| {
                let mut inputs = self.input_variables().clone();
                let mut visitor = SelfReplacer(new_self_name);
                inputs.iter_mut().for_each(|pat| visitor.visit_pat_mut(pat));
                inputs
            })
        } else {
            self.input_variables()
        }
    }
    pub fn function_arguments_tuple(&self) -> Pat {
        Pat::Tuple(syn::PatTuple {
            attrs: default(),
            paren_token: default(),
            elems: self.function_arguments_without_self().clone(),
        })
    }
    pub fn blocking_function_body(&self) -> &Block {
        self.blocking_function_body.get_or_init(|| {
            let mut body = self.body.clone();
            MacroFilter {
                remove: &self.options.only_async_attr.iter().collect::<Vec<_>>(),
                strip: &self.options.only_blocking_attr.iter().collect::<Vec<_>>(),
            }
            .visit_block_mut(&mut body);
            AsyncStripper.visit_block_mut(&mut body);
            body
        })
    }
    pub fn blocking_body_without_self(&self) -> &Block {
        if let Some(new_self_name) = self.self_new_name() {
            self.blocking_body_without_self.get_or_init(|| {
                let mut body = self.blocking_function_body().clone();
                SelfReplacer(new_self_name).visit_block_mut(&mut body);
                body
            })
        } else {
            self.blocking_function_body()
        }
    }
    pub fn async_name(&self) -> &Ident {
        self.async_name
            .get_or_init(|| self.new_ident(Ident::new("async_function", Span::mixed_site())))
    }
    pub fn async_let_statement(&self) -> &syn::ExprLet {
        self.async_let_statement.get_or_init(|| syn::ExprLet {
            attrs: default(),
            let_token: default(),
            pat: Box::new(Pat::Verbatim(self.async_name().into_token_stream())),
            eq_token: default(),
            expr: Box::new(Expr::Closure(syn::ExprClosure {
                attrs: default(),
                lifetimes: default(),
                constness: default(),
                movability: default(),
                asyncness: default(),
                capture: default(),
                or1_token: default(),
                inputs: [Pat::Type(syn::PatType {
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
                    block: self.async_body_without_self().clone(),
                })),
            })),
        })
    }
    pub fn blocking_name(&self) -> &Ident {
        self.blocking_name
            .get_or_init(|| self.new_ident(Ident::new("blocking_function", Span::mixed_site())))
    }
    pub fn blocking_let_statement(&self) -> &syn::ExprLet {
        self.blocking_let_statement.get_or_init(|| syn::ExprLet {
            attrs: default(),
            let_token: default(),
            pat: Box::new(Pat::Verbatim(self.blocking_name().into_token_stream())),
            eq_token: default(),
            expr: Box::new(Expr::Closure(syn::ExprClosure {
                attrs: default(),
                lifetimes: default(),
                constness: default(),
                movability: default(),
                asyncness: default(),
                capture: default(),
                or1_token: default(),
                inputs: [Pat::Type(syn::PatType {
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
}
