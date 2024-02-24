use crate::default;
use std::collections::HashSet;
use syn::{spanned::Spanned, visit, visit_mut, Ident, punctuated::Punctuated, Token};
pub use syn::{visit::Visit, visit_mut::VisitMut};

#[derive(Default)]
pub struct IdentityVisitor(pub HashSet<String>);

impl<'ast> Visit<'ast> for IdentityVisitor {
    fn visit_ident(&mut self, ident: &'ast Ident) {
        self.0.insert(format!("{}", ident));
        visit::visit_ident(self, ident);
    }
}

pub struct SelfReplacer<'a>(pub &'a Ident);
impl VisitMut for SelfReplacer<'_> {
    fn visit_item_struct_mut(&mut self, _: &mut syn::ItemStruct) {}
    fn visit_item_enum_mut(&mut self, _: &mut syn::ItemEnum) {}
    fn visit_item_impl_mut(&mut self, _: &mut syn::ItemImpl) {}
    fn visit_item_trait_mut(&mut self, _: &mut syn::ItemTrait) {}
    fn visit_ident_mut(&mut self, ident: &mut Ident) {
        if ident == "self" {
            *ident = self.0.clone()
        } else {
            visit_mut::visit_ident_mut(self, ident);
        }
    }
}

pub struct DefaultLifetime<'a>(pub &'a syn::Lifetime);
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

pub struct AsyncStripper;
impl VisitMut for AsyncStripper {
    fn visit_expr_async_mut(&mut self, _: &mut syn::ExprAsync) {}
    fn visit_expr_await_mut(&mut self, awaited: &mut syn::ExprAwait) {
        visit_mut::visit_expr_await_mut(self, awaited)
    }
    fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
        if let syn::Expr::Await(syn::ExprAwait {
            attrs,
            base,
            dot_token: _,
            await_token,
        }) = expr {
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
        visit_mut::visit_expr_mut(self, expr)
    }
}

pub struct MacroFilter<'a, 'b> {
    pub remove: &'a [&'b Ident],
    pub strip: &'a [&'b Ident],
}
impl VisitMut for MacroFilter<'_, '_> {
    fn visit_expr_mut(&mut self, expr: &mut syn::Expr) {
        enum Filter {
            Ignore,
            Remove,
            Strip,
        }
        let action = |check: &Punctuated<syn::PathSegment, Token![::]>| -> Filter {
            if check.len() != 1 {
                Filter::Ignore
            } else if let Some(first) = check.first() {
                if !first.arguments.is_empty() {
                    Filter::Ignore
                } else if self.remove.iter().any(|x| **x == first.ident) {
                    Filter::Remove
                } else if self.strip.iter().any(|x| **x == first.ident) {
                    Filter::Strip
                } else {
                    Filter::Ignore
                }
            } else {
                Filter::Ignore
            }
        };
        match expr {
            syn::Expr::Block(syn::ExprBlock {
                attrs,
                ..
            }) | syn::Expr::Tuple(syn::ExprTuple {
                attrs,
                ..
            }) => {
                attrs.retain(|attr| {
                    if let syn::Meta::Path(syn::Path {
                        leading_colon: None,
                        segments,
                    }) = &attr.meta {
                        !matches!(action(segments), Filter::Strip)
                    } else {
                        true
                    }
                });
                if attrs.iter().any(|attribute| if let syn::Attribute { meta: syn::Meta::Path(syn::Path {
                    leading_colon: None,
                    segments,
                }), .. } = attribute { matches!{action(segments), Filter::Remove} } else { false }) {
                    *expr = syn::Expr::Verbatim(default());
                    return
                }
            },
            _ => (),
        }
        visit_mut::visit_expr_mut(self, expr);
    }
}
