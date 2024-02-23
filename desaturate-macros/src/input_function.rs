use std::{collections::HashSet, sync::Mutex};

use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    token,
    visit::Visit,
    Block, FnArg, Generics, Ident, ReturnType, Token, Visibility, WhereClause,
};

use crate::{default, visitors::IdentityVisitor};

pub(crate) struct AsyncFunction {
    pub visibility: Visibility,
    pub constness: Option<Token![const]>,
    pub asyncness: Token![async],
    pub unsafety: Option<Token![unsafe]>,
    pub fn_token: Token![fn],
    pub ident: Ident,
    pub generics: Generics,
    pub paren_token: token::Paren,
    pub inputs: Punctuated<FnArg, Token![,]>,
    pub output: ReturnType,
    pub where_clause: Option<WhereClause>,
    pub body: Block,
    pub identities: Mutex<HashSet<String>>,
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
    pub fn new_ident(&self, ident: Ident) -> Ident {
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
