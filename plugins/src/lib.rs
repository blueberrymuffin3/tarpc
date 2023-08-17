// Copyright 2018 Google LLC
//
// Use of this source code is governed by an MIT-style
// license that can be found in the LICENSE file or at
// https://opensource.org/licenses/MIT.

#![recursion_limit = "512"]

extern crate proc_macro;
extern crate proc_macro2;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{format_ident, quote, quote_spanned, ToTokens};
use syn::{
    braced,
    ext::IdentExt,
    parenthesized,
    parse::{Parse, ParseStream},
    parse_macro_input, parse_quote, parse_str,
    spanned::Spanned,
    token::Comma,
    Attribute, FnArg, Ident, ImplItem, ImplItemMethod, ImplItemType, ItemImpl, Lit, LitBool,
    MetaNameValue, Pat, PatType, ReturnType, Token, Type, Visibility,
};

/// Accumulates multiple errors into a result.
/// Only use this for recoverable errors, i.e. non-parse errors. Fatal errors should early exit to
/// avoid further complications.
fn extend_errors<T>(errors: &mut syn::Result<T>, error: syn::Error) {
    match errors {
        Ok(_) => *errors = Err(error),
        Err(errors) => errors.extend(error),
    }
}

struct Service {
    attrs: Vec<Attribute>,
    vis: Visibility,
    ident: Ident,
    rpcs: Vec<RpcMethod>,
}

struct RpcMethod {
    attrs: Vec<Attribute>,
    ident: Ident,
    args: Vec<PatType>,
    output: ReturnType,
}

impl Parse for Service {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let vis = input.parse()?;
        input.parse::<Token![trait]>()?;
        let ident: Ident = input.parse()?;
        let content;
        braced!(content in input);
        let mut rpcs = Vec::<RpcMethod>::new();
        while !content.is_empty() {
            rpcs.push(content.parse()?);
        }
        let mut ident_errors = Ok(());
        for rpc in &rpcs {
            if rpc.ident == "new" {
                extend_errors(
                    &mut ident_errors,
                    syn::Error::new(
                        rpc.ident.span(),
                        format!(
                            "method name conflicts with generated fn `{}Client::new`",
                            ident.unraw()
                        ),
                    ),
                );
            }
            if rpc.ident == "serve" {
                extend_errors(
                    &mut ident_errors,
                    syn::Error::new(
                        rpc.ident.span(),
                        format!("method name conflicts with generated fn `{ident}::serve`"),
                    ),
                );
            }
        }
        ident_errors?;

        Ok(Self {
            attrs,
            vis,
            ident,
            rpcs,
        })
    }
}

impl Parse for RpcMethod {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        input.parse::<Token![async]>()?;
        input.parse::<Token![fn]>()?;
        let ident = input.parse()?;
        let content;
        parenthesized!(content in input);
        let mut args = Vec::new();
        let mut errors = Ok(());
        for arg in content.parse_terminated::<FnArg, Comma>(FnArg::parse)? {
            match arg {
                FnArg::Typed(captured) if matches!(&*captured.pat, Pat::Ident(_)) => {
                    args.push(captured);
                }
                FnArg::Typed(captured) => {
                    extend_errors(
                        &mut errors,
                        syn::Error::new(captured.pat.span(), "patterns aren't allowed in RPC args"),
                    );
                }
                FnArg::Receiver(_) => {
                    extend_errors(
                        &mut errors,
                        syn::Error::new(arg.span(), "method args cannot start with self"),
                    );
                }
            }
        }
        errors?;
        let output = input.parse()?;
        input.parse::<Token![;]>()?;

        Ok(Self {
            attrs,
            ident,
            args,
            output,
        })
    }
}

// If `derive_serde` meta item is not present, defaults to cfg!(feature = "serde1").
// `derive_serde` can only be true when serde1 is enabled.
#[derive(Default)]
struct Derives {
    ts: proc_macro2::TokenStream,
    derive_serde: Option<bool>,
}

impl Derives {
    fn parse_meta_item(result: &mut syn::Result<Self>, span: Span, name: &Ident, value: &Lit) {
        if name == "derive_serde" {
            match value {
                Lit::Bool(LitBool { value: true, .. }) if !cfg!(feature = "serde1") => {
                    extend_errors(
                        result,
                        syn::Error::new(
                            span,
                            "To enable serde, first enable the `serde1` feature of tarpc",
                        ),
                    );
                }
                Lit::Bool(LitBool { value, .. }) => {
                    if let Ok(result_ok) = result {
                        match result_ok.derive_serde {
                            None => result_ok.derive_serde = Some(*value),
                            Some(_) => extend_errors(
                                result,
                                syn::Error::new(span, "`derive_serde` appears more than once"),
                            ),
                        }
                    }
                }
                _ => extend_errors(
                    result,
                    syn::Error::new(
                        value.span(),
                        "`derive_serde` expects a value of type `bool`",
                    ),
                ),
            }
        } else if name == "derive" {
            match value {
                Lit::Str(lit_str) => {
                    if let Ok(result) = result {
                        let derive_ident = Ident::new(&lit_str.value(), lit_str.span());

                        result.ts.extend(quote_spanned!(span =>
                            #[derive(#derive_ident)]
                        ));
                    }
                }
                _ => extend_errors(result, syn::Error::new(span, "derive must be a string")),
            }
        } else {
            extend_errors(
                result,
                syn::Error::new(span, "tarpc::service does not support this meta item"),
            );
        }
    }

    fn into_ts(mut self) -> proc_macro2::TokenStream {
        if self.derive_serde.unwrap_or(cfg!(feature = "serde1")) {
            self.ts.extend(
                quote! {#[derive(tarpc::serde::Serialize, tarpc::serde::Deserialize)]
                #[serde(crate = "tarpc::serde")]},
            )
        }

        self.ts
    }
}

impl Parse for Derives {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut result = Ok(Default::default());

        let meta_items = input.parse_terminated::<MetaNameValue, Comma>(MetaNameValue::parse)?;
        for meta in meta_items {
            match (meta.path.segments.first(), meta.path.segments.len()) {
                (Some(segment), 1) => {
                    Self::parse_meta_item(&mut result, meta.span(), &segment.ident, &meta.lit)
                }
                _ => extend_errors(
                    &mut result,
                    syn::Error::new(meta.span(), "malformed meta item"),
                ),
            }
        }

        result
    }
}

/// A helper attribute to avoid a direct dependency on Serde.
///
/// Adds the following annotations to the annotated item:
///
/// ```rust
/// #[derive(tarpc::serde::Serialize, tarpc::serde::Deserialize)]
/// #[serde(crate = "tarpc::serde")]
/// # struct Foo;
/// ```
#[proc_macro_attribute]
pub fn derive_serde(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut gen: proc_macro2::TokenStream = quote! {
        #[derive(tarpc::serde::Serialize, tarpc::serde::Deserialize)]
        #[serde(crate = "tarpc::serde")]
    };
    gen.extend(proc_macro2::TokenStream::from(item));
    proc_macro::TokenStream::from(gen)
}

/// Generates:
/// - service trait
/// - serve fn
/// - client stub struct
/// - new_stub client factory fn
/// - Request and Response enums
/// - ResponseFut Future
#[proc_macro_attribute]
pub fn service(attr: TokenStream, input: TokenStream) -> TokenStream {
    let derives = parse_macro_input!(attr as Derives);
    let unit_type: &Type = &parse_quote!(());
    let Service {
        ref attrs,
        ref vis,
        ref ident,
        ref rpcs,
    } = parse_macro_input!(input as Service);

    let camel_case_fn_names: &Vec<_> = &rpcs
        .iter()
        .map(|rpc| snake_to_camel(&rpc.ident.unraw().to_string()))
        .collect();
    let args: &[&[PatType]] = &rpcs.iter().map(|rpc| &*rpc.args).collect::<Vec<_>>();
    let response_fut_name = &format!("{}ResponseFut", ident.unraw());

    let methods = rpcs.iter().map(|rpc| &rpc.ident).collect::<Vec<_>>();
    let request_names = methods
        .iter()
        .map(|m| format!("{ident}.{m}"))
        .collect::<Vec<_>>();

    ServiceGenerator {
        response_fut_name,
        service_ident: ident,
        server_ident: &format_ident!("Serve{}", ident),
        response_fut_ident: &Ident::new(response_fut_name, ident.span()),
        client_ident: &format_ident!("{}Client", ident),
        request_ident: &format_ident!("{}Request", ident),
        response_ident: &format_ident!("{}Response", ident),
        vis,
        args,
        method_attrs: &rpcs.iter().map(|rpc| &*rpc.attrs).collect::<Vec<_>>(),
        method_idents: &methods,
        request_names: &request_names,
        attrs,
        rpcs,
        return_types: &rpcs
            .iter()
            .map(|rpc| match rpc.output {
                ReturnType::Type(_, ref ty) => ty,
                ReturnType::Default => unit_type,
            })
            .collect::<Vec<_>>(),
        arg_pats: &args
            .iter()
            .map(|args| args.iter().map(|arg| &*arg.pat).collect())
            .collect::<Vec<_>>(),
        camel_case_idents: &rpcs
            .iter()
            .zip(camel_case_fn_names.iter())
            .map(|(rpc, name)| Ident::new(name, rpc.ident.span()))
            .collect::<Vec<_>>(),
        future_types: &camel_case_fn_names
            .iter()
            .map(|name| parse_str(&format!("{name}Fut")).unwrap())
            .collect::<Vec<_>>(),
        derive_serialize: &derives.into_ts(),
    }
    .into_token_stream()
    .into()
}

/// generate an identifier consisting of the method name to CamelCase with
/// Fut appended to it.
fn associated_type_for_rpc(method: &ImplItemMethod) -> String {
    snake_to_camel(&method.sig.ident.unraw().to_string()) + "Fut"
}

/// Transforms an async function into a sync one, returning a type declaration
/// for the return type (a future).
fn transform_method(method: &mut ImplItemMethod) -> ImplItemType {
    method.sig.asyncness = None;

    // get either the return type or ().
    let ret = match &method.sig.output {
        ReturnType::Default => quote!(()),
        ReturnType::Type(_, ret) => quote!(#ret),
    };

    let fut_name = associated_type_for_rpc(method);
    let fut_name_ident = Ident::new(&fut_name, method.sig.ident.span());

    // generate the updated return signature.
    method.sig.output = parse_quote! {
        -> ::core::pin::Pin<Box<
                dyn ::core::future::Future<Output = #ret> + ::core::marker::Send
            >>
    };

    // transform the body of the method into Box::pin(async move { body }).
    let block = method.block.clone();
    method.block = parse_quote! [{
        Box::pin(async move
            #block
        )
    }];

    // generate and return type declaration for return type.
    let t: ImplItemType = parse_quote! {
        type #fut_name_ident = ::core::pin::Pin<Box<dyn ::core::future::Future<Output = #ret> + ::core::marker::Send>>;
    };

    t
}

#[proc_macro_attribute]
pub fn server(_attr: TokenStream, input: TokenStream) -> TokenStream {
    let mut item = syn::parse_macro_input!(input as ItemImpl);
    let span = item.span();

    // the generated type declarations
    let mut types: Vec<ImplItemType> = Vec::new();
    let mut expected_non_async_types: Vec<(&ImplItemMethod, String)> = Vec::new();
    let mut found_non_async_types: Vec<&ImplItemType> = Vec::new();

    for inner in &mut item.items {
        match inner {
            ImplItem::Method(method) => {
                if method.sig.asyncness.is_some() {
                    // if this function is declared async, transform it into a regular function
                    let typedecl = transform_method(method);
                    types.push(typedecl);
                } else {
                    // If it's not async, keep track of all required associated types for better
                    // error reporting.
                    expected_non_async_types.push((method, associated_type_for_rpc(method)));
                }
            }
            ImplItem::Type(typedecl) => found_non_async_types.push(typedecl),
            _ => {}
        }
    }

    if let Err(e) =
        verify_types_were_provided(span, &expected_non_async_types, &found_non_async_types)
    {
        return TokenStream::from(e.to_compile_error());
    }

    // add the type declarations into the impl block
    for t in types.into_iter() {
        item.items.push(syn::ImplItem::Type(t));
    }

    TokenStream::from(quote!(#item))
}

fn verify_types_were_provided(
    span: Span,
    expected: &[(&ImplItemMethod, String)],
    provided: &[&ImplItemType],
) -> syn::Result<()> {
    let mut result = Ok(());
    for (method, expected) in expected {
        if !provided.iter().any(|typedecl| typedecl.ident == expected) {
            let mut e = syn::Error::new(
                span,
                format!("not all trait items implemented, missing: `{expected}`"),
            );
            let fn_span = method.sig.fn_token.span();
            e.extend(syn::Error::new(
                fn_span.join(method.sig.ident.span()).unwrap_or(fn_span),
                format!(
                    "hint: `#[tarpc::server]` only rewrites async fns, and `fn {}` is not async",
                    method.sig.ident
                ),
            ));
            match result {
                Ok(_) => result = Err(e),
                Err(ref mut error) => error.extend(Some(e)),
            }
        }
    }
    result
}

// Things needed to generate the service items: trait, serve impl, request/response enums, and
// the client stub.
struct ServiceGenerator<'a> {
    service_ident: &'a Ident,
    server_ident: &'a Ident,
    response_fut_ident: &'a Ident,
    response_fut_name: &'a str,
    client_ident: &'a Ident,
    request_ident: &'a Ident,
    response_ident: &'a Ident,
    vis: &'a Visibility,
    attrs: &'a [Attribute],
    rpcs: &'a [RpcMethod],
    camel_case_idents: &'a [Ident],
    future_types: &'a [Type],
    method_idents: &'a [&'a Ident],
    request_names: &'a [String],
    method_attrs: &'a [&'a [Attribute]],
    args: &'a [&'a [PatType]],
    return_types: &'a [&'a Type],
    arg_pats: &'a [Vec<&'a Pat>],
    derive_serialize: &'a TokenStream2,
}

impl<'a> ServiceGenerator<'a> {
    fn trait_service(&self) -> TokenStream2 {
        let &Self {
            attrs,
            rpcs,
            vis,
            future_types,
            return_types,
            service_ident,
            server_ident,
            ..
        } = self;

        let types_and_fns = rpcs
            .iter()
            .zip(future_types.iter())
            .zip(return_types.iter())
            .map(
                |(
                    (
                        RpcMethod {
                            attrs, ident, args, ..
                        },
                        future_type,
                    ),
                    output,
                )| {
                    let ty_doc = format!("The response future returned by [`{service_ident}::{ident}`].");
                    quote! {
                        #[doc = #ty_doc]
                        type #future_type: std::future::Future<Output = #output>;

                        #( #attrs )*
                        fn #ident(self, context: tarpc::context::Context, #( #args ),*) -> Self::#future_type;
                    }
                },
            );

        quote! {
            #( #attrs )*
            #vis trait #service_ident: Sized {
                #( #types_and_fns )*

                /// Returns a serving function to use with
                /// [InFlightRequest::execute](tarpc::server::InFlightRequest::execute).
                fn serve(self) -> #server_ident<Self> {
                    #server_ident { service: self }
                }
            }
        }
    }

    fn struct_server(&self) -> TokenStream2 {
        let &Self {
            vis, server_ident, ..
        } = self;

        quote! {
            /// A serving function to use with [tarpc::server::InFlightRequest::execute].
            #[derive(Clone)]
            #vis struct #server_ident<S> {
                service: S,
            }
        }
    }

    fn impl_serve_for_server(&self) -> TokenStream2 {
        let &Self {
            request_ident,
            server_ident,
            service_ident,
            response_ident,
            response_fut_ident,
            camel_case_idents,
            arg_pats,
            method_idents,
            request_names,
            ..
        } = self;

        quote! {
            impl<S> tarpc::server::Serve<#request_ident> for #server_ident<S>
                where S: #service_ident
            {
                type Resp = #response_ident;
                type Fut = #response_fut_ident<S>;

                fn method(&self, req: &#request_ident) -> Option<&'static str> {
                    Some(match req {
                        #(
                            #request_ident::#camel_case_idents{..} => {
                                #request_names
                            }
                        )*
                    })
                }

                fn serve(self, ctx: tarpc::context::Context, req: #request_ident) -> Self::Fut {
                    match req {
                        #(
                            #request_ident::#camel_case_idents{ #( #arg_pats ),* } => {
                                #response_fut_ident::#camel_case_idents(
                                    #service_ident::#method_idents(
                                        self.service, ctx, #( #arg_pats ),*
                                    )
                                )
                            }
                        )*
                    }
                }
            }
        }
    }

    fn enum_request(&self) -> TokenStream2 {
        let &Self {
            derive_serialize,
            vis,
            request_ident,
            camel_case_idents,
            args,
            ..
        } = self;

        quote! {
            /// The request sent over the wire from the client to the server.
            #[allow(missing_docs)]
            #[derive(Debug)]
            #derive_serialize
            #vis enum #request_ident {
                #( #camel_case_idents{ #( #args ),* } ),*
            }
        }
    }

    fn enum_response(&self) -> TokenStream2 {
        let &Self {
            derive_serialize,
            vis,
            response_ident,
            camel_case_idents,
            return_types,
            ..
        } = self;

        quote! {
            /// The response sent over the wire from the server to the client.
            #[allow(missing_docs)]
            #[derive(Debug)]
            #derive_serialize
            #vis enum #response_ident {
                #( #camel_case_idents(#return_types) ),*
            }
        }
    }

    fn enum_response_future(&self) -> TokenStream2 {
        let &Self {
            vis,
            service_ident,
            response_fut_ident,
            camel_case_idents,
            future_types,
            ..
        } = self;

        quote! {
            /// A future resolving to a server response.
            #[allow(missing_docs)]
            #vis enum #response_fut_ident<S: #service_ident> {
                #( #camel_case_idents(<S as #service_ident>::#future_types) ),*
            }
        }
    }

    fn impl_debug_for_response_future(&self) -> TokenStream2 {
        let &Self {
            service_ident,
            response_fut_ident,
            response_fut_name,
            ..
        } = self;

        quote! {
            impl<S: #service_ident> std::fmt::Debug for #response_fut_ident<S> {
                fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
                    fmt.debug_struct(#response_fut_name).finish()
                }
            }
        }
    }

    fn impl_future_for_response_future(&self) -> TokenStream2 {
        let &Self {
            service_ident,
            response_fut_ident,
            response_ident,
            camel_case_idents,
            ..
        } = self;

        quote! {
            impl<S: #service_ident> std::future::Future for #response_fut_ident<S> {
                type Output = #response_ident;

                fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>)
                    -> std::task::Poll<#response_ident>
                {
                    unsafe {
                        match std::pin::Pin::get_unchecked_mut(self) {
                            #(
                                #response_fut_ident::#camel_case_idents(resp) =>
                                    std::pin::Pin::new_unchecked(resp)
                                        .poll(cx)
                                        .map(#response_ident::#camel_case_idents),
                            )*
                        }
                    }
                }
            }
        }
    }

    fn struct_client(&self) -> TokenStream2 {
        let &Self {
            vis,
            client_ident,
            request_ident,
            response_ident,
            ..
        } = self;

        quote! {
            #[allow(unused)]
            #[derive(Clone, Debug)]
            /// The client stub that makes RPC calls to the server. All request methods return
            /// [Futures](std::future::Future).
            #vis struct #client_ident(tarpc::client::Channel<#request_ident, #response_ident>);
        }
    }

    fn impl_client_new(&self) -> TokenStream2 {
        let &Self {
            client_ident,
            vis,
            request_ident,
            response_ident,
            ..
        } = self;

        quote! {
            impl #client_ident {
                /// Returns a new client stub that sends requests over the given transport.
                #vis fn new<T>(config: tarpc::client::Config, transport: T)
                    -> tarpc::client::NewClient<
                        Self,
                        tarpc::client::RequestDispatch<#request_ident, #response_ident, T>
                    >
                where
                    T: tarpc::Transport<tarpc::ClientMessage<#request_ident>, tarpc::Response<#response_ident>>
                {
                    let new_client = tarpc::client::new(config, transport);
                    tarpc::client::NewClient {
                        client: #client_ident(new_client.client),
                        dispatch: new_client.dispatch,
                    }
                }

            }
        }
    }

    fn impl_client_rpc_methods(&self) -> TokenStream2 {
        let &Self {
            client_ident,
            request_ident,
            response_ident,
            method_attrs,
            vis,
            method_idents,
            request_names,
            args,
            return_types,
            arg_pats,
            camel_case_idents,
            ..
        } = self;

        quote! {
            impl #client_ident {
                #(
                    #[allow(unused)]
                    #( #method_attrs )*
                    #vis fn #method_idents(&self, ctx: tarpc::context::Context, #( #args ),*)
                        -> impl std::future::Future<Output = Result<#return_types, tarpc::client::RpcError>> + '_ {
                        let request = #request_ident::#camel_case_idents { #( #arg_pats ),* };
                        let resp = self.0.call(ctx, #request_names, request);
                        async move {
                            match resp.await? {
                                #response_ident::#camel_case_idents(msg) => std::result::Result::Ok(msg),
                                _ => unreachable!(),
                            }
                        }
                    }
                )*
            }
        }
    }
}

impl<'a> ToTokens for ServiceGenerator<'a> {
    fn to_tokens(&self, output: &mut TokenStream2) {
        output.extend(vec![
            self.trait_service(),
            self.struct_server(),
            self.impl_serve_for_server(),
            self.enum_request(),
            self.enum_response(),
            self.enum_response_future(),
            self.impl_debug_for_response_future(),
            self.impl_future_for_response_future(),
            self.struct_client(),
            self.impl_client_new(),
            self.impl_client_rpc_methods(),
        ])
    }
}

fn snake_to_camel(ident_str: &str) -> String {
    let mut camel_ty = String::with_capacity(ident_str.len());

    let mut last_char_was_underscore = true;
    for c in ident_str.chars() {
        match c {
            '_' => last_char_was_underscore = true,
            c if last_char_was_underscore => {
                camel_ty.extend(c.to_uppercase());
                last_char_was_underscore = false;
            }
            c => camel_ty.extend(c.to_lowercase()),
        }
    }

    camel_ty.shrink_to_fit();
    camel_ty
}

#[test]
fn snake_to_camel_basic() {
    assert_eq!(snake_to_camel("abc_def"), "AbcDef");
}

#[test]
fn snake_to_camel_underscore_suffix() {
    assert_eq!(snake_to_camel("abc_def_"), "AbcDef");
}

#[test]
fn snake_to_camel_underscore_prefix() {
    assert_eq!(snake_to_camel("_abc_def"), "AbcDef");
}

#[test]
fn snake_to_camel_underscore_consecutive() {
    assert_eq!(snake_to_camel("abc__def"), "AbcDef");
}

#[test]
fn snake_to_camel_capital_in_middle() {
    assert_eq!(snake_to_camel("aBc_dEf"), "AbcDef");
}
