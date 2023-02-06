extern crate proc_macro2;
use std::collections::HashMap;
use proc_macro2::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, Fields, Ident, ItemFn, Token, Attribute, Expr, FieldValue};
use syn::{Type, GenericParam, ItemImpl};
use syn::Type::Path;
use syn::token::Comma;
use syn::Data::Struct;
use syn::spanned::Spanned;
use syn::punctuated::Punctuated;
use syn::parse::{Parse, Parser, ParseStream, Result};
use quote::{quote, quote_spanned, format_ident};

use syn::{Pat};
use syn::FnArg::Typed;

#[proc_macro]
pub fn min_main(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let (server, init) = if input.vars.len() == 2 {
        let n = &input.vars[0];
        let b = &input.vars[1];
        (quote! {#n}, quote! {#b})
    } else {
        (quote! {server}, quote! {})
    };
    let q = quote! {
        pub mod prelude {
            pub use async_trait::async_trait;
            pub use crate::project::Request;
            pub use anansi::{form, import, viewer, base_view, view, redirect, transact, form_error};
            pub use anansi::web::{Result, Response, BaseUser};
            pub use anansi::forms::Form;
            pub use anansi::cache::BaseCache;
            pub use anansi::records::Record;
            pub use anansi::site::Site;
        }

        #[tokio::main]
        async fn main() {
            use server_prelude::*;

            let internal_error = || Response::internal_error(include_bytes!("http_errors/500.html").to_vec());
            if let Some(#server) = anansi::server::Server::new(APP_STATICS, None, app_url, urls::ROUTES, ErrorView::not_found, internal_error, app_services::<HttpRequest>, app_migrations).await {
                #init
                #server.run().await
            }
        }

        mod server_prelude {
            pub use std::sync::{Arc, Mutex};
            pub use crate::urls::app_url;
            pub use crate::http_errors::views::ErrorView;
            pub use crate::project::{app_services, HttpRequest};
            pub use anansi::web::Response;
        }

        #[cfg(test)]
        pub async fn test_server(sender: tokio::sync::oneshot::Sender<()>) {
            use server_prelude::*;

            let internal_error = || Response::internal_error(include_bytes!("http_errors/500.html").to_vec());
            anansi::server::Server::new(APP_STATICS, APP_ADMINS, Some(sender))
                .run(app_url, urls::ROUTES, ErrorView::not_found, internal_error, app_services::<HttpRequest>, app_migrations)
                .await;
        }
    };
    q.into()
}

#[proc_macro_derive(Record, attributes(field))]
pub fn record_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let lowercase = name.to_string().to_lowercase();
    let lowname = format_ident!("{}", lowercase);
    let mut fv = Vec::new();
    let mut fv2 = Vec::new();
    let mut members = Vec::new();
    let mut pkd = PkData::new();
    let init = record_init(&name, &lowercase, &input.data, &mut pkd, &mut members, &mut fv, &mut fv2);
    let pk_name = &pkd.name;
    let pk_id = format_ident!("{}", pkd.name);
    let values = &pkd.values;
    let params = &pkd.params;
    let mut sets = Vec::new();
    let mut saves = Vec::new();
    for member in &members {
        let lowmem = member.to_lowercase();
        let m = format_ident!("{}", member);
        sets.push(quote!{.set(#lowmem, &self.#m)});
        saves.push(quote!{.value(&self.#m)});
    }
    let (pt, _pkty, pdt) = match pkd.ty.as_str() {
        "BigInt" => (quote! {anansi::records::BigInt}, quote! {i64}, quote! {anansi::records::Record}),
        "Int" => (quote! {anansi::records::Int}, quote! {i32}, quote! {anansi::records::Record}),
        _ => unimplemented!(),
    };
    for (fname, fks) in &pkd.fkv {
        let fkn: Vec<&str> = fks.split("::").collect();
        let fk = fkn.last().unwrap().trim().to_string();
        let full: Type = syn::parse_str(fks).unwrap();
        let by_record = format_ident!("by_{}", fk.to_string().to_lowercase());
        let recordset = format_ident!("{}{}Set", fk, name);
        let mut mv = Vec::new();
        let mut mtv = Vec::new();
        let pkn = format_ident!("{}", pk_name);
        for (null, mt0, mt1) in &pkd.member_type {
            if mt0 != fname && *mt0 != pkn {
                mv.push(quote! {#mt0});
                mtv.push(quote! {#mt0: #mt1});
            } else if mt0 == fname {
                if !null {
                    mv.push(quote! {ForeignKey::new(self.record)});
                } else {
                    mv.push(quote! {Some(ForeignKey::new(self.record))});
                }
            }
        }
        let q3 = quote! {
            impl #name {
                pub fn #by_record(record: &#full) -> #recordset {
                    #recordset {record}
                }
            }
            pub struct #recordset<'a> {
                record: &'a #full,
            }
            impl<'a> #recordset<'a> {
                pub fn new(&self, #(#mtv),*) -> #name {
                    #name::_new(#(#mv),*)
                }
                pub fn order_by(&self, o: anansi::db::OrderByArg<#name>) -> anansi::db::OrderBy<#name> {
                    use anansi::records::Record;
                    #name::whose(#fname().eq(<#full as #pdt>::pk(self.record))).order_by(o)
                }
            }
        };
        fv.push(q3);
    }

    let name_string = name.to_string();
    let table = quote! {&Self::table()};
    let table_name = quote! {Self::table()};
    let record_fields = format_ident!("{}Fields", name);
    
    let primary = quote! {<#name as #pdt>::pk(&self)};
    let expanded = quote! {
        #[async_trait::async_trait]
        impl anansi::records::Record for #name {
            type Pk = #pt;
            const NAME: &'static str = #name_string;
            const PK_NAME: &'static str = #pk_name;
            fn pk(&self) -> #pt {
                self.#pk_id.clone()
            }
            fn pk_mut(&mut self) -> &mut #pt {
                &mut self.#pk_id
            }
            fn find(d: #pt) -> anansi::db::Whose<Self> {
                Self::whose(#lowname::pk().eq(d))
            }
            fn find_in(keys: &Vec<#pt>) -> anansi::db::Limit<Self> {
                assert!(!keys.is_empty());
                Self::whose(#lowname::pk().is_in(keys)).order_by(#lowname::pk().field(keys)).limit(keys.len() as u32)
            }
            fn count() -> anansi::db::Count<Self> {
                anansi::db::Count::from(anansi::db::Builder::count(#table))
            }
            fn whose(w: anansi::db::WhoseArg<Self>) -> anansi::db::Whose<Self> {
                anansi::db::Whose::from(anansi::db::Builder::select(&[#(#members),*], #table).whose().append(w.builder()))
            }
            fn delete_whose(w: anansi::db::WhoseArg<Self>) -> anansi::db::DeleteWhose<Self> {
                anansi::db::DeleteWhose::from(anansi::db::Builder::delete(#table).whose().append(w.builder()))
            }
            fn limit(n: u32) -> anansi::db::Limit<Self> {
                anansi::db::Limit::from(anansi::db::Builder::select(&[#(#members),*], #table).limit(n))
            }
            fn get_all() -> anansi::db::Limit<Self> {
                anansi::db::Limit::from(anansi::db::Builder::select(&[#(#members),*], #table))
            }
            fn get<R: anansi::db::DbRow>(row: R) -> anansi::web::Result<Self> {
                Ok(Self {#init})
            }
            fn from<V: anansi::db::DbRowVec>(rows: V) -> anansi::web::Result<anansi::records::Objects<Self>> {
                let mut mv = anansi::records::Objects::new();
                for row in rows {
                    mv.push(Self::get(row)?);
                }
                Ok(mv)
            }
            fn order_by(w: anansi::db::OrderByArg<Self>) -> anansi::db::OrderBy<Self> {
                anansi::db::OrderBy::from(anansi::db::Builder::select(&[#(#members),*], #table).order_by().append(w.builder()))
            }
            fn table_name() -> String {
                #table_name
            }
            async fn update<B: anansi::web::BaseRequest>(&mut self, req: &B) -> anansi::web::Result<()> {
                self.raw_update(req.raw().pool()).await
            }
            async fn raw_update<D: anansi::db::DbPool>(&mut self, pool: &D) -> anansi::web::Result<()> {
                let u: anansi::db::Update<Self> = anansi::db::Update::new(#table)
                    #(#sets)*.pk(Self::PK_NAME, #primary);
                u.raw_update(pool).await
            }
            async fn delete<B: anansi::web::BaseRequest>(&self, req: &B) -> anansi::web::Result<()> {
                use anansi::records::Relate;
                anansi::transact!(req, {
                    self.on_delete(req).await?;
                    anansi::db::delete_from(#table, Self::PK_NAME, #primary, req).await
                })
            }
            async fn save<R: anansi::web::BaseRequest>(&self, req: &R) -> anansi::web::Result<()> {
                use anansi::records::Relate;
                anansi::transact!(req, {
                    self.on_save(req).await?;
                    self.raw_save(req.raw().pool()).await
                })
            }
            async fn raw_save<D: anansi::db::DbPool>(&self, pool: &D) -> anansi::web::Result<()> {
                let i: anansi::db::Insert<Self> = anansi::db::Insert::new(#table, &[#(#members),*])
                    #(#saves)*;
                i.raw_save(pool).await?;
                Ok(())
            }
        }
        impl #name {
            pub fn _new(#(#params)*) -> Self {
                Self {#(#values),*}
            }
        }
        pub mod #lowname {
            use super::*;
            use anansi::db::Column;
            #(#fv)*
        }
        pub struct #record_fields<F: anansi::records::Record> {
            b: anansi::db::Builder<F>,
        }
        impl<F: anansi::records::Record> #record_fields<F> {
            pub fn new(b: anansi::db::Builder<F>) -> Self {
                Self {b}
            }
            #(#fv2)*
        }
    };
    proc_macro::TokenStream::from(expanded)
}

struct PkData {
    name: String,
    ty: String,
    params: Vec<TokenStream>,
    values: Vec<TokenStream>,
    fkv: Vec<(Ident, String)>,
    member_type: Vec<(bool, Ident, TokenStream)>,
}

impl PkData {
    fn new() -> Self {
        Self {
            name: String::new(),
            ty: String::new(),
            params: Vec::new(),
            values: Vec::new(),
            fkv: Vec::new(),
            member_type: Vec::new(),
        }
    }
}

fn get_names(data: &Data) -> Vec<Ident> {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let mut v = vec![];
                    for f in fields.named.iter().skip(4) {
                        v.push(f.ident.as_ref().unwrap().clone());
                    };
                    return v;
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}

#[proc_macro_derive(Properties)]
pub fn properties_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    builder(true, input)
}

#[proc_macro_derive(Builder)]
pub fn builder_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    builder(false, input)
}

fn builder(properties: bool, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let mut fv = vec![];
    let mut gtv = vec![];
    let mut tv = vec![];
    let mut nov = vec![];
    let mut no_ids = vec![];
    let mut no_init = vec![];
    let mut data = vec![];
    let mut ids = vec![];
    let mut n = 0;
    let mut field_structs = vec![];
    let mut options = vec![];
    let mut opt_ids = vec![];
    let mut id_field = quote!{};
    match &input.data {
        syn::Data::Struct(data_struct) => {
            for named in &data_struct.fields {
                let ty = &named.ty;
                let ty_quote = quote!{#ty};
                let id = named.ident.as_ref().unwrap();
                if ty_quote.to_string().starts_with("Option") {
                    options.push((id, ty));
                    no_init.push(quote!{#id: None});
                    fv.push(quote!{#id: #ty});
                    opt_ids.push(id.clone());
                } else {
                    if id == "id" {
                        let ts = ty_quote.to_string();
                        if ts.ends_with("BigInt") {
                            id_field = quote! {id: anansi::records::generate_id(),};
                        } else if ts.ends_with("Int") {
                            id_field = quote! {id: anansi::records::random_int(),};
                        } else {
                            unimplemented!();
                        }
                    } else {
                        let gen_ty = format_ident!("T{}", n.to_string());
                        let f = format_ident!("{}{}", name, quote!{#id}.to_string().replace("_", "").to_uppercase());
                        let no = format_ident!("No{}", f);
                        n += 1;
                        field_structs.push(f.clone());
                        no_ids.push(no.clone());
                        gtv.push(quote!{#gen_ty});
                        tv.push(ty_quote);
                        fv.push(quote!{#id: #gen_ty});
                        no_init.push(quote!{#id: #no});
                        ids.push(id.clone());
                        nov.push(quote!{pub struct #f(#ty); pub struct #no;});
                        data.push((id, gen_ty, no, ty, f));
                    }
                }
            }
        }
        _ => unimplemented!(),
    }
    let builder = format_ident!("{}Builder", name);
    let mut n = 0;
    let l = data.len();
    let mut methods = vec![];
    for (id, _gty, no, ty, f) in data {
        let mut t1 = vec![];
        let mut t2 = vec![];
        let mut t3 = vec![];
        let mut sv = vec![];
        for i in 0..l {
            if i != n {
                let gty = format_ident!("T{}", i);
                t1.push(gty.clone());
                t2.push(gty.clone());
                t3.push(gty);
                sv.push(&ids[i]);
            } else {
                t2.push(no.clone());
                t3.push(f.clone());
            }
        }
        n += 1;
        let q = if properties {
            quote! {
                impl<#(#t1),*> #builder<#(#t2),*> {
                    pub fn #id(self, #id: impl Into<#ty>) -> #builder<#(#t3),*> {
                        let Self {#(#sv,)* #(#opt_ids,)* ..} = self;
                        #builder {
                            #id: #f(#id.into()),
                            #(#sv,)*
                            #(#opt_ids,)*
                        }
                    }
                }
            }
        } else {
            quote! {
                impl<#(#t1),*> #builder<#(#t2),*> {
                    pub fn #id(self, #id: #ty) -> #builder<#(#t3),*> {
                        let Self {#(#sv,)* #(#opt_ids,)* ..} = self;
                        #builder {
                            #id: #f(#id),
                            #(#sv,)*
                            #(#opt_ids,)*
                        }
                    }
                }
            }
        };
        methods.push(q);
    }
    
    let opts = if options.is_empty() {
        vec![]
    } else {
        let mut gen_tys = vec![];
        let mut option_methods = vec![];
        for i in 0..methods.len() {
            gen_tys.push(format_ident!("T{}", i));
        }
        for (id, ty) in options {
            option_methods.push(quote! {
                impl<#(#gen_tys),*> #builder<#(#gen_tys),*> {
                    pub fn #id(mut self, #id: #ty) -> Self {
                        self.#id = #id;
                        self
                    }
                }
            });
        }
        option_methods
    };

    let last = if properties {
        quote! {
            #(#opts)*
            impl #builder<#(#field_structs),*> {
                pub fn build(self) -> #name {
                    let Self {#(#ids),*} = self;
                    #name {
                        #(#ids: #ids.0),*
                    }
                }
            }
            impl #name {
                pub fn resume(store: &mut anansi_aux::AppState, n: usize) -> Self {
                    if let anansi_aux::Obj::Js(v) = &store.objs()[n] {
                        let value: Self = serde_json::from_value(v.clone()).unwrap();
                        value
                    } else {
                        panic!("expected Rust type");
                    }
                }
            }
        }
    } else {
        let q = quote! {
            #(#opts)*
            impl #builder<#(#field_structs),*> {
                pub async fn saved<B: anansi::web::BaseRequest>(self, req: &B) -> anansi::web::Result<#name> {
                    use anansi::records::Record;
                    let Self {#(#ids),*, #(#opt_ids),*} = self;
                    let model = #name {
                        #id_field
                        #(#ids: #ids.0),*,
                        #(#opt_ids),*
                    };
                    model.save(req).await?;
                    Ok(model)
                }
                pub async fn raw_saved<D: anansi::db::DbPool>(self, pool: &D) -> anansi::web::Result<#name> {
                    use anansi::records::Record;
                    let Self {#(#ids),*, #(#opt_ids),*} = self;
                    let model = #name {
                        #id_field
                        #(#ids: #ids.0),*,
                        #(#opt_ids),*
                    };
                    model.raw_save(pool).await?;
                    Ok(model)
                }
            }
        };
        q
    };

    let expanded = quote! {
        #(#nov)*

        #(#methods)*

        pub struct #builder<#(#gtv),*> {
            #(#fv),*
        }

        impl #name {
            pub fn new() -> #builder<#(#no_ids),*> {
                #builder {#(#no_init),*}
            }
        }
        
        #last
    };

    expanded.into()
}

#[proc_macro_derive(GetData)]
pub fn get_data_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let field_names = get_names(&input.data);
    let mut v = vec![];
    for field_name in &field_names {
        let s = field_name.to_string();
        v.push(quote! {let #field_name = form_map.get(#s)?.parse()?;});
    }

    let expanded = quote! {
        #[async_trait::async_trait]
        impl<B: anansi::web::BaseRequest + anansi::web::GetRecord> anansi::forms::GetData<B> for #name {
            fn from_map(form_map: anansi::web::FormMap) -> anansi::web::Result<<Self as anansi::forms::Form>::Data> {
                #(#v)*
                Ok(<Self as anansi::forms::Form>::Data::new(#(#field_names,)*))
            }
            async fn from_record(record: <Self as anansi::forms::HasRecord>::Item, req: &B) -> anansi::web::Result<<Self as anansi::forms::Form>::Data> {
                Ok(<Self as anansi::forms::Form>::Data::new(#(record.#field_names,)*))
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(ToEdit)]
pub fn to_edit_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let field_names = get_names(&input.data);

    let expanded = quote! {
        #[async_trait::async_trait]
        impl <B: anansi::web::BaseRequest + anansi::web::GetRecord> anansi::forms::ToEdit<B> for #name {
            async fn on_post(&mut self, data: <Self as Form>::Data, req: &B) -> anansi::web::Result<Self::Item> {
                let mut record: Self::Item = req.get_record().await?;
                #(record.#field_names = data.#field_names;)*
                record.update(req).await?;
                Ok(record)
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(Relate)]
pub fn relate_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let record_tuple = format_ident!("{}Tuple", name);
    let lower = format_ident!("{}", record_tuple.to_string().to_lowercase());

    let expanded = quote! {
        impl #name {
            pub async fn owner<B: anansi::web::BaseRequest>(req: &B) -> anansi::web::Result<()> {
                use anansi::records::{Record, RecordTuple, FromParams, Text};
                #record_tuple::check(Text::from(Self::table_name()), Self::pk_from_params(req.params())?, Text::from("owner".to_string()), req).await
            }
        }

        #[async_trait::async_trait]
        impl<R: anansi::web::BaseRequest> anansi::records::Relate<R> for #name {
            async fn on_save(&self, req: &R) -> anansi::web::Result<()> {
                use anansi::records::Record;
                #record_tuple::_new(anansi::records::Text::from("auth_user".to_string()), req.user().pk(), None, self.pk(), anansi::records::Text::from("owner".to_string())).save(req).await?;
                Ok(())
            }
            async fn on_delete(&self, req: &R) -> anansi::web::Result<()> {
                use anansi::records::Record;
                #record_tuple::delete_whose(#lower::object_key().eq(self.pk())).execute(req).await
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(ToDestroy)]
pub fn to_destroy_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let expanded = quote! {
        #[async_trait::async_trait]
        impl<R: crate::project::Request> anansi::forms::ToDestroy<R> for #name {}
    };

    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(FromParams)]
pub fn from_params_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let param = format!("{}_id", name.to_string().to_lowercase());
    let expanded = quote! {
        #[async_trait::async_trait]
        impl anansi::records::FromParams for #name {
            fn pk_from_params(params: &anansi::web::Parameters) -> anansi::web::Result<anansi::records::BigInt> {
                anansi::humanize::decode(params.get(#param)?)
            }
            async fn from_params(params: &anansi::web::Parameters) -> anansi::web::Result<anansi::db::Whose<Self>> {
                let id = Self::pk_from_params(params)?;
                use anansi::records::Record;
                Ok(Self::find(id))
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(ToUrl)]
pub fn to_url_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let expanded = quote! {
        impl anansi::records::ToUrl for #name {
            fn to_url(&self) -> String {
                anansi::humanize::encode(self.id)
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_derive(Form, attributes(field))]
pub fn form_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let mut fv = Vec::new();
    let mut fv2 = Vec::new();
    let mut data_members = Vec::new();
    let mut members = Vec::new();
    let mut members2 = Vec::new();
    let mut member_names = Vec::new();
    let mut member_types = Vec::new();
    let init = form_init(&input.data, &mut data_members, &mut members, &mut members2, &mut member_names, &mut member_types, &mut fv, &mut fv2);
    let name_strings: Vec<String> = member_names.iter().map(|n| n.to_string()).collect();
    let record_data = format_ident!("{}Data", name);
    let record_fields = format_ident!("{}Fields", name);
    let expanded = quote! {
        #[derive(Clone)]
        pub struct #record_data {
            #init
        }
        impl #record_data {
            pub fn new(#(#data_members,)*) -> Self {
                Self{#(#member_names,)*}
            }
        }
        #[async_trait::async_trait]
        impl anansi::forms::Form for #name {
            type Data = #record_data;
            fn new() -> Self {
                Self {
                    csrf_token: None,
                    attrs: anansi::forms::Attributes::new(),
                    data: None,
                    errors: anansi::forms::FormErrors::new(),
                    #(#fv)*
                }
            }
            fn csrf_token(&self) -> Option<&String> {
                self.csrf_token.as_ref()
            }
            fn attrs(&self) -> &anansi::forms::Attributes {
                &self.attrs
            }
            fn insert_attr(mut self, key: &str, value: &str) -> Self {
                self.attrs.insert(key, value);
                self
            }
            fn post(&mut self, token: &anansi::web::TokenRef) {
                self.csrf_token = Some(format!("{}", token));
            }
            async fn from_data(data: Self::Data) -> Self {
                Self {
                    csrf_token: None,
                    attrs: anansi::forms::Attributes::new(),
                    data: Some(data),
                    errors: anansi::forms::FormErrors::new(),
                    #(#fv)*
                }
            }
            fn from_get<B: anansi::web::BaseRequest>(req: &mut B) -> anansi::web::Result<Self> {
                let mut form_data = req.params_mut();
                let data = #record_data {
                    #(#fv2)*
                };
                Ok(Self {
                    csrf_token: None,
                    attrs: anansi::forms::Attributes::new(),
                    data: Some(data),
                    errors: anansi::forms::FormErrors::new(),
                    #(#fv)*
                })
            }
            fn from_post<B: anansi::web::BaseRequest + anansi::web::CsrfDefense>(req: &mut B) -> anansi::web::Result<Self> {
                use anansi::web::CsrfDefense;
                let mut form_data = req.check_token()?;
                let data = #record_data {
                    #(#fv2)*
                };
                Ok(Self {
                    csrf_token: None,
                    attrs: anansi::forms::Attributes::new(),
                    data: Some(data),
                    errors: anansi::forms::FormErrors::new(),
                    #(#fv)*
                })
            }
            fn fill(&mut self) -> anansi::web::Result<()> {
                use anansi::forms::Field;
                if let Some(data) = self.data.as_ref() {
                    #(#members2)*
                    Ok(())
                } else {
                    Err(anansi::web::WebErrorKind::BadFill.to_box())
                }
            }
            fn validate(&mut self) -> anansi::web::Result<#record_data> {
                if let Some(data) = self.data.take() {
                    Ok(data)
                } else {
                    Err(anansi::web::WebErrorKind::BadValidate.to_box())
                }
            }
            fn errors(&self) -> &anansi::forms::FormErrors {
                &self.errors
            }
            fn add_error(&mut self, e: Box<dyn std::error::Error + Send + Sync>) {
                self.errors.add_error(e);
            }
            fn set_data(&mut self, data: Option<#record_data>) {
                self.data = data
            }
            fn field_names() -> &'static [&'static str] {
                &[#(#name_strings),*]
            }
            fn field(&self, n: usize) -> Option<&dyn anansi::forms::Field> {
                match n {
                    #(#members)*
                    _ => None,
                }
            }
        }
        impl<'a> IntoIterator for &'a #name {
            type Item = &'a dyn anansi::forms::Field;
            type IntoIter = #record_fields<'a>;

            fn into_iter(self) -> #record_fields<'a> {
                #record_fields {counter: 0, form: self}
            }
        }
        impl #name {
            pub fn fields(&self) -> #record_fields {
                #record_fields {counter: 0, form: self}
            }
            pub fn check_field_errors(&self) -> anansi::web::Result<()> {
                for field in self {
                    if !field.errors().is_empty() {
                        return Err(anansi::web::WebErrorKind::FieldError.to_box());
                    }
                }
                Ok(())
            }
        }
        pub struct #record_fields<'a> {
            counter: usize,
            form: &'a #name,
        }
        impl<'a> Iterator for #record_fields<'a> {
            type Item = &'a dyn anansi::forms::Field;
            fn next(&mut self) -> Option<Self::Item> {
                use anansi::forms::Form;
                let field = self.form.field(self.counter);
                self.counter += 1;
                field
            }
        }
    };
    proc_macro::TokenStream::from(expanded)
}

#[proc_macro_attribute]
pub fn form(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as SchemaArgs);
    let input = parse_macro_input!(input as DeriveInput);
    let attrs = &input.attrs;
    let name = input.ident;
    let mut v = Vec::new();
    let has_record = if !args.vars.is_empty() {
        let item = &args.vars[0];
        quote! {
            impl anansi::forms::HasRecord for #name {
                type Item = #item;
            }
        }
    } else {
        quote! {}
    };
    let record_data = format_ident!("{}Data", name);
    v.push(quote! {csrf_token: Option<String>});
    v.push(quote! {attrs: anansi::forms::Attributes});
    v.push(quote! {data: Option<#record_data>});
    v.push(quote! {errors: anansi::forms::FormErrors});
    match &input.data {
        Struct(data_struct) => {
            match &data_struct.fields {
                Fields::Named(named) => {
                    for f in &named.named {
                        v.push(quote!{#f});
                    }
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
    let q = quote! {
        #[derive(anansi::Form)]
        #(#attrs)*
        pub struct #name {
            #(#v),*
        }
        #has_record
    };
    q.into()
}

fn expr_attrs(attrs: &Vec<Expr>) -> HashMap<String, String> {
    let mut hm = HashMap::new();
    for attr in attrs {
        if let Expr::Assign(assign) = attr {
            if let Expr::Path(expr_path) = &*assign.left {
                if expr_path.path.segments[0].ident.to_owned() == "table_name" {
                    let tokens = &assign.right;
                    let tokens = quote! {#tokens}.to_string();
                    hm.insert("table_name".to_owned(), tokens[1..tokens.len()-1].to_owned());
                    break;
                }
            }
        }
        panic!("unexpected argument in macro attribute")
    }
    hm
}

fn get_attrs(attrs: &Vec<Attribute>) -> HashMap<String, String> {
    let mut hm = HashMap::new();
    for attr in attrs {
        if attr.path.segments[0].ident.to_owned() == "field" {
            let tokens = &attr.tokens;
            let tokens = quote! {#tokens}.to_string();
            let args = tokens[1..tokens.len()-1].split(',');
            for arg in args {
                let (key, value) = arg.split_once('=').unwrap();
                let value = value.trim();
                hm.insert(key.trim().to_owned(), value[1..value.len()-1].to_owned());
            }
            break;
        }
    }
    hm
}

fn form_init(data: &Data, data_members: &mut Vec<TokenStream>, members: &mut Vec<TokenStream>, members2: &mut Vec<TokenStream>, member_names: &mut Vec<Ident>, member_types: &mut Vec<TokenStream>, fv: &mut Vec<TokenStream>, fv2: &mut Vec<TokenStream>) -> TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let mut n: usize = 0;
                    let recurse = fields.named.iter().skip(4).map(|f| {
                        let name = &f.ident;
                        let ns = name.as_ref().unwrap().to_string();
                        let attrs = get_attrs(&f.attrs);
                        let label = if let Some(label) = attrs.get("label") {
                            let label = &label[1..label.len()-1];
                            quote! {#label}
                        } else {
                            let mut s = String::new();
                            let mut n = 0;
                            for c in ns.chars() {
                                if n > 0 {
                                    if c != '_' {
                                        s.push(c);
                                    } else {
                                        s.push(' ');
                                    }
                                } else {
                                    s.push_str(&c.to_uppercase().to_string());
                                }
                                n += 1;
                            }
                            quote! {#s}
                        };
                        let widget = if let Some(widget) = attrs.get("widget") {
                            let w = format_ident!("{}", widget);
                            quote! {anansi::forms::#w}
                        } else {
                            quote! {anansi::forms::TextInput}
                        };
                        let ty = &f.ty;
                        let required = if let Some(required) = attrs.get("required") {
                            if required == "false" {
                                false
                            } else {
                                panic!("unexpected value for `required` attribute");
                            }
                        } else {
                            true
                        };
                        let q = if required {
                            quote! {
                                #name: <#ty>::new(#label, Box::new(#widget {name: #ns, attrs: anansi::forms::Attributes::new().id(#ns).pass("required", "")})),
                            }
                        } else {
                            quote! {
                                #name: <#ty>::new(#label, Box::new(#widget {name: #ns, attrs: anansi::forms::Attributes::new().id(#ns)})),
                            }
                        };
                        let q2 = if required {
                            quote! {
                                #name: {
                                    let s = form_data.remove(#ns)?;
                                    if !s.is_empty() {
                                        s.parse()?
                                    } else {
                                        return Err(anansi::web::WebErrorKind::BadField.to_box());
                                    }
                                },
                            }
                        } else {
                            quote! {
                                #name: {
                                    match form_data.remove(#ns) {
                                        Ok(s) => {
                                            if !s.is_empty() {
                                                Some(<anansi::records::#ty as anansi::records::DataType>::from_val(s)?)
                                            } else {
                                                None
                                            }
                                        },
                                        Err(_) => None,
                                    }
                                },
                            }
                        };
                        let q3 = quote! {
                            #n => Some(&self.#name as &dyn anansi::forms::Field),
                        };
                        let q4 = if required {
                            quote! {
                                self.#name.mut_widget().mut_attrs().insert("value", &anansi::web::html_escape(&format!("{}", data.#name)));
                            }
                        } else {
                            quote! {
                                self.#name.mut_widget().mut_attrs().insert("value", &anansi::web::html_escape(&match data.#name {Some(ref f) => format!("{}", f), None => "".to_string(),}));
                            }
                        };
                        n += 1;
                        fv.push(q);
                        fv2.push(q2);
                        if required {
                            data_members.push(quote! {#name: anansi::records::#ty});
                        } else {
                            data_members.push(quote! {#name: Option<anansi::records::#ty>});
                        };
                        members.push(q3);
                        members2.push(q4);
                        member_names.push(name.clone().unwrap());
                        if required {
                            member_types.push(quote!{#ty});
                            quote_spanned! {f.span() =>
                                pub #name: anansi::records::#ty,
                            }    
                        } else {
                            member_types.push(quote!{Option<#ty>});
                            quote_spanned! {f.span() =>
                                pub #name: Option<anansi::records::#ty>,
                            }
                        }
                        
                    });
                    quote! {
                        #(#recurse)*
                    }
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}

fn ty_string(ty: &Type, segment: &String) -> String {
    let ty = quote! {#ty}.to_string();
    let t = ty[segment.len()+3..ty.len()-2].to_string();
    t
}

fn record_init(mname: &Ident, fname: &str, data: &Data, pkd: &mut PkData, members: &mut Vec<String>, fv: &mut Vec<TokenStream>, fv2: &mut Vec<TokenStream>) -> TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let recurse = fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let member = name.as_ref().unwrap().to_string();
                        let m2 = member.to_ascii_lowercase();
                        let column = quote! {format!("{}.{}", #mname::table(), #member)};
                        let lowcolumn = quote! {&format!("{}.{}", super::#mname::table(), #member)};
                        let attrs = get_attrs(&f.attrs);
                        let mut fty = f.ty.clone();
                        match &f.ty {
                            Path(path) => {
                                let mut segment = path.path.segments.last().unwrap().ident.to_string();
                                let mut null = false;
                                
                                let mut is_pk = false;
                                if let Some(pk) = attrs.get("primary_key") {
                                    if pkd.name.is_empty() && pk == "true" {
                                        is_pk = true;
                                        pkd.name = member.clone();
                                        pkd.ty = segment.clone();
                                        let df = attrs.get("default_fn").expect("expected default function for primary key");
                                        let df: proc_macro2::TokenStream = df.parse().expect("error parsing default function");
                                        pkd.values.push(quote! {#name: #df()});
                                    } else {
                                        panic!("only one primary key permitted");
                                    }
                                } else if segment != "ManyToMany" {
                                    pkd.params.push(quote! {#name: #fty,});
                                    pkd.values.push(quote! {#name});
                                } else {
                                    pkd.values.push(quote! {#name: anansi::records::ManyToMany::new()});
                                }
                                if segment == "Option" {
                                    pkd.member_type.push((true, name.as_ref().unwrap().clone(), quote! {#fty}));
                                    null = true;
                                    let ty = &f.ty;
                                    let ty = quote! {#ty}.to_string();
                                    let mut s = ty[segment.len()+3..ty.len()-2].to_string();
                                    s = match s.rsplit_once("::") {
                                        Some((_, second)) => second.to_string(),
                                        None => s,
                                    };
                                    match s.rsplit_once('<') {
                                        Some((first, _)) => {
                                            segment = first.trim().to_string()
                                        },
                                        None => {
                                            segment = s.trim().to_string()
                                        },
                                    }
                                    let (t, u) = ty.rsplit_once("Option <").unwrap();
                                    let (u, _) = u.rsplit_once('>').unwrap();
                                    let v: syn::Type = syn::parse_str(&format!("{}{}", t, u)).unwrap();
                                    fty = v;
                                } else {
                                    pkd.member_type.push((false, name.as_ref().unwrap().clone(), quote! {#fty}));
                                }
                                match segment.as_str() {
                                    "ManyToMany" => {
                                        let ty = &f.ty;
                                        let ty = quote! {#ty}.to_string();
                                        let ty = &ty[segment.len()+3..ty.len()-2];
                                        let lower = ty.to_lowercase();
                                        let mfield = format_ident!("{}Fields", ty);
                                        let q = quote! {pub fn #name() -> #mfield<#mname> {let full_name = format!("{}_{}", super::super::APP_NAME, #fname); let join = format!("{}_{}", full_name, #lower); #mfield::new(anansi::db::Builder::new().inner_join(&join, &full_name, "id", &format!("{}_id", #fname)).inner_join(&format!("{}_{}", super::super::APP_NAME, #lower), &join, &format!("{}_id", #lower), "id"))}};
                                        let q2 = quote! {pub fn #name(self) -> #mfield<F> {let full_name = format!("{}_{}", super::APP_NAME, #fname); let join = format!("{}_{}", full_name, #lower); #mfield::new(anansi::db::Builder::new().inner_join(&join, &full_name, "id", &format!("{}_id", #fname)).inner_join(&format!("{}_{}", super::APP_NAME, #lower), &join, &format!("{}_id", #lower), "id"))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        quote_spanned! {f.span() =>
                                            #name: anansi::records::ManyToMany::new(),
                                        }
                                    },
                                    "BigInt" => {
                                        let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::records::BigInt> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::records::BigInt> {anansi::db::Column::from(self.b.push_val(anansi::db::Clause::Column(#column)))}};
                                        fv.push(q);
                                        if is_pk {
                                            let q3 = quote! {pub fn pk() -> anansi::db::Column<#mname, anansi::records::BigInt> {anansi::db::Column::new(#lowcolumn)}};
                                            fv.push(q3);
                                        }
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::records::BigInt as anansi::records::DataType>::from_val(row.try_i64(#m2)?)?,
                                        }
                                    },
                                    "Int" => {
                                        let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::records::Int> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::records::Int> {anansi::db::Column::from(self.b.push_val(anansi::db::Clause::Column(#column)))}};
                                        fv.push(q);
                                        if is_pk {
                                            let q3 = quote! {pub fn pk() -> anansi::db::Column<#mname, anansi::records::Int> {anansi::db::Column::new(#lowcolumn)}};
                                            fv.push(q3);
                                        }
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::records::Int as anansi::records::DataType>::from_val(row.try_i32(#m2)?)?,
                                        }
                                    },
                                    "ForeignKey" => {
                                        if pkd.ty == "BigInt" {
                                            let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::records::BigInt> {anansi::db::Column::new(#lowcolumn)}};
                                            let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::records::BigInt> {anansi::db::Column::from(self.b.push_val(anansi::db::Clause::Column(#column)))}};

                                            fv.push(q);
                                            fv2.push(q2);
                                            members.push(member);
                                            let mut ts = ty_string(&fty, &segment);
                                            if let Some(t) = ts.split_once(',') {
                                                ts = t.0.trim().to_string();
                                            }
                                            pkd.fkv.push((name.as_ref().unwrap().clone(), ts));
                                            let qs = if !null {
                                                quote_spanned! {f.span() =>
                                                    #name: <anansi::records::#fty as anansi::records::DataType>::from_val(row.try_i64(#m2)?)?,
                                                }
                                            } else {
                                                quote_spanned! {f.span() =>
                                                    #name: {
                                                        let o = row.try_i64(#m2);
                                                        if let Ok(n) = o {
                                                            Some(<anansi::records::#fty as anansi::records::DataType>::from_val(n)?)
                                                        } else {
                                                            None
                                                        }
                                                    }
                                                }
                                            };
                                            qs
                                        } else {
                                            panic!("unexpected primary key type for foreign key");
                                        }
                                    },
                                    "DateTime" => {
                                        let q = quote! {pub fn #name<'a>() -> anansi::db::Column<#mname, anansi::records::DateTime> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name<'a>(self) -> anansi::db::Column<F, anansi::records::DateTime> {anansi::db::Column::from(self.b.push_val(anansi::db::Clause::Column(#column)))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::records::DateTime as anansi::records::DataType>::from_val(row.try_date_time(#m2)?)?,
                                        }
                                    },
                                    "Boolean" => {
                                        let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::records::Boolean> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::records::Boolean> {anansi::db::Column::from(self.b.push_val(anansi::db::Clause::Column(#column)))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::records::Boolean as anansi::records::DataType>::from_val(row.try_bool(#m2)?)?,
                                        }
                                    },
                                    "VarChar" => {
                                        let q = quote! {pub fn #name<'a>() -> anansi::db::Column<#mname, anansi::records::#fty> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name<'a>(self) -> anansi::db::Column<F, anansi::records::#fty> {anansi::db::Column::from(self.b.push_val(anansi::db::Clause::Column(#column)))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        if !null {
                                            quote_spanned! {f.span() =>
                                                #name: <anansi::records::#fty as anansi::records::DataType>::from_val(row.try_string(#m2)?)?,
                                            }
                                        } else {
                                            quote_spanned! {f.span() =>
                                                #name: {
                                                    let o = row.try_string(#m2);
                                                    if let Ok(s) = o {
                                                        Some(<anansi::records::#fty as anansi::records::DataType>::from_val(s)?)
                                                    } else {
                                                        None
                                                    }
                                                }
                                                ,
                                            }
                                        }
                                    },
                                    "Text" => {
                                        let q = quote! {pub fn #name<'a>() -> anansi::db::Column<#mname, anansi::records::Text> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name<'a>(self) -> anansi::db::Column<F, anansi::records::Text> {anansi::db::Column::from(self.b.push_val(anansi::db::Clause::Column(#column)))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        if !null {
                                            quote_spanned! {f.span() =>
                                                #name: anansi::records::Text::from(row.try_string(#m2)?),
                                            }
                                        } else {
                                            quote_spanned! {f.span() =>
                                                #name: {
                                                    let o = row.try_option_string(#m2)?;
                                                    if let Some(s) = o {
                                                        Some(anansi::records::Text::from(s))
                                                    } else {
                                                        None
                                                    }
                                                },
                                            }
                                        }
                                    },
                                    _ => {
                                        panic!("{}", segment.as_str());
                                    },
                                }
                            },
                            _ => unimplemented!(),
                        }
                    });
                    quote! {
                        #(#recurse)*
                    }
                },
                _ => unimplemented!(),
            }
        },
        _ => unimplemented!(),
    }
}

#[proc_macro]
pub fn app_components(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let comps = &input.vars;
    let q = quote! {
        pub const COMPONENTS: &'static [anansi_aux::Mounts] = &[#(#comps::CB,)*];
    };
    q.into()
}

#[cfg(not(target_os = "windows"))]
macro_rules! main_separator {
    () => {r"/"}
}

#[cfg(target_os = "windows")]
macro_rules! main_separator {
    () => {r"\"}
}

#[proc_macro]
pub fn start(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let comps = &input.vars[0];
 
    let q = quote! {
        #[wasm_bindgen::prelude::wasm_bindgen]
        pub fn start() {
            let mut callbacks = std::collections::HashMap::new();
            for comp in #comps::COMPONENTS {
                for (name, new, call) in *comp {
                    callbacks.insert(name.to_string(), anansi_aux::CallbackData {new: *new, call: *call, is_mounted: false});
                }
            }
            anansi_aux::setup(callbacks);
        }
    };
    q.into()
}

#[proc_macro]
pub fn wasm_statics(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let var = &input.vars[0];
    let name = quote! {#var}.to_string();
    let name = name.replace('"', "");
    let under_name = name.replace("-", "_");
    let name_js = format!("{}.js", under_name);
    let name_wasm = format!("{}_bg.wasm", under_name);
    let pkg = format!("..{}{}{0}pkg{0}", main_separator!(), name);
    let main = format!("..{}{}{0}main.js", main_separator!(), name);
    let sw = format!("..{}{}{0}sw.js", main_separator!(), name);
    let stat = "/static/pkg/";
    let names = quote! {
        &[
            ("/static/main.js", include_bytes!(#main)),
            ("/static/sw.js", include_bytes!(#sw)),
            (concat!(#stat, #name_js), include_bytes!(concat!(#pkg, #name_js))),
            (concat!(#stat, #name_wasm), include_bytes!(concat!(#pkg, #name_wasm)))
        ]
    };
    names.into()
}

#[proc_macro]
pub fn middleware(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);

    let mut args = quote! {<anansi::web::SecurityHeaders<anansi::web::ViewService> as Service<B>>::init(vs, settings).await};
    let retval = if input.vars.is_empty() {
        quote! {anansi::web::SecurityHeaders<anansi::web::ViewService>}
    } else {
        let last = input.vars.last().unwrap();
        quote! {#last}
    };
    for arg in input.vars {
        args = quote!{#arg::init(#args, settings).await}
    }
    let q = quote! {
        pub fn app_services<B: anansi::web::BaseRequest>(settings: &anansi::server::Settings) -> std::pin::Pin<Box<dyn std::future::Future<Output = #retval> + Send + '_>> {
            use anansi::web::Service;
            let vs = anansi::web::ViewService;
            Box::pin(async {#args})
        }
        anansi::setup!();
    }.into();
    q
}

#[proc_macro]
pub fn app_statics(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let mut args = vec![];
    for arg in input.vars {
        if let syn::Expr::Macro(a) = arg {
            args.push(quote! {#a});
        } else {
            args.push(quote! {#arg::STATICS});
        }
    }
    let q = quote! {
        static APP_STATICS: &[&[anansi::web::Static]] = &[
            #(#args),*
        ];
    }.into();
    q
}

#[proc_macro_attribute]
pub fn base_view(_args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let sig = &input.sig;
    let generics = &sig.generics;
    let fname = &sig.ident;
    let fnargs = &sig.inputs;
    let name = String::from(concat!(main_separator!(), ".parsed", main_separator!())) + &sig.ident.to_string() + ".in";
    let args_name = format!("{}.parsed{0}", main_separator!()) + &sig.ident.to_string() + "_args.in";
    let stmts = input.block.stmts;
    let rty = match &input.sig.output {
        syn::ReturnType::Type(_, ty) => match &**ty {
            Type::Path(path) => {
                &path.path.segments
            },
            _ => panic!("Could not get path"),
        },
        _ => panic!("Could not get return type"),
    };
    let q = quote! {
        pub mod #fname {
            use super::*;
            include!(concat!("templates", #args_name));
            pub fn base #generics (#fnargs, _base_args: Args) -> #rty {
                #(#stmts)*
                include!(concat!("templates", #name))
            }
        }
    };

    q.into()
}

#[proc_macro_attribute]
pub fn cacheable(_args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let attrs = input.attrs;
    let vis = input.vis;
    let sig = input.sig;
    let (req, ty) = if let syn::FnArg::Typed(pat_type) = sig.inputs.first().unwrap() {
        (pat_type.pat.clone(), pat_type.ty.clone())
    } else {
        panic!("expected typed argument for cached view");
    };
    let ret = sig.output;
    let sig_ident = &sig.ident;
    let base_ident = format_ident!("_base_{}", sig_ident);
    let cache_ident = format_ident!("_cache_{}", sig_ident);
    
    let name = format!("{}.parsed{0}", main_separator!()) + &sig.ident.to_string() + ".in";

    let attr_ident = attrs[0].path.segments.last().unwrap().ident.to_string();
    let tokens = &attrs[0].tokens;
    let key = if tokens.to_string().contains("Group :: is_visitor") {
        quote!{format!("_c{}", Self::#cache_ident::<N> as usize)}
    } else {
        panic!("expected cache to be set to visitor");
    };
    let extend = match attr_ident.as_str() {
        "view" => quote! {{let mut _args = base::Args::new(); include!(concat!("templates", #name))}},
        "check" => quote! {},
        _ => panic!("expected `view` or `check` macro"),
    };
    let stmts = input.block.stmts;
    let q = quote! {
        #[anansi::check(#tokens)]
        #vis async fn #cache_ident<const N: usize>(#req: #ty) #ret {
            let key = #key;
            let _b = if let Ok(v) = #req.cache().get(&key).await {
                base::Args::from_bytes(v)?
            } else {
                let _b = Self::#base_ident(#req).await?;
                #req.cache_mut().set_ex(&key, &_b.to_bytes(), Some(N)).await?;
                _b
            };
            base::base(#req, _b)
        }
        async fn #base_ident(#req: #ty) -> anansi::web::Result<base::Args> {
            #(#stmts)*
            Ok(#extend)
        }
        #[anansi::check(#tokens)]
        #vis async fn #sig_ident(#req: #ty) #ret {
            let _b =  Self::#base_ident(#req).await?;
            base::base(#req, _b)
        }
    };
    q.into()
}

#[proc_macro_attribute]
pub fn view(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as SchemaArgs);
    let vars = args.vars;
    let vis = input.vis;
    let sig = input.sig;
    let name = &sig.ident.to_string();

    let stmts = input.block.stmts;
    let q = quote! {
        #[anansi::check(#(#vars)*)]
        #vis #sig {
            #(#stmts)*
            anansi::extend!(req, base, #name)
        }
    };
    q.into()
}

#[proc_macro_attribute]
pub fn check(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as SchemaArgs);
    let vars = &args.vars;
    if vars.is_empty() {
        panic!("Expected function");
    }
    let generics = input.sig.generics;
    let vis = input.vis;
    let sig_ident = input.sig.ident;
    let _sig_ident = format_ident!("_{}", sig_ident);
    let (req, ty) = match &input.sig.inputs[0] {
        Typed(pat) => {
            let req = match &*pat.pat {
                Pat::Ident(id) => id,
                _ => panic!("Could not get request"),
            };
            let ty = &pat.ty;
            let ty = quote! {#ty};
            (req, ty)
        },
        _ => panic!("Not type"),
    };
    let rty = match &input.sig.output {
        syn::ReturnType::Type(_, ty) => match &**ty {
            Type::Path(path) => {
                &path.path.segments
            },
            _ => panic!("Could not get path"),
        },
        _ => panic!("Could not get return type"),
    };
    let stmts = input.block.stmts;
    let mut generic_idents = quote!{};
    let where_clause = &generics.where_clause;
    let _sig_generic = if generics.lt_token.is_none() {
        quote! {#_sig_ident}
    } else {
        let mut v = vec![];
        for param in &generics.params {
            match param {
                GenericParam::Type(ty) => {
                    let t = &ty.ident;
                    v.push(quote!{#t});
                }
                GenericParam::Lifetime(lt) => {
                    let l = &lt.lifetime;
                    v.push(quote!{#l});
                }
                GenericParam::Const(co) => {
                    let c = &co.ident;
                    v.push(quote!{#c});
                }
            }
        }
        generic_idents = quote! {::<#(#v,)*>};
        quote! {#_sig_ident::#generics}
    };
    let q = quote! {
        async fn #_sig_ident #generics (#req: #ty) -> #rty #where_clause {
            #(#vars(#req).await?;)*
            #(#stmts)*
        }
        #vis fn #sig_ident #generics (_raw: #ty) -> std::pin::Pin<Box<dyn std::future::Future<Output = #rty> + Send + '_>> #where_clause {
            Box::pin(Self::#_sig_ident #generic_idents(_raw))
        }
    };
    q.into()
}

#[proc_macro_attribute]
pub fn check_fn(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as Args);
    let generics = input.sig.generics;
    let vis = input.vis;
    let sig_ident = input.sig.ident;
    let _sig_ident = format_ident!("_{}", sig_ident);
    let (req, ty) = match &input.sig.inputs[0] {
        Typed(pat) => {
            let req = match &*pat.pat {
                Pat::Ident(id) => id,
                _ => panic!("Could not get request"),
            };
            let ty = match &*pat.ty {
                Type::Path(path) => {
                    &path.path.segments
                },
                _ => panic!("Could not get type"),
            };
            (req, ty)
        },
        _ => panic!("Not type"),
    };
    let rty = match &input.sig.output {
        syn::ReturnType::Type(_, ty) => match &**ty {
            Type::Path(path) => {
                &path.path.segments
            },
            _ => panic!("Could not get path"),
        },
        _ => panic!("Could not get return type"),
    };
    let func = &args.vars[0];
    let stmts = input.block.stmts;
    let mut generic_idents = quote!{};
    let _sig_generic = if generics.lt_token.is_none() {
        quote! {#_sig_ident}
    } else {
        let mut v = vec![];
        for param in &generics.params {
            match param {
                GenericParam::Type(ty) => v.push(ty.ident.clone()),
                _ => unimplemented!(),
            }
        }
        generic_idents = quote! {::<#(#v,)*>};
        quote! {#_sig_ident::#generics}
    };
    let q = quote! {
        async fn #_sig_ident #generics (#req: #ty) -> #rty {
            #(#stmts)*
        }
        #vis fn #sig_ident #generics(_raw: #ty) -> std::pin::Pin<Box<dyn std::future::Future<Output = #rty> + Send>> {
            Box::pin(#func(_raw, #_sig_ident #generic_idents))
        }
    };
    q.into()
}

#[proc_macro_attribute]
pub fn viewer(_metadata: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ViewerArgs);
    let imp = input.imp;
    let ty = &imp.self_ty;
    let gen = &imp.generics;
    let id = match &gen.params[0] {
        GenericParam::Type(t) => {
            &t.ident
        },
        _ => unimplemented!(),
    };
    let q = quote! {
        pub struct #ty {g: std::marker::PhantomData<#id>}
        #imp
    };
    q.into()
}

#[proc_macro_attribute]
pub fn store(_metadata: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let s = parse_macro_input!(input as syn::ItemStruct);
 
    let fields = match &s.fields {
        syn::Fields::Named(fields_named) => {
            &fields_named.named
        }
        _ => unimplemented!(),
    };
    let state = &s.ident;
    let _state = format_ident!("_{}", s.ident);
    let vis = &s.vis;
    let mut nfields = vec![];
    let mut names = vec![];
    let mut methods = vec![];
    let mut n: i64 = 1;
    let attrs = &s.attrs;
    for field in fields {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        let upper = format_ident!("{}", name.to_string().to_uppercase());
        let name_mut = format_ident!("{}_mut", name);
        methods.push(quote! {
            pub fn #name(&mut self) -> &#ty {
                self._proxy.set(Self::#upper);
                &self._state.#name
            }
            pub fn #name_mut(&mut self) -> &mut #ty {
                self._proxy._invalid = true;
                &mut self._state.#name
            }
        });
        nfields.push(quote! {#vis const #upper: i64 = #n;});
        n *= 2;
        names.push(name);
    }
    let c = quote! {
        #(#attrs)*
        #vis struct #_state {
            #fields
        }

        #vis struct #state {
            _proxy: Proxy,
            _state: #_state
        }

        impl #state {
            #(#nfields)*
            #(#methods)*
            #vis fn resume(store: &mut anansi_aux::AppState, n: usize) -> Self {
                if let anansi_aux::Obj::Js(v) = store.objs()[n].clone() {
                    let state: #_state = serde_json::from_value(v).unwrap();
                    let subs = store.subs_mut().pop().expect("problem getting subs");
                    Self {_proxy: Proxy::new(subs), _state: state}
                } else {
                    panic!("expected JavaScript value")
                }
            }
            #vis fn store(#fields) -> Self {
                Self {_proxy: Proxy::new(vec![]), _state: #_state {#(#names),*}}
            }
            #vis fn get_subs(&self) -> Vec<String> {
                self._proxy.get_subs()
            }
            #vis fn into_inner(self) -> #_state {
                self._state
            }
        }
    };
    c.into()
}

#[proc_macro_attribute]
pub fn record(metadata: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
    let meta = parse_macro_input!(metadata as SchemaArgs);
    match &mut ast.data {
        syn::Data::Struct(ref mut struct_data) => {
            match &mut struct_data.fields {
                syn::Fields::Named(fields) => {
                    let mut has_pk = false;
                    for field in &fields.named {
                        let attrs = get_attrs(&field.attrs);
                        if let Some(primary) = attrs.get("primary_key") {
                            if primary == "true" {
                                has_pk = true;
                                break;
                            }
                        }
                    }
                    if !has_pk {
                        fields.named.insert(0, syn::Field::parse_named.parse2(quote! { #[field(primary_key = "true", default_fn = "anansi::records::generate_id")] id: anansi::records::BigInt}).unwrap());
                    }
                }
                _ => {},
            }

            let meta_attrs = expr_attrs(&meta.vars);
            let lowercase = format!("{}", ast.ident.to_string().to_lowercase());
            let t = if let Some(name) = meta_attrs.get("table_name") {
                quote! {#name.to_string()}
            } else {
                quote! {format!("{}_{}", super::APP_NAME, #lowercase)}
            };
            let ident = &ast.ident;
            let table = quote! {
                impl #ident {
                    fn table() -> String {
                        #t
                    }
                }
            };

            let record_tuple = format_ident!("{}Tuple", ast.ident);
            let record_lower = record_tuple.to_string().to_lowercase();
            let tt = quote! {format!("{}_{}", super::APP_NAME, #record_lower)};
            let tuple_table = quote! {
                impl #record_tuple {
                    fn table() -> String {
                        #tt
                    }
                }
            };

            let lower = format_ident!("{}", record_tuple.to_string().to_lowercase());
            let tuple = quote! {
                #[derive(anansi::Record, anansi::Builder)]
                pub struct #record_tuple {
                    #[field(primary_key = "true", default_fn = "anansi::records::generate_id")]
                    id: anansi::records::BigInt,
                    pub subject_namespace: anansi::records::Text,
                    pub subject_key: anansi::records::BigInt,
                    pub subject_predicate: Option<anansi::records::Text>,
                    pub object_key: anansi::records::BigInt,
                    pub object_predicate: anansi::records::Text,
                }
                impl<B: anansi::web::BaseRequest> anansi::records::Relate<B> for #record_tuple {}
                #tuple_table
                
                #[async_trait::async_trait]
                impl<R: anansi::web::BaseRequest> anansi::records::RecordTuple<R> for #record_tuple {
                    async fn check(object_namespace: anansi::records::Text, object_key: anansi::records::BigInt, object_predicate: anansi::records::Text, req: &R) -> anansi::web::Result<()> {
                        use anansi::records::Record;
                        let tuples = Self::whose(#lower::object_key().eq(object_key)).and(#lower::object_predicate().eq(object_predicate)).get_all().query(req).await?;
                        for tuple in tuples {
                            match tuple.subject_predicate {
                                None => {
                                    if tuple.subject_namespace == "auth_user" && tuple.subject_key == req.user().pk() {
                                        return Ok(());
                                    }
                                },
                                Some(predicate) => {
                                    if Self::check(tuple.subject_namespace, tuple.subject_key, predicate, req).await.is_ok() {
                                        return Ok(());
                                    }
                                },
                            }
                        }
                        Err(anansi::web::WebErrorKind::NoPermission.to_box())
                    }
                }
            };
            
            let q = quote! {
                #[derive(anansi::Record, anansi::Builder)]
                #ast
                #tuple
                #table
            };
            return q.into();
        }
        _ => panic!("macro has to be used on a struct"),
    }
}

struct CacheArgs {
    n: Expr,
    view: syn::Path,
}

impl Parse for CacheArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let n = input.parse().unwrap();
        let _c: Comma = input.parse().unwrap();
        let view = input.parse().unwrap();
        Ok(Self {view, n,})
    }
}

struct ViewerArgs {
    imp: ItemImpl,
}

impl Parse for ViewerArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            imp: input.parse().unwrap(),
        })
    }
}

struct RecordAdminArgs {
    record: Type,
    hm: HashMap<String, Expr>,
}

impl Parse for RecordAdminArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let record = input.parse().unwrap();
        let _comma = input.parse::<Comma>().unwrap();
        let vars = Punctuated::<FieldValue, Token![,]>::parse_terminated(input)?;
        let mut hm = HashMap::new();
        for v in vars {
            let name = if let syn::Member::Named(id) = v.member {
                id
            } else {
                panic!("expected named member");
            };
            hm.insert(name.to_string(), v.expr);
        }
        Ok(Self {
            record,
            hm,
        })
    }
}
struct SchemaArgs {
    vars: Vec<Expr>,
}

impl Parse for SchemaArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let vars = Punctuated::<Expr, Token![,]>::parse_terminated(input)?;
        Ok(Self {
            vars: vars.into_iter().collect(),
        })
    }
}

struct Args {
    vars: Vec<Ident>,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self> {
        let vars = Punctuated::<Ident, Token![,]>::parse_terminated(input)?;
        Ok(Self {
            vars: vars.into_iter().collect(),
        })
    }
}

struct UrlArgs {
    req: Expr,
    first: Expr,
    exprs: Vec<Expr>,
}

impl Parse for UrlArgs {
    fn parse(stream: ParseStream) -> Result<Self> {
        let req = stream.parse().unwrap();
        let _comma = stream.parse::<Comma>();
        let first = stream.parse().unwrap();
        let mut exprs = Vec::new();
        while let Ok(_) = stream.parse::<Comma>() {
            exprs.push(stream.parse().unwrap());
        }
        Ok(Self {req, first, exprs})
    }
}

#[proc_macro]
pub fn extend(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let req = &input.vars[0];
    let base = &input.vars[1];
    let name = &input.vars[2];
    let q = quote! {
        {
            let mut _args = #base::Args::new();
            _args = include!(concat!("templates", anansi::main_separator!(), ".parsed", anansi::main_separator!(), #name, ".in"));
            #base::#base(#req, _args)
        }
    };
    q.into()
}

#[proc_macro]
pub fn cache_view(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as CacheArgs);
    let mut view = input.view.segments.clone();
    let name = if let syn::punctuated::Pair::End(t) = view.pop().unwrap() {
        format_ident!("_cache_{}", t.ident)
    } else {
        unimplemented!();
    };
    let e = &input.n;
    let n = match e {
        Expr::Lit(n) => quote! {#n},
        _ => quote! {{#e}}
    };
    let q = quote! {
        #view #name::<#n>
    };
    q.into()
}

#[proc_macro]
pub fn aux_path_literal(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let mut l = 1;
    let mut qv = vec![];
    for arg in &input.vars {
        qv.push(if l >= input.vars.len() {
            quote! {#arg}
        } else {
            quote! {#arg, main_separator!(),}
        });
        l += 1;
    }
    quote! {
        concat!(#(#qv)*)
    }.into()
}

#[proc_macro]
pub fn path_literal(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let mut l = 1;
    let mut qv = vec![];
    for arg in &input.vars {
        qv.push(if l >= input.vars.len() {
            quote! {#arg}
        } else {
            quote! {#arg, anansi::main_separator!(),}
        });
        l += 1;
    }
    quote! {
        concat!(#(#qv)*)
    }.into()
}

#[proc_macro]
pub fn record_admin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as RecordAdminArgs);
    let hm = &mut input.hm;
    let path = input.record;
    let ps = quote! {#path}.to_string();
    let name = if let Some((_, n)) = ps.rsplit_once("::") {
        n.trim().to_string()
    } else {
        ps.clone()
    };
    let lower = format_ident!("{}", name.to_lowercase());
    let (cra, request) = if !ps.starts_with("auth ::") {
        (quote! {anansi}, quote!{crate::project::Request})
    } else {
        (quote! {crate}, quote!{crate::util::auth::admin::Request})
    };
    let form_path: syn::Path = syn::parse_str(&format!("super::forms::{}Form", name)).unwrap();
    let mut admin_form = quote! {type AdminForm = #form_path;};
    let mut add_form = quote! {type AddForm = #form_path;};
    let mut fv = vec![];
    let mut auto_fields = true;
    let search = if let Some(search) = hm.remove("search_fields") {
        let all = if let Expr::Array(arr) = search {
            let mut v = vec![];
            for elem in arr.elems.iter() {
                v.push(elem.clone());
            }
            v
        } else {
            panic!("expected array");
        };
        let first = &all[0];
        let rest = &all[1..];
        let q = quote! {
            fn searchable() -> bool {
                true
            }
            fn search(terms: &Vec<String>) -> anansi::db::Whose<Self> {
                use anansi::records::Record;
                use super::records::#lower::*;
                let mut s = if terms.is_empty() {
                    return Self::whose(#first().contains(""));
                } else {
                    Self::whose(#first().icontains(&terms[0]))
                };
                s = s #(.or(#rest().icontains(&terms[0])))*;
                for term in &terms[1..] {
                    s = s #(.or(#all().icontains(term)))*
                }
                s
            }
        };
        q
    } else {
        quote! {}
    };
    fv.push(search);
    for (name, expr) in &input.hm {
        match name.as_str() {
            "form" => {
                admin_form = quote! {type AdminForm = #expr;};
            },
            "add_form" => {
                add_form = quote! {type AddForm = #expr;};
            },
            "fields" => {
                if let Expr::Array(arr) = &expr {
                    auto_fields = false;
                    let mut es = vec![];
                    let mut elements = vec![];
                    let elems = &arr.elems;
                    for elem in elems {
                        es.push(quote! {#elem}.to_string());
                        elements.push(elem);
                    }
                    fv.push(quote! {
                        fn field_names() -> &'static [&'static str] {
                            &[#(#es,)*]
                        }
                        async fn fields(self, _req: &R) -> Vec<String> {
                            use anansi::admin_site::AdminField;
                            vec![#(self.#elements.admin_field(),)*]
                        }
                    });
                } else {
                    panic!("Expected array");
                }
            },
            _ => panic!("Unexpected field: {}", name),
        }
    }
    if auto_fields {
        fv.push(quote! {
            fn field_names() -> &'static [&'static str] {
                use anansi::forms::Form;
                Self::AdminForm::field_names()
            }
            async fn fields(self, req: &R) -> Vec<String> {
                use anansi::forms::{Form, GetData};
                let data = Self::AdminForm::from_record(self, req).await.unwrap();
                let mut form = Self::AdminForm::from_data(data).await;
                form.fill().unwrap();
                let mut v = vec![];
                for field in form.fields() {
                    let s = field.widget().attrs().get("value").unwrap().clone();
                    v.push(s);
                }
                v
            }
        });
    }
    let q = quote! {
        #[async_trait::async_trait]
        impl<R: #request> #cra::util::admin::site::RecordAdmin<R> for #path {
            #admin_form
            #add_form
            #(#fv)*
        }
    };
    q.into()
}

#[proc_macro]
pub fn raw_bulk_update(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let vars = input.vars;
    let req = &vars[0];
    let record_type = &vars[1];
    let records = &vars[2];
    let field_names = &vars[3..];
    let mut fields = vec![];
    for field_name in field_names {
        let lowfield = quote! {#field_name}.to_string().to_lowercase();
        fields.push(quote! {
            u.bulk_set(#lowfield, #record_type::PK_NAME);
            for record in #records {
                u.when(record.pk().to_sql());
                u.then(record.#field_name.to_sql());
            }
            u.end();
        });
    }
    let q = quote! {
        {
            use anansi::records::ToSql;
            let mut u = anansi::db::Update::new(&#record_type::table_name());
            #(
                #fields
            )*
            u.where_pk(#record_type::PK_NAME.to_string())
                .is_in(#records)
                .raw_update(#req.raw().pool())
        }
    };
    q.into()
}

#[proc_macro]
pub fn init_admin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let vars = input.vars;
    let q = quote! {
        pub fn initialize_admin<R: anansi::util::auth::admin::Request + crate::project::Request>(site: anansi::util::admin::site::AdminRef<R>) {
            let mut site = site.lock().unwrap();
            #(#vars)*
        }
    };
    q.into()
}

#[proc_macro]
pub fn register(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as syn::Expr);
    let ps = quote! {#input}.to_string();
    let name = if let Some((_, n)) = ps.rsplit_once("::") {
        n
    } else {
        &ps
    };
    let lower = name.to_lowercase();
    let q = quote! {
        {
            use super::APP_NAME;
            use anansi::admin_site::RecordAdmin;
            site.register(anansi::humanize::capitalize(APP_NAME), anansi::admin_site::RecordEntry {name: #name, index: anansi::util::auth::admin::AuthAdminView::record_index::<#input>, new: anansi::util::auth::admin::AuthAdminView::record_new::<#input>});
            site.urls_mut().push((concat!("/admin/", #lower), anansi::util::auth::admin::AuthAdminView::record_index::<#input>));
            site.urls_mut().push((concat!("/admin/", #lower, "/new"), anansi::util::auth::admin::AuthAdminView::record_new::<#input>));
            site.urls_mut().push((concat!("/admin/", #lower, "/edit/{", #lower, "_id}"), anansi::util::auth::admin::AuthAdminView::record_edit::<#input>));
            if <#input as RecordAdmin<R>>::searchable() {
                site.urls_mut().push((concat!("/admin/", #lower, "/search"), anansi::util::auth::admin::AuthAdminView::record_search::<#input>));
                site.urls_mut().push((concat!("/admin/", #lower, "/filter/new"), anansi::util::auth::admin::AuthAdminView::filter_new::<#input>));
            }
        }
    };
    q.into()
}

#[proc_macro]
pub fn url(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as UrlArgs);
    let req = &input.req;
    let first = &input.first;
    let exprs = &input.exprs;
    let q = quote! {
        #req.reverse(#first, &[#(&anansi::records::ToUrl::to_url(&#exprs)),*])
    };
    q.into()
}

#[proc_macro]
pub fn schemas(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let vars = &input.vars;
    let q = quote! {
        pub const SCHEMAS: &[anansi::syntax::Schema] = &[
            #(#vars::schema),*
        ];
    };
    q.into()
}

#[proc_macro]
pub fn routes(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as SchemaArgs);
    let vars = &input.vars;
    let q = quote! {
        pub const ROUTES: &[anansi::web::Route<crate::project::HttpRequest>] = &[#(#vars),*];
        pub fn app_url(hm: &mut std::collections::HashMap<usize, Vec<String>>) {
            use crate::project::HttpRequest;
            let mut v = vec![];
            for route in ROUTES {
                match route {
                    anansi::web::Route::Path((url, f)) => {
                        v.push(((*url).to_string(), *f));
                    },
                    anansi::web::Route::Import((url, r)) => {
                        for rt in *r {
                            match rt {
                                anansi::web::Route::Path((u, f)) => {
                                    v.push((format!("{}/{}", url, *u), *f));
                                },
                                _ => unimplemented!(),
                            }
                        }
                    }
                }
            }
            for (url, f) in v {
                let cap = anansi::router::get_capture(&url).unwrap();
                hm.insert(f as usize, cap);
            }
        }
    };
    q.into()
}

#[proc_macro_attribute]
pub fn function_component(args: proc_macro::TokenStream, _input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as Ident);
    let id = quote!{#args}.to_string().trim().to_lowercase();
    let name = format!(".parsed/{}.rs", id);
    quote! {
        include!(#name);
    }.into()
}

#[proc_macro_attribute]
pub fn component(args: proc_macro::TokenStream, _input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let args = parse_macro_input!(args as Ident);
    let id = quote!{#args}.to_string().trim().to_lowercase();
    let name = format!(".parsed/{}.rs", id);
    quote! {
        include!(#name);
    }.into()
}
