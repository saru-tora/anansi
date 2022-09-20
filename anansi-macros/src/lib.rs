extern crate proc_macro2;
use std::collections::HashMap;
use proc_macro2::TokenStream;
use syn::{parse_macro_input, DeriveInput, Data, Fields, Ident, ItemFn, Token, Attribute, Expr};
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

#[proc_macro_derive(Model, attributes(field))]
pub fn model_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let lowercase = name.to_string().to_lowercase();
    let lowname = format_ident!("{}", lowercase);
    let mut fv = Vec::new();
    let mut fv2 = Vec::new();
    let mut members = Vec::new();
    let mut pkd = PkData::new();
    let init = model_init(&name, &lowercase, &input.data, &mut pkd, &mut members, &mut fv, &mut fv2);
    let pk_name = &pkd.name;
    let pk_id = format_ident!("{}", pkd.name);
    let values = &pkd.values;
    let params = &pkd.params;
    let mut sets = Vec::new();
    let mut saves = Vec::new();
    for member in &members {
        let m = format_ident!("{}", member);
        sets.push(quote!{.set(#member, &self.#m)});
        saves.push(quote!{.value(&self.#m)});
    }
    let (pt, _pkty, pdt) = match pkd.ty.as_str() {
        "BigInt" => (quote! {anansi::models::BigInt}, quote! {i64}, quote! {anansi::models::Model}),
        _ => unimplemented!(),
    };
    for (fname, fks) in &pkd.fkv {
        let fkn: Vec<&str> = fks.split("::").collect();
        let fk = fkn.last().unwrap().trim().to_string();
        let full: Type = syn::parse_str(fks).unwrap();
        let by_model = format_ident!("by_{}", fk.to_string().to_lowercase());
        let modelset = format_ident!("{}{}Set", fk, name);
        let mut mv = Vec::new();
        let mut mtv = Vec::new();
        let pkn = format_ident!("{}", pk_name);
        for (mt0, mt1) in &pkd.member_type {
            if mt0 != fname && *mt0 != pkn {
                mv.push(quote! {#mt0});
                mtv.push(quote! {#mt0: #mt1});
            } else if mt0 == fname {
                mv.push(quote! {ForeignKey::new(self.model)});
            }
        }
        let q3 = quote! {
            impl #name {
                pub fn #by_model(model: &#full) -> #modelset {
                    #modelset {model}
                }
            }
            pub struct #modelset<'a> {
                model: &'a #full,
            }
            impl<'a> #modelset<'a> {
                pub fn new(&self, #(#mtv),*) -> #name {
                    #name::new(#(#mv),*)
                }
                pub fn order_by(&self, o: anansi::db::OrderByArg<#name>) -> anansi::db::OrderBy<#name> {
                    use anansi::models::Model;
                    #name::whose(#fname().eq().data(<#full as #pdt>::pk(self.model))).order_by(o)
                }
            }
        };
        fv.push(q3);
    }
    let table = quote! {&format!("{}_{}", super::init::APP_NAME, #lowercase)};
    let model_fields = format_ident!("{}Fields", name);
    
    let primary = quote! {<#name as #pdt>::pk(&self)};
    let expanded = quote! {
        #[async_trait::async_trait]
        impl anansi::models::Model for #name {
            type Pk = #pt;
            const PK_NAME: &'static str = #pk_name;
            fn pk(&self) -> #pt {
                self.#pk_id.clone()
            }
            fn find(d: #pt) -> anansi::db::Whose<Self> {
                Self::whose(#lowname::pk().eq().data(d))
            }
            fn find_in(keys: &Vec<#pt>) -> anansi::db::Limit<Self> {
                Self::whose(#lowname::pk().is_in().data(keys)).order_by(#lowname::pk().field(keys)).limit(keys.len() as u32)
            }
            fn count() -> anansi::db::Count<Self> {
                anansi::db::Count::from(anansi::db::Builder::count(#table))
            }
            fn whose(w: anansi::db::WhoseArg<Self>) -> anansi::db::Whose<Self> {
                anansi::db::Whose::from(anansi::db::Builder::select(&[#(#members),*], #table).whose().append(w.builder()))
            }
            fn get(row: anansi::db::DbRow) -> anansi::web::Result<Self> {
                Ok(Self {#init})
            }
            fn from(rows: anansi::db::DbRowVec) -> anansi::web::Result<anansi::models::Objects<Self>> {
                let mut mv = anansi::models::Objects::new();
                for row in rows {
                    mv.push(Self::get(row)?);
                }
                Ok(mv)
            }
            fn order_by(w: anansi::db::OrderByArg<Self>) -> anansi::db::OrderBy<Self> {
                anansi::db::OrderBy::from(anansi::db::Builder::select(&[#(#members),*], #table).order_by().push_str(&w.builder().val()))
            }
            async fn update<B: anansi::web::BaseRequest>(&mut self, req: &B) -> anansi::web::Result<()> {
                                                                                                self.raw_update(req.raw().pool()).await
            }
            async fn raw_update(&mut self, pool: &anansi::db::DbPool) -> anansi::web::Result<()> {
                let u: anansi::db::Update<Self> = anansi::db::Update::new(#table)
                    #(#sets)*.pk(Self::PK_NAME, #primary);
                u.raw_update(pool).await
            }
            async fn delete<B: anansi::web::BaseRequest>(&self, req: &B) -> anansi::web::Result<()> {
                anansi::db::delete_from(#table, Self::PK_NAME, #primary, req).await
            }
            async fn save<B: anansi::web::BaseRequest>(self, req: &B) -> anansi::web::Result<Self> {
                self.raw_save(req.raw().pool()).await
            }
            async fn raw_save(self, pool: &anansi::db::DbPool) -> anansi::web::Result<Self> {
                let i: anansi::db::Insert<Self> = anansi::db::Insert::new(#table, &[#(#members),*])
                    #(#saves)*;
                i.raw_save(pool).await?;
                Ok(self)
            }
        }
        impl #name {
            pub fn new(#(#params)*) -> Self {
                Self {#(#values),*}
            }
        }
        pub mod #lowname {
            use super::*;
            use anansi::db::Column;
            #(#fv)*
        }
        pub struct #model_fields<F: anansi::models::Model> {
            b: anansi::db::Builder<F>,
        }
        impl<F: anansi::models::Model> #model_fields<F> {
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
    member_type: Vec<(Ident, TokenStream)>,
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

#[proc_macro_derive(FromParams)]
pub fn from_params_macro_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;
    let param = format!("{}_id", name.to_string().to_lowercase());
    let expanded = quote! {
        #[async_trait::async_trait]
        impl anansi::models::FromParams for #name {
            async fn from_params(params: &anansi::web::Parameters) -> anansi::web::Result<anansi::db::Whose<Self>> {
                let id = anansi::humanize::decode(params.get(#param)?)?;
                use anansi::models::Model;
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
        impl anansi::models::ToUrl for #name {
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
    let init = form_init(&input.data, &mut data_members, &mut members, &mut members2, &mut member_names, &mut fv, &mut fv2);
    let model_data = format_ident!("{}Data", name);
    let model_fields = format_ident!("{}Fields", name);
    let expanded = quote! {
        #[derive(Clone)]
        pub struct #model_data {
            #init
        }
        impl #model_data {
            pub fn new(#(#data_members,)*) -> Self {
                Self{#(#member_names,)*}
            }
        }
        #[async_trait::async_trait]
        impl anansi::forms::Form for #name {
            type Data = #model_data;
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
            fn from_post<B: anansi::web::BaseRequest + anansi::web::CsrfDefense>(req: &mut B) -> anansi::web::Result<Self> {
                use anansi::web::CsrfDefense;
                let mut form_data = req.check_token()?;
                let data = #model_data {
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
                    Err(anansi::db::invalid())
                }
            }
            fn validate(&mut self) -> anansi::web::Result<#model_data> {
                if let Some(data) = self.data.take() {
                    Ok(data)
                } else {
                    Err(anansi::db::invalid())
                }
            }
            fn errors(&self) -> &anansi::forms::FormErrors {
                &self.errors
            }
            fn add_error(&mut self, e: Box<dyn std::error::Error + Send + Sync>) {
                self.errors.add_error(e);
            }
        }
        impl #name {
            pub fn set_data(&mut self, data: Option<#model_data>) {
                self.data = data
            }
            pub fn fields(&self) -> #model_fields {
                #model_fields {counter: 0, form: &self}
            }
            pub fn check_field_errors(&self) -> anansi::web::Result<()> {
                for field in self.fields() {
                    if !field.errors().is_empty() {
                        return Err(anansi::db::invalid());
                    }
                }
                Ok(())
            }
        }
        pub struct #model_fields<'a> {
            counter: usize,
            form: &'a #name,
        }
        impl<'a> Iterator for #model_fields<'a> {
            type Item = &'a dyn anansi::forms::Field;
            fn next(&mut self) -> Option<Self::Item> {
                let field = match self.counter {
                    #(#members)*
                    _ => None,
                };
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
    let name = input.ident;
    let mut v = Vec::new();
    let has_model = if !args.vars.is_empty() {
        let item = &args.vars[0];
        quote! {
            impl anansi::forms::HasModel for #name {
                type Item = #item;
            }
        }
    } else {
        quote! {}
    };
    let model_data = format_ident!("{}Data", name);
    v.push(quote! {csrf_token: Option<String>});
    v.push(quote! {attrs: anansi::forms::Attributes});
    v.push(quote! {data: Option<#model_data>});
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
        pub struct #name {
            #(#v),*
        }
        #has_model
    };
    q.into()
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

fn form_init(data: &Data, data_members: &mut Vec<TokenStream>, members: &mut Vec<TokenStream>, members2: &mut Vec<TokenStream>, member_names: &mut Vec<Ident>, fv: &mut Vec<TokenStream>, fv2: &mut Vec<TokenStream>) -> TokenStream {
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
                            quote! {anansi::forms::Text}
                        };
                        let ty = &f.ty;
                        let q = quote! {
                            #name: <#ty>::new(#label, Box::new(#widget {name: #ns, attrs: anansi::forms::Attributes::new().id(#ns).pass("required", "")})),
                        };
                        let q2 = quote! {
                            #name: {
                                let s = form_data.remove(#ns)?;
                                if !s.is_empty() {
                                    <anansi::models::#ty as anansi::models::DataType>::from_val(s)?
                                } else {
                                    return Err(anansi::db::invalid());
                                }
                            },
                        };
                        let q3 = quote! {
                            #n => Some(&self.form.#name as &dyn anansi::forms::Field),
                        };
                        let q4 = quote! {
                            self.#name.mut_widget().mut_attrs().insert("value", &anansi::web::html_escape(&format!("{}", data.#name)));
                        };
                        n += 1;
                        fv.push(q);
                        fv2.push(q2);
                        data_members.push(quote! {#name: anansi::models::#ty});
                        members.push(q3);
                        members2.push(q4);
                        member_names.push(name.clone().unwrap());
                        quote_spanned! {f.span() =>
                            pub #name: anansi::models::#ty,
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

fn model_init(mname: &Ident, fname: &str, data: &Data, pkd: &mut PkData, members: &mut Vec<String>, fv: &mut Vec<TokenStream>, fv2: &mut Vec<TokenStream>) -> TokenStream {
    match *data {
        Data::Struct(ref data) => {
            match data.fields {
                Fields::Named(ref fields) => {
                    let mut n: usize = 0;
                    let recurse = fields.named.iter().map(|f| {
                        let name = &f.ident;
                        let member = name.as_ref().unwrap().to_string();
                        let m2 = member.clone();
                        let column = quote! {&format!("{}_{}.{}", super::init::APP_NAME, #fname, #member)};
                        let lowcolumn = quote! {&format!("{}_{}.{}", super::super::init::APP_NAME, #fname, #member)};
                        let attrs = get_attrs(&f.attrs);
                        let fty = &f.ty;
                        pkd.member_type.push((name.as_ref().unwrap().clone(), quote! {#fty}));
                        match &f.ty {
                            Path(path) => {
                                let segment = path.path.segments.last().unwrap().ident.to_string();
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
                                    pkd.values.push(quote! {#name: anansi::models::ManyToMany::new()});
                                }
                                match segment.as_str() {
                                    "ManyToMany" => {
                                        let ty = &f.ty;
                                        let ty = quote! {#ty}.to_string();
                                        let ty = &ty[segment.len()+3..ty.len()-2];
                                        let lower = ty.to_lowercase();
                                        let mfield = format_ident!("{}Fields", ty);
                                        let q = quote! {pub fn #name() -> #mfield<#mname> {let full_name = format!("{}_{}", super::super::init::APP_NAME, #fname); let join = format!("{}_{}", full_name, #lower); #mfield::new(anansi::db::Builder::new().inner_join(&join, &full_name, "id", &format!("{}_id", #fname)).inner_join(&format!("{}_{}", super::super::init::APP_NAME, #lower), &join, &format!("{}_id", #lower), "id"))}};
                                        let q2 = quote! {pub fn #name(self) -> #mfield<F> {let full_name = format!("{}_{}", super::init::APP_NAME, #fname); let join = format!("{}_{}", full_name, #lower); #mfield::new(anansi::db::Builder::new().inner_join(&join, &full_name, "id", &format!("{}_id", #fname)).inner_join(&format!("{}_{}", super::init::APP_NAME, #lower), &join, &format!("{}_id", #lower), "id"))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        quote_spanned! {f.span() =>
                                            #name: anansi::models::ManyToMany::new(),
                                        }
                                    },
                                    "BigInt" => {
                                        let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::models::BigInt> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::models::BigInt> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        if is_pk {
                                            let q3 = quote! {pub fn pk() -> anansi::db::Column<#mname, anansi::models::BigInt> {anansi::db::Column::new(#lowcolumn)}};
                                            fv.push(q3);
                                        }
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::models::BigInt as anansi::models::DataType>::from_val(row.try_get(#m2)?)?,
                                        }
                                    },
                                    "ForeignKey" => {
                                        if pkd.ty == "BigInt" {
                                            let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::models::BigInt> {anansi::db::Column::new(#lowcolumn)}};
                                            let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::models::BigInt> {anansi::db::Column::from(self.b.push_str(#column))}};

                                            fv.push(q);
                                            fv2.push(q2);
                                            members.push(member);
                                            let mut ts = ty_string(&f.ty, &segment);
                                            if let Some(t) = ts.split_once(',') {
                                                ts = t.0.trim().to_string();
                                            }
                                            pkd.fkv.push((name.as_ref().unwrap().clone(), ts));
                                            let ty = &f.ty;
                                            quote_spanned! {f.span() =>
                                                #name: <anansi::models::#ty as anansi::models::DataType>::from_val(row.try_get(#m2)?)?,
                                            }
                                        } else {
                                            panic!("unexpected primary key type for foreign key");
                                        }
                                    },
                                    "DateTime" => {
                                        let q = quote! {pub fn #name<'a>() -> anansi::db::Column<#mname, anansi::models::DateTime> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name<'a>(self) -> anansi::db::Column<F, anansi::models::DateTime> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::models::DateTime as anansi::models::DataType>::from_val(row.try_get(#m2)?)?,
                                        }
                                    },
                                    "Boolean" => {
                                        let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::models::Boolean> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::models::Boolean> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::models::Boolean as anansi::models::DataType>::from_val(row.try_get(#m2)?)?,
                                        }
                                    },
                                    "VarChar" => {
                                        let ty = &f.ty;
                                        let q = quote! {pub fn #name<'a>() -> anansi::db::Column<#mname, anansi::models::#ty> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name<'a>(self) -> anansi::db::Column<F, anansi::models::#ty> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::models::#ty as anansi::models::DataType>::from_val(row.try_get(#m2)?)?,
                                        }
                                    },
                                    "Text" => {
                                        let ty = &f.ty;
                                        let q = quote! {pub fn #name<'a>() -> anansi::db::Column<#mname, anansi::models::#ty> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name<'a>(self) -> anansi::db::Column<F, anansi::models::#ty> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: Text::from(row.try_get(#m2)?),
                                        }
                                    },
                                    _ => {
                                        n += 1;
                                        let m = n - 1;
                                        quote_spanned! {f.span() =>
                                            #name: #path::from(&dv[n + #m])?,
                                        }
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

#[proc_macro_attribute]
pub fn view(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as Args);
    let vis = input.vis;
    let sig = input.sig;
    let func = &args.vars[0];
    let name = String::from("/.parsed/") + &sig.ident.to_string() + ".in";

    let stmts = input.block.stmts;
    let q = quote! {
        #[anansi::check(#func)]
        #vis #sig {
            #(#stmts)*
            include!(concat!("templates", #name))
        }
    };
    q.into()
}

#[proc_macro_attribute]
pub fn check(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as Args);
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
    let q = quote! {
        async fn #_sig_ident(#req: #ty) -> #rty {
            #(#stmts)*
        }
        pub fn #sig_ident(_raw: #ty) -> std::pin::Pin<Box<dyn std::future::Future<Output = #rty> + Send>> {
            Box::pin(#func(_raw, Self::#_sig_ident))
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
pub fn model(_metadata: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut ast = parse_macro_input!(input as DeriveInput);
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
                        fields.named.insert(0, syn::Field::parse_named.parse2(quote! { #[field(primary_key = "true", default_fn = "anansi::models::generate_id")] id: anansi::models::BigInt}).unwrap());
                    }
                }
                _ => {},
            }
            
            return quote! {
                #[derive(anansi::Model)]
                #ast
            }.into();
        }
        _ => panic!("macro has to be used on a struct"),
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
pub fn url(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as UrlArgs);
    let req = &input.req;
    let first = &input.first;
    let exprs = &input.exprs;
    let q = quote! {
        #req.reverse(#first, &[#(&anansi::models::ToUrl::to_url(&#exprs)),*])
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
        pub const ROUTES: &[anansi::web::Route<crate::settings::HttpRequest>] = &[#(#vars),*];
        pub fn app_url(hm: &mut std::collections::HashMap<usize, Vec<String>>) {
            use crate::settings::HttpRequest;
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
