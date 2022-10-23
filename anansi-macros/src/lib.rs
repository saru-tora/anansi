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
        let m = format_ident!("{}", member);
        sets.push(quote!{.set(#member, &self.#m)});
        saves.push(quote!{.value(&self.#m)});
    }
    let (pt, _pkty, pdt) = match pkd.ty.as_str() {
        "BigInt" => (quote! {anansi::records::BigInt}, quote! {i64}, quote! {anansi::records::Record}),
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
                    #name::new(#(#mv),*)
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
    let table = quote! {&format!("{}_{}", super::init::APP_NAME, #lowercase)};
    let table_name = quote! {format!("{}_{}", super::init::APP_NAME, #lowercase)};
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
            fn get(row: anansi::db::DbRow) -> anansi::web::Result<Self> {
                Ok(Self {#init})
            }
            fn from(rows: anansi::db::DbRowVec) -> anansi::web::Result<anansi::records::Objects<Self>> {
                let mut mv = anansi::records::Objects::new();
                for row in rows {
                    mv.push(Self::get(row)?);
                }
                Ok(mv)
            }
            fn order_by(w: anansi::db::OrderByArg<Self>) -> anansi::db::OrderBy<Self> {
                anansi::db::OrderBy::from(anansi::db::Builder::select(&[#(#members),*], #table).order_by().push_str(&w.builder().val()))
            }
            fn table_name() -> String {
                #table_name
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
                use anansi::records::Relate;
                anansi::transact!(req, {
                    self.on_delete(req).await?;
                    anansi::db::delete_from(#table, Self::PK_NAME, #primary, req).await
                })
            }
            async fn save<R: anansi::web::BaseRequest>(self, req: &R) -> anansi::web::Result<Self> {
                use anansi::records::Relate;
                anansi::transact!(req, {
                    self.on_save(req).await?;
                    self.raw_save(req.raw().pool()).await
                })
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
                #record_tuple::new(anansi::records::Text::from("auth_user".to_string()), req.user().pk(), None, self.pk(), anansi::records::Text::from("owner".to_string())).save(req).await?;
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
                    Err(anansi::db::invalid())
                }
            }
            fn validate(&mut self) -> anansi::web::Result<#record_data> {
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
                        return Err(anansi::db::invalid());
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
                                        <anansi::records::#ty as anansi::records::DataType>::from_val(s)?
                                    } else {
                                        return Err(anansi::db::invalid());
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
                        let m2 = member.clone();
                        let column = quote! {&format!("{}_{}.{}", super::init::APP_NAME, #fname, #member)};
                        let lowcolumn = quote! {&format!("{}_{}.{}", super::super::init::APP_NAME, #fname, #member)};
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
                                        let q = quote! {pub fn #name() -> #mfield<#mname> {let full_name = format!("{}_{}", super::super::init::APP_NAME, #fname); let join = format!("{}_{}", full_name, #lower); #mfield::new(anansi::db::Builder::new().inner_join(&join, &full_name, "id", &format!("{}_id", #fname)).inner_join(&format!("{}_{}", super::super::init::APP_NAME, #lower), &join, &format!("{}_id", #lower), "id"))}};
                                        let q2 = quote! {pub fn #name(self) -> #mfield<F> {let full_name = format!("{}_{}", super::init::APP_NAME, #fname); let join = format!("{}_{}", full_name, #lower); #mfield::new(anansi::db::Builder::new().inner_join(&join, &full_name, "id", &format!("{}_id", #fname)).inner_join(&format!("{}_{}", super::init::APP_NAME, #lower), &join, &format!("{}_id", #lower), "id"))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        quote_spanned! {f.span() =>
                                            #name: anansi::records::ManyToMany::new(),
                                        }
                                    },
                                    "BigInt" => {
                                        let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::records::BigInt> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::records::BigInt> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        if is_pk {
                                            let q3 = quote! {pub fn pk() -> anansi::db::Column<#mname, anansi::records::BigInt> {anansi::db::Column::new(#lowcolumn)}};
                                            fv.push(q3);
                                        }
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::records::BigInt as anansi::records::DataType>::from_val(row.try_get(#m2)?)?,
                                        }
                                    },
                                    "ForeignKey" => {
                                        if pkd.ty == "BigInt" {
                                            let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::records::BigInt> {anansi::db::Column::new(#lowcolumn)}};
                                            let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::records::BigInt> {anansi::db::Column::from(self.b.push_str(#column))}};

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
                                                    #name: <anansi::records::#fty as anansi::records::DataType>::from_val(row.try_get(#m2)?)?,
                                                }
                                            } else {
                                                quote_spanned! {f.span() =>
                                                    #name: {
                                                        let o: Option<i64> = row.try_get(#m2)?;
                                                        if let Some(n) = o {
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
                                        let q2 = quote! {pub fn #name<'a>(self) -> anansi::db::Column<F, anansi::records::DateTime> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::records::DateTime as anansi::records::DataType>::from_val(row.try_get(#m2)?)?,
                                        }
                                    },
                                    "Boolean" => {
                                        let q = quote! {pub fn #name() -> anansi::db::Column<#mname, anansi::records::Boolean> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name(self) -> anansi::db::Column<F, anansi::records::Boolean> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        quote_spanned! {f.span() =>
                                            #name: <anansi::records::Boolean as anansi::records::DataType>::from_val(row.try_get(#m2)?)?,
                                        }
                                    },
                                    "VarChar" => {
                                        let q = quote! {pub fn #name<'a>() -> anansi::db::Column<#mname, anansi::records::#fty> {anansi::db::Column::new(#lowcolumn)}};
                                        let q2 = quote! {pub fn #name<'a>(self) -> anansi::db::Column<F, anansi::records::#fty> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        if !null {
                                            quote_spanned! {f.span() =>
                                                #name: <anansi::records::#fty as anansi::records::DataType>::from_val(row.try_get(#m2)?)?,
                                            }
                                        } else {
                                            quote_spanned! {f.span() =>
                                                #name: {
                                                    let o: Option<String> = row.try_get(#m2)?;
                                                    if let Some(s) = o {
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
                                        let q2 = quote! {pub fn #name<'a>(self) -> anansi::db::Column<F, anansi::records::Text> {anansi::db::Column::from(self.b.push_str(#column))}};
                                        fv.push(q);
                                        fv2.push(q2);
                                        members.push(member);
                                        if !null {
                                            quote_spanned! {f.span() =>
                                                #name: anansi::records::Text::from(row.try_get(#m2)?),
                                            }
                                        } else {
                                            quote_spanned! {f.span() =>
                                                #name: {
                                                    let o: Option<String> = row.try_get(#m2)?;
                                                    o.map(|s| anansi::records::Text::from(s))
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

#[proc_macro_attribute]
pub fn base_view(_args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let sig = &input.sig;
    let generics = &sig.generics;
    let fname = &sig.ident;
    let fnargs = &sig.inputs;
    let name = String::from("/.parsed/") + &sig.ident.to_string() + ".in";
    let args_name = String::from("/.parsed/") + &sig.ident.to_string() + "_args.in";
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
pub fn view(args: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as ItemFn);
    let args = parse_macro_input!(args as SchemaArgs);
    let vars = args.vars;
    let vis = input.vis;
    let sig = input.sig;
    let name = String::from("/.parsed/") + &sig.ident.to_string() + ".in";

    let stmts = input.block.stmts;
    let q = quote! {
        #[anansi::check(#(#vars)*)]
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
                },
                GenericParam::Lifetime(lt) => {
                    let l = &lt.lifetime;
                    v.push(quote!{#l});
                },
                _ => unimplemented!(),
            }
        }
        generic_idents = quote! {::<#(#v,)*>};
        quote! {#_sig_ident::#generics}
    };
    let q = quote! {
        async fn #_sig_ident #generics (#req: #ty) -> #rty #where_clause {
            #(#vars(&#req).await?;)*
            #(#stmts)*
        }
        #vis fn #sig_ident #generics (_raw: #ty) -> std::pin::Pin<Box<dyn std::future::Future<Output = #rty> + Send>> #where_clause {
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
pub fn record_view(_metadata: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
pub fn record(_metadata: proc_macro::TokenStream, input: proc_macro::TokenStream) -> proc_macro::TokenStream {
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
                        fields.named.insert(0, syn::Field::parse_named.parse2(quote! { #[field(primary_key = "true", default_fn = "anansi::records::generate_id")] id: anansi::records::BigInt}).unwrap());
                    }
                }
                _ => {},
            }

            let record_tuple = format_ident!("{}Tuple", ast.ident);
            let lower = format_ident!("{}", record_tuple.to_string().to_lowercase());
            let tuple = quote! {
                #[derive(anansi::Record)]
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
                        Err(anansi::db::invalid())
                    }
                }
            };
            
            let q = quote! {
                #[derive(anansi::Record)]
                #ast
                #tuple
            };
            return q.into();
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

struct RecordAdminArgs {
    hm: HashMap<String, Expr>,
}

impl Parse for RecordAdminArgs {
    fn parse(input: ParseStream) -> Result<Self> {
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
pub fn record_admin(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut input = parse_macro_input!(input as RecordAdminArgs);
    let hm = &mut input.hm;
    let path = hm.remove("record").unwrap();
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
            use super::init::APP_NAME;
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
