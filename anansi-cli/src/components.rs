use std::fs;
use std::path::PathBuf;
use std::str::Chars;
use crate::{collect, collect_var, collect_name, Parser};
use syn::{Ident};
use syn::parse::{Result, Parse, ParseStream};
use quote::{quote, format_ident};
use proc_macro2::TokenStream;
use toml::Value;
use toml::map::Map;

use syn::Expr::*;
use std::collections::{HashSet, HashMap};

use crate::{VERSION, cargo, make_file, append, get_src};

pub fn get_expr(chars: &mut Chars) -> String {
    custom_get_expr(chars, 0, 0)
}

pub fn custom_get_expr(chars: &mut Chars, mut paren: u32, mut bracket: u32) -> String {
    let mut s = String::new();
    while let Some(c) = chars.next() {
        match c {
            ';' => if bracket == 0 && paren == 0 {
                break
            },
            '(' => paren += 1,
            ')' => {
                paren -= 1;
                if bracket == 0 && paren == 0 {
                    s.push(c);
                    break;
                }
            }
            '{' => {
                bracket += 1;
            }
            '}' => {
                bracket -= 1;
                if bracket == 0 && paren == 0 {
                    s.push(c);
                    break;
                }
            }
            _ => {}
        }
        s.push(c);
    }
    s
}

pub fn check_components(path: PathBuf, changed: &mut bool) {
    let content = fs::read_to_string(&path).unwrap();
    match content.split_once("#[component(") {
        Some((_, split)) => {
            parse_component(split, &path, false);
            *changed = true;
        }
        None => {}
    };
    match content.split_once("#[function_component(") {
        Some((_, split)) => {
            parse_component(split, &path, true);
            *changed = true;
        }
        None => {}
    };
}

#[derive(Clone, Debug)]
pub struct Local(Vec<(String, (TokenStream, usize))>);

impl Local {
    pub fn new() -> Self {
        Self {0: vec![]}
    }
    fn insert(&mut self, name: String, ty: TokenStream) {
        self.0.push((name, (ty, self.0.len())));
    }
    pub fn get(&self, name: &str) -> Option<&(TokenStream, usize)> {
        for (k, v) in &self.0 {
            if k == name {
                return Some(v);
            }
        }
        None
    }
    fn len(&self) -> usize {
        self.0.len()
    }
}

fn var_ident(var: &str) -> (TokenStream, Ident) {
    if let Some((m, v)) = var.split_once(" ") {
        if m.trim() == "mut" {
            (quote! {mut}, format_ident!("{}", v.trim()))
        } else {
            panic!("expected mut keyword");
        }
    } else {
        (quote! {}, format_ident!("{}", var.trim()))
    }
}

fn parse_component(split: &str, path: &PathBuf, fn_comp: bool) {
    let mut chars = split.chars();
    let component = format_ident!("{}", collect(&mut chars, ')').trim());
    let component = quote! {#component};
    let fcomp = if fn_comp {
        quote! {
            pub struct #component;
        }
    } else {
        quote! {}
    };
    collect(&mut chars, ']');
    let f = collect_nws(&mut chars);
    if f.trim() != "fn" {
        panic!("expected function for component");
    }
    let (fname, _) = collect_name(&mut chars);
    if fname.trim() != "init" {
        panic!("expected init function for component");
    }
    let p_arg = collect(&mut chars, ')').trim().to_string();
    let (props, properties) = if let Some((p, t)) = p_arg.split_once(':') {
        let props = format_ident!("{}", p);
        let properties: syn::Path = syn::parse_str(&t).unwrap();
        (props, quote! {#properties})
    } else {
        let props = format_ident!("_props");
        (props, quote! {anansi_aux::EmptyProp})
    };
    collect(&mut chars, '{');
    let mut n: u8 = 1;
    let mut body = String::new();
    while let Some(c) = chars.next() {
        body.push(c);
        if c == '{' {
            n += 1;
        } else if c == '}' {
            n -= 1;
            if n == 0 {
                break;
            }
        }
    }
    let mut bchars = body.chars();
    let mut rsx = quote!{};
    let mut html = quote!{};
    let mut start = vec![];
    let mut callbacks = vec![];
    let mut init = vec![];
    let mut restart_init = vec![];
    let mut resource_calls = vec![];
    let mut res_fn = vec![];
    let mut resource_map = HashMap::new();
    let mut resource_types = HashMap::new();
    let mut selectors = HashSet::new();
    let mut style = String::new();
    let mut comp_rsx_ids = vec![];
    let mut restart_comp_rsx_ids = vec![];
    let mut local = Local::new();
    if props != "_props" {
        local.insert(props.to_string(), properties.clone());
    }

    let cs = quote! {#component}.to_string();
    let lower_comp = format_ident!("{}", cs.to_lowercase());
    let comp_mount = format_ident!("{}_mount", lower_comp);
    let comp_render = format_ident!("{}_render", lower_comp);
    let comp_set_render = format_ident!("{}_set_render", lower_comp);
    let mut comp_refs = HashMap::new();
    let mut svars = vec![];

    loop {
        let token = collect_nws(&mut bchars);
        if token.is_empty() {
            break;
        }
        if token.starts_with("//") {
            collect(&mut bchars, '\n');
            continue;
        }
        let trimmed = token.trim();
        match trimmed {
            "let" => {
                let var = collect(&mut bchars, '=');
                let expr = get_expr(&mut bchars);
                if expr.contains("Self") {
                    let vs = var.trim().to_string();
                    let (mt, var) = var_ident(&vs);
                    local.insert(var.to_string(), component.clone());
                    let e: syn::Expr = syn::parse_str(&expr).unwrap();
                    init.push(quote!{let #mt #var = #e;});
                    restart_init.push(quote!{let #mt #var = #e;});
                } else {
                    let parsed: syn::Expr = syn::parse_str(&expr).unwrap();
                    let mut add_proxy = AddProxy::new();
                    let mut members = vec![];

                    let p = match &parsed {
                        Macro(expr_macro) => {
                            let id = expr_macro.mac.path.segments.last().unwrap().ident.to_string();
                            match id.as_str() {
                                "signal" => {
                                    let tokens = &expr_macro.mac.tokens;
                                    let s = quote! {#tokens}.to_string();
                                    let (ty, val) = s.split_once(',').expect("expected comma");
                                    let ty: syn::Path = syn::parse_str(ty).expect("problem parsing type");
                                    let val: syn::Expr = syn::parse_str(val).expect("problem parsing type");
                                    let (mt, var_id) = var_ident(&var);
                                    local.insert(var_id.to_string(), quote!{anansi_aux::Signal<#ty>});
                                    let q = quote!{let #mt #var_id = <anansi_aux::Signal<#ty>>::new(#val);};
                                    q
                                }
                                "callback" => {
                                    let tokens = &expr_macro.mac.tokens;
                                    let mut args: ResourceArgs = syn::parse2(tokens.clone()).unwrap();

                                    let name = format_ident!("{}_{}", component.to_string().to_lowercase(), var);
                                    let ns = name.to_string();
                                    start.push(quote! {(#ns, #comp_mount, #name)});

                                    let block = args.exprs.pop().expect("expected block");
                                    let mut scope_vars = vec![];
                                    for var in &args.exprs {
                                        let vs = quote! {#var}.to_string();
                                        let (ty, n) = local.get(&vs).expect("unexpected variable");
                                        scope_vars.push(quote! {
                                            let mut #var = _scope[#n].borrow_mut();
                                            let #var = #var.downcast_mut::<#ty>().expect("problem restoring variable");
                                        });
                                    }
                                    let block = add_proxy.check_expr(&block, &mut members);

                                    local.insert(ns, quote!{()});

                                    let q = quote! {
                                        fn #name() {
                                            let _scope = anansi_aux::lexical_scope();
                                            #(#scope_vars)*
                                            #block
                                        }
                                    };
                                    callbacks.push(q);
                                    continue;
                                }
                                "resource" => {
                                    let tokens = &expr_macro.mac.tokens;
                                    let mut args: ResourceArgs = syn::parse2(tokens.clone()).unwrap();
                                    let (_, vars) = var.split_once("(").unwrap();
                                    let (first, rest) = vars.split_once(',').unwrap();

                                    let res_match = format_ident!("{}_match", first.trim());
                                    let (name, _) = rest.split_once(')').unwrap();
                                    let simple_name = name.trim().to_string();
                                    let name = format_ident!("{}_{}", component.to_string().to_lowercase(), name.trim());

                                    let ns = name.to_string();
                                    start.push(quote! {(#ns, #comp_mount, #name)});

                                    let ty = &args.ty;
                                    let block = args.exprs.pop().expect("expected block");
                                    let mut scope_vars = vec![];
                                    let mut n = 0_usize;
                                    let mut ref_nums = vec![];
                                    for var in &args.exprs {
                                        let vs = quote! {#var}.to_string();
                                        let (ty, rn) = local.get(&vs).expect("unexpected variable");
                                        scope_vars.push(quote! {
                                            let mut #var = _scope[#n].borrow_mut();
                                            let #var = #var.downcast_mut::<#ty>().expect("problem restoring variable");
                                        });
                                        ref_nums.push(*rn);
                                        n += 1;
                                    }
                                    resource_types.insert(first.to_string(), quote! {#ty});
                                    comp_refs.insert(simple_name, ref_nums);
                                    let block = add_proxy.check_expr(&block, &mut members);

                                    resource_map.insert(first.trim().to_string(), ty.clone());

                                    let res_num = local.len();
                                    local.insert(first.to_string(), quote! {Rendered});

                                    callbacks.push(quote! {
                                        fn #name() {
                                            let req = {
                                                let _scope = anansi_aux::lexical_scope();
                                                #(#scope_vars)*
                                                #block
                                            };
                                            anansi_aux::APP_STATE.with(|state| {
                                                let state = state.borrow();
                                                let state = state.as_ref().expect("problem borrowing state");
                                                #res_match(state, Resource::Pending, #res_num);
                                            });
                                            #comp_set_render();
                                            wasm_bindgen_futures::spawn_local(async move {
                                                let text = req.send().await;
                                                let text = match text {
                                                    Ok(r) => r.text().await.or_else(|e| Err(Box::new(e) as Box<dyn std::error::Error>)),
                                                    Err(e) => Err(Box::new(e) as Box<dyn std::error::Error>),
                                                };
                                                anansi_aux::APP_STATE.with(|state| {
                                                    let mut state = state.borrow_mut();
                                                    let state = state.as_mut().expect("problem borrowing state");
                                                    let _resource = match text {
                                                        Ok(t) => {
                                                            match serde_json::from_str::<#ty>(&t) {
                                                                Ok(r) => Resource::Resolved(r),
                                                                Err(e) => Resource::Rejected(Box::new(e) as Box<dyn std::error::Error>),
                                                            }
                                                        }
                                                        Err(e) => Resource::Rejected(e),
                                                    };
                                                    #res_match(state, _resource, #res_num);
                                                });
                                                #comp_set_render();
                                            });
                                        }
                                    });
                                    continue;
                                }
                                _ => quote! {#expr_macro}
                            }
                        }
                        _ => {
                            let checked = add_proxy.check_expr(&parsed, &mut members);
                            quote! {#checked}
                        }
                    };
                    init.push(p);
                }
            }
            "style!" => {
                let s = get_expr(&mut bchars);
                let (_, s) = s.split_once('{').unwrap();
                let (t, _) = s.rsplit_once('}').unwrap();
                let mut tchars = t.chars();
                loop {
                    let n = collect_nws(&mut tchars);
                    if n.is_empty() {
                        break;
                    }
                    let e = get_expr(&mut tchars);
                    if n.starts_with('@') {
                        style.push_str(&n);
                        style.push(' ');
                        style.push_str(&e);
                        continue;
                    }
                    if let Some((first, rest)) = n.split_once("::") {
                        selectors.insert(first.to_string());
                        style.push_str(&format!("{}.anansi-{}::{}", first, lower_comp, rest));
                    } else {
                        style.push_str(&n);
                        selectors.insert(n);
                    }
                    style.push_str(&e);
                }
                continue;
            }
            
            ";" => {}
            "}" => {}
            _ => {
                if trimmed.starts_with("rsx!") {
                    let s = if trimmed.starts_with("rsx!(") {
                        let (_, c) = trimmed.split_once('(').expect("expected parenthesis");
                        let mut c = c.to_string();
                        c.push_str(&collect(&mut bchars, '{'));
                        svars = c.split(',').map(|sv| format!("{}", sv.trim())).collect();
                        svars.pop();

                        custom_get_expr(&mut bchars, 1, 1)
                    } else {
                        let t = get_expr(&mut bchars);
                        let (_, t) = t.split_once('{').unwrap();
                        t.to_string()
                    };
                    let (s, _) = s.rsplit_once('}').unwrap();

                    let lower = component.to_string().to_lowercase();
                    let mut c_parser = CompParser {start: vec![], callbacks: vec![], rchildren: HashMap::new(), refs: comp_refs.clone(), in_resource: false, lower_comp: lower.clone(), in_block: false, in_element: false, res_types: resource_types.clone(), resource_calls: vec![], res_fn: vec![], selectors: selectors.clone(), comp_rsx_ids: vec![], restart_comp_rsx_ids: vec![], local: local.clone()};
                    let c_parsed = c_parser.parse_rsx(&s);
                    comp_rsx_ids = c_parser.comp_rsx_ids;
                    restart_comp_rsx_ids = c_parser.restart_comp_rsx_ids;
                    resource_calls.append(&mut c_parser.resource_calls);
                    res_fn.append(&mut c_parser.res_fn);

                    start.append(&mut c_parser.start);
                    callbacks.append(&mut c_parser.callbacks);
                    let parsed: syn::Expr = syn::parse_str(&c_parsed).unwrap();
                    let mut add_proxy = AddProxy::new();
                    let mut members = vec![];
                    rsx = add_proxy.check_expr(&parsed, &mut members);
                    add_proxy.vars.insert(props.clone());
                    let mut parser = Parser::comp(comp_refs.clone(), lower.to_string(), selectors.clone(), local.clone());
                    let h = parser.to_html(&s);
                    html = syn::parse_str(&h).unwrap();
                } else {
                    panic!("unexpected token for component {}", token.trim());
                }
            }
        }
    }
    let mut lex_n = 0_usize;
    let mut lexical_scope = vec![];
    let mut set_render_idx = vec![];
    for name in svars {
        let (ty, n) = local.get(&name).expect("problem getting type for rendering function");
        let ident = format_ident!("{}", name);
        lexical_scope.push(quote! {
            let mut #ident = _scope[#lex_n].borrow_mut();
            let #ident = #ident.downcast_mut::<#ty>().expect("problem restoring variable");
        });
        set_render_idx.push(quote! {#n.to_string()});
        lex_n += 1;
    }
    
    let set_ids = if resource_map.is_empty() {
        quote! {}
    } else {
        quote! {
            fn #comp_set_render() {
                anansi_aux::IDS.with(|ids| {
                    let mut v = vec![];
                    #(v.push(#set_render_idx);)*
                    *ids.borrow_mut() = v;
                });
                let _rsx = #comp_render();
                anansi_aux::rerender(_rsx);
            }
        }
    };

    let qr = quote! {
        fn #comp_render() -> Rsx {
            let mut _scope = anansi_aux::lexical_scope();
            #(#lexical_scope)*
            #rsx
        }
        #set_ids
    };

    let use_styles = if style.is_empty() {
        quote! {}
    } else {
        let mut s_path = path.clone();
        s_path.pop();
        s_path.push("static");
        s_path.push("styles");
        let n = path.file_name().unwrap().to_str().unwrap();
        let (n, _) = n.split_once('.').unwrap();
        s_path.push(format!("{}.css", n));
        fs::write(&s_path, style.into_bytes()).unwrap();
        let load_path = format!("/static/styles/{}.css", n);
        quote! {
            anansi_aux::load_style(#load_path);
        }
    };
    
    let mut pause = vec![];
    for (name, (ty, _)) in &local.0 {
        let name = format_ident!("{}", name);
        let ts = ty.to_string();
        if ts == "Rendered" {
            pause.push(quote! {
                _p.push_obj("{}".to_string());
            });
            continue;
        }
        if ts != properties.to_string() {
            pause.push(quote! {
                _p.push_subs(#name.get_subs());
                _p.push_obj(serde_json::to_string(&#name.into_inner()).unwrap());
            });
        } else {
            pause.push(quote! {
                _p.push_obj(serde_json::to_string(&#name).unwrap());
            });
        }
    }
    let mut set_scope = vec![];

    let mut ln = 0_usize;
    for (_, (ty, _)) in local.0 {
        let ts = ty.to_string();
        if ts == "anansi_aux::EmptyProp" {
            continue;
        }
        set_scope.push(quote!{
            {
                let rs = anansi_aux::Obj::Rs(std::rc::Rc::new(std::cell::RefCell::new(<#ty>::resume(store, #ln))));
                store.objs_mut()[#ln] = rs;
            }
        });
        ln += 1;
    }

    let mut comp_rsx_init = quote!{};
    if !comp_rsx_ids.is_empty() {
        let mut cri = vec![];
        let mut cnum = 0_usize;
        for _ in comp_rsx_ids {
            cri.push(quote!{comp_rsx.insert(anansi_aux::CompId::new(node_id.to_string(), #cnum), None);});
            cnum += 1;
        }
        for _ in restart_comp_rsx_ids {
            cri.push(quote!{comp_rsx.insert(anansi_aux::CompId::new(node_id.to_string(), #cnum), None);});
            cnum += 1;
        }
        comp_rsx_init = quote! {
            anansi_aux::COMP_RSX.with(|comp_rsx| {
                let mut comp_rsx = comp_rsx.borrow_mut();
                anansi_aux::NODE_ID.with(|node_id| {
                    let node_id = node_id.borrow();
                    #(#cri)*
                });
            });
        };
    }
    let c_init = if set_scope.is_empty() {
        quote! {}
    } else {
        quote! {
            anansi_aux::APP_STATE.with(|a| {
                let mut app_state = a.borrow_mut();
                let store = if let Some(state) = app_state.as_mut() {
                    state
                } else {
                    let mut contexts = std::collections::HashMap::new();
                    anansi_aux::DOCUMENT.with(|document| {
                        *app_state = anansi_aux::get_state(&document, &mut contexts);
                    });
                    anansi_aux::CONTEXTS.with(|c| *c.borrow_mut() = contexts);
                    app_state.as_mut().unwrap()
                };

                #(#set_scope)*
            });
        }
    };
    let restart_prop = if props != "_props" {
        quote! {
            anansi_aux::APP_STATE.with(|app_state| {
                let mut app_state = app_state.borrow_mut();
                let app_state = app_state.as_mut().expect("problem getting app state");
                if let anansi_aux::Obj::Rs(p) = &mut app_state.objs_mut()[0] {
                    let mut p = p.borrow_mut();
                    *p.downcast_mut::<#properties>().expect("problem restoring prop") = #props;
                } else {
                    panic!("expected Rust type");
                }
            });
        }
    } else {
        quote! {}
    };
    let q = quote! {
        pub fn #comp_mount(_node_id: String) {
            #comp_rsx_init
            #c_init
            
            #use_styles
        }
        
        #(#callbacks)*
        #(#res_fn)*
        
        #qr
        #fcomp
        impl<'c> anansi_aux::components::Component<'c> for #component {
            type Properties = #properties;

            fn init(#props: #properties, _p: &mut anansi_aux::components::Pauser) -> String {
                #(#init)*
                #html
                #(#pause)*
                _c
            }
        }
        impl #component {
            pub const CB: &'static [(&'static str, fn(String), fn())] = &[#(#start),*];
            pub fn restart(#props: #properties) -> Rsx {
                #restart_prop
                
                #use_styles
                #comp_render()
            }
        }
    };
    let nl = q.to_string().replace(";", ";\n");
    let mut parsed = path.clone();
    let name = path.file_name().unwrap();
    parsed.pop();
    parsed.push(".parsed");
    parsed.push(name);
    fs::write(parsed, nl).unwrap();
}

struct AddProxy {
    vars: HashSet<Ident>,
}

impl AddProxy {
    fn new() -> Self {
        Self {vars: HashSet::new()}
    }
    fn check_expr(&mut self, expr: &syn::Expr, _members: &mut Vec<Ident>) -> TokenStream {
        quote! {#expr}
    }
}

pub fn collect_nws(chars: &mut Chars) -> String {
    let mut s = String::new();
    while let Some(c) = chars.next() {
        if c != ' ' && c != '\n' {
            s.push(c);
            break;
        }
    }
    while let Some(c) = chars.next() {
        if c == ' ' || c == '\n' {
            break;
        }
        s.push(c);
    }
    s
}

fn collect_str(chrs: &mut Chars) -> Option<String> {
    let mut s = String::new();
    while let Some(c) = chrs.next() {
        if c == '"' {
            break;
        }
    }
    loop {
        if let Some(c) = chrs.next() {
            if c == '"' {
                break;
            } else {
                s.push(c);
            }
        } else {
            return None;
        }
    }
    Some(s)
}

fn component_rsx(name: &str) -> String {
    format!("{}_rsx", name.trim().to_lowercase())
}

struct CompParser {
    start: Vec<TokenStream>,
    callbacks: Vec<TokenStream>,
    rchildren: HashMap<String, (TokenStream, usize)>,
    refs: HashMap<String, Vec<usize>>,
    in_block: bool,
    in_resource: bool,
    in_element: bool,
    lower_comp: String,
    comp_rsx_ids: Vec<Ident>,
    restart_comp_rsx_ids: Vec<Ident>,
    local: Local,
    resource_calls: Vec<TokenStream>,
    res_fn: Vec<TokenStream>,
    res_types: HashMap<String, TokenStream>,
    selectors: HashSet<String>,
}

pub fn collect_tag(chrs: &mut Chars) -> String {
    let mut s = String::new();
    let mut bracket = 0;
    while let Some(d) = chrs.next() {
        match d {
            '>' => if bracket == 0 {
                break
            }
            '{' => bracket += 1,
            '}' => bracket -= 1,
            _ => {}
        }
        s.push(d);
    }
    s
}

impl CompParser {
    fn parse_rsx(&mut self, content: &str) -> String {
        let mut view = String::new();
        let children = self.process(content);
        view.push_str(&format!("Rsx::Component(Comp {{children: {}}})", children));
        view
    }
    fn attr_tuple(&mut self, attr_str: &str) -> String {
        let mut s = String::new();
        let mut chrs = attr_str.chars();
        loop {
            let mut name = String::new();
            loop {
                if let Some(c) = chrs.next() {
                    match c {
                        '@' => {
                            let mut at = String::new();
                            let mut bracket = 0;
                            while let Some(d) = chrs.next() {
                                match d {
                                    '>' => if bracket == 0 {
                                        break
                                    }
                                    '{' => bracket += 1,
                                    '}' => bracket -= 1,
                                    _ => {}
                                }
                                at.push(d);
                            }
                            if at.starts_with("onclick") {
                                let (_, second) = at.split_once('(').unwrap();
                                let mut schars = second.chars();
                                let mut expr = custom_get_expr(&mut schars, 1, 0);
                                let mut rchildren = vec![];
                                if !expr.starts_with("callback!") {
                                    expr.pop();
                                    s = format!("(\"on:click\".to_string(), format!(\"{}_{}[", self.lower_comp, expr);
                                    let refs = self.refs.get(&expr).expect("could not get callback");
                                    for n in refs {
                                        s.push_str(&format!("{} ", n));
                                    }
                                    if !refs.is_empty() {
                                        s.pop();
                                    }
                                } else {
                                    let (_, rest) = expr.split_once("callback!(").expect("problem parsing callback");
                                    let name = format_ident!("{}_on_click_{}", self.lower_comp, self.callbacks.len());
                                    s = format!("(\"on:click\".to_string(), format!(\"{}[", name.to_string());
                                    let mut processed = rest.to_string();
                                    processed.pop();
                                    processed.pop();
                                    let callback: CallbackArgs = syn::parse_str(&processed).unwrap();
                                    let mut args = vec![];
                                    let mut n = 0usize;
                                    for var in &callback.args {
                                        let vty = if let Some((vty, _)) = self.local.get(&var.to_string()) {
                                            s.push_str(&format!("{} ", n));
                                            vty
                                        } else {
                                            let (vty, n) = self.rchildren.get(&var.to_string()).expect("problem getting variable data");
                                            s.push_str(&format!("{}-{{}} ", n));
                                            rchildren.push(var.clone());
                                            vty
                                        };
                                        args.push(quote! {
                                            let mut #var = _scope[#n].borrow_mut();
                                            let #var = #var.downcast_mut::<#vty>().expect("problem restoring variable");
                                        });
                                        n += 1;

                                    };
                                    if !callback.args.is_empty() {
                                        s.pop();
                                    }
                                    let block = callback.block;
                                    let q = quote! {
                                        fn #name() {
                                            let mut _scope = anansi_aux::lexical_scope();
                                            #(#args)*
                                            #block
                                        }
                                    };
                                    let ns = name.to_string();
                                    let comp_mount = format_ident!("{}_mount", self.lower_comp);
                                    self.start.push(quote! {(#ns, #comp_mount, #name)});
                                    self.callbacks.push(q);
                                }
                                s.push_str("]\"");
                                for child in rchildren {
                                    s.push_str(&format!(", {}.pos()", child));
                                }
                                s.push_str("))");
                            } else {
                                unimplemented!();
                            }
                            return s;
                        }
                        '=' => {
                            break;
                        }
                        _ => {}
                    }
                    name.push(c);
                } else {
                    return s;
                }
            }
            let name = name.trim().to_string();
            let mut dchrs = chrs.clone();
            let nws = collect(&mut dchrs, '\n');
            if nws.starts_with('"') {
                if let Some(attr) = collect_str(&mut chrs) {
                    s.push_str(&format!("(\"{name}\".to_string(), \"{attr}\".to_string()),"));
                }
            } else {
                let mut u = String::new();
                if nws.starts_with("@if") {
                    u.push_str("if ");
                    collect(&mut chrs, ' ');
                    loop {
                        let expr = get_expr(&mut chrs);
                        let mut echars = expr.chars();
                        while let Some(c) = echars.next() {
                            if c == '@' {
                                let d = echars.next().expect("problem parsing attribute");
                                if d == ':' {
                                    let t = collect(&mut echars, '\n');
                                    u.push_str(&t);
                                } else {
                                    unimplemented!();
                                }
                            } else {
                                u.push(c)
                            }
                        }
                        if expr.trim().starts_with("else") {
                            break;
                        }
                    }
                    s.push_str(&format!("(\"{name}\".to_string(), {u}.to_string()),"));
                } else {
                    unimplemented!();
                }
            }
        }
    }
    fn process(&mut self, content: &str) -> String {
        let mut view = String::from("{let mut _children = vec![];");
        let mut chars = content.chars();
        let mut tags = vec![];
        while let Some(c) = chars.next() {
            match c {
                '@' => self.at(&mut view, &mut chars),
                '<' => self.tag(&mut tags, &mut view, &mut chars),
                _ => if self.in_element || !self.in_block {
                    self.text(c, &mut tags, &mut view, &mut chars)
                } else {
                    view.push(c);
                },
            }
        }
        view.push_str("_children}");
        view
    }
    fn text(&mut self, c: char, tags: &mut Vec<String>, view: &mut String, chars: &mut Chars) {
        let mut txt = String::new();
        txt.push(c);
        let mut vw = String::new();
        while let Some(d) = chars.next() {
            match d {
                '"' => {
                    txt.push_str("\\\"");
                }
                '\\' => {
                    txt.push_str("\\\\");
                }
                '@' => {
                    self.at(&mut vw, chars);
                    break;
                }
                '}' => {
                    vw.push('}');
                    break;
                }
                '<' => {
                    self.tag(tags, &mut vw, chars);
                    break;
                }
                _ => txt.push(d),
            }
        }
        view.push_str(&format!("_children.push(Rsx::Text(\"{}\".to_string()));", txt.trim()));
        view.push_str(&vw);
    }
    fn tag(&mut self, tags: &mut Vec<String>, view: &mut String, chars: &mut Chars) {
        let mut inner = String::new();
        if let Some(c) = chars.next() {
            if c.is_ascii_lowercase() || c == '/' {
                inner.push(c);
            } else if c.is_ascii_uppercase() {
                inner.push(c);
                inner.push_str(&collect(chars, '>'));
                inner.pop();
                let comp_rsx = component_rsx(&inner);
                let comp_rsx = format_ident!("{}", comp_rsx);
                let comp_num = self.comp_rsx_ids.len();
                let inner = format_ident!("{}", inner.trim());
                if self.in_resource {
                    self.comp_rsx_ids.push(comp_rsx.clone());
                } else {
                    self.restart_comp_rsx_ids.push(comp_rsx.clone());
                }
                let rs = quote! {
                    anansi_aux::NODE_ID.with(|nid| {
                        let node_id = nid.borrow().clone();
                        anansi_aux::COMP_RSX.with(|c| {
                            let mut _comp_rsx = c.borrow_mut();
                            let #comp_rsx = _comp_rsx.get_mut(&anansi_aux::CompId::new(node_id, #comp_num)).expect("problem getting component RSX");
                            if let Some(c) = #comp_rsx {
                                match c {Rsx::Component(comp) => _children.append(&mut comp.children.clone()), _ => unimplemented!()};
                            } else {
                                let _r = #inner::restart(anansi_aux::EmptyProp {});
                                match _r {Rsx::Component(ref comp) => _children.append(&mut comp.children.clone()), _ => unimplemented!()};
                                *#comp_rsx = Some(_r);
                            }
                        });
                    });
                };
                view.push_str(&rs.to_string());
                return;
            } else {
                view.push('<');
                view.push(c);
                return;
            }
        }
        let collected = collect_tag(chars);
        inner.push_str(&collected);
        let mut attrs = String::new();
        let name = if let Some((name, av)) = inner.trim().split_once(' ') {
            attrs = self.attr_tuple(av);
            name.to_string()
        } else {
            inner.clone()
        };
        if name.starts_with('/') {
            tags.pop();
            if tags.is_empty() {
                self.in_element = false;
            }
            view.push_str("_children}));");
            return;
        } else {
            if !inner.ends_with('/') {
                self.in_element = true;
                tags.push(name.clone());
            }
        }
        if self.selectors.contains(&name) {
            attrs.push_str(&format!("(\"class\".to_string(), \"anansi-{}\".to_string())", self.lower_comp));
        }
        let name = name.trim().to_uppercase();
        view.push_str(&format!("_children.push(element!(\"{name}\", attributes![{attrs}], {{let mut _children = vec![];"));
        if inner.ends_with('/') {
            view.push_str("_children}));");
        }
    }
    fn at(&mut self, view: &mut String, chars: &mut Chars) {
        let mut s = String::new();
        let mut extra = String::new();
        if let Some(c) = chars.next() {
            match c {
                '(' => {
                    let mut p = 0;
                    while let Some(d) = chars.next() {
                        if d == '(' {
                            p += 1;
                        } else if d == ')' {
                            if p == 0 {
                                break;
                            } else {
                                p -= 1;
                            }
                        }
                        s.push(d);
                    }
                    view.push_str(&format!("_c.push_str(&anansi_aux::html_escape(&format!(\"{{}}\", {})));_c.push_str(\"", s));
                    return;
                }
                '{' => {
                    let mut e = custom_get_expr(chars, 0, 1);
                    e.pop();
                    view.push_str(&e);
                    return;
                }
                ':' => {
                    let l = collect(chars, '\n');
                    view.push_str(&l);
                    return;
                }
                _ => {}
            }
            s.push(c);
        } else {
            return;
        }
        while let Some(c) = chars.next() {
            if c == ' ' || c == '<' || c == '\n' || c == '(' || c == '/' || c == '!' {
                extra.push(c);
                break;
            } else if c == '"' {
                extra.push_str("\\\"");
                break;
            }
            s.push(c);
        }
        let keyword = s.clone();
        let mut find_brace = true;
        match s.trim() {
            "if" => {}
            "for" => {}
            "loop" => {}
            "while" => {}
            "build" => {
                let (name, ex) = collect_name(chars);
                s = format!("_c.push_str(&{}.tag()); if let Some(token_tag) = form.token_tag() {{ _c.push_str(&token_tag) }}", name);
                if ex == '{' {
                    find_brace = false;
                }
            }
            "unescape" => {
                let (name, ex) = collect_name(chars);
                view.push_str(&format!("_c.push_str(&format!(\"{{}}\", {}));_c.push_str(\"{}", name, ex));
                return;
            }
            "href" => {
                let (attrs, segments) = html_attrs(&mut s, chars);
                let mut u = String::from(segments[0].trim().to_string());
                let mut v = String::from("{}");
                for segment in &segments[1..] {
                    u.push_str(&format!(",{}", segment));
                    v.push_str("/{}");
                }
                view.push_str(&format!("_children.push(element!(\"A\", attributes![(\"href\".to_string(), format!(\"{}\", {}))", v, u));
                for attr in attrs {
                    let (lhs, rhs) = attr.split_once('=').unwrap();
                    view.push_str(&format!(", (\"{}\", \"{}\")", lhs, rhs));
                }
                view.push_str("],");
                let blk = collect(chars, '}');
                view.push_str(&self.process(&blk));
                view.push_str("));");
                return;
            }
            "resource" => {
                let mut name = collect_nws(chars);
                let mut args = vec![];
                if name.ends_with(',') {
                    name.pop();
                    loop {
                        let mut var = collect_nws(chars);
                        let mut b = true;
                        if var.ends_with(',') {
                            b = false;
                            var.pop();
                        }
                        let var_ident = format_ident!("{}", var);
                        let (vty, n) = self.local.get(&var).expect("problem getting variable type");
                        args.push(quote! {
                            let #var_ident = if let anansi_aux::Obj::Rs(_r) = &_state.objs()[#n] {
                                _r.clone()
                            } else {
                                panic!("expected Rust type");
                            };

                            let mut #var_ident = #var_ident.borrow_mut();
                            let #var_ident = #var_ident.downcast_mut::<#vty>().expect("problem restoring variable");
                        });
                        if b {
                            break;
                        }
                    }
                }
                let name_match = format_ident!("{}_match", name);
                let mut block = String::from("match _res ");
                let e = get_expr(chars);
                block.push_str(&e);
                self.in_block = true;
                self.in_resource = true;
                let processed = self.process(&block);
                self.in_resource = false;
                let processed: syn::Expr = syn::parse_str(&processed).unwrap();
                let ty = self.res_types.get(&name).expect("problem getting Resource type");

                let name_fn = quote! {
                    fn #name_match(_state: &anansi_aux::AppState, _res: Resource<#ty>, n: usize) {
                        #(#args)*
                        if let anansi_aux::Obj::Rs(_r) = _state.objs()[n].clone() {
                            let mut _r = _r.borrow_mut();
                            let mut _r = _r.downcast_mut::<Rendered>().expect("expected Rendered");
                            *_r = Rendered::new(#processed);
                        } else {
                            panic!("expected Rust type");
                        }
                    }
                };
                self.res_fn.push(name_fn);

                let v = format!("if !{}.rsx().is_empty() {{_children.append(&mut {0}.rsx().clone());}}", name);
                view.push_str(&v);
                return;
            }
            "onclick" => {
                let expr = custom_get_expr(chars, 0, 0);
                let mut rchildren = vec![];
                if !expr.contains("callback!") {
                    view.push_str(&format!("(\"on:click\".to_string(), format!(\"{}_{}[", self.lower_comp, expr));
                    let refs = self.refs.get(&expr).expect("could not get callback");
                    for n in refs {
                        view.push_str(&format!("{} ", n));
                    }
                    if !refs.is_empty() {
                        view.pop();
                    }
                } else {
                    let (cb, rest) = expr.split_once("callback!(").expect("problem parsing callback");
                    let name = format_ident!("{}_on_click_{}", self.lower_comp, self.callbacks.len());
                    view.push_str(&format!("(\"on:click\".to_string(), format!(\"{}[", name.to_string()));
                    let mut processed = rest.to_string();
                    processed.pop();
                    let callback: CallbackArgs = syn::parse_str(&processed).unwrap();
                    let mut args = vec![];
                    let mut n = 0;
                    for var in &callback.args {
                        let vty = if let Some((vty, _)) = self.local.get(&var.to_string()) {
                            view.push_str(&format!("{} ", n));
                            vty
                        } else {
                            let (vty, n) = self.rchildren.get(&var.to_string()).expect("problem getting variable data");
                            view.push_str(&format!("{}-{{}} ", n));
                            rchildren.push(var.clone());
                            vty
                        };
                        let borrow = if cb == "callback!" {
                            quote! {
                                let mut #var = _scope[#n].borrow_mut();
                                let #var = #var.downcast_mut::<#vty>().expect("problem restoring variable");
                            }
                        } else {
                            quote! {}
                        };
                        args.push(quote! {
                            #borrow
                        });
                        n += 1;

                    }
                    if !callback.args.is_empty() {
                        view.pop();
                    }
                    let block = callback.block;
                    let q = quote! {
                        fn #name() {
                            let _scope = anansi_aux::lexical_scope();
                            #(#args)*
                            #block
                        }
                    };
                    self.callbacks.push(q);
                }
                view.push_str("]\"");
                for child in rchildren {
                    view.push_str(&format!(", {}.pos()", child));
                }
                view.push_str("))");
                return;
            }
            "url!" => {
                s = "anansi::url!".to_string();
                comp_var(&extra, &mut s, chars, view);
                return;
            }
            _ => {
                let mut c = s.chars();
                if c.next().unwrap() == '{' {
                    let r: String = c.collect();
                    let blk = collect(chars, '}');
                    view.push_str(&r);
                    view.push(' ');
                    view.push_str(&blk);
                    view.push_str("_c.push_str(\"");
                } else {
                    comp_var(&extra, &mut s, chars, view);
                    if extra == "<" {
                        let mut tags = vec![];
                        self.tag(&mut tags, view, chars);
                    }
                }
                return;
            }
        }
        s.push_str(&extra);
        if find_brace {
            while let Some(c) = chars.next() {
                s.push(c);
                if c == '{' {
                    if let Some((_, rest)) = s.split_once("for") {
                        let (name, rest) = rest.split_once("in").expect("problem parsing for loop");
                        let container = if let Some((container, _)) = rest.split_once('.') {
                            container.trim().to_string()
                        } else {
                            rest.trim().to_string()
                        };
                        if let Some((ty, n)) = self.local.get(&container) {
                            let nm = if let Some((_, nm)) = name.split_once("mut") {
                                nm
                            } else {
                                &name
                            };
                            self.rchildren.insert(nm.trim().to_string(), (quote! {<<#ty as anansi_aux::Parent>::Item as anansi_aux::Parent>::Item}, *n));
                        }
                    }
                    break;
                }
            }
        }
        view.push_str(&s);
        let mut tags = vec![];
        while let Some(c) = chars.next() {
            match c {
                '}' => {
                    match keyword.as_str() {
                        "build" => view.push_str("</form>"),
                        "if" => {
                            let mut preview = chars.clone();
                            let t = collect_nws(&mut preview);
                            if t == "else" {
                                let e = collect(chars, '{');
                                view.push_str(&format!("{}{{", e));
                                continue;
                            }
                        }
                        _ => {}
                    }
                    view.push_str("}");
                    self.rchildren.clear();
                    return;
                }
                '@' => {
                    self.at(view, chars);
                }
                '<' => {
                    self.tag(&mut tags, view, chars);
                }
                _ => {
                    self.text(c, &mut tags, view, chars);
                    let mut chrs = chars.clone();
                    if let Some(c) = chrs.next() {
                        if c == ' ' {
                            chars.next();
                        }
                    }
                }
            }
        }
    }
}

fn comp_var(extra: &str, s: &mut String, chars: &mut Chars, view: &mut String) {
    if extra == "(" {
        s.push_str(extra);
        let expr = custom_get_expr(chars, 1, 0);
        s.push_str(&expr);
        view.push_str(&format!("_children.push(Rsx::Text(anansi_aux::html_escape(&format!(\"{{}}\", {}))));", s));
    } else {
        let var = collect_var(chars);
        s.push_str(&var);
        view.push_str(&format!("_children.push(Rsx::Text(anansi_aux::html_escape(&format!(\"{{}}\", {}))));", s));
    }
}

fn html_attrs(s: &mut String, chars: &mut Chars) -> (Vec<String>, Vec<String>) {
    s.clear();
    let line = collect(chars, '{');
    let args: Vec<&str> = line.split(',').collect();
    let mut segments = vec![];
    let mut attrs = vec![];
    for arg in args {
        if arg.contains("=") {
            attrs.push(arg.to_string());
        } else {
            segments.push(arg.to_string());
        }
    }
    (attrs, segments)
}

pub struct CallbackArgs {
    pub args: Vec<Ident>,
    pub block: syn::ExprBlock,
}

impl Parse for CallbackArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = vec![];
        loop {
            if let Ok(id) = input.parse::<Ident>() {
                args.push(id);
                input.parse::<syn::token::Comma>().unwrap();
            } else {
                if let Ok(block) = input.parse::<syn::ExprBlock>() {
                    break Ok(Self {args, block});
                } else {
                    panic!("problem getting callback args");
                }
            }
        }
    }
}

struct ResourceArgs {
    ty: syn::Path,
    exprs: Vec<syn::Expr>,
}

impl Parse for ResourceArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let ty = input.parse().expect("expected path");
        let mut exprs = vec![];
        while let Ok(_c) = input.parse::<syn::token::Comma>() {
            exprs.push(input.parse().expect("expected expression"));
        }
        Ok(Self { ty, exprs })
    }
}

pub fn init_components(args: &Vec<String>) {
    let (_, dir) = get_src(args, "");
    let name = dir.file_name().unwrap().to_str().unwrap();
    let cargo_toml = fs::read_to_string("Cargo.toml").expect("Could not find Cargo.toml");
    let comps = format!("{}-comps", name);
    let mut cargo_toml: Map<String, Value> = toml::from_str(&cargo_toml).expect("Could not parse settings.toml");
    {
        let members = cargo_toml.get_mut("workspace").unwrap().get_mut("members").unwrap().as_array_mut().unwrap();
        members.push(Value::String(comps.clone()));
    }
    {
        let deps = cargo_toml.get_mut("dependencies").unwrap().as_table_mut().unwrap();
        deps.insert("anansi-aux".to_string(), Value::String(format!("{}", VERSION)));
        {
            let mut table = Map::new();
            table.insert("path".to_string(), Value::String(comps.clone()));
            table.insert("version".to_string(), Value::String("*".to_string()));
            deps.insert(comps.clone(), Value::Table(table));
        }
        {
            let mut table = Map::new();
            table.insert("version".to_string(), Value::String("1.0".to_string()));
            table.insert("features".to_string(), Value::Array(vec![Value::String("derive".to_string())]));
            deps.entry("serde").or_insert(Value::Table(table));
        }
        deps.entry("serde_json").or_insert(Value::String("1.0".to_string()));
    }
    fs::write("Cargo.toml", toml::to_string(&cargo_toml).unwrap().into_bytes()).unwrap();
    let comps_args = vec!["".to_string(), "new".to_string(), comps.clone(), "--lib".to_string()];
    cargo(&comps_args);
    let mut comp_path = PathBuf::from(&comps);
    append(comp_path.to_str().unwrap(), "Cargo.toml", &format!("wasm-bindgen = \"0.2\"
serde = {{ version = \"1.0\", features = [\"derive\"] }}
serde_json = \"1.0\"
wasm-bindgen-futures = \"0.4\"
anansi-aux = \"{}\"

[features]
server = []
client = []", VERSION).into_bytes());
    comp_path.push("src");
    let mut parsed_path = comp_path.clone();
    parsed_path.push(".parsed");
    fs::create_dir(parsed_path).unwrap();
    comp_path.push("lib.rs");
    let wasm = format!("{}-wasm", name);
    let under_wasm = wasm.replace('-', "_");
    let under_comps = comps.replace('-', "_");
    fs::write(comp_path, "anansi_aux::app_components! {}".to_string().into_bytes()).unwrap();
    let wasm_args = vec!["".to_string(), "new".to_string(), wasm.clone(), "--lib".to_string()];
    {
        let members = cargo_toml.get_mut("workspace").unwrap().get_mut("members").unwrap().as_array_mut().unwrap();
        members.push(Value::String(wasm.clone()));
        fs::write("Cargo.toml", toml::to_string(&cargo_toml).unwrap().into_bytes()).unwrap();
    }
    cargo(&wasm_args);
    let mut wasm_path = PathBuf::from(&wasm);
    let mut js = "const registerServiceWorker = async () => {
  if ('serviceWorker' in navigator) {
    try {
      const registration = await navigator.serviceWorker.register(
        '/static/sw.js',
        {
          scope: '/static/',
        }
      );
    } catch (error) {
      console.error(`Registration failed with ${error}`);
    }
  }
};
registerServiceWorker();

let mod;
const ids = new Map();

document.addEventListener('click', (e) => {
  let paths = e.composedPath();
  let callback;
  let id;

  for (let i = 0; i < paths.length; i++) {
    let el = paths[i];

    let attributes = el.attributes;
    if (attributes) {
      let onclick = attributes.getNamedItem('on:click');
      if (onclick) {
        let aid = ids.get(onclick.value);
        if (!aid) {
          aid = attributes.getNamedItem('a:id');
          ids.set(onclick.value, aid);
        }
        if (onclick && aid) {
          callback = onclick.value;
          id = aid.value;
          break;
        }
      }
      let rid = attributes.getNamedItem('rid');
      if (rid) {
        let called = mod.recall(rid.value);
        if (called) {
          return;
        }
      }
    }
  }
  if (callback) {
    if (mod) {
      mod.call(callback, id);
    } else {
      import('/static/pkg/".to_string();
    js.push_str(&under_wasm);
    js.push_str(".js').then((module) => {
        module.default().then(() => {
          module.start();
          mod = module;
          mod.call(callback, id);
        });
      });
    }
  }
});");
    make_file(&wasm_path, "main", ".js", js);
    let mut sw = "const addResourcesToCache = async (resources) => {
  const cache = await caches.open('v1');
  await cache.addAll(resources);
};

const putInCache = async (request, response) => {
  const cache = await caches.open('v1');
  await cache.put(request, response);
};

const cacheFirst = async ({ request, preloadResponsePromise }) => {
  const responseFromCache = await caches.match(request);
  if (responseFromCache) {
    return responseFromCache;
  }

  const preloadResponse = await preloadResponsePromise;
  if (preloadResponse) {
    console.info('using preload response', preloadResponse);
    putInCache(request, preloadResponse.clone());
    return preloadResponse;
  }

  try {
    const responseFromNetwork = await fetch(request);
    putInCache(request, responseFromNetwork.clone());
    return responseFromNetwork;
  } catch (error) {
    return new Response('Network error happened', {
      status: 408,
      headers: { 'Content-Type': 'text/plain' },
    });
  }
};

const enableNavigationPreload = async () => {
  if (self.registration.navigationPreload) {
    await self.registration.navigationPreload.enable();
  }
};

self.addEventListener('activate', (event) => {
  event.waitUntil(enableNavigationPreload());
});

self.addEventListener('install', (event) => {
  event.waitUntil(
    addResourcesToCache([".to_string();
    sw.push_str(&format!("
      '/pkg/main.js',
      '/pkg/{}.js',
      '/pkg/{0}_bg.wasm',
    ", under_wasm));
    sw.push_str("])
  );
});

self.addEventListener('fetch', (event) => {
  event.respondWith(
    cacheFirst({
      request: event.request,
      preloadResponsePromise: event.preloadResponse,
    })
  );
});");
    make_file(&wasm_path, "sw", ".js", sw);
    append(wasm_path.to_str().unwrap(), "Cargo.toml", &format!("wasm-bindgen = \"0.2\"
anansi-aux = \"{}\"
{} = {{ path = \"../{1}\" , version = \"*\" }}

[lib]
crate-type = [\"cdylib\"]", VERSION, comps).into_bytes());
    wasm_path.push("src");
    wasm_path.push("lib.rs");
    fs::write(wasm_path, format!("anansi_aux::start!({});", under_comps)).unwrap();
}
