use std::fs;
use std::path::PathBuf;
use std::str::Chars;
use crate::{collect, collect_var, collect_name, Parser};
use syn::{Ident};
use syn::parse::{Result, Parse, ParseStream};
use quote::{quote, format_ident};
use proc_macro2::TokenStream;

use syn::Block;
use syn::Expr::*;
use syn::Stmt::{Semi, Item, Local};
use std::collections::HashSet;

fn get_expr(chars: &mut Chars) -> String {
    let mut s = String::new();
    let mut n = 0;
    while let Some(c) = chars.next() {
        match c {
            ';' => if n == 0 {break},
            '{' => n += 1,
            '}' => {
                n -= 1;
                if n < 0 {
                    break;
                }
            }
            _ => {}
        }
        s.push(c);
    }
    s
}

pub fn check_components(path: PathBuf) {
    let content = fs::read_to_string(&path).unwrap();
    let (_, split) = match content.split_once("#[component(") {
        Some((f, split)) => (f, split),
        None => return,
    };
    let mut chars = split.chars();
    let component = format_ident!("{}", collect(&mut chars, ')').trim());
    let component = quote! {#component};
    collect(&mut chars, ']');
    let f = collect_nws(&mut chars);
    if f.trim() != "fn" {
        panic!("expected function for component");
    }
    let (fname, _) = collect_name(&mut chars);
    if fname.trim() != "init" {
        panic!("expected init function for component");
    }
    let props = format_ident!("{}", collect(&mut chars, ':').trim());
    let properties: syn::Path = syn::parse_str(&collect(&mut chars, ')')).unwrap();
    collect(&mut chars, '{');
    let mut n = 1;
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
    let mut comp = None;
    let mut rsx = quote!{};
    let mut render_args = quote!{};
    let mut render_call_args = quote!{};
    let mut html = quote!{};
    let mut cbv = vec![];
    let mut start = vec![];
    let mut callbacks = vec![];
    let mut cbn: u8 = 0;
    let mut init = vec![];
    let mut resources = vec![];
    let mut resource_calls = vec![];

    let cs = quote! {#component}.to_string();
    let lower_comp = format_ident!("{}", cs.to_lowercase());
    let comp_mount = format_ident!("{}_mount", lower_comp);
    let comp_render = format_ident!("{}_render", lower_comp);

    loop {
        let token = collect_nws(&mut bchars);
        if token.is_empty() {
            break;
        }
        match token.trim() {
            "let" => {
                let var = format_ident!("{}", collect_nws(&mut bchars).trim());
                collect(&mut bchars, '=');
                let expr = get_expr(&mut bchars);
                if expr.contains("Self") {
                    comp = Some(var.clone());
                    let e: syn::Expr = syn::parse_str(&expr).unwrap();
                    init.push(quote!{let #var = #e;});
                } else {
                    let parsed: syn::Expr = syn::parse_str(&expr).unwrap();
                    let mut add_proxy = AddProxy::new(comp.clone().unwrap(), component.clone());
                    let mut members = vec![];

                    let p = match &parsed {
                        Macro(expr_macro) => {
                            let id = expr_macro.mac.path.segments.last().unwrap().ident.to_string();
                            match id.as_str() {
                                "callback" => {
                                    let tokens = &expr_macro.mac.tokens;
                                    let block: syn::Block = syn::parse_str(&quote! {{#tokens}}.to_string()).unwrap();
                                    let t = add_proxy.proxy(&block);

                                    let name = format_ident!("{}_{}", component.to_string().to_lowercase(), var);
                                    let cb = quote!{#cbn => #name(&mut #comp, &#props, &_sender),};
                                    cbv.push(cb);
                                    let ns = name.to_string();
                                    start.push(quote! {(#ns, #comp_mount, #cbn)});
                                    cbn += 1;

                                    let q = quote! {
                                        fn #name(#comp: &mut #component, #props: &#properties, _sender: &async_channel::Sender<CbCmd>) {
                                            #(#t)*
                                        }
                                    };
                                    resources.append(&mut add_proxy.callbacks);
                                    resource_calls.append(&mut add_proxy.calls);
                                    callbacks.push(q);
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
            "rsx!" => {
                let s = get_expr(&mut bchars);
                let (_, s) = s.split_once('{').unwrap();
                let (s, _) = s.rsplit_once('}').unwrap();

                let lower = component.to_string().to_lowercase();
                let mut c_parser = CompParser {lower_comp: lower.clone()};
                let c_parsed = c_parser.parse_rsx(&s);
                let parsed: syn::Expr = syn::parse_str(&c_parsed).unwrap();
                let mut add_proxy = AddProxy::new(comp.clone().unwrap(), component.clone());
                let mut members = vec![];
                rsx = add_proxy.check_expr(&parsed, &mut members);
                add_proxy.vars.insert(props.clone());
                let has_props = add_proxy.vars.get(&props).is_some();
                render_args = if has_props {
                    quote! {
                        (#comp : &mut #component, #props: &#properties)
                    }
                } else {
                    quote! {
                        (#comp : &mut #component)
                    }
                };
                render_call_args = if has_props {
                    quote! {
                        (&mut #comp, &#props)
                    }
                } else {
                    quote! {
                        (&mut #comp)
                    }
                };
                let mut parser = Parser::comp(lower.to_string());
                html = syn::parse_str(&parser.to_html(&s)).unwrap();
            }
            _ => panic!("unexpected token for component"),
        }
    }
    let qr = quote! {
        fn #comp_render #render_args -> Rsx {
            #rsx
        }
    };
    let q = quote! {
        pub fn #comp_mount(state_val: Value, props_val: Value, _subs: Vec<Sub>, _sender: async_channel::Sender<CbCmd>, _receiver: async_channel::Receiver<CbCmd>, _gtx: async_channel::Sender<anansi_aux::Cmd>, _node_id: String) {
            wasm_bindgen_futures::spawn_local(async move {
                let #props: #properties = serde_json::from_value(props_val).unwrap();
                let mut #comp = #component::new(state_val, _subs);
                while let Ok(cmd) = _receiver.recv().await {
                    match cmd {
                        CbCmd::Callback(_n) => match _n  {
                            #(#cbv)*
                            _ => unimplemented!(),
                        },
                        CbCmd::Resource(_n, _text) => match _n {
                            #(#resource_calls)*
                            _ => unimplemented!(),
                        },
                    }
                    if #comp._proxy._dirty > 0 {
                        let _subs = #comp._proxy.start_proxy();
                        let _rsx = #comp_render #render_call_args;
                        _gtx.send(anansi_aux::Cmd::Update(_rsx, _node_id.clone())).await.unwrap();
                        #comp._proxy.stop_proxy(_subs);
                    }
                }
            });
        }
        #(#callbacks)*
        #(#resources)*
        #qr
        impl<'c> anansi_aux::components::Component<'c> for #component {
            type Properties = #properties;

            fn init(props: #properties, _p: &mut anansi_aux::components::Pauser) -> String {
                #(#init)*
                #html
                _p.push_subs(#comp._proxy.get_subs());
                _p.push_obj(serde_json::to_string(&#comp.into_inner()).unwrap());
                _p.push_obj(serde_json::to_string(&#props).unwrap());
                _c
            }
        }
        impl #component {
            pub const CB: &'static [(&'static str, fn(Value, Value, Vec<Sub>, async_channel::Sender<CbCmd>, async_channel::Receiver<CbCmd>, async_channel::Sender<anansi_aux::Cmd>, String), u8)] = &[#(#start),*];
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
    comp: Ident,
    component: TokenStream,
    calls: Vec<TokenStream>,
    callbacks: Vec<TokenStream>,
    vars: HashSet<Ident>,
}

impl AddProxy {
    fn new(comp: Ident, component: TokenStream) -> Self {
        Self {comp, component, calls: vec![], callbacks: vec![], vars: HashSet::new()}
    }
    fn get_members(&self, members: &mut Vec<Ident>) -> Vec<TokenStream> {
        let mut v = vec![];
        for member in members {
            let name = format_ident!("{}", member.to_string().to_uppercase());
            let comp = &self.comp;
            let component = &self.component;
            v.push(quote! {#comp._proxy.set(#component::#name);});
        }
        v
    }
    fn check_pat(&self, pat: &syn::Pat, _members: &mut Vec<Ident>) -> TokenStream {
        match pat {
            syn::Pat::Ident(pat_ident) => {
                quote! {#pat_ident}
            }
            syn::Pat::Path(pat_path) => {
                if pat_path.path.segments.first().unwrap().ident != self.comp {
                    quote! {#pat_path}
                } else {
                    unimplemented!();
                }
            }
            syn::Pat::Range(range) => {
                quote! {#range}
            }
            _ => unimplemented!(),
        }
    }
    fn check_expr(&mut self, expr: &syn::Expr, members: &mut Vec<Ident>) -> TokenStream {
        match expr {
            Assign(assign) => {
                self.check_expr(&*assign.left, members);
                let mv = self.get_members(members);
                members.clear();
                let cv = self.check_expr(&*assign.right, members);
                let left = &assign.left;
                let q = quote! {#(#mv)* #left = #cv};
                q
            }
            AssignOp(assign) => {
                let op = assign.op;
                self.check_expr(&*assign.left, members);
                let mv = self.get_members(members);
                let cv = self.check_expr(&*assign.right, members);
                members.clear();
                let left = &assign.left;
                let q = quote! {#(#mv)* #left #op #cv};
                q
            }
            Await(a) => {
                let base = self.check_expr(&*a.base, members);
                quote! {#base.await}
            }
            Array(array) => {
                let mut av = vec![];
                for elem in &array.elems {
                    av.push(self.check_expr(&elem, members));
                }
                quote! {[#(#av),*]}
            }
            Binary(binary) => {
                let left = self.check_expr(&*binary.left, members);
                let right = self.check_expr(&*binary.right, members);
                let op = binary.op;
                quote! {#left #op #right}
            }
            Block(expr_block) => {
                let t = self.proxy(&expr_block.block);
                quote! {{#(#t)*}}
            }
            Call(call) => {
                let mut av = vec![];
                for arg in &call.args {
                    av.push(self.check_expr(&arg, members));
                }
                let func = &call.func;
                quote! {#func(#(#av)*)}
            }
            Cast(cast) => {
                let e = self.check_expr(&*cast.expr, members);
                let ty = &cast.ty;
                quote! {#e as #ty}
            }
            ForLoop(for_loop) => {
                let pat = self.check_pat(&for_loop.pat, members);
                let expr = self.check_expr(&*for_loop.expr, members);
                let body = self.proxy(&for_loop.body);
                quote! {for #pat in #expr {#(#body)*}}
            }
            Field(expr_field) => {
                match &*expr_field.base {
                    syn::Expr::Path(expr_path) => {
                        if expr_path.path.segments.first().unwrap().ident == self.comp {
                            match &expr_field.member {
                                syn::Member::Named(ident) => {
                                    members.push(ident.clone());
                                    let mv = self.get_members(members);
                                    let q = quote! {{#(#mv)* #expr_field}};
                                    q
                                }
                                _ => unimplemented!(),
                            }
                        } else {
                            quote! {#expr_field}
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            If(expr_if) => {
                let cond = self.check_expr(&*expr_if.cond, members);
                let b = self.proxy(&expr_if.then_branch);
                let q_if = quote! {if #cond {#(#b)*}};
                let q_else = if let Some((_, else_branch)) = &expr_if.else_branch {
                    let e = self.check_expr(&*else_branch, members);
                    quote! {else {#e}}
                } else {
                    quote! {}
                };
                quote! {#q_if #q_else}
            }
            Index(expr_index) => {
                let expr = self.check_expr(&*expr_index.expr, members);
                let index = self.check_expr(&*expr_index.index, members);
                quote! {#expr[#index]}
            }
            Lit(lit) => {
                quote! {#lit}
            }
            Macro(expr_macro) => {
                match expr_macro.mac.path.segments.last().unwrap().ident.to_string().as_str() {
                    "resource" => {
                        let tokens = &expr_macro.mac.tokens;
                        let args: ResourceArgs = syn::parse2(tokens.clone()).unwrap();
                        let req = &args.req;
                        let ty = &args.ty;
                        let (arg, block) = (args.cb.inputs.first().clone(), args.cb.body.clone());
                        let block = self.check_expr(&block, members);
                        let n = self.callbacks.len() as u8;
                        let name = format_ident!("resource{}", n);

                        let comp = &self.comp;
                        let component = &self.component;

                        self.calls.push(quote! {
                            #n => #name(&mut #comp, _text),
                        });

                        self.callbacks.push(quote! {
                            fn #name(#comp: &mut #component, _text: Result<String, Box<dyn std::error::Error>>) {
                                let #arg = match _text {
                                    Ok(t) => serde_json::from_str::<#ty>(&t).or_else(|e| Err(Box::new(e) as Box<dyn std::error::Error>)),
                                    Err(e) => Err(e),
                                };
                                #block
                            }
                        });
                        quote! {
                            {
                                let _sender = _sender.clone();
                                wasm_bindgen_futures::spawn_local(async move {
                                    let text = #req.send().await;
                                    let r = match text {
                                        Ok(r) => r.text().await.or_else(|e| Err(Box::new(e) as Box<dyn std::error::Error>)),
                                        Err(e) => Err(Box::new(e) as Box<dyn std::error::Error>),
                                    };
                                    _sender.send(CbCmd::Resource(#n, r)).await.unwrap();
                                });
                            }
                        }
                    }
                    _ => quote! {#expr_macro}
                }
            }
            Match(expr_match) => {
                let mut av = vec![];
                let mut members = vec![];
                for arm in &expr_match.arms {
                    let a = self.check_expr(&*arm.body, &mut members);
                    let pat = &arm.pat;
                    av.push(quote! {#pat => {#a}});
                }
                let expr = &expr_match.expr;
                quote! {match #expr {#(#av)*}}
            }
            MethodCall(call) => {
                let receiver = &call.receiver;
                let method = &call.method;
                let turbofish = if let Some(t) = &call.turbofish {
                    quote! {#t}
                } else {
                    quote! {}
                };
                self.check_expr(&*call.receiver, members);
                let mv = self.get_members(members);
                members.clear();
                let mut av = vec![];
                for arg in &call.args {
                    let e = self.check_expr(&arg, members);
                    av.push(e);
                }
                if mv.is_empty() {
                    quote! {#receiver.#method #turbofish(#(#av),*)}
                } else {
                    quote! {{#(#mv)* #receiver.#method #turbofish(#(#av),*)}}
                }
            }
            Paren(paren) => {
                let e = self.check_expr(&*paren.expr, members);
                quote!{#e}
            }
            syn::Expr::Path(expr_path) => {
                if expr_path.path.segments.first().unwrap().ident != self.comp {
                    quote! {#expr_path}
                } else {
                    unimplemented!();
                }
            }
            Range(expr_range) => {
                quote!{#expr_range}
            }
            Reference(reference) => {
                let r = self.check_expr(&*reference.expr, members);
                let mutability = if reference.mutability.is_some() {
                    quote! {mut}
                } else {
                    quote! {}
                };
                let q = if members.is_empty() {
                    quote! {&#mutability #r}
                } else {
                    let e = &reference.expr;
                    let mem = self.get_members(members);
                    quote! {{#(#mem)* &#mutability #e}}
                };
                q
            }
            Return(expr_return) => {
                let t = if let Some(expr) = &expr_return.expr {
                    let q = self.check_expr(&*expr, members);
                    quote!{#q}
                } else {
                    quote! {}
                };
                quote! {return {#t}}
            }
            syn::Expr::Struct(expr_struct) => {
                let mut fv = vec![];
                for field in &expr_struct.fields {
                    let member = &field.member;
                    let e = self.check_expr(&field.expr, members);
                    fv.push(quote!{#member: #e})
                }
                let path = &expr_struct.path;
                quote! {
                    #path {#(#fv),*}
                }
            }
            Tuple(tuple) => {
                let mut ev = vec![];
                for elem in &tuple.elems {
                    ev.push(self.check_expr(&elem, members))
                }
                quote! {(#(#ev),*)}
            }
            Unary(unary) => {
                let expr = self.check_expr(&*unary.expr, members);
                let op = unary.op;
                quote! {#op {#expr}}
            }
            _ => panic!("problem parsing `{}`", quote!{#expr}), 
        }
    }
    fn proxy(&mut self, b: &Block) -> Vec<TokenStream> {
        let mut qv = vec![];
        for stmt in &b.stmts {
            match stmt {
                Local(local) => {
                    let (_, expr) = local.init.as_ref().unwrap();
                    let mut members = vec![];
                    let t = self.check_expr(&*expr, &mut members);
                    let pat = &local.pat;
                    qv.push(quote! {let #pat = #t;});
                }
                Item(item) => {
                    qv.push(quote!{#item})
                }
                syn::Stmt::Expr(expr) => {
                    match &expr {
                        Macro(expr_macro) => if expr_macro.mac.path.segments.last().unwrap().ident == "rsx" {
                            let _s = expr_macro.mac.tokens.to_string();
                        }
                        _ => {
                            let mut members = vec![];
                            let t = self.check_expr(&expr, &mut members);
                            qv.push(quote!{#t});
                        }
                    }
                }
                Semi(expr, _) => {
                    let mut members = vec![];
                    let t = self.check_expr(&expr, &mut members);
                    qv.push(quote!{#t;});
                }
            }
        }
        qv
    }
}

fn collect_nws(chars: &mut Chars) -> String {
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

struct CompParser {
    lower_comp: String,
}

impl CompParser {
    fn parse_rsx(&mut self, content: &str) -> String {
        let mut view = String::new();
        let children = self.process(content);
        view.push_str(&format!("Rsx::Component(Comp {{children: {}}})", children));
        view
    }
    fn attr_tuple(&self, attr_str: &str) -> String {
        let mut s = String::new();
        let mut chrs = attr_str.chars();
        loop {
            let mut name = String::new();
            while let Some(c) = chrs.next() {
                match c {
                    '@' => {
                        let mut at = String::new();
                        while let Some(d) = chrs.next() {
                            match d {
                                '>' => break,
                                _ => {}
                            }
                            at.push(d);
                        }
                        let (first, second) = at.split_once('(').unwrap();
                        if first == "onclick" {
                            let (second, _) = second.rsplit_once(')').unwrap();
                            s = format!("(\"on:click\".to_string(), \"{}_{}[0]\".to_string())", self.lower_comp, second);
                        }
                        return s;
                    }
                    '=' => break,
                    _ => {}
                }
                name.push(c);
            }
            let name = name.trim().to_string();
            if let Some(attr) = collect_str(&mut chrs) {
                s.push_str(&format!("(\"{name}\", \"{attr}\")"));
            } else {
                break s;
            }
        }
    }
    fn process(&mut self, content: &str) -> String {
        let mut view = String::from("{let mut _children = vec![];");
        let mut chars = content.chars();
        let mut tags = vec![];
        while let Some(c) = chars.next() {
            match c {
                '@' => {
                    self.at(&mut view, &mut chars);
                }
                '<' => {
                    self.tag(&mut tags, &mut view, &mut chars);
                }
                _ => {
                    self.text(c, &mut tags, &mut view, &mut chars);
                }
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
        let inner = collect(chars, '>');
        let mut attrs = String::new();
        let name = if let Some((name, av)) = inner.trim().split_once(' ') {
            attrs = self.attr_tuple(av);
            name.to_string()
        } else {
            inner
        };
        if name.starts_with('/') {
            tags.pop();
            view.push_str("_children}));");
            return;
        } else {
            tags.push(name.clone());
        }
        let name = name.trim().to_uppercase();
        view.push_str(&format!("_children.push(element!(\"{name}\", attributes![{attrs}], {{let mut _children = vec![];"));
    }
    fn at(&mut self, view: &mut String, chars: &mut Chars) {
        let mut s = String::new();
        let mut extra = String::new();
        if let Some(c) = chars.next() {
            if c == '(' {
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
                        _ => {}
                    }
                    view.push_str("}");
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
        unimplemented!();
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

struct ResourceArgs {
    req: Ident,
    ty: syn::Path,
    cb: syn::ExprClosure,
}

impl Parse for ResourceArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let req = input.parse().expect("expected ident");
        let _c: syn::token::Comma = input.parse().unwrap();
        let ty = input.parse().expect("expected path");
        let _c: syn::token::Comma = input.parse().unwrap();
        let cb = input.parse().expect("expected closure");
        Ok(Self { req, ty, cb })
    }
}
