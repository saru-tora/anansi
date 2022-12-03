use std::cell::RefCell;
use std::collections::HashMap;

use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{Element, Node, NodeList, Document, Text};

use serde_json::Value;
use serde::{Serialize, Deserialize};
use wasm_bindgen_futures::spawn_local;

use async_channel::{unbounded, Sender, Receiver};

pub use anansi_macros::*;

pub mod prelude {
    pub use serde_json::Value;
    pub use serde::{Serialize, Deserialize};
    pub use anansi_macros::{store, Properties, component};
    pub use super::{attributes, element, Rsx, Sub, Proxy, Comp, Elem, Attribute};
}

pub mod components;

pub type Mounts = &'static [(&'static str, fn(Value, Value, Vec<Sub>, async_channel::Receiver<u8>, async_channel::Sender<Cmd>, String), u8)];

thread_local! {
    pub static DOCUMENT: Document = {
        let window = web_sys::window().expect("should have a window");
         window.document().expect("window should have a document")
    };
    pub static CALLBACKS: RefCell<HashMap<String, CallbackData>> = RefCell::new(HashMap::new());
    pub static APP_STATE: RefCell<Option<AppState>> = RefCell::new(None);
    pub static CHANNEL: RefCell<(Sender<Cmd>, Option<Receiver<Cmd>>)> = {
        let (tx, rx) = async_channel::unbounded(); 
        RefCell::new((tx, Some(rx)))
    };
    pub static SENDERS: RefCell<HashMap<usize, Sender<u8>>> = RefCell::new(HashMap::new());
}

pub enum Cmd {
    Update(Rsx, String),
    Set(HashMap<String, Ctx>),
}

pub struct Proxy {
    pub _learning: bool,
    pub _node: u32,
    pub _dirty: i64,
    pub _subs: Vec<Sub>,
}

impl Proxy {
    pub fn new(subs: Vec<Sub>) -> Self {
        Self {_learning: false, _node: 0, _dirty: -1, _subs: subs}
    }
    pub fn set(&mut self, n: i64) {
        if self._learning {
            self._subs.push((self._node, n));
        } else {
            if self._dirty == -1 {
                self._dirty = 0;
            }
            self._dirty |= n;
        }
    }
    pub fn start_proxy(&mut self) -> Vec<Sub> {
        self._learning = true;
        self._dirty = -1;
        let mut subs = vec![];
        subs.append(&mut self._subs);
        subs
    }
    pub fn stop_proxy(&mut self, subs: Vec<Sub>) {
        self._subs = subs;
        self._learning = false;
    }
    pub fn get_subs(&self) -> Vec<String> {
        let mut v = Vec::with_capacity(self._subs.len());
        for sub in &self._subs {
            v.push(format!("{} {}", sub.0, sub.1));
        }
        v
    }
}

#[derive(Debug)]
pub struct Comp {
    pub children: Vec<Rsx>,
}

#[derive(Debug)]
pub struct Elem {
    pub name: &'static str,
    pub attrs: Vec<Attribute>,
    pub children: Vec<Rsx>,
}

#[derive(Debug)]
pub struct Attribute {
    pub key: String,
    pub value: String,
}

#[macro_export]
macro_rules! attributes {
    ($(($k:expr, $v:expr)),* $(,)?) => {
        vec![$(Attribute {key: $k, value: $v},)*]
    }
}

impl Elem {
    fn to_node(&self, document: &Document) -> Node {
        let el = document.create_element(self.name).unwrap();
        for attr in &self.attrs {
            el.set_attribute(&attr.key, &attr.value).unwrap();
        }
        for child in &self.children {
            el.append_child(&child.to_node(document)).unwrap();
        }
        el.dyn_into::<Node>().unwrap()
    }
    fn diff(&self, node: &mut Node) {
        if self.name == node.node_name() {
            let el = node.dyn_ref::<Element>().unwrap();
            let attributes = el.attributes();
            if self.attrs.len() as u32 == attributes.length() {
                let mut same = true;
                for attr in &self.attrs {
                    if let Some(attribute) = attributes.get_named_item(&attr.key) {
                        if attribute.value() != attr.value {
                            same = false;
                            break;
                        }
                    }
                }
                if same {
                    return;
                }
            }
        } else {
            let parent = node.parent_node().unwrap();
            DOCUMENT.with(|document| {
                let new = self.to_node(&document);
                parent.replace_child(&new, &node).unwrap();
                *node = new;
            });
        }
    }
}

#[macro_export]
macro_rules! element {
    ($n:literal, $a:expr, $c: expr) => {
        Rsx::Element(Elem {name: $n, attrs: $a, children: $c})
    }
}

#[derive(Debug)]
pub enum Rsx {
    Component(Comp),
    Element(Elem),
    Text(String),
}

impl Rsx {
    fn edit(&self, node: &Node) {
        DOCUMENT.with(|document| {
            match self {
                Self::Element(elem) => {
                    let new = elem.to_node(&document);
                    add_sibling(node, &new);
                }
                Self::Text(text) => {
                    let new = document.create_text_node(&text).dyn_into::<Node>().unwrap();
                    add_sibling(node, &new);
                }
                Self::Component(_) => unimplemented!(),
            }
        });
    }
    fn to_node(&self, document: &Document) -> Node {
        match self {
            Self::Element(elem) => {
                elem.to_node(document)
            }
            Self::Text(text) => {
                document.create_text_node(&text).dyn_into::<Node>().unwrap()
            }
            Self::Component(_) => unimplemented!(),
        }
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub enum Ctx {
    R(String),
}

pub type Sub = (u32, i64);

pub struct AppState {
    objs: Vec<Value>,
    subs: Vec<Vec<Sub>>,
}

pub fn html_escape(s: &str) -> String {
    let mut escaped = String::new();
    for c in s.chars() {
        let html = match c {
            '<' => "&lt;",
            '>' => "&gt;",
            '\'' => "&#x27;",
            '"' => "&quot;",
            '&' => "&amp;",
            _ => {
                escaped.push(c);
                continue;
            }
        };
        escaped.push_str(html);
    }
    escaped
}

pub struct CallbackData {
    pub new: fn(Value, Value, Vec<Sub>, Receiver<u8>, Sender<Cmd>, String),
    pub num: u8,
}

#[macro_export]
macro_rules! log {
    ($f:literal $($t:tt)*) => {
        web_sys::console::log_1(&format!($f $($t)*).into());
    };
    ($($t:tt)*) => {
        web_sys::console::log_1(&$($t)*);
    };
}

fn add_sibling(node: &Node, new: &Node) {
    match node.node_type() {
        Node::ELEMENT_NODE => node.dyn_ref::<Element>().unwrap().after_with_node_1(new).unwrap(),
        Node::TEXT_NODE => node.dyn_ref::<Text>().unwrap().after_with_node_1(new).unwrap(),
        _ => unimplemented!(),
    }
}

fn get_state(document: &Document, ctx_map: &mut HashMap<String, Ctx>) -> Option<AppState> {
    let script = document.query_selector_all("script[type='app/json']").unwrap().get(0).unwrap();
    let text = script.text_content().unwrap();
    let json: Value = serde_json::from_str(&text).unwrap();
    let values = json.as_object().unwrap();
    let ctx = values.get("ctx").unwrap();
    let contexts = ctx.as_object().unwrap();
    let mut cmap = HashMap::new();
    for (id, n) in contexts {
        let c = serde_json::from_value(n.clone()).unwrap();
        cmap.insert(id.to_string(), c);
    }
    let object_array = values.get("objs").unwrap();
    let mut objs = vec![];
    for object in object_array.as_array().unwrap() {
        objs.push(object.clone());
    }
    let sub_array = values.get("subs").unwrap();
    let mut subs = vec![];
    for arr in sub_array.as_array().unwrap() {
        let mut sv = vec![];
        for sub in arr.as_array().unwrap() {
            let s = sub.as_str().unwrap();
            let nums: Vec<&str> = s.split(' ').collect();
            sv.push((nums[0].parse().unwrap(), nums[1].parse().unwrap()));
        }
        subs.push(sv);
    }
    let parent = script.parent_node().unwrap();
    parent.remove_child(&script).unwrap();
    *ctx_map = cmap;
    Some(AppState {objs, subs})
}

fn check_vnodes(nodes: &NodeList, vnode_map: &mut HashMap<String, Node>) {
    let mut ident;
    for i in 0..nodes.length() {
        let node = nodes.get(i).unwrap();
        if node.node_type() == Node::COMMENT_NODE {
            let comment = node.text_content().unwrap();
            if comment.starts_with("av ") {
                let attrs: Vec<&str> = comment.split(' ').collect();
                let mut id = false;
                for attr in &attrs[1..] {
                    let (k, v) = attr.split_once('=').unwrap();
                    if k == "a:id" {
                        ident = v.to_string();
                        vnode_map.insert(ident, node.clone());
                        id = true;
                        break;
                    }
                }
                if !id {
                    panic!("expected id for virtual node");
                }
            }
        } else {
            check_vnodes(&node.child_nodes(), vnode_map);
        }
    }
}
   
pub fn setup(callbacks: HashMap<String, CallbackData>) {
    std::panic::set_hook(Box::new(console_error_panic_hook::hook));
    CALLBACKS.with(|c| {
        let mut cb = c.borrow_mut();
        *cb = callbacks;
    });
    let mut contexts = HashMap::new();
    let mut vnode_map = HashMap::new();
    
    CHANNEL.with(|channel| {
        let grx = channel.borrow_mut().1.take().unwrap();
        spawn_local(async move {
            while let Ok(cmd) = grx.recv().await {
                match cmd {
                    Cmd::Update(rsx, node_id) => {
                        DOCUMENT.with(|document| {
                            let nodes = document.body().unwrap().child_nodes();
                            check_vnodes(&nodes, &mut vnode_map);
                            let vn_index = match contexts.get(&node_id).unwrap() {
                                Ctx::R(s) => s,
                            };
                            let mut node = vnode_map.get(vn_index).unwrap().clone().next_sibling().unwrap();
                            update(&rsx, &mut node);
                            close_vnode(&document, &node);
                        });
                    }
                    Cmd::Set(ctx) => {
                        contexts = ctx;
                    }
                }
            }
        });
    });
}

#[wasm_bindgen]
pub fn call(callback: &str, node_id: &str) -> Result<(), JsValue> {
    let (name, arr) = callback.split_once('[').unwrap();
    let (arr, _) = arr.rsplit_once(']').unwrap();
    let arr: Vec<&str> = arr.split(' ').collect();
    let arr: Vec<usize> = arr.iter().map(|e| e.parse().unwrap()).collect();

    CALLBACKS.with(|c| {
        let callbacks = c.borrow();
        if let Some(cb) = callbacks.get(name) {
            CHANNEL.with(|channel| {
                let gtx = channel.borrow().0.clone();
                APP_STATE.with(|a| {
                    let mut app_state = a.borrow_mut();
                    let store = if let Some(state) = app_state.as_ref() {
                        state
                    } else {
                        let mut contexts = HashMap::new();
                        DOCUMENT.with(|document| {
                            *app_state = get_state(&document, &mut contexts);
                        });
                        let gtx = gtx.clone();
                        spawn_local(async move {gtx.send(Cmd::Set(contexts)).await.unwrap()});
                        app_state.as_mut().unwrap()
                    };

                    let n = arr[0];
                    SENDERS.with(|s| {
                        let mut senders = s.borrow_mut();
                        let sender = senders.entry(n).or_insert_with(|| {
                            let v1 = store.objs[n].clone();
                            let v2 = store.objs[n+1].clone();
                            let subs = store.subs[0].clone();
                            let (sender, receiver) = unbounded();
                            (cb.new)(v1, v2, subs, receiver, gtx, node_id.to_string());
                            sender
                        });
                        let sender = sender.clone();
                        
                        let n = cb.num;
                        spawn_local(async move {sender.send(n).await.unwrap()});
                    });
                });
            });
        }
    });

    Ok(())
}

fn update(rsx: &Rsx, node: &mut Node) {
    match rsx {
        Rsx::Element(element) => {
                element.diff(node);
                check_siblings(&element.children, &mut node.first_child().unwrap());
        }
        Rsx::Text(text) => {
            set_content(&node, &text);
        }
        Rsx::Component(comp) => {
            check_siblings(&comp.children, node);
        }
    }
}

fn check_siblings(children: &Vec<Rsx>, node: &mut Node) {
    let mut children = children.iter();
    let l = children.len();
    let mut n = 0;
    let mut button = false;
    
    loop {
        if let Some(child) = children.next() {
            if node.node_name() == "BUTTON" {
                match child {
                    Rsx::Element(el) => {
                        if el.name == "BUTTON" {
                            button = true;
                            continue;
                        }
                    }
                    _ => {}
                }
                DOCUMENT.with(|document| {
                    let parent = node.parent_node().unwrap();
                    let new = child.to_node(&document);
                    parent.insert_before(&new, Some(&node)).unwrap();
                });
                continue;
            } else {
                update(child, node);
            }
            
            if let Some(sib) = node.next_sibling() {
                if sib.node_type() == Node::COMMENT_NODE && sib.text_content().unwrap() == "/av" {
                    while let Some(c) = children.next() {
                        c.edit(&node);
                        *node = node.next_sibling().unwrap();
                    }
                    return;
                }

                if n < l - 1 {
                    *node = sib;
                }
            } else {
                if n < l - 1 {
                    child.edit(&node);
                    while let Some(c) = children.next() {
                        *node = node.next_sibling().unwrap();
                        c.edit(&node);
                    }
                }
                return;
            };
        } else {
            if !button && node.node_name() == "BUTTON" {
                let parent = node.parent_node().unwrap();
                parent.remove_child(&node).unwrap();
                return;
            }
            if let Some(s) = node.next_sibling() {
                let parent = node.parent_node().unwrap();
                parent.remove_child(&s).unwrap();
                while let Some(sib) = node.next_sibling() {
                    parent.remove_child(&sib).unwrap();
                }
            }
            return;
        }
        n += 1;
    }
}

fn set_content(n: &Node, content: &str) {
    if n.text_content().unwrap() != content {
        n.set_text_content(Some(content));
    }
}

fn close_vnode(document: &Document, node: &Node) {
    if let Some(n) = node.next_sibling() {
        if n.node_type() == Node::COMMENT_NODE && n.text_content().unwrap() != "/av" {
            let c = document.create_comment("/av").dyn_into::<Node>().unwrap();
            add_sibling(&n, &c);
        }
    }
}
