use std::fs::{self, read_dir};
use std::time::SystemTime;
use std::path::{PathBuf, Component, MAIN_SEPARATOR};
use std::io::Write;
use std::str::Chars;
use std::env;
use std::collections::{HashMap, HashSet};
use std::process::Command;

use which::which;

mod components;

use components::{get_expr, check_components, init_components};

const VERSION: &str = env!("CARGO_PKG_VERSION");

pub fn main() {
    let mut args: Vec<String> = env::args().collect();
    if args.len() > 1 {
        let first = args[1].as_str();
        match first {
            "run" => {
                build_tailwind(&args);
                cargo(&args);
            }
            "check" => {
                template(&args, false, "");
                check_wasm(&args, false, false);
                cargo(&args);
            }
            "build" => {
                build_tailwind(&args);
                cargo(&args);
            }
            "force-check" => {
                template(&args, true, "");
                check_wasm(&args, true, true);
                args[1] = "check".to_string();
                cargo(&args);
            }
            "new" => {
                cargo(&args);
                new(&args);
            }
            "init-components" => {
                init_components(&args);
            }
            "init-tailwind" => {
                init_tailwind(&args);
            }
            "app" => {
                if args.len() > 2 {
                    app(&args);
                } else {
                    eprintln!("Expected name");
                }
            }
            "migrate" => {
                cargo_run(&mut args);
            }
            "make-view" => {
                make_view(&mut args);
            }
            "sql-migrate" => {
                cargo_run(&mut args);
            }
            "make-migrations" => {
                cargo_run(&mut args);
            }
            "admin" => {
                cargo_run(&mut args);
            }
            "--version" => println!("ananc {}", VERSION),
            _ => usage(),
        }
    }
}

#[cfg(not(host_family = "windows"))]
macro_rules! main_separator {
    () => {r"/"}
}

#[cfg(host_family = "windows")]
macro_rules! main_separator {
    () => {r"\"}
}

macro_rules! cp {
    ($name:expr, $file:expr) => {
        let mut n = $name.clone();
        n.push($file);
        fs::write(&n, include_bytes!(concat!("skeleton", main_separator!(), $file))).expect(&format!("Error copying to {}", n.to_str().unwrap()));
    };
    ($name:expr, $($file:expr),*) => {
        $(cp!($name, $file);)*
    };
}

macro_rules! cp_as {
    ($name:expr, $file:expr) => {
        fs::write($name.to_owned(), include_bytes!(concat!("skeleton", main_separator!(), $file))).expect("Error copying file");
    };
}

fn build_tailwind(args: &Vec<String>) {
    let a = template(args, false, "");
    let b = check_wasm(args, false, true);
    if a || b {
        let (_, mut dir) = get_src(args, "");
        dir.push("src");
        let mut global = dir.clone();
        global.push("global.css");
        let mut cmd = Command::new("npx");
        cmd.arg("tailwindcss");
        cmd.arg("-i");
        cmd.arg(global);
        dir.push("static");
        dir.push("styles");
        dir.push("global.css");
        cmd.arg("-o");
        cmd.arg(dir);
        let mut child = cmd.spawn().expect("Failed to start tailwindcss");
        child.wait().expect("Failed to wait on child");
    }
}

fn init_tailwind(args: &Vec<String>) {
    let (_, mut dir) = get_src(args, "");
    let mut cmd = Command::new("npm");
        cmd.arg("install");
        cmd.arg("-D");
        cmd.arg("tailwindcss");
    let mut child = cmd.spawn().expect("Failed to install tailwindcss");
    child.wait().expect("Failed to wait on child");
    make_file(&dir, "tailwind", ".config.js", 
"/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [\"./src/**/templates/*\", \"./*comps/src/*\"],
  theme: {
    extend: {},
  },
  plugins: [],
}".to_string());
    dir.push("src");
    make_file(&dir, "global", ".css", 
"@tailwind base;
@tailwind components;
@tailwind utilities;".to_string());
    dir.push("static");
    if fs::create_dir(&dir).is_ok() {
        dir.push("styles");
        if fs::create_dir(&dir).is_err() {
            eprintln!("Did not create styles directory");
        }
    }
}

fn uppercase(s: &str) -> String {
    let mut c = s.chars();
    c.next().unwrap().to_uppercase().collect::<String>() + c.as_str()
}

fn make_view(args: &Vec<String>) {
    for arg in &args[2..] {
        let mview = format!("{}", arg);
        fs::create_dir(&mview).expect("Failed to create path");
        let mut temp = PathBuf::from(arg);
        temp.push("templates");
        fs::create_dir(&temp).expect("Failed to create path");
        let parsed = format!("{}{MAIN_SEPARATOR}templates{MAIN_SEPARATOR}.parsed", arg);
        fs::create_dir(&parsed).expect("Failed to create path");
        let upper = uppercase(arg);
        let arg_path = PathBuf::from(arg);
        make_file(&arg_path, "views", ".rs", format!("use crate::prelude::*;\nuse super::super::records::{{{}}};\n\n#[base_view]\nfn base<R: Request>(_req: &mut R) -> Result<Response> {{}}\n\n#[viewer]\nimpl<R: Request> {0}View<R> {{\n    #[view(Site::is_visitor)]\n    pub async fn index(req: &mut R) -> Result<Response> {{\n        let title = \"Title\";\n    }}\n}}", upper));
        make_file(&arg_path, "mod", ".rs", "pub mod views;".to_string());
        let mut t2 = temp.clone();
        let mut t3 = temp.clone();
        t2.push("index.rs.html");
        t3.push("base.rs.html");
        cp_as!(&t2, concat!("templates", main_separator!(), "index.rs.html"));
        cp_as!(&t3, concat!("templates", main_separator!(), "base.rs.html"));
        append(".", "mod.rs", &format!("\npub mod {};\n", arg).into_bytes());
        println!("Created view \"{}\"", arg);
    }
}

fn usage() {
    eprintln!("Anansi's project manager\n\nUSAGE:\n    ananc [OPTIONS] [SUBCOMMAND]\n\nOPTIONS:\n    --version\tPrint version info and exit\n\nIn addition to Cargo's commands, some others are:\n    app\t\t\tCreate an app\n    sql-migrate\t\tView SQL for migration files\n    make-migrations\tCreate migration files for the project\n    migrate\t\tApply migrations");
}

fn get_src(args: &Vec<String>, extra: &str) -> (SystemTime, PathBuf) {
    let path = match fs::canonicalize(&args[0]) {
        Ok(o) => o,
        Err(_) => which(&args[0]).unwrap(),
    };
    let date = fs::metadata(&path).unwrap().modified().unwrap();
    let mut cwd = env::current_dir().unwrap();
    cwd.push(extra);
    let dir;
    let mut first_src = None;

    'outer: loop {
        let dirs = read_dir(&cwd).unwrap();
        for f in dirs {
            let f = f.unwrap();
            if !f.file_type().unwrap().is_dir() {
                let name = f.file_name().to_str().unwrap().to_string();
                if name == "settings.toml" {
                    dir = &cwd;
                    break 'outer;
                }
            } else if first_src.is_none() {
                let name = f.file_name().to_str().unwrap().to_string();
                if name == "src" {
                    first_src = Some(cwd.clone());
                }
            }
        }
        if !cwd.pop() {
            if let Some(s) = first_src {
                return (date, s)
            } else {
                panic!("expected src directory");
            }
        }
    }
    let d = dir.clone();
    (date, d)
}

fn check_wasm(args: &Vec<String>, force: bool, build: bool) -> bool {
    let mut changed = false;
    let (mut date, dir) = get_src(args, "");
    let name = dir.file_name().unwrap().to_str().unwrap().to_string();
    let mut cpath = dir.clone();
    cpath.push(format!("{}-comps", name));
    if cpath.canonicalize().is_ok() {
        cpath.push("src");
        check_parsed_comps(&date, &cpath, &mut changed);
    }
    if build {
        let mut wpath = dir.clone();
        wpath.push(format!("{}-wasm", name));
        if wpath.canonicalize().is_ok() {
            check_files(&mut date, &dir, wpath, force, &mut changed);
        }
    }
    changed
}

fn check_parsed_comps(date: &SystemTime, dir: &PathBuf, changed: &mut bool) {
    let dirs = read_dir(&dir).unwrap();
    let mut path = dir.clone();
    path.push(".parsed");
    for f in dirs {
        let f = f.unwrap();
        if !f.file_type().unwrap().is_dir() {
            let name = f.file_name();
            let mut parsed_path = path.clone();
            parsed_path.push(name);
            let parsed = match fs::metadata(parsed_path) {
                Ok(m) => m,
                Err(_) => {
                    check_components(f.path(), changed);
                    continue;
                }
            };
            let modified = f.metadata().unwrap().modified().unwrap();
            let parsed_modified = parsed.modified().unwrap();
            if parsed_modified < *date || modified > parsed_modified {
                check_components(f.path(), changed);
            }
        }
    }
}

fn check_files(date: &mut SystemTime, dir: &PathBuf, mut wasm: PathBuf, force: bool, changed: &mut bool) {
    let name = dir.file_name().unwrap().to_str().unwrap().to_string();
    wasm.push("pkg");
    let dirs = match read_dir(&wasm) {
        Ok(d) => d,
        Err(_) => {
            wasm.pop();
            build_wasm(&wasm);
            return;
        }
    };
    for f in dirs {
        let f = f.unwrap();
        if f.file_name().to_str().unwrap().ends_with(".wasm") {
            let modified = f.metadata().unwrap().modified().unwrap();
            if *date > modified {
                wasm.pop();
                build_wasm(&wasm);
                return;
            } else {
                *date = modified
            }
            break;
        }
    }
    let comp_name = format!("{}-comps", name);
    let mut comp = dir.clone();
    comp.push(comp_name);
    comp.push("src");
    wasm.pop();
    check_files2(&comp, &wasm, &date, force, changed);
}

fn build_wasm(dir: &PathBuf) {
    let mut cmd = Command::new("wasm-pack");
    cmd.arg("build");
    cmd.arg("--target");
    cmd.arg("web");
    cmd.arg(dir);
    let mut child = cmd.spawn().expect("Failed to start wasm-pack");
    child.wait().expect("failed to wait on child");
}

fn check_files2(dir: &PathBuf, wasm: &PathBuf, date: &SystemTime, force: bool, changed: &mut bool) {
    let src = read_dir(&dir).unwrap();
    let mut b = false;
    for s in src {
        let s = s.unwrap();
        if s.file_type().unwrap().is_file() {
            let modified = s.metadata().unwrap().modified().unwrap();
            if modified > *date || force {
                if s.file_name() != "lib.rs" {
                    check_components(s.path(), changed);
                }
                b = true;
            }
        }
    }
    if b {
        build_wasm(&wasm);
    }
}

fn new(args: &Vec<String>) {
    let name = &args[2];
    let arg_path = PathBuf::from(&name);
    let mut src = PathBuf::from(&name);
    src.push("src");
    cp!(arg_path, "settings.toml");
    cp!(src, "project.rs", "urls.rs", "main.rs");
    append(name, ".gitignore", b"/settings.toml\n/database.db*");
    prepend(name, "Cargo.toml", b"[workspace]\nmembers = [\n    \".\",\n]\n\n");
    append(name, "Cargo.toml", &format!("anansi = {{ version = \"{}\", features = [\"sqlite\"] }}\nasync-trait = \"0.1.57\"", VERSION).into_bytes());
    fs::create_dir(format!("{}{MAIN_SEPARATOR}src{MAIN_SEPARATOR}http_errors", name)).unwrap();
    cp!(src, concat!("http_errors", main_separator!(), "500.html"));
    cp!(src, concat!("http_errors", main_separator!(), "views.rs"));
    cp!(src, concat!("http_errors", main_separator!(), "mod.rs"));
    let mut s2 = src.clone();
    s2.push("http_errors");
    s2.push("templates");
    fs::create_dir(&s2).unwrap();
    cp!(src, concat!("http_errors", main_separator!(), "templates", main_separator!(), "not_found.rs.html"));
    s2.push(".parsed");
    fs::create_dir(s2).unwrap();
    template(args, false, &format!("{name}"));
}

fn prepend(dir_name: &str, file_name: &str, content: &[u8]) {
    let path = format!("{}{}{}", dir_name, MAIN_SEPARATOR, file_name);
    let mut original = fs::read(&path).unwrap();
    let mut content = content.to_vec();
    content.append(&mut original);
    let mut file = fs::OpenOptions::new()
      .write(true)
      .open(format!("{}{}{}", dir_name, MAIN_SEPARATOR, file_name))
      .expect(&format!("error with {}{}{}", dir_name, MAIN_SEPARATOR, file_name));
   file.write_all(&content).unwrap();
}

fn append(dir_name: &str, file_name: &str, content: &[u8]) {
    let mut file = fs::OpenOptions::new()
      .write(true)
      .append(true)
      .open(format!("{}{}{}", dir_name, MAIN_SEPARATOR, file_name))
      .expect(&format!("error with {}{}{}", dir_name, MAIN_SEPARATOR, file_name));
   file.write_all(content).unwrap();
}

fn app(args: &Vec<String>) {
    let name = PathBuf::from(&args[2]);
    fs::create_dir(&name).expect("Failed to create app directory");
    let mut migrations = name.clone();
    migrations.push("migrations");
    fs::create_dir(migrations).expect("Failed to create migrations directory");
    make(&name, "mod", format!("pub mod urls;\npub mod records;\npub mod migrations;\n\npub const APP_NAME: &'static str = \"{}\";", args[2]));
    make(&name, "urls", "use anansi::web::prelude::*;\n\nroutes! {}".to_string());
    let mut m = format!("migrations{}", main_separator!());
    m.push_str("mod");
    make(&name, &m, "use anansi::migrations::prelude::*;\n\nlocal_migrations! {}".to_string());
    cp!(name, "records.rs");
    println!("Created app \"{}\"", args[2]);
}

fn make_file(dir: &PathBuf, name: &str, ext: &str, content: String) {
    let mut f = dir.clone();
    f.push(format!("{name}{ext}"));
    fs::write(&f, content).expect(&format!("Could not create {}", f.to_str().expect("Could not convert file to string")));
}

fn make(dir: &PathBuf, name: &str, content: String) {
    make_file(dir, name, ".rs", content);
}

fn template(args: &Vec<String>, mut force: bool, extra: &str) -> bool {
    let (date, dir) = get_src(args, extra);
    search(&date, &dir, &mut force)
}

fn search(date: &SystemTime, current: &PathBuf, force: &mut bool) -> bool {
    let mut changed = false;
    let dirs = read_dir(current).unwrap();
    for f in dirs {
        let f = f.unwrap();
        if f.file_type().unwrap().is_dir() {
            if f.file_name() == "templates" {
                let parent = f.path();
                let dirs = read_dir(f.path()).unwrap();
                for f in dirs {
                    let f = f.unwrap();
                    let name = f.path();
                    if name.to_str().unwrap().ends_with(".rs.html") {
                        let b = check_template(f, &parent, &name, *date, *force, &mut changed);
                        if b {
                            *force = true;
                        }
                    } else if f.file_type().unwrap().is_dir() {
                        let parent = f.path();
                        let dirs = read_dir(f.path()).unwrap();
                        for f in dirs {
                            let f = f.unwrap();
                            let name = f.path();
                            if name.ends_with(".rs.html") {
                                check_template(f, &parent, &name, *date, *force, &mut changed);
                            }
                        }
                    }
                }
            } else {
                search(date, &f.path(), force);
            }
        }
    }
    changed
}

fn check_template(f: std::fs::DirEntry, parent: &PathBuf, name: &PathBuf, date: std::time::SystemTime, force: bool, changed: &mut bool) -> bool {
    let template = f.metadata().unwrap().modified().unwrap();
    let n = f.file_name().into_string().unwrap();
    let (n, _) = n.split_once('.').unwrap();
    let mut prs = parent.clone();
    prs.push(".parsed");
    let mut p = prs.clone();
    p.push(format!("{}.in", n));
    let mut parser = Parser::new();
    if std::path::Path::new(&prs).exists() {
        let parsed = fs::metadata(p);
        if parsed.is_err() {
            parser.parse(&name);
            *changed = true;
        } else {
            let modified = parsed.unwrap().modified().unwrap();
            if force || template > modified || modified < date {
                parser.parse(&name);
                *changed = true;
                return true;
            }
        }
    } else {
        eprintln!("{} was not parsed", name.clone().into_os_string().into_string().unwrap());
    }
    false
}

fn cargo_run(args: &mut Vec<String>) {
    args.insert(1, "run".to_string());
    cargo(&args);
}

fn cargo(args: &Vec<String>) {
    let mut cmd = Command::new("cargo");
    for arg in &args[1..] {
        cmd.arg(arg);
    }
    let mut child = cmd.spawn().expect("Failed to start cargo");
    child.wait().expect("failed to wait on child");
}

struct Parser {
    lower_comp: String,
    blocks: Vec<String>,
    selectors: HashSet<String>,
}

impl Parser {
    fn new() -> Self {
        Self {lower_comp: String::new(), blocks: vec![], selectors: HashSet::new()}
    }
    fn comp(lower_comp: String, selectors: HashSet<String>) -> Self {
        Self {lower_comp, blocks: vec![], selectors}
    }
    fn to_html(&mut self, content: &str) -> String {
        let mut view = String::from("let mut _c = String::new();");
        view.push_str(&self.process(content));
        view
    }
    fn parse(&mut self, name: &PathBuf) {
        let content = fs::read_to_string(name).unwrap().trim().to_string();
        let ext = content.starts_with("@block");
        let base_ext = content.starts_with("@base_extend");
        let mut chrs = content.chars();
        let mut dir: Vec<Component> = name.components().collect();
        let temp = dir.pop().unwrap();
        let mut base = false;
        let view = if ext {
            self.extend(&mut chrs)
        } else if base_ext {
            base = true;
            self.extend(&mut chrs)
        } else {
            let mut view = String::from("{let mut _c = String::new();");
            view.push_str(&self.process(&content));
            view.push_str("Ok(anansi::web::Response::new(200, _c.into_bytes()))}");
            view
        };
        let out = if let Component::Normal(temp) = temp {
            let temp = temp.to_str().unwrap();
            temp[..temp.find('.').unwrap()].to_string()
        } else {
            panic!("Could not get output file name");
        };

        let mut n2 = name.clone();
        n2.pop();
        n2.push(".parsed");

        if !self.blocks.is_empty() || base {
            let mut s = "pub struct Args {".to_string();
            let mut t = String::new();
            let mut u = String::new();
            let mut v = String::new();
            let mut new = String::new();
            for block in &self.blocks {
                s.push_str(&format!("pub _{}: String,", block));
                t.push_str(&format!("v.append(&mut self._{}.len().to_ne_bytes().to_vec());v.append(&mut self._{0}.as_bytes().to_vec());", block));
                u.push_str(&format!("let mut buf = __b.split_off(8); let l = usize::from_ne_bytes(__b.try_into().unwrap()); let mut __b = buf.split_off(l); let _{} = String::from_utf8(buf)?;", block));
                v.push_str(&format!("_{}, ", block));
                new.push_str(&format!("_{}: String::new(), ", block));
            }
            s.push_str("}");
            s.push_str("impl anansi::cache::Cacheable for Args {fn to_bytes(&self) -> Vec<u8> {let mut v = vec![];");
            s.push_str(&t);
            s.push_str("v} fn from_bytes(mut __b: Vec<u8>) -> anansi::web::Result<Self> {");
            s.push_str(&u);
            s.push_str("Ok(Self {");
            s.push_str(&v);
            s.push_str("})}} impl Args {pub fn new() -> Self {Self {");
            s.push_str(&new);
            s.push_str("}}}");
            let mut n3 = n2.clone();
            n3.push(format!("{}_args.in", out));
            let mut f = fs::File::create(n3).unwrap();
            write!(f, "{}", s).unwrap();
        }

        n2.push(format!("{}.in", out));
        let mut f = fs::File::create(n2).unwrap();
        write!(f, "{}", view).unwrap();
    }
    fn process(&mut self, content: &str) -> String {
        let mut view = String::from("_c.push_str(\"");
        let mut chars = content.chars();
        while let Some(c) = chars.next() {
            match c {
                '"' => {
                    view.push_str("\\\"");
                }
                '\\' => {
                    view.push_str("\\\\");
                }
                '@' => {
                    self.at(&mut view, &mut chars);
                }
                '<' => {
                    self.tag(&mut view, &mut chars);
                }
                _ => {
                    view.push(c);
                }
            }
        }
        view.push_str("\");");
        view
    }
    fn tag(&mut self, view: &mut String, chars: &mut Chars) {
        let (name, extra) = collect_name(chars);
        if !name.starts_with(char::is_uppercase) {
            view.push('<');
            view.push_str(&name);
            if self.selectors.contains(&name) {
                view.push_str(&format!(" class=\\\"anansi-{}\\\"", self.lower_comp.trim()));
            }
        } else {
            let args = collect(chars, '>');
            let (args, _) = args.rsplit_once('/').unwrap();
            let mut chrs = args.chars();
            let mut list = String::new();
            loop {
                let arg = collect(&mut chrs, ' ');
                if arg.is_empty() {
                    break;
                }
                if arg.starts_with('@') {
                    let (_, a) = arg.split_once('@').unwrap();
                    list.push_str(&format!(".{}({0})", a));
                } else {
                    let (n, a) = arg.split_once('=').unwrap();
                    let ar = if a.trim().starts_with('@') {
                        let (_, a) = a.split_once('@').unwrap();
                        a.to_string()
                    } else {
                        a.to_string()
                    };
                    list.push_str(&format!(".{}({})", n, ar));
                }
            }
            let prop = if !list.is_empty() {
                format!("<{} as anansi_aux::components::Component>::Properties::new(){}.build(), &mut _p", name, list)
            } else {
                "(), _p".to_string()
            };
            view.push_str(&format!("\");_c.push_str(&format!(\"<!--av a:id={{}}-->\", _p.id()));_c.push_str(&<{} as anansi_aux::components::Component>::init({}));_c.push_str(\"<!--/av-->", name, prop));
        }
        if extra == '@' {
            self.at(view, chars);
        } else {
            view.push(extra);
        }
    }
    fn extend(&mut self, chars: &mut Chars) -> String {
        let mut blocks = HashMap::new();
        loop {
            skip(chars, '@');
            let s = collect(chars, ' ');
            if s == "block" {
                let name = collect(chars, ' ');
                skip(chars, '{');
                let mut block = String::new();
                if let Some(c) = chars.next() {
                    if c != '\n' {
                        block.push(c);
                    }
                }
                get_block(chars, &mut block);
                let block = self.process(&block);
                let block = format!("{{let mut _c = String::new();{} _c}}", block);
                blocks.insert(name, block);
            } else {
                break;
            }
        }
        let mut s = String::from("{");
        for (name, src) in blocks {
            s.push_str(&format!("_args._{} = {};", name, src));
        }
        s.push_str("_args}");
        s
    }
}

fn skip(chars: &mut Chars, chr: char) {
    while let Some(c) = chars.next() {
        if c == chr {
            break;
        }
    }
}

fn collect_paren(chars: &mut Chars) -> String {
    let mut s = String::new();
    let mut n = 0;
    let mut quote = false;
    while let Some(c) = chars.next() {
        s.push(c);
        if !quote {
            match c {
                ')' => {
                    if n == 0 {
                        break;
                    }
                    n -= 1;
                }
                '(' => {
                    n += 1;
                }
                '"' => {
                    quote = true;
                }
                _ => {}
            }
        } else {
            match c {
                '\'' => {
                    if let Some(d) = chars.next() {
                        s.push(d);
                    } else {
                        break;
                    }
                }
                '"' => {
                    quote = false;
                }
                _ => {}
            }
        }
    }
    s
}

fn collect(chars: &mut Chars, chr: char) -> String {
    let mut s = String::new();
    let mut take = chars.take_while(|c| *c != chr);
    while let Some(c) = take.next() {
        s.push(c);
    }
    s
}

fn collect_name(chars: &mut Chars) -> (String, char) {
    let mut name = String::new();
    loop {
        if let Some(c) = chars.next() {
            match c {
                ' ' => {}
                '<' => {}
                '>' => {}
                '(' => {}
                ')' => {}
                '\n' => {}
                _ => {
                    name.push(c);
                    continue;
                }
            }
            break (name, c);
        }
        panic!("error collecting name");
    }
}

fn get_block(chars: &mut Chars, block: &mut String) {
    let mut n = 0;
    let mut quote = false;
    while let Some(c) = chars.next() {
        if c == '\\' {
            block.push(c);
            if let Some(d) = chars.next() {
                block.push(d);
            }
        } else {
            if !quote {
                match c {
                    '{' => n += 1,
                    '}' => {
                        if n == 0 {
                            return;
                        }
                        n -= 1;
                    }
                    '"' => quote = true,
                    '\n' => {
                        if let Some(d) = chars.next() {
                            if d == '}' {
                                if n == 0 {
                                    return;
                                } else {
                                    n -= 1;
                                }
                            }
                            block.push(c);
                            block.push(d);
                            continue;
                        }
                    }
                    _ => {}
                }
            } else {
                if c == '"' {
                    quote = false;
                }
            }
            block.push(c);
        }
    }
}

impl Parser {
    fn escape_path(&self) -> &'static str {
        if self.lower_comp.is_empty() {
            "anansi::web"
        } else {
            "anansi_aux"
        }
    }
    fn at(&mut self, view: &mut String, chars: &mut Chars) {
        view.push_str("\");");
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
                view.push_str(&format!("_c.push_str(&{}::html_escape(&format!(\"{{}}\", {})));_c.push_str(\"", self.escape_path(), s));
                return;
            }
            s.push(c);
        } else {
            return;
        }
        while let Some(c) = chars.next() {
            if c == ' ' || c == '<' || c == '\n' || c == '(' || c == '/' {
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
            "block" => {
                let (name, ex) = collect_name(chars);
                self.blocks.push(name.clone());
                view.push_str(&format!("_c.push_str(&_base_args._{});_c.push_str(\"{}", name, ex));
                return;
            }
            "load" => {
                let (name, ex) = collect_name(chars);
                if name == "components" {
                    s = format!("let mut _p = anansi_aux::components::Pauser::new();");
                } else {
                    unimplemented!();
                }
                if ex == '{' {
                    find_brace = false;
                }
            }
            "build" => {
                let (name, ex) = collect_name(chars);
                s = format!("_c.push_str(&{}.tag()); if let Some(token_tag) = form.token_tag() {{ _c.push_str(&token_tag) }}", name);
                if ex == '{' {
                    find_brace = false;
                }
            }
            "unescape" => {
                let (mut name, ex) = collect_name(chars);
                if ex == '(' {
                    name.push(ex);
                    let mut n = 1;
                    while let Some(c) = chars.next() {
                        name.push(c);
                        if c == ')' {
                            n -= 1;
                            if n == 0 {
                                view.push_str(&format!("_c.push_str(&format!(\"{{}}\", {}));_c.push_str(\"", name));
                                break;
                            }
                        } else if c == '(' {
                            n += 1;
                        }
                    }
                } else {
                    view.push_str(&format!("_c.push_str(&format!(\"{{}}\", {}));_c.push_str(\"{}", name, ex));
                }
                return;
            }
            "link" => {
                let (av, segments) = get_attrs(&mut s, chars);
                let mut attrs = String::new();
                for attr in av {
                    attrs.push_str(&format!(" {}", attr.replace("\"", "\\\"")));
                }
                let mut u = String::from(segments[0].clone());
                for segment in &segments[1..] {
                    u.push_str(&format!(", {}", segment));
                }
                view.push_str(&format!("_c.push_str(&format!(\"<a href=\\\"{{}}\\\"{}>\", anansi::url!({})));", attrs, u));
                let blk = collect(chars, '}');
                view.push_str(&self.process(&blk));
                view.push_str("_c.push_str(\"</a>");
                return;
            }
            "href" => {
                let (_, segments) = get_attrs(&mut s, chars);
                let mut u = String::from(segments[0].clone());
                let mut v = String::from("{}");
                for segment in &segments[1..] {
                    u.push_str(&format!(", {}", segment));
                    v.push_str("/{}");
                }
                view.push_str(&format!("_c.push_str(&format!(\"<a href=\\\"{}\\\">\", {}));", v, u));
                let blk = collect(chars, '}');
                view.push_str(&self.process(&blk));
                view.push_str("_c.push_str(\"</a>");
                return;
            }
            "resource" => {
                get_expr(chars);
                view.push_str("_c.push_str(\"");
                return;
            }
            "onclick" => {
                let (callback, _) = collect_name(chars);
                view.push_str(&format!("_c.push_str(&format!(\"on:click=\\\"{}_{}[0]\\\" a:id=\\\"{{}}\\\"\", _p.add()));_c.push_str(\"", self.lower_comp, callback));
                return;
            }
            "cache" => {
                s.clear();
                let line = collect(chars, '{');
                let args: Vec<&str> = line.split(',').collect();
                let req = args[0];
                let timeout = args[1];
                let key = args[2];
                let key = if args.len() == 2 {
                    format!("&format!(\"{key}{{}}\", {req}.user().username())")
                } else if args[3] == "visitor" {
                    format!("\"{key}\"")
                } else {
                    unimplemented!()
                };

                view.push_str(&format!("{{let _d = if let Ok(res) = {req}.cache().get({key}) {{String::from_utf(res)?}} else {{let mut _c = String::new();"));
                let blk = collect(chars, '}');
                view.push_str(&self.process(&blk));
                view.push_str(&format!("{req}.cache_mut().set_ex({key}, _c.as_bytes(), {timeout}).await?; _c\"}}; _c.push_str(&_d);}} _c.push_str(\""));
                return;
            }
            "url!" => {
                s = "anansi::url!".to_string();
                variable(self.escape_path(), &extra, &mut s, chars, view);
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
                    get_var(self.escape_path(), &extra, &mut s, chars, view);
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
        view.push_str(&format!("{}_c.push_str(\"", s));
        while let Some(c) = chars.next() {
            match c {
                '"' => {
                    view.push_str("\\\"");
                }
                '\\' => {
                    view.push_str("\\\\");
                    if let Some(d) = chars.next() {
                        view.push(d);
                    }
                }
                '}' => {
                    match keyword.as_str() {
                        "build" => view.push_str("</form>\");}_c.push_str(\""),
                        "load" => view.push_str("\");_c.push_str(&_p.to_string());}_c.push_str(\""),
                        _ => view.push_str("\");}_c.push_str(\""),
                    }
                    return;
                }
                '@' => {
                    self.at(view, chars);
                }
                '<' => {
                    self.tag(view, chars);
                }
                _ => {
                    view.push(c);
                }
            }
        }
        view.push_str("_c.push_str(\"");
    }
}

fn get_var(esc_path: &str, extra: &str, s: &mut String, chars: &mut Chars, view: &mut String) {
    if extra == "(" {
        s.push('(');
        let mut n = 1;
        while let Some(c) = chars.next() {
            s.push(c);
            if c == ')' {
                n -= 1;
                if n == 0 {
                    view.push_str(&format!("_c.push_str(&{}::html_escape(&format!(\"{{}}\", {})));_c.push_str(\"", esc_path, s));
                    break;
                }
            } else if c == '(' {
                n += 1;
            }
        }
    } else {
        s.push_str(&collect_var(chars));
        view.push_str(&format!("_c.push_str(&{}::html_escape(&format!(\"{{}}\", {})));_c.push_str(\"{}", esc_path, s, extra));
    }
}

fn get_attrs(s: &mut String, chars: &mut Chars) -> (Vec<String>, Vec<String>) {
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

fn variable(esc_path: &str, extra: &str, s: &mut String, chars: &mut Chars, view: &mut String) {
    if extra == "(" {
        s.push_str(extra);
        s.push_str(&collect_paren(chars));
        view.push_str(&format!("_c.push_str(&{}::html_escape(&format!(\"{{}}\", {})));_c.push_str(\"", esc_path, s));
    } else {
        view.push_str(&format!("_c.push_str(&{}::html_escape(&format!(\"{{}}\", {})));_c.push_str(\"{}", esc_path, s, extra));
    }
}

fn collect_var(chars: &mut Chars) -> String {
    let mut s = String::new();
    let mut c2 = chars.clone();
    while let Some(c) = c2.next() {
        match c {
            ' ' => {}
            '.' => {
                while let Some(d) = chars.next() {
                    s.push(d);
                    if d == '.' {
                        break;
                    }
                }
                if let Some(d) = chars.next() {
                    s.push(d);
                }
                while let Some(d) = chars.next() {
                    s.push(d);
                    if d == ' ' {
                        break;
                    }
                }
                s.push_str(&collect_var(chars));
                break;
            }
            _ => break,
        } 
    }
    s
}
