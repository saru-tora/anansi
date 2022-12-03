---
# Feel free to add content and custom Front Matter to this file.
# To modify the layout, see https://jekyllrb.com/docs/themes/#overriding-theme-defaults

layout: page
title: Overview
---

Anansi is a simple MVC web framework for Rust. [Get started](/anansi/start).

🛡️ Safety first
---------------

In addition to being written in Rust, Anansi provides defenses for common web security vulnerabilities.

⚙️  Performant
-------------

Anansi also allows web applications to run asynchronously with Rust's speed.

✨ Easy to get started
----------------------

Anansi handles many of the repetitive parts of web development, letting you work on the important parts of your app more quickly.

Records
=======

Work with databases in Rust instead of SQL.

```rust
// A topic in a forum.
#[record]
#[derive(Relate, FromParams, ToUrl)]
pub struct Topic {
    pub title: VarChar<200>,
    #[field(app = "auth")]
    pub user: ForeignKey<User>,
    pub content: VarChar<40000>,
    pub date: DateTime,
}

// A comment in a topic.
#[record]
#[derive(Relate, FromParams)]
pub struct Comment {
    pub topic: ForeignKey<Topic>,
    #[field(app = "auth")]
    pub user: ForeignKey<User>,
    pub content: VarChar<40000>,
    pub date: DateTime,
}
```

Views
=====

Mapping requests to views is simple.

```rust
routes! {
    path!("", TopicView::index),
    path!("new", TopicView::new),
    path!("{topic_id}", TopicView::show),
}
```

```rust
#[record_view]
impl<R: Request> TopicView<R> {
    // A view of the last 25 topics.
    #[view(Group::is_visitor)]
    pub async fn index(req: &mut R) -> Result<Response> {
        let title = "Latest Topics";
        let topics = Topic::order_by(date().desc())
    	    .limit(25).query(req).await?;
    }
}
```

Templates
=========

Templates allow you to mix Rust with HTML for formatting.

```html
@block title {@title}

@block content {
    <h1>@title</h1>
    @if req.user().is_auth() {
        @link req, Self::new {New Topic}
    }
    <ul>
        @for topic in topics {
    	    <li>@link req, Self::show, topic {@topic.title}</li>
        }
    </ul>
}
```

Components
==========

Reactivity can be added with WebAssembly.

```rust
#[derive(Properties, Serialize, Deserialize)]
pub struct LoaderProps {
    pub load_url: String,
    pub show_url: String,
}

#[derive(Serialize, Deserialize)]
pub struct Data {
    pub id: String,
    pub title: String,
}

#[store]
#[derive(Serialize, Deserialize)]
pub struct Loader {
    visible: bool,
    page: u32,
    fetched: Vec<Data>,
    status: String,
}

#[component(Loader)]
fn init(props: LoaderProps) -> Rsx {
    let state = Self::store(true, 0, vec![], String::new());

    let handle_click = async_callback! {
        let resp = Request::get(&props.load_url)
            .query([("page", state.page.to_string())])
            .send().await;
        let json = match resp {
            Ok(r) => r.json::<Vec<Data>>().await,
            Err(_) => return state.status = "Problem getting topics".to_string(),
        };
        match json {
            Ok(mut f) => {
                if f.len() < 25 || state.page >= 2 {
                    state.visible = false;
                } else {
                    state.page += 1;
                }
                state.fetched.append(&mut f);
                state.status = "".to_string();
            }
            Err(_) => state.status = "Problem loading topics".to_string(),
        }
    };

    rsx! {
        @for data in &state.fetched {
            <li>@href props.show_url, data.id {@data.title}</li>
        }
        @if !state.status.is_empty() {
            <div>@state.status</div>
        }
        @if state.visible {
            <button @onclick=handle_click>Load more</button>
        }
    }
}
```
