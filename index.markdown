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

Models
======

Work with databases in Rust instead of SQL.

```rust
// A topic in a forum.
#[model]
#[derive(Relate, FromParams, ToUrl)]
pub struct Topic {
    pub title: VarChar<200>,
    pub user: ForeignKey<auth::models::User>,
    pub content: VarChar<40000>,
    pub date: DateTime,
}

// A comment in a topic.
#[model]
#[derive(Relate, FromParams)]
pub struct Comment {
    pub topic: ForeignKey<Topic>,
    pub user: ForeignKey<auth::models::User>,
    pub content: VarChar<40000>,
    pub date: DateTime,
}
```

Views
=====

Mapping requests to views is simple.

```rust
routes! {
    path("", TopicView::index),
    path("new", TopicView::new),
    path("{topic_id}", TopicView::show),
}
```

```rust
#[checker]
impl<R: Request> TopicView<R> {
    // A view of the last 25 topics.
    #[check(if_guest)]
    pub async fn index(req: R) -> Result<Response> {
        let title = "Latest Topics";
        let topics = Topic::order_by(date().desc())
    	    .limit(25).query(&req).await?;
    }
}
```

Templates
=========

Templates allow you to mix Rust with HTML for formatting.

```html
@extend base

@block title {@title}

@block content {
    <h1>@title</h1>
    <ul>
        @for topic in topics {
    	    <li>@link req, Self::show, topic {@topic.title}</li>
        }
    </ul>
    @if req.user().is_auth() {
        @link req, Self::new {New Topic}
    }
}
```

Forms
=====

Forms can be created without much effort.

```rust
#[form(Topic)]
pub struct TopicForm {
    pub title: VarChar<200>,
    pub content: VarChar<40000>,
}
```
