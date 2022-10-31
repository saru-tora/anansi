---
layout: page
title: Tutorial
permalink: /tutorial/
---

If you don't already have Anansi installed, check out [this page](/install).

To see which version of Anansi you have, run the following command:
```shell
$ ananc --version
```

<br>

Creating a basic site
---------------------

Let's make a simple forum. In a terminal, go to the directory where you want to create the project, and run:

```shell
$ ananc new mini-forum
```

This will create a crate called `mini-forum` with the following files:

- **http_errors/**: Includes http error pages.
- **main.rs**: Has a list of apps.
- **project.rs**: Details project settings.
- **urls.rs**: Manages the routing.

The default database is Sqlite. If you want to use PostgreSQL, in `Cargo.toml`, change `features = ["sqlite"]` to `features = ["postgres"]`. In `src/project.rs`, change `database!(sqlite)` to `database!(postgres)`. Finally, in `settings.toml`, change `[databases.default]` to:

```toml
engine = "postgresql"
name = "mydatabase"
user = "myuser"
password = "mypassword"
address = "127.0.0.1:5432"
```

<br>

Starting the server
===================

To start the web server, go to `mini-forum/` directory and execute:
```shell
$ ananc run
```

Visit [http://127.0.0.1:9090/](http://127.0.0.1:9090/). If everything went well, you should see the default page. You can find the full code for this project [here](https://github.com/saru-tora/mini-forum).

<br>

Creating an app
===============

To create an app, go to `mini-forum/src` and run:

```shell
$ ananc app forum
```

This will make a `forum` directory with the following files:

```
.
├── init.rs
├── migrations
├── mod.rs
├── records.rs
└── urls.rs
```

To include this app, add it to `main.rs`:

```rust
mod forum;

apps! {
    auth,
    sessions,
    forum,
}
```

<br>

Setting up records
------------------

To set up records, edit `forum/records.rs`:

```rust
use anansi::records::{VarChar, DateTime, ForeignKey};
use anansi::util::auth::records::User;

#[record]
#[derive(Relate, FromParams)]
pub struct Topic {
    pub title: VarChar<200>,
    #[field(app = "auth")]
    pub user: ForeignKey<User>,
    pub content: VarChar<40000>,
    pub date: DateTime,
}

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

`#[record]` adds an `id` field by default, and functions that reference the record's fields (like `topic::date`), which can be used with methods like `order_by` to query the database. `Relate` handles access control between records, and `FromParams` will allow you to get a record from a request's parameters.

Both records have `ForeignKey` fields, which means that it has many-to-one relationships with `Topic` and `User`. For `user`, the app name (auth) is specified since it is from another app. Not including the app name will result in an error later on.

<br>

Adding the records
==================

To prepare the migration files (which are used to keep track of the database), run:

```shell
$ ananc make-migrations forum/
```

If you want to view the SQL for this migration, you can run:

```shell
$ ananc sql-migrate forum 0001
```

The output will depend on which database you chose. For PostgreSQL, you should see something like:

```sql
CREATE TABLE "forum_topic" (
	"id" bigint NOT NULL PRIMARY KEY,
	"title" varchar(200) NOT NULL,
	"user" bigint NOT NULL
		REFERENCES "auth_user" ("id")
		ON DELETE CASCADE
		DEFERRABLE INITIALLY DEFERRED,
	"content" varchar(40000) NOT NULL,
	"date" timestamp NOT NULL
);
CREATE INDEX "forum_topic_user_index" ON "forum_topic" ("user");

--snip--

CREATE TABLE "forum_comment" (
	"id" bigint NOT NULL PRIMARY KEY,
	"topic" bigint NOT NULL
		REFERENCES "forum_topic" ("id")
		ON DELETE CASCADE
		DEFERRABLE INITIALLY DEFERRED,
	"user" bigint NOT NULL
		REFERENCES "auth_user" ("id")
		ON DELETE CASCADE
		DEFERRABLE INITIALLY DEFERRED,
	"content" varchar(40000) NOT NULL,
	"date" timestamp NOT NULL
);
CREATE INDEX "forum_comment_topic_index" ON "forum_comment" ("topic");
CREATE INDEX "forum_comment_user_index" ON "forum_comment" ("user");

--snip--
```

To apply the migrations, run:

```shell
$ ananc migrate
```

<br>

Showing the records
===================

You can start creating views by going to `forum/` and running:

```shell
$ ananc make-view topic
```

Now edit `forum/topic/views.rs`:

```rust
use super::super::records::{Topic, topic::date};

#[record_view]
impl<R: Request> TopicView<R> {
    #[view(Group::is_visitor)]
    pub async fn index(req: &mut R) -> Result<Response> {
        let title = "Latest Topics";
        let topics = Topic::order_by(date().desc())
            .limit(25).query(req).await?;
    }
}
```

`Group::is_visitor` will check if the visitor is a visitor, which is always true. Then, it will put `"Latest Topics"` into the `title` variable. The last 25 topics are put into the `topics` variable.

To use these variables, edit `forum/topic/templates/index.rs.html`:

```html
@extend base

@block title {@title}

@block content {
    <h1>@title</h1>
    <ul>
        @for topic in topics {
            <li>@topic.title</li>
        }
    </ul>
}
```

In a template, the `@` symbol allows the use of keywords. `extend` puts the `block`s in the current template into a parent template (in this case, `base.rs.html`). The `@` symbol can also format variables into strings.

To add this view to the routes, change `urls.rs` to the following:

```rust
use crate::forum::{self, topic::views::TopicView};

routes! {
    path!("/", TopicView::index),
    import!("/topic", forum),
}
```

At this point, you can visit the index page you wrote at [http://127.0.0.1:9090/](http://127.0.0.1:9090/), but there won't be much to see since there aren't any topics.

<br>

Creating an admin
-----------------

To create an admin, run:

```shell
$ ananc admin
```

Then enter a username and a strong password.

<br>

Visit the admin site
====================

If the server isn't up, execute:

```shell
$ ananc run
```

Then, in a browser, go to the login page for the admin site (e.g. [http://127.0.0.1:9090/admin/login](http://127.0.0.1:9090/admin/login)), which has the login screen:

![Image](login.png)

<br>

Admin dashboard
===============

If you login to the admin page, you will be greeted by the admin index page:

![Image](admin.png)

<br>

Create a user
=============

Next to "Users", click "Add", then fill the form to create a new user.

![Image](user.png)

<br>

Views
-----

Views handle what will happen when a url is visited.

<br>

Adding more views
=================

Update `forum/records.rs` to add more helper methods.

```rust
use anansi::db::OrderBy;
use anansi::ToUrl;

#[record]
#[derive(Relate, FromParams, ToUrl)]
pub struct Topic {
    pub title: VarChar<200>,
    #[field(app = "auth")]
    pub user: ForeignKey<User>,
    pub content: VarChar<40000>,
    pub date: DateTime,
}

impl Topic {
    pub fn recent_comments(&self) -> OrderBy<Comment> {
        Comment::by_topic(self).order_by(comment::date().desc())
    }
}
```

`ToUrl` will return a shortened version of `id` (e.g. `ixNr1-tGUe9`) by default. `by_topic`, which was added by the `ForeignKey`, filters results by parent.

Now edit `forum/urls.rs`:

```rust
use super::topic::views::TopicView;

routes! {
    // e.g. /topic/ixNr1-tGUe9
    path!("{topic_id}", TopicView::show),
}
```

`{topic_id}` will capture the segment of the url it corresponds to (e.g. `ixNr1-tGUe9`) in a parameter that can be accessed from the request. Since `show` will match any url with the pattern `/topic/SEGMENT`, it should be the last of any views with that pattern.

Update `forum/topic/views.rs`:

```rust
use anansi::get_or_404;
use anansi::humanize::ago;

#[record_view]
impl<R: Request> TopicView<R> {
    // --snip--
    #[view(Group::is_visitor)]
    pub async fn show(req: &mut R) -> Result<Response> {
        let topic = get_or_404!(Topic, req);
        let title = &topic.title;
        let poster = topic.user.get(req).await?.username;
        let comments = topic.recent_comments().limit(25).query(req).await?;
        let users = comments.parents(req, |c| &c.user).await?;
    }
}
```

In `show`, `get_or_404!` retrieves a specific topic by `topic_id` or returns a 404 error. `parents` queries the users associated with the comments.

Now let's add the template for this view in `forum/topic/templates/show.rs.html`:

```html
@extend base

@block title {@title}

@block content {
    <h1>@title</h1>
    <p><small>Posted by @poster @ago(topic.date)</small></p>
    <p>@topic.content</p>
    @for (comment, user) in comments.iter().zip(users.iter()) {
        <p><small>Posted by @user.username @ago(comment.date)</small></p>
        <p>@comment.content</p>
    }
}
```

We can also go back to `forum/topic/templates/index.rs` and link each topic to its page.

```html
@block content {
    <h1>@title</h1>
    <ul>
        @for topic in topics {
            <li>@link req, Self::show, topic {@topic.title}</li>
        }
    </ul>
}
```

`link` will create a link to a view (in this case, `show`), which will expand to something like:

```html
<a href="topic/@topic.to_url()">@topic.title</a>
```

<br>

Logging in
----------

To log in, we can reuse the admin page's user login form in `forum/topic/views.rs` (of course, you can write your own if you want to):

```rust
use anansi::handle;
use anansi::forms::ToRecord;
use anansi::util::auth::forms::UserLogin;

#[record_view]
impl<R: Request> TopicView<R> {
    // --snip--
    #[view(Group::is_visitor)]
    pub async fn login(req: &mut R) -> Result<Response> {
        let title = "Log in";
        let button = "Log in";
        let form = handle!(UserLogin, ToRecord<R>, req, user, {
            req.auth(&user).await?;
    	    req.session().set_and_redirect(req, Self::index)
        })?;
    }
}
```

`handle!` creates a new form if the request method is GET. Otherwise, it tries to do something with the submitted form (in this case, log in the user), and if that fails, gives back the form. The form can be used in `forum/topic/templates/login.rs.html`:

```html
@extend base

@block title {@title}

@block content {
    <h1>@title</h1>
    <div>
    	@build form {
	    @unescape form.errors()
    	    @for field in form.fields() {
    	        @unescape field.label_tag()
    	        <div>
		    @unescape field
		    @unescape field.errors()
    	        </div>
    	    }
	    @unescape form.submit(button)
    	}
    </div>
}
```

Using the template system to `build` forms should be safer and less tedious than writing pure HTML. Variables containing HTML tags must be `unescape`d to be processed properly. The `errors` method will display an unordered list of errors. Form errors will look something like:

```html
<ul class="form-errors">
    <li>Problem with username or password.</li>
</ul>
```

Errors in fields will have the class `field-errors` instead.

To include everything, update `urls.rs`:

```rust
routes! {
    path!("/", TopicView::index),
    import!("/topic", forum),
    path!("/login", TopicView::login),
}
```

Now you can log in with the user you created in the admin page.

<br>

Creating
--------

To add topics, we can start by creating a form in the file `forum/forms.rs`:

```rust
use crate::prelude::*;
use anansi::records::{DateTime, ForeignKey};
use anansi::forms::{VarChar, ToRecord};
use super::records::Topic;

#[form(Topic)]
pub struct TopicForm {
    pub title: VarChar<200>,
    pub content: VarChar<40000>,
}

#[async_trait]
impl<R: Request> ToRecord<R> for TopicForm {
    async fn on_post(&mut self, data: TopicFormData, req: &R) -> Result<Topic> {
        let now = DateTime::now();
        let user_fk = ForeignKey::from_data(req.user().pk())?;
        Topic::new(data.title, user_fk, data.content, now).save(req).await
            .or(form_error!("Problem adding topic"))
    }
}
```

`#[form(Topic)]` generates a `TopicFormData` struct, which holds the data for the form, and associates the form with `Topic`. `on_post` will try to convert the form to a record. `form_error` will simply create a `FormError`, which can be accessed with the form's `errors` method.

Update `forum/mod.rs`:

```rust
pub mod forms;
```

Now use it in `forum/topic/views.rs`:

```rust
use anansi::{check, render};
use crate::forum::forms::TopicForm;

#[record_view]
impl<R: Request> TopicView<R> {
    // --snip--
    #[check(Group::is_auth)]
    pub async fn new(req: &mut R) -> Result<Response> {
        let title = "New Topic";
        let button = "Create";
        let form = handle!(TopicForm, ToRecord<R>, req, |topic| {
    	    Ok(redirect!(req, Self::show, topic))
        })?;
        render!("login")
    }
}
```

`Group::is_auth` will redirect the visitor if they aren't authenticated. This time, for `handle`, a closure is passed this time since redirection isn't async. For the template, in this simple case, you can just reuse `login.rs.html` by using the `check` and `render` macros, though for an actual site, you'd probably write a custom one. You can also have a link to the page added in `index.rs.html` if the user is authenticated:

```html
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

To bring it all together, update `forum/urls.rs`:

```rust
routes! {
    path!("new", TopicView::new),
    path!("{topic_id}", TopicView::show),
}
```

Well, that was a lot of code, but now you can finally create a topic!

Updating
--------

To edit topics, we can first add some traits to `forum/forms.rs`:

```rust
use anansi::{GetData, ToEdit};

#[form(Topic)]
#[derive(GetData, ToEdit)]
pub struct TopicForm {
    pub title: VarChar<200>,
    pub content: VarChar<40000>,
}
```

And add a view to `forum/topic/views.rs`:

```rust
use anansi::handle_or_404;
use anansi::forms::ToEdit;

#[record_view]
impl<R: Request> TopicView<R> {
    // --snip--
    #[check(Topic::owner)]
    pub async fn edit(req: &mut R) -> Result<Response> {
        let title = "Update Topic";
        let button = "Update";
        let form = handle_or_404!(TopicForm, ToEdit<R>, req, |topic| {
    	    Ok(redirect!(req, Self::show, topic))
        })?;
        render!("login")
    }
}
```

`Topic::owner` checks if the user owns the topic. `handle_or_404!` is like `handle!`, but returns a 404 error if the record can't be found. Like before, for the template, you can just reuse `login.rs.html`. You can also have the link for it added in `show.rs.html` if the topic is the user's:

```html
@block content {
    <h1>@title</h1>
    <p><small>Posted by @poster @ago(topic.date)</small></p>
    <p>@topic.content</p>
    @if topic.user.pk() == req.user().pk() {
        @link req, Self::edit, topic {Edit}
    }
    @for (comment, user) in comments.iter().zip(users.iter()) {
        <p><small>Posted by @user.username @ago(comment.created)</small></p>
        <p>@comment.content</p>
    }
}
```

As always, update `forum/urls.rs`:

```rust
routes! {
    path!("new", TopicView::new),
    path!("{topic_id}", TopicView::show),
    path!("{topic_id}/edit", TopicView::edit),
}
```

Admin
-----

At this point, you can add the `Topic` record to the admin page. First, create `forum/admin.rs`:

```rust
use anansi::{init_admin, register, record_admin};
use super::records::Topic;

init_admin! {
    register!(Topic),
}

record_admin! {Topic,
    // You can specify which fields (if any) should be searchable
    search_fields: [title, content, date],
}
```

Add it to `forum/mod.rs`:

```rust
pub mod admin;
```

Finally, add the app to `main.rs`:

```rust
app_admins! {
    auth,
    forum,
}
```

`Topic` should show up on the admin page now.

![Image](admin2.png)

<br>

Deleting
--------

To delete topics edit `forum/topic/views.rs`:

```rust
#[record_view]
impl<R: Request> TopicView<R> {
    // --snip--
    #[view(Topic::owner)]
    pub async fn destroy(req: &mut R) -> Result<Response> {
        let title = "Delete topic";
        let topic = get_or_404!(Topic, req);
        let form = handle!(req, R, {
            topic.delete(req).await?;
            Ok(redirect!(req, Self::index))
        })?;
    }
}
```

`forum/topic/templates/destroy.rs.html` is relatively simple:

```html
@extend base

@block title {@title}

@block content {
    <h1>@title</h1>
    Are you sure you want to delete the topic "@topic.title"?
    @build form {
        @unescape form.submit("Confirm")
    }
}
```

Again, you can add the link for it in `forum/topic/templates/show.rs.html`:

```html
@block content {
    <h1>@title</h1>
    <p><small>Posted by @poster @ago(topic.date)</small></p>
    <p>@topic.content</p>
    @if topic.user.pk() == req.user().pk() {
        @link req, Self::edit, topic {Edit}
        @link req, Self::destroy, topic {Delete}
    }
    @for (comment, user) in comments.iter().zip(users.iter()) {
        <p><small>Posted by @user.username @ago(comment.created)</small></p>
        <p>@comment.content</p>
    }
}
```

And finally, update `forum/urls.rs`:

```rust
routes! {
    path!("new", TopicView::new),
    path!("{topic_id}", TopicView::show),
    path!("{topic_id}/edit", TopicView::edit),
    path!("{topic_id}/destroy", TopicView::destroy),
}
```

There are more features you can add, but hopefully, this tutorial is enough for you to get started.
