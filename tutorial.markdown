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

Let's create a simple forum. The full  In a terminal, go to the directory where you want to create the project, and run:

```shell
$ ananc new mini-forum
```

This will create the following a crate called `mini-forum` with the following files:

- **http_errors/**: Includes http error pages.
- **main.rs**: Has a list of apps.
- **settings.rs**: Details project settings.
- **urls.rs**: Manages the routing.

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
├── base.rs.html
├── init.rs
├── migrations
├── models.rs
├── mod.rs
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

Generating models
-----------------

To generate models, edit `forum/models.rs`:

```rust
use anansi::models::{VarChar, DateTime, ForeignKey};
use anansi::util::auth;

#[model]
pub struct Topic {
    pub title: VarChar<200>,
    pub date: DateTime,
}

#[model]
pub struct Comment {
    pub topic: ForeignKey<Topic>,
    pub user: ForeignKey<auth::models::User>,
    pub content: VarChar<40000>,
    pub created: DateTime,
}
```

`#[model]` adds an `id` field by default, and functions that reference the model's fields (like `topic::date`), which can be used with methods like `order_by` to query the database.

`Comment` has `ForeignKey` fields, which means that it has many-to-one relationships with `Topic` and `User`. For `user`, the app name (auth) is specified since it is from another app. Not including the app name will result in an error later on.

<br>

Adding the models
=================

To prepare the migration files (which are used to keep track of the database), run:

```shell
$ ananc make-migrations forum/
```

If you want to view the SQL for this migration, you can run:

```shell
$ ananc sql-migrate forum 0001
```

You should see something like:

```sql
BEGIN;

CREATE TABLE "forum_topic" (
	"id" bigint NOT NULL PRIMARY KEY,
	"title" varchar(200) NOT NULL,
	"date" datetime NOT NULL
);

CREATE TABLE "forum_comment" (
	"id" bigint NOT NULL PRIMARY KEY,
	"topic" bigint NOT NULL,
	"user" bigint NOT NULL,
	"content" varchar(40000) NOT NULL,
	"created" datetime NOT NULL,
	FOREIGN KEY ("topic")
	REFERENCES "forum_topic" ("id")
	ON DELETE CASCADE,
	FOREIGN KEY ("user")
	REFERENCES "auth_user" ("id")
	ON DELETE CASCADE
);

COMMIT;
```

To apply the migrations, run:

```shell
$ ananc migrate
```

<br>

Showing the models
==================

You can start creating views by going to `forum/` and running:

```shell
$ ananc make-view topic
```

Now edit `forum/topic/views.rs`:

```rust
use super::super::models::{Topic, topic::date};

#[viewer]
impl<R: Request> TopicView<R> {
    #[view(if_guest)]
    pub async fn index(req: R) -> Result<Response> {
        let title = "Latest Topics";
        let topics = Topic::order_by(date().desc())
            .limit(25).query(&req).await?;
    }
}
```

`if_guest` will check if the visitor is at least a guest, which is always true. Then, it will put `"Latest Topics"` into the `title` variable. The last 25 topics are put into the `topics` variable.

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
    path("/", TopicView::index),
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

Update `forum/models.rs` to add more helper methods.

```rust
use anansi::db::OrderBy;
use anansi::{ToUrl, FromParams};

#[model]
#[derive(ToUrl, FromParams)]
pub struct Topic {
    pub title: VarChar<200>,
    pub date: DateTime,
}

impl Topic {
    pub fn recent_comments(&self) -> OrderBy<Comment> {
        Comment::by_topic(self).order_by(comment::created().desc())
    }
}
```

`ToUrl` will return a shortened version of `id` (e.g. `ixNr1-tGUe9`) by default. `by_topic`, which was added by the `ForeignKey`, filters results by parent.

Now edit `forum/urls.rs`:

```rust
use super::topic::views::TopicView;

routes! {
    // e.g. /topic/ixNr1-tGUe9
    path("{topic_id}", TopicView::show),
}
```

`{topic_id}` will capture the segment of the url it corresponds to (e.g. `ixNr1-tGUe9`) in a parameter that can be accessed from the request. Since `show` will match any url with the pattern `/topic/SEGMENT`, it should be the last of any views with that pattern.

Update `forum/topic/views.rs`:

```rust
use anansi::get_or_404;
use anansi::humanize::ago;

#[viewer]
impl<R: Request> TopicView<R> {
    // --snip--
    #[view(if_guest)]
    pub async fn show(req: R) -> Result<Response> {
        let topic = get_or_404!(Topic, req);
        let title = &topic.title;
        let comments = topic.recent_comments().limit(25).query(&req).await?;
        let users = comments.parents(|c| &c.user).query(&req).await?;
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
    @for (comment, user) in comments.iter().zip(users.iter()) {
        <p><small>Posted by @user.username @ago(comment.created)</small></p>
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
use anansi::forms::ToModel;
use anansi::util::auth::forms::UserLogin;

#[viewer]
impl<R: Request> TopicView {
    // --snip--
    #[view(if_guest)]
    pub async fn login(mut req: R) -> Result<Response> {
        let title = "Log in";
        let button = "Log in";
        let form = handle!(UserLogin, ToModel<R>, req, user, {
            req.auth(&user).await?;
    	    req.session().set_and_redirect(&req, Self::index)
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
    path("/", TopicView::index),
    import!("/topic", forum),
    path("/login", TopicView::login),
}
```

Now you can log in with the user you created in the admin page.

<br>

Creating
--------

To add topics, we can start by creating a form in the file `forum/forms.rs`:

```rust
use crate::prelude::*;
use anansi::models::{DateTime, ForeignKey};
use anansi::forms::{VarChar, ToModel};
use super::models::{Topic, Comment};

#[form(Topic)]
pub struct TopicForm {
    pub title: VarChar<200>,
    pub content: VarChar<40000>,
}

#[async_trait]
impl<R: Request> ToModel<R> for TopicForm {
    async fn on_post(&mut self, data: TopicFormData, req: &R) -> Result<Topic> {
        let now = DateTime::now();
        transact!(req, {
            let topic = Topic::new(data.title, now).save(req).await
                .or(form_error!("Problem adding topic"))?;
            let topic_fk = ForeignKey::new(&topic);
            let user_fk = ForeignKey::new(req.user());
            Comment::new(topic_fk, user_fk, data.content, now).save(req).await
                .or(form_error!("Problem adding comment"))?;
            Ok(topic)
        })
    }
}
```

`#[form(Topic)]` generates a `TopicFormData` struct, which holds the data for the form, and associates the form with `Topic`. `on_post` will try to convert the form to a model. `transact` will only commit changes if an `Ok` is returned. `form_error` will simply create a `FormError`, which can be accessed with the form's `errors` method.

Update `forum/mod.rs`:

```rust
pub mod forms;
```

Now use it in `forum/topic/views.rs`:

```rust
use anansi::checker;
use crate::forum::forms::TopicForm;

checker!(if_auth<R: Request>, |req| req.check_auth(),
    redirect!(req, TopicView::login)
);

#[viewer]
impl<R: Request> TopicView<R> {
    // --snip--
    #[view(if_auth)]
    pub async fn new(mut req: R) -> Result<Response> {
        let title = "New Topic";
        let button = "Create";
        let form = handle!(TopicForm, ToModel<R>, req, |topic| {
    	    Ok(redirect!(req, Self::show, topic))
        })?;
    }
}
```

`checker!` creates an `if_auth` function, which redirects the visitor if they aren't authenticated. This time, for `handle`, a closure is passed this time since redirection isn't async. For the template `forum/topic/templates/new.rs.html`, in this simple case, you can just copy `login.rs.html`, though for an actual site, you'd probably write a custom one. You can also have a link to the page added in `index.rs.html` if the user is authenticated:

```html
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

To bring it all together, update `forum/urls.rs`:

```rust
routes! {
    path("new", TopicView::new),
    path("{topic_id}", TopicView::show),
}
```

Well, that was a lot of code, but now you can finally create a topic!

Updating
--------

To edit topics, we can first add another method to `forum/models.rs`:

```rust
use anansi::web::Result;
use crate::project::Request;

impl Topic {
    // --snip--
    pub async fn first_post<R: Request>(req: &R) -> Result<(Self, Comment)> {
        let topic: Self = req.to_model().await?;
        let comment = topic.recent_comments().get(req).await?;
        Ok((topic, comment))
    }
}
```

Then add some traits to `forum/forms.rs`:

```rust
use anansi::web::FormMap;
use anansi::forms::{GetData, ToEdit};

#[async_trait]
impl<R: Request> GetData<R> for TopicForm {
    fn from_map(form_map: FormMap) -> Result<TopicFormData> {
        let title = form_map.get("title")?.parse()?;
        let content = form_map.get("content")?.parse()?;
        Ok(TopicFormData::new(title, content))
    }
    async fn from_model(topic: Topic, req: &R) -> Result<TopicFormData> {
        let comment = topic.recent_comments().get(req).await?;
        Ok(TopicFormData::new(topic.title, comment.content))
    }
}

#[async_trait]
impl<R: Request> ToEdit<R> for TopicForm {
    async fn on_post(&mut self, data: TopicFormData, req: &R) -> Result<Topic> {
        let (mut topic, mut comment) = Topic::first_post(req).await?;
        topic.title = data.title;
        comment.content = data.content;
        transact!(req, {
            topic.update(req).await?;
            comment.update(req).await?;
            Ok(topic)
        })
    }
}
```

And add a view to `forum/topic/views.rs`:

```rust
use anansi::handle_or_404;
use anansi::forms::ToEdit;

impl<R: Request> TopicView<R> {
    // --snip--
    #[view(if_auth)]
    pub async fn edit(mut req: R) -> Result<Response> {
        let title = "Update Topic";
        let button = "Update";
        let form = handle_or_404!(TopicForm, ToEdit<R>, req, |topic| {
    	    Ok(redirect!(req, Self::show, topic))
        })?;
    }
}
```

`handle_or_404!` is like `handle!`, but returns a 404 error if the model can't be found. Like before, for `edit.rs.html`, you can just copy `login.rs.html`. You can also have the link for it added in `show.rs.html` if the first post is the user's:

```html
@block content {
    <h1>@title</h1>
    @for (comment, user) in comments.iter().zip(users.iter()) {
        <p><small>Posted by @user.username @ago(comment.created)</small></p>
        <p>@comment.content</p>

        @if comment.user.eq_model(req.user()) {
            @if comment.pk() == comments[0].pk() {
                @link req, Self::edit, topic {Edit}
            }
        }
    }
}
```

As always, update `forum/urls.rs`:

```rust
routes! {
    path("new", TopicView::new),
    path("{topic_id}", TopicView::show),
    path("{topic_id}/edit", TopicView::edit),
}
```

Admin
-----

At this point, you can add the `Topic` model to the admin page. First, create `forum/admin.rs`:

```rust
use anansi::{init_admin, register, model_admin};
use super::models::Topic;

init_admin! {
    register!(Topic),
}

model_admin!(Topic {});
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

To delete topics, edit `forum/topic/views.rs`:

```rust
#[viewer]
impl<R: Request> TopicView<R> {
    // --snip--
    #[view(if_auth)]
    pub async fn destroy(mut req: R) -> Result<Response> {
        let title = "Delete topic";
        let topic = get_or_404!(Topic, req);
        let form = handle!(req, R, {
            topic.delete(&req).await?;
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
    @for (comment, user) in comments.iter().zip(users.iter()) {
        <p><small>Posted by @user.username @ago(comment.created)</small></p>
        <p>@comment.content</p>

        @if comment.user.eq_model(req.user()) {
            @if comment.pk() == comments[0].pk() {
                @link req, Self::edit, topic {Edit}
                @link req, Self::destroy, topic {Delete}
            }
        }
    }
}
```

And finally, update `forum/urls.rs`:

```rust
routes! {
    path("new", TopicView::new),
    path("{topic_id}", TopicView::show),
    path("{topic_id}/edit", TopicView::edit),
    path("{topic_id}/destroy", TopicView::destroy),
}
```

There are more features you can add, but hopefully, this tutorial is enough for you to get started.
