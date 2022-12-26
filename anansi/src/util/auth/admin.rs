use std::str::Chars;
use std::fmt::Debug;
use anansi::records::{Record, ToUrl, FromParams, VarChar};
use anansi::admin_site::{AdminRef};
use anansi::db::{invalid, WhoseArg, Builder};
use anansi::web::{Result, Method, Response, Reverse, BaseUser, BaseRequest, CsrfDefense, GetRecord};
use crate::util::auth;
use anansi::forms::{Form, Field, ToRecord, HasRecord, ToEdit};
use anansi::{extend, render, redirect, handle, base_view, handle_or_404};
use super::forms::{UserLogin, AdminSearch, FilterForm};
use anansi::{check, viewer, record_admin};
use crate::register;
use super::middleware::Auth;
use super::super::sessions::middleware::Sessions;
use super::super::admin::site::{HasAdmin, BasicAdminSite};
use super::records::{User, Group, Filter, filter};
use crate::util::admin::site::RecordAdmin;

pub trait Request: BaseRequest + Auth + HasAdmin + Reverse + CsrfDefense + Sessions + GetRecord + Debug {}

#[base_view]
fn base<R: Request>(req: &mut R) -> Result<Response> {}

record_admin! {auth::records::User,
    form: auth::forms::UserForm,
    add_form: auth::forms::UserNew,
    fields: [username],
    search_fields: [username],
}

record_admin! {auth::records::Group,
    form: auth::forms::GroupForm,
    add_form: auth::forms::GroupForm,
    fields: [groupname],
}

record_admin! {auth::records::Filter,
    form: auth::forms::FilterForm,
    add_form: auth::forms::FilterForm,
    fields: [filter_name, filter],
    search_fields: [filter_name],
}

pub fn initialize_admin<R: Request>(site: AdminRef<R>) {
    let mut site = site.lock().unwrap();
    register!(User);
    register!(Group);
    site.urls_mut().push(("/admin/filter", anansi::util::auth::admin::AuthAdminView::record_index::<Filter>));
    site.urls_mut().push(("/admin/filter/edit", anansi::util::auth::admin::AuthAdminView::record_edit::<Filter>));
    site.urls_mut().push(("/admin/filter/new", anansi::util::auth::admin::AuthAdminView::filter_new::<Filter>));
    site.urls_mut().push(("/admin/filter/search", anansi::util::auth::admin::AuthAdminView::record_search::<Filter>));
}

#[viewer]
impl<R: Request> AuthAdminView<R> {
    #[check(Group::is_visitor)]
    pub async fn login(req: &mut R) -> Result<Response> {
        let form = handle!(UserLogin, ToRecord<R>, req, user, {
            req.auth_admin(&user).await?;
            req.session().set_and_redirect(req, BasicAdminSite::index)
        })?.class("cred");
        render!("login")
    }

    #[check(Group::is_admin)]
    pub async fn logout(req: &mut R) -> Result<Response> {
        let title = "Log out";
        let form = handle!(req, R, {
            req.session().delete(req).await?;
            Ok(redirect!())
        })?;
        extend!(req, base, "logout")
    }

    #[check(Group::is_admin)]
    pub async fn record_index<M: RecordAdmin<R> + Send + ToUrl + 'static>(req: &mut R) -> Result<Response>
where <<M as RecordAdmin<R>>::AdminForm as HasRecord>::Item: FromParams, <M as Record>::Pk: std::fmt::Display
    {
        let title = M::NAME;
        let records = Some(M::limit(100).query(req).await?);
        let field_names = <M as RecordAdmin<R>>::field_names();
        let m_edit = Self::record_edit::<M>;
        let filters = Filter::whose(filter::table_name().eq(M::table_name())).limit(25).query(req).await?;
        let search = if M::searchable() {
            Some(AdminSearch::new().action(req, Self::record_search::<M>))
        } else {
            None
        };
        extend!(req, base, "record_index")
    }

    #[check(Group::is_admin)]
    pub async fn record_search<M: RecordAdmin<R> + Send + ToUrl + 'static>(req: &mut R) -> Result<Response>
where <<M as RecordAdmin<R>>::AdminForm as HasRecord>::Item: FromParams, <M as Record>::Pk: std::fmt::Display
    {
        let title = M::NAME;
        let mut search = if let Ok(f) = AdminSearch::from_get(req) {
            f
        } else {
            AdminSearch::new()
        };
        let records = if let Ok(data) = search.validate() {
            let q = match data.q {
                Some(ref q) => q.clone(),
                None => VarChar::new(),
            };
            let terms: Vec<&str> = q.split(' ').collect();
            let terms = terms.iter().map(|s| s.to_string()).collect();
            search.set_data(Some(data));
            search.fill()?;
            let mut q = M::search(&terms);
            for (key, _) in req.params().iter() {
                if let Ok(f) = Filter::whose(filter::filter_name().eq(key as &str)).get(req).await {
                    q = q.and(WhoseArg::from(Builder::new().push_str(&format!("({})", f.raw_query))));
                }
            }
            Some(q.limit(25).query(req).await?)
        } else {
            None
        };
        let field_names = <M as RecordAdmin<R>>::field_names();
        let m_edit = Self::record_edit::<M>;
        let filters = Filter::whose(filter::table_name().eq(M::table_name())).limit(25).query(req).await?;
        let search = Some(search.action(req, Self::record_search::<M>));
        extend!(req, base, "record_index")
    }

    #[check(Group::is_admin)]
    pub async fn filter_new<M: RecordAdmin<R> + 'static>(req: &mut R) -> Result<Response>
where <<M as RecordAdmin<R>>::AdminForm as HasRecord>::Item: FromParams
    {
        let title = "Add filter".to_string();
        if *req.method() == Method::POST {
            req.params_mut().insert("table_name".to_string(), M::table_name().to_string());
            let checker = FilterChecker::new(M::field_names());
            let form_map = req.to_form_map()?;
            let filter = form_map.get("filter")?;
            match checker.check_filter(filter) {
                Ok(q) => {
                    req.params_mut().insert("raw_query".to_string(), q)
                },
                Err(_) => {
                    req.params_mut().remove("filter")?;
                },
            }
        }
        let mut form = handle!(FilterForm, ToRecord<R>, req, || Ok(redirect!(req, BasicAdminSite::index)))?;
        form.filter.mut_widget().mut_attrs().insert("placeholder", "e.g. created > &quot;2022-02-22&quot;");
        let button = "Create";
        extend!(req, base, "record_new")
    }

    #[check(Group::is_admin)]
    pub async fn record_new<M: RecordAdmin<R> + Send + Sync + anansi::records::ToUrl + 'static>(req: &mut R) -> Result<Response>
where <<M as RecordAdmin<R>>::AdminForm as HasRecord>::Item: FromParams, <M as anansi_core::records::Record>::Pk: std::fmt::Display
    {
        let title = format!("Add {}", M::NAME);
        let form = handle!(<M as RecordAdmin<R>>::AddForm, ToRecord<R>, req, || Ok(redirect!(req, Self::record_index::<M>)))?;
        let button = "Create";
        extend!(req, base, "record_new")
    }

    #[check(Group::is_admin)]
    pub async fn record_edit<M: RecordAdmin<R> + 'static>(req: &mut R) -> Result<Response>
where <<M as RecordAdmin<R>>::AdminForm as HasRecord>::Item: FromParams
    {
        let title = format!("Edit {}", M::NAME);
        let form = handle_or_404!(<M as RecordAdmin<R>>::AdminForm, ToEdit<R>, req, || Ok(redirect!(req, BasicAdminSite::index)))?;
        let button = "Edit";
        extend!(req, base, "record_new")
    }
}

struct FilterChecker<'a> {
    fields: &'a [&'static str],
    query: String,
    depth: usize,
    left: String,
    op: String,
    right: String,
    last: Option<char>,
    logic: bool,
}

impl<'a> FilterChecker<'a> {
    fn new(fields: &'a [&'static str]) -> Self {
        Self {fields, query: String::new(), depth: 0, left: String::new(), op: String::new(), right: String::new(), last: None, logic: false}
    }
    fn check_filter(mut self, filter: &str) -> Result<String> {
        let mut f = filter.chars();
        let mut c = if let Some(d) = f.next()  {
            d
        } else {
            return Err(invalid());
        };
        loop {
            match c {
                '(' => self.depth += 1,
                ' ' => {},
                _ => if c.is_alphanumeric() {
                    self.lhs(c, &mut f)?;
                    if let Some(d) = self.last {
                        c = d;
                        continue;
                    } else {
                        break;
                    };
                } else {
                    return Err(invalid());
                },
            } 
            c = if let Some(d) = f.next() {
                d
            } else {
                break;
            };
        }
        if self.depth == 0 && !self.logic {
            Ok(self.query)
        } else {
            Err(invalid())
        }
    }
    fn lhs(&mut self, c: char, f: &mut Chars) -> Result<()> {
        let mut s = c.to_string();
        while let Some(mut d) = f.next() {
            if d.is_alphanumeric() {
                s.push(d);
            } else {
                return if self.fields.iter().find(|&&field| field == s).is_some() {
                    self.left = s;
                    if d == ' ' {
                        while let Some(e) = f.next() {
                            if e != ' ' {
                                d = e;
                                break;
                            }
                        }
                    }
                    self.operator(d, f)
                } else {
                    Err(invalid())
                };
            }
        }
        Err(invalid())
    }
    fn operator(&mut self, c: char, f: &mut Chars) -> Result<()> {
        if let Some(d) = f.next() {
            if ['=', '!', '>', '<', '~'].iter().find(|&&t| t == c).is_none() {
                return Err(invalid());
            }
            return if (d.is_alphanumeric() || d == ' ') && c != '!' {
                self.op = c.to_string();
                self.rhs(d, f)
            } else {
                let s = format!("{}{}", c, d);
                return if ["!=", ">=", "<=", "!~"].iter().find(|&&t| t == s).is_some() {
                    if let Some(g) = f.next() {
                        self.op = s;
                        self.rhs(g, f)
                    } else {
                        Err(invalid())
                    }
                } else {
                    Err(invalid())
                };
            }
        }
        Err(invalid())
    }
    fn rhs(&mut self, mut c: char, f: &mut Chars) -> Result<()> {
        if c == ' ' {
            while let Some(d) = f.next() {
                if d == ' ' {
                    continue;
                } else {
                    c = d;
                    break;
                }
            }
        }
        let quote = if c == '\'' || c == '"' {
            Some(c)
        } else {
            None
        };
        let mut s = c.to_string();
        while let Some(d) = f.next() {
            if let Some(q) = quote {
                if d != q {
                    self.right.push(d);
                } else {
                    let t = match self.op.as_str() {
                        "~" => {
                            let u = if self.right.contains("%") {
                                self.right.clone()
                            } else {
                                format!("%{}%", self.right)
                            };
                            format!("LOWER({}) LIKE LOWER({q}{}{q})", self.left, u)
                        },
                        "!~" => {
                            let u = if self.right.contains("%") {
                                self.right.clone()
                            } else {
                                format!("%{}%", self.right)
                            };
                            format!("LOWER({}) NOT LIKE LOWER({q}{}{q})", self.left, u)
                        },
                        _ => {
                            format!("{} {} {q}{}{q}", self.left, self.op, self.right)
                        },
                    };
                    self.query.push_str(&t);
                    return if let Some(e) = f.next() {
                        self.logic(e, f)
                    } else {
                        Ok(())
                    };
                }
            } else {
                if d.is_alphanumeric() {
                    s.push(d);
                } else {
                    if s.contains(char::is_alphabetic) {
                        if self.fields.iter().find(|&&t| t == s).is_some() {
                            self.query.push_str(&format!("{} {} {}", self.left, self.op, self.right));
                        } else if s == "true" {
                            self.query.push_str(&format!("{} {} 1", self.left, self.op));
                        } else if s == "false" {
                            self.query.push_str(&format!("{} {} 0", self.left, self.op));
                        } else {
                            return Err(invalid());
                        }
                    } else {
                        for e in s.chars() {
                            if !e.is_ascii_digit() {
                                return Err(invalid());
                            }
                        }
                        self.query.push_str(&format!("{} {} {}", self.left, self.op, self.right));
                    }
                    return self.logic(d, f);
                }
            }
        }
        Ok(())
    }
    fn logic(&mut self, mut c: char, f: &mut Chars) -> Result<()> {
        if c == ' ' {
            self.query.push(c);
            while let Some(d) = f.next() {
                if d == ' ' {
                    self.query.push(d);
                } else {
                    c = d;
                    break;
                }
            }
        }
        self.logic = false;
        while let Some(d) = f.next() {
            if d.is_alphanumeric() {
                self.last = Some(d);
                return Ok(());
            } else {
                return if d == ')' {
                    self.depth -= 1;
                    while let Some(e) = f.next() {
                        self.query.push(e);
                        if d == ')' {
                            self.depth -= 1;
                        } else if d == ' ' {
                            continue;
                        } else {
                            self.last = Some(e);
                            break;
                        }
                    }
                    Ok(())
                } else {
                    let s = format!("{}{}", c, d);
                    if ["&&", "||"].iter().find(|&&t| t == s).is_some() {
                        self.logic = true;
                        let t = if s == "&&" {
                            format!("{} AND {}", self.left, self.right)
                        } else {
                            format!("{} OR {}", self.left, self.right)
                        };

                        self.query.push_str(&t);
                        self.last = if let Some(e) = f.next() {
                            Some(e)
                        } else {
                            None
                        };
                        Ok(())
                    } else {
                        Err(invalid())
                    }
                };
            }
        }
        Err(invalid())
    }
}
