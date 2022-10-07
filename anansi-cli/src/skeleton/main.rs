use anansi::*;
use anansi::util::{auth, sessions, admin};

mod urls;
mod project;
mod http_errors;

apps! {
    auth,
    sessions,
}

app_statics! {
    admin,
}

app_admins! {
    auth,
}

main!();
