use anansi::project::prelude::*;

mod urls;
mod settings;
mod http_errors;

apps! {
    auth,
    sessions,
}

app_statics! {
    admin,
}

pages! {
    auth,
}

main!();
