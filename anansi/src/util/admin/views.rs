#[macro_export]
macro_rules! pages {
    ($($name:ident,)*) => {
        pub static PAGES: anansi::server::Pages<crate::settings::HttpRequest> = &[
            $($name::init::admin_page,)*
        ];
    }
}

#[macro_export]
macro_rules! admin_page {
    ($(($p:literal, $v:path),)*) => {
        pub fn admin_page<R: Request>(v: &mut Vec<(&'static str, View<R>)>) {
            $(v.push(($p, $v));)*
        }
    }
}
