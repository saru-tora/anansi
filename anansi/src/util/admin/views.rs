#[macro_export]
macro_rules! app_admins {
    ($($name:ident,)*) => {
        pub static APP_ADMINS: &[fn(anansi::admin_site::AdminRef<crate::project::HttpRequest>)] = &[
            $($name::admin::initialize_admin::<crate::project::HttpRequest>,)*
        ];
    }
}
