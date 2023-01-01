#[macro_export]
macro_rules! test_server {
    () => {
        use crate::test_server;
        let (tx, rx) = tokio::sync::oneshot::channel();
        let server_handle = tokio::spawn(async {test_server(tx).await});
        rx.await.unwrap();
    }
}

#[macro_export]
macro_rules! test_get {
    ($url:expr) => {
        {
            let url = oauth2::url::Url::parse(&format!("http://127.0.0.1:9090{}", $url)).unwrap();
            let method = hyper::Method::GET;
            let headers = hyper::HeaderMap::new();
            let body = vec![];
            oauth2::reqwest::async_http_client(oauth2::HttpRequest {url, method, headers, body})
        }
    }
}
