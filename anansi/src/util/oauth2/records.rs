use anansi::web::{Result, BaseRequest};
use anansi::db::invalid;
use anansi::records::{Text, Relate};
use anansi::record;

use super::super::sessions::middleware::Sessions;

use oauth2::{
    AuthorizationCode,
    AuthorizationRequest,
    AuthUrl,
    Client,
    ClientId,
    ClientSecret,
    CsrfToken,
    PkceCodeChallenge,
    PkceCodeVerifier,
    RedirectUrl,
    Scope,
    TokenUrl,
    StandardTokenResponse,
    EmptyExtraTokenFields,
    StandardErrorResponse,
    basic::BasicErrorResponseType,
    StandardTokenIntrospectionResponse,
    StandardRevocableToken,
    RevocationErrorResponseType,
};
use oauth2::basic::{BasicClient,  BasicTokenType};
use oauth2::reqwest::async_http_client;
use oauth2::url::Url;

#[record]
#[derive(Debug, Clone)]
pub struct Provider {
    pub name: Text,
    pub auth_url: Text,
    pub token_url: Text,
    pub client_id: Text,
    pub client_secret: Text,
}

impl<B: BaseRequest> Relate<B> for Provider {}

impl Provider {
    fn redirect_key(&self) -> String {
        format!("_{}_redirect", self.name)
    }
    fn csrf_key(&self) -> String {
        format!("_{}_csrf_token", self.name)
    }
    fn verifier_key(&self) -> String {
        format!("_{}_verifier", self.name)
    }
    fn to_client(&self, redirect_uri: String) -> Result<BasicClient> {
        let client = BasicClient::new(
            ClientId::new(self.client_id.to_string()),
            Some(ClientSecret::new(self.client_secret.to_string())),
            AuthUrl::new(self.auth_url.to_string())?,
            Some(TokenUrl::new(self.token_url.to_string())?)
        ).set_redirect_uri(RedirectUrl::new(redirect_uri)?);
        Ok(client)
    }
    pub fn redirect<'a, R: BaseRequest + Sessions>(self, req: &'a mut R, redirect_uri: String) -> Result<AuthClient<'a, R>> {
        let client = self.to_client(redirect_uri.clone())?;
        req.session_data_mut().insert(self.redirect_key(), serde_json::json!(redirect_uri));
        Ok(AuthClient {req, provider: self, client})
    }
    pub async fn token<R: Sessions + BaseRequest>(self, req: &mut R) -> Result<StandardTokenResponse<EmptyExtraTokenFields, BasicTokenType>> {
        let state = req.params().get("state")?;
        let code = req.params().get("code")?;
        let csrf_token = req.session_data().get(&self.csrf_key())?;
        if state == csrf_token {
            let redirect_key = req.session_data().get(&self.redirect_key())?;
            let pkce_verifier = req.session_data().get(&self.verifier_key())?;
            let body = self.to_client(redirect_key.to_string())?
                .exchange_code(AuthorizationCode::new(code.to_string()))
                .set_pkce_verifier(PkceCodeVerifier::new(pkce_verifier.to_string()))
                .request_async(async_http_client).await?;
            Ok(body)
        } else {
            Err(invalid())
        }
    }
}

pub struct AuthClient<'a, R: BaseRequest + Sessions> {
    req: &'a mut R,
    provider: Provider,
    client: Client<StandardErrorResponse<BasicErrorResponseType>, StandardTokenResponse<EmptyExtraTokenFields, BasicTokenType>, BasicTokenType,
        StandardTokenIntrospectionResponse<EmptyExtraTokenFields, BasicTokenType>, StandardRevocableToken, StandardErrorResponse<RevocationErrorResponseType>>,
}

impl<'a, R: BaseRequest + Sessions> AuthClient<'a, R> {
    pub fn authorize_url(&'a mut self) -> AuthBuilder<'a, R> {
        let auth_req = self.client.authorize_url(CsrfToken::new_random);
        AuthBuilder {req: &mut self.req, provider: &mut self.provider, auth_req}
    }
}

#[derive(Debug)]
pub struct AuthBuilder<'a, R: BaseRequest + Sessions> {
    req: &'a mut R,
    provider: &'a Provider,
    auth_req: AuthorizationRequest<'a>,
}

impl<'a, R: BaseRequest + Sessions> AuthBuilder<'a, R> {
    pub fn add_scope(mut self, scope: String) -> Self {
        self.auth_req = self.auth_req.add_scope(Scope::new(scope));
        self
    }
    pub fn url(self) -> Url {
        let (pkce_challenge, pkce_verifier) = PkceCodeChallenge::new_random_sha256();
        let (auth_url, csrf_token) =  self.auth_req
            .set_pkce_challenge(pkce_challenge)
            .url();

        self.req.session_data_mut().insert(self.provider.csrf_key(), serde_json::json!(csrf_token.secret()));
        self.req.session_data_mut().insert(self.provider.verifier_key(), serde_json::json!(pkce_verifier.secret()));
        auth_url
    }
}
