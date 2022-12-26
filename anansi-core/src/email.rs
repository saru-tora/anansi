use std::error::Error;
use lettre::transport::smtp::{response::Response, authentication::Credentials};
use lettre::{Message, AsyncSmtpTransport, AsyncTransport, Tokio1Executor};
use lettre::message::{Mailbox, MessageBuilder};

use crate::db::invalid;
use crate::web::Result;
use crate::server::Settings;

pub struct Email<'a> {
    mailer: &'a Option<Mailer>,
    message: Message,
}

impl<'a> Email<'a> {
    pub fn builder(mailer: &'a Option<Mailer>) -> EmailBuilder<'a> {
        EmailBuilder {mailer, builder: Message::builder()}
    }
    pub async fn send(self) -> Result<EmailResponse> {
        if let Some(mailer) = self.mailer {
            Ok(EmailResponse {0: mailer.0.send(self.message).await?})
        } else {
            Err(invalid())
        }
    }
}

#[derive(Debug, Clone)]
pub struct Mailer(AsyncSmtpTransport<Tokio1Executor>);

impl Mailer {
    pub fn new(settings: &Settings) -> Result<Self> {
        if let Some(smtp_username) = settings.get("smtp_username") {
            if let Some(smtp_username) = smtp_username.as_str() {
                if let Some(smtp_password) = settings.get("smtp_password") {
                    if let Some(smtp_host) = settings.get("smtp_relay") {
                        if let Some(smtp_host) = smtp_host.as_str() {
                            if !smtp_username.is_empty() && !smtp_host.is_empty() {
                                let creds = Credentials::new(smtp_username.to_string(), smtp_password.to_string());
                                let mailer = AsyncSmtpTransport::<Tokio1Executor>::relay(smtp_host)?
                                    .credentials(creds)
                                    .build();
                                return Ok(Self {0: mailer});
                            }
                        }
                    }
                }
            }
        }
        Err(invalid())
    }
}

pub struct EmailAddress(Mailbox);

pub struct EmailBuilder<'a> {
    mailer: &'a Option<Mailer>,
    builder: MessageBuilder,
}

impl<'a> EmailBuilder<'a> {
    pub fn to(self, address: EmailAddress) -> Self {
        Self {mailer: self.mailer, builder: self.builder.to(address.0)}
    }
    pub fn subject<S: Into<String>>(self, subject: S) -> Self {
        Self {mailer: self.mailer, builder: self.builder.subject(subject)}
    }
    pub fn body(self, body: String) -> Result<Email<'a>> {
        match self.builder.body(body) {
            Ok(message) => Ok(Email {mailer: self.mailer, message}),
            Err(e) => Err(Box::new(e) as Box<dyn Error + Send + Sync>),
        }
    }
}

pub struct EmailResponse(Response);
