use anansi::web::BaseUser;
use anansi::{record, FromParams, ToUrl, Relate, records::{VarChar, Text}};

#[record]
#[derive(Debug, Clone, FromParams, ToUrl, Relate)]
pub struct DummyUser {
    pub username: VarChar<150>,
}

impl BaseUser for DummyUser {
    type Name = VarChar<150>;
    type Secret = Text;
    fn username(&self) -> &Self::Name {
        unimplemented!();
    }
    fn secret(&self) -> &Option<Self::Secret> {
        unimplemented!();
    }
    fn is_auth(&self) -> bool {
        unimplemented!();
    }
}
