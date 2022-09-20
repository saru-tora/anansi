use rpassword::prompt_password;

use anansi::db::DbPool;
use anansi::web::Result;
use anansi::models::{Model, DataType, Boolean};
use super::models::{User, hash_password};

pub fn admin(pool: DbPool) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>> {
    Box::pin(_admin(pool))
}

fn prints(s: &str) {
    use std::io::Write;
    print!("{}", s);
    std::io::stdout().flush().unwrap();
}

async fn _admin(pool: DbPool) -> Result<()> {
    let mut input = String::new();
    let name = loop {
        prints("Username: ");
        std::io::stdin().read_line(&mut input)?;
        if input.is_empty() {
            continue;
        }
        match User::validate_username(&input, &pool).await {
            Ok(name) => break name,
            Err(feedback) => {
                eprintln!("{}", feedback.warning());
                input.clear();
            },
        }
    };
    let password = loop {
        let password = prompt_password("Password: ")?;
        let entropy = User::check_password(&password);
        if let Some(feedback) = entropy.feedback() {
            eprintln!("{}", feedback.warning());
            eprintln!("{}\n", feedback.suggestion());
        } else {
            break password;
        }
    };
    loop {
        let confirm = prompt_password("Confirm password: ")?;
        if password != confirm {
            eprintln!("Password does not match.\n");
        } else {
            let u = User::new(name, hash_password(&password).unwrap(), Boolean::from_val(true).unwrap()).raw_save(&pool).await.expect("Problem creating admin.");
            println!("Created admin \"{}\"", u.username);
            break Ok(());
        }
    }
}
