use rpassword::prompt_password;

use anansi::raw_transact;
use anansi::db::DbPool;
use anansi::web::Result;
use anansi::records::{Record, Text};
use super::records::{User, Group, group::groupname, GroupTuple, hash_password};

pub fn admin<D: DbPool + 'static>(pool: D) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<()>> + Send>> {
    Box::pin(_admin(pool))
}

fn prints(s: &str) {
    use std::io::Write;
    print!("{}", s);
    std::io::stdout().flush().unwrap();
}

async fn _admin<D: DbPool>(pool: D) -> Result<()> {
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
                eprintln!("{}\n", feedback.suggestion());
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
            break raw_transact!(pool, {
                let u = User::new(name, hash_password(&password).unwrap()).raw_save(&pool).await.expect("Problem creating admin.");
                let group = Group::whose(groupname().eq("admin")).raw_get(&pool).await?;
                GroupTuple::new(Text::from("auth_user".to_string()), u.pk(), None, group.pk(), Text::from("member".to_string())).raw_save(&pool).await.expect("Problem adding admin.");
                println!("Created admin \"{}\"", u.username);
                Ok(())
            })
        }
    }
}
