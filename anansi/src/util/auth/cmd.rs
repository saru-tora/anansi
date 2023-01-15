use std::io;
use rpassword::prompt_password;

use anansi::raw_transact;
use anansi::db::DbPool;
use anansi::web::Result;
use anansi::records::{Record, DateTime, Text};
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
        io::stdin().read_line(&mut input)?;
        input.pop();
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
    let email = loop {
        prints("Email (optional): ");
        io::stdin().read_line(&mut input)?;
        input.pop();
        break if input.is_empty() {
            None
        } else {
            let email = Text::from(input.clone());
            input.clear();
            Some(email)
        };
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
                let now = DateTime::now();
                let u = User::new()
                    .username(name)
                    .email(email)
                    .password(hash_password(&password).unwrap())
                    .last_login(now)
                    .date_joined(now)
                    .raw_saved(&pool)
                    .await
                    .expect("Problem creating admin.");
                let group = Group::whose(groupname().eq("admin")).raw_get(&pool).await?;
                GroupTuple::new()
                    .subject_namespace(Text::from("auth_user".to_string()))
                    .subject_key(u.pk())
                    .object_key(group.pk())
                    .object_predicate(Text::from("member".to_string()))
                    .raw_saved(&pool)
                    .await
                    .expect("Problem adding admin.");
                println!("Created admin \"{}\"", u.username);
                Ok(())
            })
        }
    }
}
