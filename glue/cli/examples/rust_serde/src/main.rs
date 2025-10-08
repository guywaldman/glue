mod generated_models;

use crate::generated_models::{User, UserRegistrationStatus};

fn main() {
   let user = User {
       id: "123".into(),
       registration_status: UserRegistrationStatus::Active,
   };
   let user_json = serde_json::to_string(&user).unwrap();
    println!("Serialized user: {}", user_json);
}
