use tarpc::serde::{Deserialize, Serialize};

#[tarpc::service(derive = "Deserialize", derive = "Serialize")]
trait World {
    async fn hello(name: String) -> String;
}

fn main() {}
