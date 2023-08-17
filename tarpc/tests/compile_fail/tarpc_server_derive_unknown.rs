#[tarpc::service(derive_serde = false, derive = "IDontExist")]
trait World {
    async fn hello(name: String) -> String;
}

fn main() {}
