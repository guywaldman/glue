mod lsp;

use tower_lsp::{LspService, Server};

use crate::lsp::Lsp;

#[tokio::main]
async fn main() {
    let (service, socket) = LspService::build(Lsp::new).finish();
    Server::new(tokio::io::stdin(), tokio::io::stdout(), socket).serve(service).await;
}
