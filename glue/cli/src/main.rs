mod args;
mod cli;

use crate::cli::GlueCli;
use anyhow::Result;

fn main() -> Result<()> {
    pretty_env_logger::init();

    let args = std::env::args().collect::<Vec<String>>();
    let args_str: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    GlueCli::new().run(&args_str)?;

    Ok(())
}
