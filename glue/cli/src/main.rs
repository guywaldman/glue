mod cli;
mod codegen;

#[cfg(test)]
mod test_utils;

use anyhow::Result;

use crate::cli::GlueCli;

fn main() -> Result<()> {
    pretty_env_logger::init();

    let args = std::env::args().collect::<Vec<String>>();
    let args_str: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    GlueCli::new().run(&args_str)?;

    Ok(())
}
