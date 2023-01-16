use std::path::PathBuf;

use anyhow::Result;
use clap::Parser;

mod parser;

#[derive(Debug, Parser)]
struct Cli {
    input: PathBuf,
    #[arg(short, long)]
    output: Option<PathBuf>,
}

pub fn main() -> Result<()> {
    let args = Cli::parse();

    let input = std::fs::read_to_string(&args.input)?;
    let tokens = parser::tokenize(&input);
    let unit = parser::parse(
        &tokens,
        args.input
            .file_name()
            .unwrap()
            .to_string_lossy()
            .to_string(),
        args.input,
    )?;

    Ok(())
}
