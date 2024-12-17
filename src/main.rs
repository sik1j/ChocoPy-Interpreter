use std::{env, fs};
use crate::tokenizer::Tokenizer;

mod tokenizer;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let file_path = &args[1];
    let source = fs::read_to_string(file_path)?;

    let tokens = Tokenizer::tokenize(source);
    for tok in tokens {
        println!("{:?}", tok);
    }

    Ok(())
}
