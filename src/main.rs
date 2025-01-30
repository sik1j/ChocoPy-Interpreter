use crate::parser::parse;
use crate::tokenizer::tokenize;
use std::{env, fs};

mod compiler;
mod parser;
mod tokenizer;
mod virtual_machine;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let file_path = &args[1];
    let source = fs::read_to_string(file_path)?;
    for node in parse(&mut tokenize(&source)) {
        println!("{:?}", node)
    }

    Ok(())
}
