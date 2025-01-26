use std::{env, fs};
use crate::tokenizer::{tokenize, TokenKind};

mod tokenizer;
mod parser;
mod compiler;
mod virtual_machine;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let file_path = &args[1];
    let source = fs::read_to_string(file_path)?;
    for kind in tokenize(&source).iter().map(|tok| &tok.kind) {
        print!("{:?} ", kind);
        if kind == &TokenKind::Newline {
            println!();
        }
    }

    Ok(())
}
