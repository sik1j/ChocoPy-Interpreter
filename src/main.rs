use parser::Program;

use crate::parser::parse;
use crate::tokenizer::tokenize;
use std::{env, fs};

mod compiler;
mod parser;
mod tokenizer;
mod type_checker;
mod virtual_machine;

fn main() -> std::io::Result<()> {
    env::set_var("RUST_BACKTRACE", "1");
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let file_path = &args[1];
    let source = fs::read_to_string(file_path)?;
    let Program {
        definitions,
        statements,
    } = parse(&mut tokenize(&source));

    for def in definitions {
        println!("def: {:?}\n\n", def);
    }

    for stmt in statements {
        println!("stmt: {:?}\n\n", stmt);
    }

    Ok(())
}
