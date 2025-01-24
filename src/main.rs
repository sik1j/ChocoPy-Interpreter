use std::{env, fs};
use crate::compiler::compile;
use crate::parser::parse;
use crate::tokenizer::tokenize;

mod tokenizer;
mod parser;
mod compiler;

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let file_path = &args[1];
    let source = fs::read_to_string(file_path)?;

    let tokens = tokenize(&source);
    // for tok in tokens {
    //     println!("{:?}", tok);
    // }

    let t = parse(&tokens);
    println!("{:?}", t);

    let bytecode = compile(&t);
    println!("{:?}", bytecode);


    Ok(())
}
