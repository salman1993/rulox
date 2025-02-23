use std::env;

use rulox::evaluate::interpret;
use rulox::parser::{parse, Parser};
use rulox::reader::read_source;
use rulox::tokenize::tokenize;

struct Config {
    filepath: String,
}

impl Config {
    fn build(args: &[String]) -> Result<Config, String> {
        if args.len() < 2 {
            println!("Not enough arguments.\nUsage: cargo run -- simple.lox");
            let error_msg = "Not enough arguments.";
            return Err(error_msg.to_string());
        }

        let filepath = args[1].clone();
        Ok(Config { filepath })
    }
}

// To run this file: `cargo run -- simple.lox`
fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let config = Config::build(&args)?;

    let source = read_source(&config.filepath)?;
    println!("Source: {source:?}");

    let tokens = tokenize(source)?;
    println!("\nTokens: {tokens:?}");

    let parser = Parser::new(&tokens);
    let ast = parse(parser)?;
    println!("\nAST: {ast:?}");

    println!("\nResult:\n");
    interpret(&ast)?;

    Ok(())
}
