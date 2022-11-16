use std::io::Write;
use std::{env, fs, io};

use anyhow::Result;
use interpreter::Interpreter;
use log::debug;

mod environment;
mod error;
mod expression;
mod interpreter;
mod macros;
mod parser;
mod scanner;
mod token;

fn main() -> Result<()> {
    env_logger::init();

    let mut args = env::args();

    match args.len() {
        n if n == 2 => run_file(&args.nth(1).unwrap())?,
        n if n == 1 => run_prompt()?,
        _ => {
            eprintln!("Usage: rox [script]");
            panic!();
        }
    }

    Ok(())
}

fn run_file(file_path: &str) -> Result<()> {
    debug!("Reading file: {file_path}");
    let program = fs::read_to_string(file_path).expect("Failed to read file");
    run(&program, &Interpreter::new())?;

    Ok(())
}

fn run_prompt() -> Result<()> {
    let interpreter = Interpreter::new();
    let mut buffer = String::new();

    println!("Rox prompt, hit ctrl-d to quit.");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let bytes_read = io::stdin().read_line(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }

        let input = buffer.trim_end();

        // Do not crash on error!
        if let Err(err) = run(input, &interpreter) {
            eprintln!("ERROR: {}", err)
        }

        buffer.clear();
    }

    Ok(())
}

fn run(program: &str, interpreter: &Interpreter) -> Result<()> {
    let scanner = scanner::Scanner::new(&program);
    let tokens = scanner.scan_tokens()?;
    let parser = parser::Parser::new(&tokens);
    let statements = parser.parse()?;
    interpreter.interpret(&statements)?;

    Ok(())
}
