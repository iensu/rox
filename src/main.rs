use std::io::Write;
use std::{env, fs, io};

use anyhow::Result;
use environment::Environment;
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
    run(&program, &Interpreter::new(), &Environment::new())?;

    Ok(())
}

const HELP_TEXT: &str = "
:?        Display help
:v        List defined variables
:q        Quit
";

fn run_prompt() -> Result<()> {
    let interpreter = Interpreter::new();
    let environment = Environment::new();
    let mut buffer = String::new();
    let mut nested_depth = 0;

    println!("Rox prompt, hit ctrl-d to quit. Type :? for help.");

    loop {
        if nested_depth < 1 {
            buffer.clear();
            print!("> ");
        } else {
            print!(". ");
        }

        io::stdout().flush().unwrap();

        let bytes_read = io::stdin().read_line(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }

        let input = buffer.trim_end();
        nested_depth = input.chars().filter(|&c| c == '{').count()
            - input.chars().filter(|&c| c == '}').count();

        if nested_depth > 0 {
            continue;
        }

        if input.starts_with(":") {
            match input {
                ":?" => println!("{}", HELP_TEXT),
                ":q" => break,
                ":v" => println!("{}", environment),
                _ => eprintln!(
                    "Unknown command '{}', available commands:\n{}",
                    input, HELP_TEXT
                ),
            }
            continue;
        }

        let run_function = if input.ends_with(';') || input.ends_with('}') {
            run
        } else {
            run_expression
        };

        // Do not crash on error!
        if let Err(err) = run_function(input, &interpreter, &environment) {
            eprintln!("ERROR: {}", err)
        }
    }

    Ok(())
}

fn run(program: &str, interpreter: &Interpreter, env: &Environment) -> Result<()> {
    let scanner = scanner::Scanner::new(program);
    let tokens = scanner.scan_tokens()?;
    let parser = parser::Parser::new(&tokens);
    let statements = parser.parse()?;
    interpreter.interpret(&statements, env)?;

    Ok(())
}

fn run_expression(program: &str, interpreter: &Interpreter, env: &Environment) -> Result<()> {
    let scanner = scanner::Scanner::new(program);
    let tokens = scanner.scan_tokens()?;
    let parser = parser::Parser::new(&tokens);
    let expression = parser.expression()?;
    let result = interpreter.evaluate(&expression, env)?;

    println!("{}", result);

    Ok(())
}
