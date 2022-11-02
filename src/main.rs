use std::io::Write;
use std::{env, fs, io};

use eyre::Result;

mod expression;
mod keywords;
mod parser;
mod token;

fn main() -> Result<()> {
    let mut args = env::args();

    if args.len() > 2 {
        eprintln!("Usage: rox [script]");
        panic!();
    } else if args.len() == 2 {
        run_file(args.next().unwrap())?;
    } else {
        run_prompt()?;
    }

    Ok(())
}

fn run_file(file_path: String) -> Result<()> {
    let program = fs::read_to_string(file_path).expect("Failed to read file");
    run(program)?;

    Ok(())
}

fn run_prompt() -> Result<()> {
    let mut buffer = String::new();

    println!("Rox prompt, hit ctrl-d to quit.");

    loop {
        print!("> ");
        io::stdout().flush().unwrap();
        let bytes_read = io::stdin().read_line(&mut buffer)?;
        if bytes_read == 0 {
            break;
        }

        let input = buffer.to_string().trim_end().to_string();

        // Do not crash on error!
        if let Err(err) = run(input) {
            report(err);
        }

        buffer.clear();
    }

    Ok(())
}

fn run(program: String) -> Result<()> {
    let mut scanner = token::Scanner::new(&program);
    let tokens = scanner.scan_tokens()?;
    let parser = parser::Parser::new();
    let expression = parser.parse(&tokens)?;

    println!("{expression}");

    Ok(())
}

fn report(err: eyre::Report) {
    eprintln!("{:?}", err);
}
