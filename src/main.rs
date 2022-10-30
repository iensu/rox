use std::io::Write;
use std::{env, fs, io};

use eyre::Result;

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

        // Do not crash on error!
        if let Err(err) = run(buffer.to_string()) {
            report(err);
        }

        buffer.clear();
    }

    Ok(())
}

fn run(program: String) -> Result<()> {
    let tokens = token::scan_tokens(&program);

    for t in tokens {
        println!("{t:?}");
    }

    Ok(())
}

fn report(err: eyre::Report) {
    eprintln!("{:?}", err);
}
