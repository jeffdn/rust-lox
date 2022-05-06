pub mod environment;
pub mod errors;
pub mod expressions;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod statements;
pub mod tokens;

use std::env;
use std::fs;
use std::io::{self, Write};

use crate::errors::LoxError;
use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::scanner::Scanner;

fn eval(source: String) -> Result<(), LoxError> {
    let mut scanner = Scanner::new(source);
    let (tokens, errors) = scanner.scan_tokens();
    let mut parser = Parser::new(tokens);
    let parse_output = parser.parse();

    match parse_output {
        Ok(output) => {
            let mut interpreter = Interpreter::new(None);

            match interpreter.interpret(output) {
                Ok(_) => {},
                Err(e) => println!("{}", e),
            };
        },
        Err(e) => {
            println!("parse errors:");
            println!(" - {}", e);
        }
    };

    if errors.len() > 0 {
        println!("syntax errors:");
        for error in errors.iter() {
            println!(" - {}", error);
        }
    }

    Ok(())
}

fn run_lox_file(script_path: &str) -> Result<(), LoxError> {
    let source = fs::read_to_string(script_path)
        .ok()
        .ok_or(
            LoxError::InputError(
                format!("unable to parse source file '{}'", script_path)
            )
        )?;

    let output = eval(source)?;

    Ok(output)
}

fn slurp_expr() -> String {
    let mut input_expr = String::new();

    io::stdin()
        .read_line(&mut input_expr)
        .expect("failed to read line");

    input_expr
}

fn run_lox_repl() -> Result<(), LoxError>{
    loop {
        print!("lox > ");
        io::stdout().flush().unwrap();

        let input_expr = slurp_expr();
        eval(input_expr)?;
    }
}

fn main() -> Result<(), LoxError> {
    let args: Vec<String> = env::args().collect();

    if args.len() > 2 {
        println!("usage: lox [script]");
        return Err(LoxError::UsageError);
    } else if args.len() == 2 {
        run_lox_file(&args[1])?;
    } else {
        run_lox_repl()?;
    }

    Ok(())
}
