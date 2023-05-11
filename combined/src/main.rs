pub mod builtin;
pub mod chunk;
pub mod compiler;
pub mod errors;
pub mod expressions;
pub mod object;
pub mod parser;
pub mod scanner;
pub mod statements;
pub mod tokens;
pub mod value;
pub mod vm;

use std::{
    env, fs,
    io::{self, Write},
};

use crate::{errors::LoxError, vm::VirtualMachine};

fn eval(source: String) -> Result<(), LoxError> {
    let mut vm = VirtualMachine::new();
    vm.interpret(&source)
}

fn run_lox_file(script_path: &str) -> Result<(), LoxError> {
    let source = fs::read_to_string(script_path).ok().ok_or_else(|| {
        LoxError::InputError(format!("unable to parse source file '{}'", script_path))
    })?;

    eval(source)
}

fn slurp_expr() -> String {
    let mut input_expr = String::new();

    io::stdin()
        .read_line(&mut input_expr)
        .expect("failed to read line");

    input_expr
}

fn run_lox_repl() -> Result<(), LoxError> {
    loop {
        print!("lox > ");
        io::stdout().flush().unwrap();

        let input_expr = slurp_expr();
        eval(input_expr)?;
    }
}

fn main() -> Result<(), LoxError> {
    let args: Vec<String> = env::args().collect();

    match args.len() {
        2 => run_lox_file(&args[1]),
        1 => run_lox_repl(),
        _ => {
            println!("usage: lox [script]");
            Err(LoxError::UsageError)
        },
    }
}
