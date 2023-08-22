use std::{boxed::Box, fmt};

use crate::tokens::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum LoxError {
    UsageError,
    AstError,
    CompileError(String),
    ResolutionError(String),
    ParseError(Token, String),
    TypeError(String),
    InputError(String),
    RuntimeError(String),
    SyntaxError(usize, String),
    Collection(Box<Vec<LoxError>>),
}

pub type LoxResult<T> = Result<T, LoxError>;

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            LoxError::UsageError => "usage: lox [script]".to_string(),
            LoxError::AstError => "internal error in parsing AST".to_string(),
            LoxError::CompileError(msg) => format!("compilation error: {}", msg),
            LoxError::ResolutionError(msg) => format!("resolution error: {}", msg),
            LoxError::ParseError(_, msg) => format!("parse error: {}", msg),
            LoxError::TypeError(msg) => format!("type error: {}", msg),
            LoxError::InputError(msg) => msg.clone(),
            LoxError::SyntaxError(line, msg) => format!("error on line {}: {}", line, msg),
            LoxError::RuntimeError(msg) => format!("runtime error: {}", msg),
            LoxError::Collection(errors) => errors
                .iter()
                .map(|e| format!("{}", e))
                .collect::<Vec<String>>()
                .join("\n"),
        };

        write!(f, "{}", output)
    }
}

impl LoxError {
    pub fn print_parse_error(&self, source: &str) {
        match self {
            LoxError::Collection(errors) => {
                for (idx, error) in errors.iter().enumerate() {
                    error.print_parse_error(source);

                    if idx < errors.len() - 1 {
                        println!("\n");
                    }
                }
            },
            LoxError::ParseError(token, msg) => {
                let line = source
                    .split('\n')
                    .enumerate()
                    .find_map(|(idx, line)| {
                        if idx + 1 == token.line {
                            Some(line)
                        } else {
                            None
                        }
                    })
                    .unwrap();
                let space: String = (0..token.column).map(|_| " ").collect();
                let marker: String = (0..token.length).map(|_| "-").collect();

                let sep_len = line.len().max(20).min(100);
                let hashes: String = (0..=sep_len).map(|_| "-").collect();

                println!("{}", hashes);
                println!("parse error on line {}: {}\n", token.line, msg);
                println!("{}", line);
                println!("{}^{}", space, marker);
                println!("{}", hashes);
            },
            _ => println!("{}", self),
        }
    }
}
