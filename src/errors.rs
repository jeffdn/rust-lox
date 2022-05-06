use std::fmt;

use crate::tokens::Literal;

#[derive(Clone, Debug)]
pub enum LoxError {
    UsageError,
    AstError,
    ParseError(usize, String),
    TypeError(String),
    InputError(String),
    RuntimeError(String),
    SyntaxError(usize, String),
    FunctionReturn(Literal),
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            LoxError::UsageError => "usage: lox [script]".to_string(),
            LoxError::AstError => "internal error in parsing AST".to_string(),
            LoxError::ParseError(line, msg) => format!("parse error on line {}: {}", line, msg),
            LoxError::TypeError(msg) => format!("type error: {}", msg),
            LoxError::InputError(msg) => msg.clone(),
            LoxError::SyntaxError(line, msg) => format!("error on line {}: {}", line, msg),
            LoxError::RuntimeError(msg) => format!("runtime error: {}", msg),
            LoxError::FunctionReturn(_) => "you shouldn't see this!".to_string(),
        };

        write!(f, "{}", output)
    }
}

