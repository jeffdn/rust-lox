use std::fmt;

pub type LoxResult<T> = Result<T, LoxError>;

#[derive(Debug)]
pub enum LoxError {
    CompileError(String),
    InputError(String),
    ResolutionError,
    ParseError(String),
    RuntimeError(String),
    UsageError,
}

impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = match self {
            LoxError::CompileError(message) => format!("compilation error: {message}"),
            LoxError::InputError(message) => format!("input error: {message}"),
            LoxError::ResolutionError => "this should never propagate to the surface".into(),
            LoxError::ParseError(token) => format!("unexpected token: {token}"),
            LoxError::RuntimeError(message) => format!("runtime error: {message}"),
            LoxError::UsageError => "usage: lox [script]".into(),
        };

        write!(f, "{val}")
    }
}
