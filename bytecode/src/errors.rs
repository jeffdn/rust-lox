use std::fmt;

#[derive(Debug)]
pub enum LoxError {
    CompileError(String),
    InputError(String),
    ParseError(String),
    RuntimeError,
    UsageError,
}


impl fmt::Display for LoxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let val = match self {
            LoxError::CompileError(message) => format!("compilation error: {}", message),
            LoxError::InputError(message) => format!("input error: {}", message),
            LoxError::ParseError(token) => format!("unexpected token: {}", token),
            LoxError::RuntimeError => "runtime error".into(),
            LoxError::UsageError => "usage: lox [script]".into(),
        };

        write!(f, "{}", val)
    }
}
