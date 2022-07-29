use std::{
    boxed::Box,
    fmt,
};

use crate::chunk::Chunk;

#[derive(Clone)]
pub enum Object {
    String(Box<String>),
    Function(Box<Function>),
}

#[derive(Clone)]
pub enum FunctionType {
    Function,
    Script,
}

#[derive(Clone)]
pub struct Function {
    pub function_type: FunctionType,
    pub obj: Option<Object>,
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
}


impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            Object::String(string) => *string.clone(),
            Object::Function(function) => function.name.clone(),
        };

        write!(f, "'{}'", output)
    }
}
