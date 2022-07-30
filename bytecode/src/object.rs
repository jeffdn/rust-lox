use std::{
    boxed::Box,
    fmt,
};

use crate::{
    chunk::Chunk,
    errors::LoxError,
    value::Value,
};

#[derive(Clone)]
pub enum Object {
    String(Box<String>),
    Function(Box<Function>),
    Native(Box<NativeFunction>),
}

#[derive(Clone)]
pub enum FunctionType {
    Function,
    Script,
}

pub type NativeFn = fn(&[Value]) -> Result<Value, LoxError>;

#[derive(Clone)]
pub struct NativeFunction {
    pub function: NativeFn,
    pub obj: Option<Object>,
    pub name: String,
    pub arity: usize,
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
            Object::Native(_) => "built-in".into(),
        };

        write!(f, "'{}'", output)
    }
}
