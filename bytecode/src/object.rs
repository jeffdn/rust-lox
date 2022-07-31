use std::{
    boxed::Box,
    cell::RefCell,
    fmt,
    rc::Rc,
};

use crate::{
    chunk::Chunk,
    errors::LoxError,
    value::{Value, ValuePtr},
};

#[derive(Clone, Debug)]
pub enum Object {
    String(Box<String>),
    Function(Box<Function>),
    Native(Box<NativeFunction>),
    Closure(Box<Closure>),
    UpValue(Box<ObjUpValue>),
}

#[derive(Clone, Debug)]
pub struct ObjUpValue {
    pub location: ValuePtr,
    pub obj: Option<Object>,
}

pub type ObjUpValuePtr = Rc<RefCell<ObjUpValue>>;

#[derive(Clone, Debug)]
pub enum FunctionType {
    Function,
    Script,
}


pub type NativeFn = fn(&[ValuePtr]) -> Result<Value, LoxError>;

#[derive(Clone, Debug)]
pub struct Closure {
    pub obj: Option<Object>,
    pub upvalues: Vec<ObjUpValuePtr>,
    pub function: Function,
}

#[derive(Clone)]
pub struct NativeFunction {
    pub function: NativeFn,
    pub obj: Option<Object>,
    pub name: String,
    pub arity: usize,
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native function>")
    }
}
#[derive(Clone, Debug)]
pub struct Function {
    pub function_type: FunctionType,
    pub obj: Option<Object>,
    pub arity: usize,
    pub upvalue_count: usize,
    pub chunk: Chunk,
    pub name: String,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            Object::String(string) => *string.clone(),
            Object::Function(function) => function.name.clone(),
            Object::Native(_) => "built-in".into(),
            Object::Closure(closure) => closure.function.name.clone(),
            Object::UpValue(_) => "up-value".into(),
        };

        write!(f, "'{}'", output)
    }
}
