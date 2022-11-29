use std::{
    boxed::Box,
    cell::RefCell,
    collections::HashMap,
    fmt,
    rc::Rc,
};

use crate::{
    chunk::Chunk,
    errors::LoxError,
    value::{Value, ValuePtr},
};

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    String(Box<String>),
    Function(Box<Function>),
    Native(Box<NativeFunction>),
    Closure(Box<Closure>),
    Class(Box<Class>),
    Instance(Box<Instance>),
    UpValue(Box<ObjUpValue>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: String,
    pub obj: Option<Object>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    pub class: ValuePtr,
    pub fields: HashMap<String, ValuePtr>,
    pub obj: Option<Object>,
}

impl Instance {
    pub fn new(class: ValuePtr) -> Self {
        Self {
            class,
            fields: HashMap::new(),
            obj: None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjUpValue {
    pub location: ValuePtr,
    pub location_index: usize, // the position of `location` in the stack
    pub obj: Option<Object>,
}

pub type ObjUpValuePtr = Rc<RefCell<ObjUpValue>>;

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionType {
    Function,
    Script,
}

pub type NativeFn = fn(&[ValuePtr]) -> Result<Value, LoxError>;

#[derive(Clone, Debug, PartialEq)]
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

impl PartialEq for NativeFunction {
    fn eq(&self, rhs: &Self) -> bool {
        self.name == rhs.name
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<native function>")
    }
}
#[derive(Clone, Debug, PartialEq)]
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
            Object::Class(class) => class.name.clone(),
            Object::Instance(instance) => {
                let Value::Object(Object::Class(class)) = &*instance.class.borrow() else {
                    unreachable!();
                };

                format!("<{} instance>", class.name)
            },
            Object::UpValue(_) => "up-value".into(),
        };

        write!(f, "'{}'", output)
    }
}
