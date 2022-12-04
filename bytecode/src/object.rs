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
    BoundMethod(Box<BoundMethod>),
    Class(Box<Class>),
    Closure(Box<Closure>),
    Function(Box<Function>),
    Instance(Box<Instance>),
    List(Box<Vec<ValuePtr>>),
    Map(Box<ValueMap>),
    Native(Box<NativeFunction>),
    String(Box<String>),
    UpValue(Box<UpValue>),
}

#[derive(Clone, Debug)]
pub struct ValueMap {
    pub map: HashMap<ValuePtr, ValuePtr>,
}

impl PartialEq for ValueMap {
    fn eq(&self, other: &Self) -> bool {
        if self.map.len() != other.map.len() {
            return false;
        }

        for (key, val) in self.map.iter() {
            if let Some(other_val) = other.map.get(key) {
                if val != other_val {
                    return false;
                }
            }
        }

        true
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, ValuePtr>,
    pub obj: Option<Object>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            methods: HashMap::new(),
            obj: None,
        }
    }
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
pub struct BoundMethod {
    pub receiver: ValuePtr,
    pub closure: ValuePtr,
    pub obj: Option<Object>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UpValue {
    pub location: ValuePtr,
    pub location_index: usize, // the position of `location` in the stack
    pub obj: Option<Object>,
}

pub type UpValuePtr = Rc<RefCell<UpValue>>;

#[derive(Clone, Debug, PartialEq)]
pub enum FunctionType {
    Function,
    Initializer,
    Method,
    Script,
}

pub type NativeFn = fn(&[ValuePtr]) -> Result<Value, LoxError>;

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub obj: Option<Object>,
    pub upvalues: Vec<UpValuePtr>,
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
            Object::BoundMethod(method) => {
                let Value::Object(Object::Instance(receiver)) = &*method.receiver.0.borrow() else {
                    unreachable!();
                };
                let Value::Object(Object::Class(class)) = &*receiver.class.0.borrow() else {
                    unreachable!();
                };
                let Value::Object(Object::Closure(closure)) = &*method.closure.0.borrow() else {
                    unreachable!();
                };

                format!(
                    "<method {} on {} instance>",
                    closure.function.name,
                    class.name,
                )
            },
            Object::Class(class) => class.name.clone(),
            Object::Closure(closure) => closure.function.name.clone(),
            Object::Function(function) => function.name.clone(),
            Object::Instance(instance) => {
                let Value::Object(Object::Class(class)) = &*instance.class.0.borrow() else {
                    unreachable!();
                };

                format!("<{} instance>", class.name)
            },
            Object::List(list) => format!(
                "[{}]",
                list.iter()
                    .map(|x| x.borrow().to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Map(hmap) => format!(
                "{{{}}}",
                hmap.map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.borrow().to_string(), v.borrow().to_string()))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Native(_) => "built-in".into(),
            Object::String(string) => *string.clone(),
            Object::UpValue(_) => "up-value".into(),
        };

        write!(f, "'{}'", output)
    }
}
