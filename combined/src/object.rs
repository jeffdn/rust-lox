use core::hash::BuildHasherDefault;
use std::{boxed::Box, cell::RefCell, fmt, rc::Rc};

use rustc_hash::{FxHashMap as HashMap, FxHasher};

use crate::{
    chunk::Chunk,
    errors::{LoxError, LoxResult},
    value::{Value, ValuePtr},
};

type LoxBuildHasher = BuildHasherDefault<FxHasher>;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    BoundMethod(Box<BoundMethod>),
    Class(Box<Class>),
    Closure(Box<Closure>),
    Function(Box<Function>),
    Instance(Box<Instance>),
    Iterator(Box<ValueIter>),
    List(Box<Vec<ValuePtr>>),
    Map(Box<ValueMap>),
    Module(Box<Module>),
    Native(Box<NativeFunction>),
    String(Box<String>),
    UpValue(Box<UpValue>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValueIter {
    pub items: Vec<ValuePtr>,
    pub next: usize,
}

impl ValueIter {
    pub fn new(instance: ValuePtr) -> LoxResult<Self> {
        let value = match &*instance.borrow() {
            Value::Object(object) => match object {
                Object::List(list) => ValueIter {
                    items: (0..list.len())
                        .map(|x| ValuePtr::new(Value::Number(x as f64)))
                        .collect(),
                    next: 0,
                },
                Object::Map(hmap) => ValueIter {
                    items: hmap.map.keys().cloned().collect(),
                    next: 0,
                },
                Object::String(string) => ValueIter {
                    items: string
                        .chars()
                        .map(|x| ValuePtr::new(Value::Object(Object::String(Box::new(x.into())))))
                        .collect(),
                    next: 0,
                },
                _ => {
                    return Err(LoxError::RuntimeError(
                        "only lists, maps, and strings can be iterated".into(),
                    ))
                },
            },
            _ => {
                return Err(LoxError::RuntimeError(
                    "only lists, maps, and strings can be iterated".into(),
                ))
            },
        };

        Ok(value)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub name: String,
    pub map: HashMap<String, ValuePtr>,
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
    pub parent: Option<ValuePtr>,
    pub name: String,
    pub methods: HashMap<String, ValuePtr>,
}

impl Class {
    pub fn new(name: String) -> Self {
        Self {
            name,
            parent: None,
            methods: HashMap::with_hasher(LoxBuildHasher::default()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Instance {
    pub class: ValuePtr,
    pub fields: HashMap<String, ValuePtr>,
}

impl Instance {
    pub fn new(class: ValuePtr) -> Self {
        Self {
            class,
            fields: HashMap::with_hasher(LoxBuildHasher::default()),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoundMethod {
    pub receiver: ValuePtr,
    pub closure: ValuePtr,
}

#[derive(Clone, Debug, PartialEq)]
pub struct UpValue {
    pub location: ValuePtr,
    pub location_index: usize, // the position of `location` in the stack
}

pub type UpValuePtr = Rc<RefCell<UpValue>>;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FunctionType {
    Function,
    Initializer,
    Method,
    Script,
}

pub type NativeFn = fn(&[ValuePtr]) -> LoxResult<Value>;

#[derive(Clone, Debug, PartialEq)]
pub struct Closure {
    pub upvalues: Vec<UpValuePtr>,
    pub function: Function,
}

#[derive(Clone)]
pub struct NativeFunction {
    pub function: NativeFn,
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
    pub arity: usize,
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
                    closure.function.name, class.name,
                )
            },
            Object::Class(class) => class.name.clone(),
            Object::Closure(closure) => format!("<closure: {}>", closure.function.name),
            Object::Function(function) => format!("<function: {}>", function.name),
            Object::Instance(instance) => {
                let Value::Object(Object::Class(class)) = &*instance.class.0.borrow() else {
                    unreachable!();
                };

                format!("<{} instance>", class.name)
            },
            Object::Iterator(iter) => format!(
                "<iterator: next={}, items=[{}]>",
                iter.next,
                iter.items
                    .iter()
                    .map(|x| x.borrow().to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
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
                    .map(|(k, v)| format!("{}: {}", k.borrow(), v.borrow()))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Module(module) => format!("<module '{}'>", module.name),
            Object::Native(_) => "built-in".into(),
            Object::String(string) => string.clone().replace("\\n", "\n"),
            Object::UpValue(_) => "up-value".into(),
        };

        write!(f, "{output}")
    }
}
