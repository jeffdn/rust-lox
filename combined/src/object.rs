use core::hash::BuildHasherDefault;
use std::{boxed::Box, fmt};

use rustc_hash::{FxHashMap as HashMap, FxHasher};

use crate::{
    chunk::Chunk,
    errors::{LoxError, LoxResult},
    gc::{Heap, ObjPtr, Trace},
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

impl Object {
    pub fn as_upvalue(&self) -> Option<&UpValue> {
        match self {
            Object::UpValue(uv) => Some(uv),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValueIter {
    pub items: Vec<ValuePtr>,
    pub next: usize,
}

impl ValueIter {
    pub fn new(heap: &mut Heap, value: ValuePtr) -> LoxResult<Self> {
        let value = match value {
            Value::Obj(ptr) => match &ptr.obj {
                Object::List(list) => ValueIter {
                    items: *list.clone(),
                    next: 0,
                },
                Object::Map(map) => ValueIter {
                    items: map.map.keys().cloned().collect(),
                    next: 0,
                },
                Object::String(string) => ValueIter {
                    items: string
                        .chars()
                        .map(|x| Value::Obj(heap.allocate(Object::String(Box::new(x.into())))))
                        .collect(),
                    next: 0,
                },
                _ => {
                    return Err(LoxError::RuntimeError(
                        "only lists, maps, and strings can be iterated".into(),
                    ));
                },
            },
            _ => {
                return Err(LoxError::RuntimeError(
                    "only lists, maps, and strings can be iterated".into(),
                ));
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
            if let Some(other_val) = other.map.get(key)
                && val != other_val
            {
                return false;
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

pub type UpValuePtr = ObjPtr;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum FunctionType {
    Function,
    Initializer,
    Method,
    Script,
}

pub type NativeFn = fn(&mut Heap, &[ValuePtr]) -> LoxResult<Value>;

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
                let Value::Obj(receiver_ptr) = method.receiver else {
                    unreachable!();
                };
                let Object::Instance(receiver) = &receiver_ptr.obj else {
                    unreachable!();
                };
                let Value::Obj(class_ptr) = receiver.class else {
                    unreachable!();
                };
                let Object::Class(class) = &class_ptr.obj else {
                    unreachable!();
                };

                let Value::Obj(closure_ptr) = method.closure else {
                    unreachable!();
                };
                let Object::Closure(closure) = &closure_ptr.obj else {
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
                let Value::Obj(class_ptr) = instance.class else {
                    unreachable!();
                };
                let Object::Class(class) = &class_ptr.obj else {
                    unreachable!();
                };

                format!("<{} instance>", class.name)
            },
            Object::Iterator(iter) => format!(
                "<iterator: next={}, items=[{}]>",
                iter.next,
                iter.items
                    .iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::List(list) => format!(
                "[{}]",
                list.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Map(hmap) => format!(
                "{{{}}}",
                hmap.map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
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

impl Trace for Object {
    fn trace(&self, heap: &mut Heap) {
        match self {
            Object::BoundMethod(method) => {
                method.receiver.trace(heap);
                method.closure.trace(heap);
            },
            Object::Class(class) => {
                if let Some(parent) = &class.parent {
                    parent.trace(heap);
                }
                for method in class.methods.values() {
                    method.trace(heap);
                }
            },
            Object::Closure(closure) => {
                for upvalue in &closure.upvalues {
                    heap.mark_object(*upvalue);
                }
                closure.function.trace(heap);
            },
            Object::Function(function) => {
                function.trace(heap);
            },
            Object::Instance(instance) => {
                instance.class.trace(heap);
                for field in instance.fields.values() {
                    field.trace(heap);
                }
            },
            Object::Iterator(iter) => {
                for item in &iter.items {
                    item.trace(heap);
                }
            },
            Object::List(list) => {
                for item in list.iter() {
                    item.trace(heap);
                }
            },
            Object::Map(map) => {
                for (key, value) in &map.map {
                    key.trace(heap);
                    value.trace(heap);
                }
            },
            Object::Module(module) => {
                for val in module.map.values() {
                    val.trace(heap);
                }
            },
            Object::Native(_) => {},
            Object::String(_) => {},
            Object::UpValue(upvalue) => {
                upvalue.location.trace(heap);
            },
        }
    }
}

impl Trace for Function {
    fn trace(&self, heap: &mut Heap) {
        for constant in &self.chunk.constants {
            constant.trace(heap);
        }
    }
}
