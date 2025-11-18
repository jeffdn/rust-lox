use std::{
    fmt,
    hash::{Hash, Hasher},
};

use crate::gc::{Heap, ObjPtr, Trace};

use crate::object::Object;

#[derive(Clone, Copy, Debug)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    Obj(ObjPtr),
}

// impl From<Literal> for Value removed because it requires allocation which needs Heap access
// We will handle literal conversion in Compiler/VM

pub type ValuePtr = Value;

impl Value {
    pub fn new(value: Value) -> Self {
        value
    }
}

fn integer_decode(val: f64) -> (u64, i16, i8) {
    let bits: u64 = val.to_bits();
    let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
    let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
    let mantissa = if exponent == 0 {
        (bits & 0xfffffffffffff) << 1
    } else {
        (bits & 0xfffffffffffff) | 0x10000000000000
    };

    exponent -= 1023 + 52;

    (mantissa, exponent, sign)
}

static CLASS_HASH: u64 = 14280250521012660747;
static CLOSURE_HASH: u64 = 9108330286593894302;
static FUNCTION_HASH: u64 = 14516789967880590265;
static INSTANCE_HASH: u64 = 16751306561248198995;
static LIST_HASH: u64 = 6029635940393011764;
static MAP_HASH: u64 = 3238540539993370792;
static METHOD_HASH: u64 = 16751306561248198995;

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Nil, Value::Nil) => true,
            (Value::Number(a), Value::Number(b)) => a == b,
            (Value::Obj(a_ptr), Value::Obj(b_ptr)) => {
                if a_ptr == b_ptr {
                    return true;
                }
                unsafe {
                    match (&a_ptr.deref().obj, &b_ptr.deref().obj) {
                        (Object::String(a), Object::String(b)) => a == b,
                        (Object::List(a), Object::List(b)) => a == b,
                        (Object::Map(a), Object::Map(b)) => a == b,
                        _ => false,
                    }
                }
            },
            _ => false,
        }
    }
}

impl Eq for Value {}

#[allow(clippy::derived_hash_with_manual_eq)]
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Bool(boolean) => boolean.hash(state),
            Value::Nil => 0.hash(state),
            Value::Number(number) => {
                let (mantissa, exponent, sign) = integer_decode(*number);

                mantissa.hash(state);
                exponent.hash(state);
                sign.hash(state);
            },
            Value::Obj(obj_ptr) => unsafe {
                match &obj_ptr.deref().obj {
                    Object::BoundMethod(_) => METHOD_HASH.hash(state),
                    Object::Class(_) => CLASS_HASH.hash(state),
                    Object::Closure(_) => CLOSURE_HASH.hash(state),
                    Object::Function(_) | Object::Native(_) => FUNCTION_HASH.hash(state),
                    Object::Instance(_) => INSTANCE_HASH.hash(state),
                    Object::Iterator(iterator) => {
                        LIST_HASH.hash(state);
                        for item in iterator.items.iter() {
                            item.hash(state);
                        }
                    },
                    Object::List(list) => {
                        LIST_HASH.hash(state);
                        for item in list.iter() {
                            item.hash(state);
                        }
                    },
                    Object::Map(hmap) => {
                        MAP_HASH.hash(state);
                        for (key, val) in hmap.map.iter() {
                            key.hash(state);
                            val.hash(state);
                        }
                    },
                    Object::Module(module) => {
                        MAP_HASH.hash(state);
                        module.name.hash(state);
                        for (key, val) in module.map.iter() {
                            key.hash(state);
                            val.hash(state);
                        }
                    },
                    Object::String(_) => {},
                    Object::UpValue(uv) => uv.location.hash(state),
                }

                match &obj_ptr.deref().obj {
                    Object::List(_) | Object::Map(_) | Object::UpValue(_) => {},
                    _ => obj_ptr.deref().obj.to_string().hash(state),
                }
            },
        };
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
            Value::Number(n) => write!(f, "{}", n),
            Value::Obj(ptr) => unsafe { write!(f, "{}", ptr.deref().obj) },
        }
    }
}

impl Trace for Value {
    fn trace(&self, heap: &mut Heap) {
        if let Value::Obj(ptr) = self {
            heap.mark_object(*ptr);
        }
    }
}
