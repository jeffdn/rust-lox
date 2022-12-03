use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    fmt,
    hash::{Hash, Hasher},
    rc::Rc,
};

use crate::object::Object;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    List(Box<Vec<ValuePtr>>),
    Map(Box<ValueMap>),
    Nil,
    Number(f64),
    Object(Object),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValuePtr(pub Rc<RefCell<Value>>);

impl Eq for ValuePtr {}
impl Hash for ValuePtr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.borrow().hash(state);
    }
}

impl ValuePtr {
    pub fn new(value: Value) -> Self {
        Self(
            Rc::new(
                RefCell::new(value)
            )
        )
    }

    pub fn borrow(&self) -> Ref<'_, Value> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, Value> {
        self.0.borrow_mut()
    }
}

#[derive(Clone, Debug)]
pub struct ValueMap {
    pub map: HashMap<ValuePtr, ValuePtr>,
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

impl Eq for Value {}
impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Bool(boolean) => boolean.hash(state),
            Value::List(list) => {
                LIST_HASH.hash(state);
                for item in list.iter() {
                    item.borrow().hash(state);
                }
            },
            Value::Map(hmap) => {
                MAP_HASH.hash(state);
                for (key, val) in hmap.map.iter() {
                    key.borrow().hash(state);
                    val.borrow().hash(state);
                }
            },
            Value::Nil => 0.hash(state),
            Value::Number(number) => {
                let (mantissa, exponent, sign) = integer_decode(*number);

                mantissa.hash(state);
                exponent.hash(state);
                sign.hash(state);
            },
            Value::Object(object) => {
                match object {
                    Object::BoundMethod(_) => METHOD_HASH.hash(state),
                    Object::Class(_) => CLASS_HASH.hash(state),
                    Object::Closure(_) => CLOSURE_HASH.hash(state),
                    Object::Function(_) | Object::Native(_) => FUNCTION_HASH.hash(state),
                    Object::Instance(_) => INSTANCE_HASH.hash(state),
                    Object::String(_) => {},
                    Object::UpValue(uv) => uv.location.borrow().hash(state),
                };

                match object {
                    Object::UpValue(_) => {},
                    _ => object.to_string().hash(state),
                };
            },
        };
    }
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

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            Value::Bool(boolean) => boolean.to_string(),
            Value::List(list) => format!(
                "[{}]",
                list.iter()
                    .map(|x| x.borrow().to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Value::Map(hmap) => format!(
                "{{{}}}",
                hmap.map
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k.borrow().to_string(), v.borrow().to_string()))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Value::Nil => "nil".into(),
            Value::Number(number) => number.to_string(),
            Value::Object(object) => object.to_string(),
        };

        write!(f, "{}", output)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ValueSet {
    pub values: Vec<ValuePtr>,
}

impl Default for ValueSet {
    fn default() -> Self {
        Self::new()
    }
}

impl ValueSet {
    pub fn new() -> ValueSet {
        ValueSet {
            values: Vec::new(),
        }
    }

    pub fn write(&mut self, value: Value) -> usize {
        self.values.push(ValuePtr::new(value));

        self.values.len() - 1
    }

    pub fn iter(&self) -> impl Iterator<Item = &ValuePtr> {
        self.values.iter()
    }

    pub fn get(&self, index: usize) -> &ValuePtr {
        &self.values[index]
    }
}
