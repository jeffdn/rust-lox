use std::{
    cell::RefCell,
    fmt,
    rc::Rc,
};

use crate::object::Object;

#[derive(Clone, Debug)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
    Object(Object),
}

pub type ValuePtr = Rc<RefCell<Value>>;

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            Value::Nil => "nil".into(),
            Value::Number(number) => number.to_string(),
            Value::Bool(boolean) => boolean.to_string(),
            Value::Object(object) => object.to_string(),
        };

        write!(f, "{}", output)
    }
}

#[derive(Clone, Debug)]
pub struct ValueSet {
    pub values: Vec<ValuePtr>,
}

impl ValueSet {
    pub fn new() -> ValueSet {
        ValueSet {
            values: Vec::new(),
        }
    }

    pub fn write(&mut self, value: Value) -> usize {
        self.values.push(Rc::new(RefCell::new(value)));

        self.values.len() - 1
    }

    pub fn iter(&self) -> impl Iterator<Item = &ValuePtr> {
        self.values.iter()
    }

    pub fn get(&self, index: usize) -> &ValuePtr {
        &self.values[index]
    }
}
