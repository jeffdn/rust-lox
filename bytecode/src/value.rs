use std::fmt;

use crate::object::Object;


#[derive(Clone)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
    Object(Object),
}

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

#[derive(Clone)]
pub struct ValueSet {
    pub values: Vec<Value>,
}

impl ValueSet {
    pub fn new() -> ValueSet {
        ValueSet {
            values: Vec::new(),
        }
    }

    pub fn write(&mut self, value: Value) -> usize {
        self.values.push(value);

        self.values.len() - 1
    }

    pub fn iter(&self) -> impl Iterator<Item = &Value> {
        self.values.iter()
    }

    pub fn get(&self, index: usize) -> &Value {
        &self.values[index]
    }
}
