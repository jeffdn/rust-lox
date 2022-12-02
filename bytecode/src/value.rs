use std::{
    cell::RefCell,
    fmt,
    rc::Rc,
};

use crate::object::Object;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Nil,
    Number(f64),
    Bool(bool),
    Object(Object),
    List(Box<Vec<ValuePtr>>),
}

pub type ValuePtr = Rc<RefCell<Value>>;

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
