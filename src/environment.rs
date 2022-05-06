use std::boxed::Box;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::errors::LoxError;
use crate::tokens::Literal;

pub struct Environment {
    enclosing: Option<Rc<RefCell<Box<Environment>>>>,
    values: HashMap<String, Literal>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn set_enclosing(&mut self, env: Rc<RefCell<Box<Environment>>>) {
        self.enclosing = Some(env);
    }

    pub fn define(&mut self, key: String, value: Literal) {
        self.values.insert(key, value);
    }

    pub fn assign(&mut self, key: String, value: Literal) -> Result<(), LoxError>{
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            return Ok(());
        } else if let Some(ref mut enclosing) = self.enclosing {
            return enclosing.borrow_mut().assign(key, value);
        }

        Err(LoxError::RuntimeError(format!("undefined variable '{}'", key)))
    }

    pub fn get(&self, key: String) -> Result<Literal, LoxError> {
        match self.values.get(&key) {
            Some(val) => Ok(val.clone()),
            None => match self.enclosing {
                Some(ref enclosing) => enclosing.borrow_mut().get(key),
                None => Err(
                    LoxError::RuntimeError(
                        format!("undefined variable '{}'", key),
                    )
                ),
            },
        }
    }
}
