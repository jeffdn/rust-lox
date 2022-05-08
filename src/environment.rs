use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::callable::LoxCallable;
use crate::errors::LoxError;
use crate::tokens::Literal;

#[derive(Clone, Debug)]
pub enum LoxEntity {
    Callable(LoxCallable),
    Literal(Literal),
}

impl fmt::Display for LoxEntity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            LoxEntity::Callable(callable) => format!("{}", callable),
            LoxEntity::Literal(literal) => format!("{}", literal),
        };

        write!(f, "{}", output)
    }
}

#[derive(Clone, Debug)]
pub struct Environment<T> {
    pub parent: Option<Rc<RefCell<Environment<T>>>>,
    pub values: HashMap<String, T>,
    pub id: usize,
}

static mut COUNTER: usize = 0;

fn counter() -> usize {
    unsafe {
        COUNTER = COUNTER + 1;
        COUNTER.clone()
    }
}

impl Environment<LoxEntity> {
    pub fn new(parent: Option<Rc<RefCell<Environment<LoxEntity>>>>) -> Environment<LoxEntity> {
        Environment {
            parent,
            values: HashMap::new(),
            id: counter(),
        }
    }

    pub fn define(&mut self, key: String, value: LoxEntity) {
        self.values.insert(key.clone(), value);
    }

    pub fn assign(&mut self, key: String, value: LoxEntity) -> Result<(), LoxError>{
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            return Ok(());
        }

        match self.parent {
            Some(ref parent) => parent.borrow_mut().assign(key, value),
            None => Err(
                LoxError::RuntimeError(
                    format!("(env id {:?}) undefined variable '{}'", self.id, key),
                ),
            )
        }
    }

    pub fn get(&self, key: &str) -> Result<LoxEntity, LoxError> {
        match self.values.get(key) {
            Some(val) => {
                Ok(val.clone())
            },
            None => match self.parent {
                Some(ref parent) => parent.borrow_mut().get(key),
                None =>  Err(
                    LoxError::RuntimeError(
                        format!("(env id {:?}) undefined variable '{}'", self.id, key)
                    )
                ),
            },
        }
    }
}
