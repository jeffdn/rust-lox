use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
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
pub struct Environment<K, V> {
    pub parent: Option<Rc<RefCell<Environment<K, V>>>>,
    pub values: HashMap<K, V>,
}

impl<K: Clone + fmt::Debug + Eq + Hash, V: Clone + fmt::Debug> Environment<K, V> {
    pub fn new(parent: Option<Rc<RefCell<Environment<K, V>>>>) -> Environment<K, V> {
        Environment {
            parent,
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, key: K, value: V) {
        self.values.insert(key.clone(), value);
    }

    pub fn assign(&mut self, key: K, value: V) -> Result<(), LoxError>{
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            return Ok(());
        }

        match self.parent {
            Some(ref parent) => parent.borrow_mut().assign(key, value),
            None => Err(
                LoxError::RuntimeError(
                    format!("undefined variable '{:?}'", key),
                ),
            )
        }
    }

    pub fn assign_at(&mut self, key: K, value: V, depth: usize) -> Result<(), LoxError>{
        match depth > 0 {
            true => match self.parent {
                Some(ref mut parent) => parent.borrow_mut().assign_at(key, value, depth - 1),
                None => Err(
                    LoxError::RuntimeError(
                        format!("undefined variable '{:?}' -- too deep!", key)
                    )
                ),
            },
            false => self.assign(key, value),
        }
    }

    pub fn get(&self, key: &K) -> Option<V> {
        match self.values.get(key) {
            Some(val) => Some(val.clone()),
            None => None,
        }
    }

    pub fn get_at(&self, key: &K, depth: usize) -> Result<V, LoxError> {
        match depth > 1 {
            true => match self.parent {
                Some(ref parent) => parent.borrow().get_at(key, depth - 1),
                None => Err(
                    LoxError::RuntimeError(
                        format!("undefined variable '{:?}' -- too deep!", key)
                    )
                ),
            },
            false => match self.get(key) {
                Some(val) => Ok(val),
                None => Err(
                    LoxError::RuntimeError(
                        format!("undefined variable '{:?}'", key)
                    )
                ),
            },
        }
    }
}
