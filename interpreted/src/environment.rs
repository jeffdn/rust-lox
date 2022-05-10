use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;
use std::rc::Rc;

use crate::callable::LoxCallable;
use crate::errors::LoxError;
use crate::statements::Statement;
use crate::tokens::{Literal, Token};

#[derive(Clone, Debug)]
pub struct LoxInstance {
    pub name: Token,
    pub statement: Statement,
    pub fields: HashMap<String, LoxEntity>,
    pub methods: HashMap<String, LoxCallable>,
}

impl LoxInstance {
    pub fn new(
        name: Token,
        statement: Statement,
        methods: HashMap<String, LoxCallable>,
    ) -> LoxInstance {
        LoxInstance {
            name,
            statement,
            fields: HashMap::new(),
            methods,
        }
    }

    pub fn get(&self, key: &str) -> Result<LoxEntity, LoxError> {
        match self.fields.get(key) {
            Some(value) => Ok(value.clone()),
            None => match self.methods.get(key) {
                Some(method) => Ok(
                    LoxEntity::Callable(
                        method.clone()
                    )
                ),
                None => Err(
                    LoxError::RuntimeError(
                        format!("undefined property {}", key)
                    )
                ),
            }
        }
    }

    pub fn set(&mut self, key: String, value: LoxEntity) -> Result<(), LoxError> {
        self.fields.insert(key, value);

        Ok(())
    }
}

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
        match self.values.contains_key(&key) {
            false => match self.parent {
                Some(ref parent) => parent.borrow_mut().assign(key, value),
                None => Err(
                    LoxError::RuntimeError(
                        format!("undefined variable '{:?}' -- too deep!", key)
                    )
                ),
            },
            true => {
                self.values.insert(key, value);
                Ok(())
            },
        }
    }

    pub fn get(&self, key: &K) -> Result<V, LoxError> {
        match self.values.contains_key(key) {
            false => match self.parent {
                Some(ref parent) => parent.borrow().get(key),
                None => Err(
                    LoxError::RuntimeError(
                        format!("undefined variable '{:?}' -- too deep!", key)
                    )
                ),
            },
            true => match self.values.get(key) {
                Some(val) => Ok(val.clone()),
                _ => Err(
                    LoxError::RuntimeError(
                        format!("undefined variable '{:?}'", key)
                    )
                ),
            },
        }
    }

    pub fn get_current_depth(&self, depth: usize) -> usize {
        match self.parent {
            Some(ref parent) => parent.borrow().get_current_depth(depth + 1),
            None => depth,
        }
    }
}
//
        // match self.get(key) {
        //     Some(val) => Ok(val),
        //     None => match self.parent {
        //         Some(ref parent) => parent.borrow().get_at(key, 0),
        //         None => Err(
        //             LoxError::RuntimeError(
        //                 format!("undefined variable '{:?}'", key)
        //             )
        //         ),
        //     }
        // }
