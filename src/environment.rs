use std::collections::HashMap;

use crate::callable::LoxCallable;
use crate::errors::LoxError;
use crate::tokens::Literal;

#[derive(Clone)]
pub struct Environment<T> {
    pub values: HashMap<String, T>,
}

impl Environment<Literal> {
    pub fn new_literal() -> Environment<Literal> {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, key: String, value: Literal) {
        self.values.insert(key, value);
    }

    pub fn assign(&mut self, key: String, value: Literal) -> Result<(), LoxError>{
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            return Ok(());
        }

        Err(LoxError::RuntimeError(format!("undefined variable '{}'", key)))
    }

    pub fn get(&self, key: &str) -> Result<Literal, LoxError> {
        match self.values.get(key) {
            Some(val) => Ok(val.clone()),
            None =>  Err(
                LoxError::RuntimeError(
                    format!("undefined variable '{}'", key),
                )
            ),
        }
    }
}

impl Environment<LoxCallable> {
    pub fn new_callable() -> Environment<LoxCallable> {
        Environment {
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, key: String, value: LoxCallable) {
        self.values.insert(key, value);
    }

    pub fn assign(&mut self, key: String, value: LoxCallable) -> Result<(), LoxError>{
        if self.values.contains_key(&key) {
            self.values.insert(key, value);
            return Ok(());
        }

        Err(LoxError::RuntimeError(format!("undefined variable '{}'", key)))
    }

    pub fn get(&self, key: &str) -> Result<LoxCallable, LoxError> {
        match self.values.get(key) {
            Some(val) => Ok(val.clone()),
            None =>  Err(
                LoxError::RuntimeError(
                    format!("undefined variable '{}'", key),
                )
            ),
        }
    }
}
