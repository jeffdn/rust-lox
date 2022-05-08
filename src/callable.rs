use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use crate::environment::{LoxEntity, Environment};
use crate::errors::LoxError;
use crate::interpreter::Interpreter;
use crate::statements::Statement;
use crate::tokens::Literal;

#[derive(Clone, Debug)]
pub enum LoxCallable {
    Function {
        statement: Statement,
        environment: Rc<RefCell<Environment<String, LoxEntity>>>,
    },
}

impl fmt::Display for LoxCallable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            LoxCallable::Function {
                statement,
                environment: _,
            } => match statement {
                Statement::Function {
                    name,
                    params: _,
                    body: _,
                } => format!("function '{}'", name.lexeme),
                _ => "this shouldn't be happening!".to_string(),
            },
        };

        write!(f, "{}", output)
    }
}

impl LoxCallable {
    pub fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<LoxEntity, LoxError> {
        match self {
            LoxCallable::Function {
                statement,
                environment: closure,
            } => match statement {
                Statement::Function {
                    name: _,
                    params,
                    body,
                } => {
                    let runtime_env = Rc::new(
                        RefCell::new(
                            Environment::new(
                                Some(closure.clone())
                            )
                        )
                    );

                    for (ref param, ref argument) in params.iter().zip(arguments) {
                        runtime_env.borrow_mut().define(
                            param.lexeme.clone(),
                            LoxEntity::Literal(argument.clone()),
                        );
                    }

                    match interpreter.execute_block(body, runtime_env.clone()) {
                        Ok(_) => Ok(
                            LoxEntity::Literal(
                                Literal::Nil,
                            )
                        ),
                        Err(e) => match e {
                            LoxError::FunctionReturn(val) => Ok(val),
                            _ => Err(e),
                        },
                    }
                },
                _ => Err(
                    LoxError::RuntimeError(
                        "received invalid callable statement".to_string(),
                    ),
                ),
            },
        }
    }

    pub fn arity(&self) -> Result<usize, LoxError> {
        match self {
            LoxCallable::Function {
                statement,
                environment: _,
            } => match statement {
                Statement::Function {
                    name: _,
                    params,
                    body: _,
                } => Ok(params.len()),
                _ => Err(
                    LoxError::RuntimeError(
                        "received invalid callable statement".to_string(),
                    ),
                ),
            },
        }
    }
}
