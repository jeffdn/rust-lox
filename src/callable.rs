use crate::environment::Environment;
use crate::errors::LoxError;
use crate::interpreter::Interpreter;
use crate::statements::Statement;
use crate::tokens::{Literal, Token};

#[derive(Clone, Debug)]
pub enum LoxCallable {
    Function(Statement),
    Class(Statement),
}

impl LoxCallable {
    pub fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: Vec<Literal>,
    ) -> Result<Literal, LoxError> {
        match self {
            LoxCallable::Function(statement) => match statement {
                Statement::Function {
                    name,
                    params,
                    body,
                } => {
                    let mut environment = Environment::new_literal();

                    println!("{:?}", params);
                    for (idx, param) in params.iter().enumerate() {
                        environment.define(
                            param.lexeme.clone(),
                            arguments.get(idx).unwrap().clone(),
                        );
                    }

                    interpreter.execute_block(
                        body,
                        environment,
                        false,
                    )?;


                    Ok(Literal::Nil)
                },
                _ => Err(
                    LoxError::RuntimeError(
                        "received invalid callable statement".to_string(),
                    ),
                ),
            },
            _ => Err(
                LoxError::RuntimeError(
                    "received invalid callable statement".to_string(),
                ),
            ),
        }
    }

    pub fn arity(&self) -> Result<usize, LoxError> {
        match self {
            LoxCallable::Function(statement) => match statement {
                Statement::Function {
                    name,
                    params,
                    body,
                } => Ok(params.len()),
                _ => Err(
                    LoxError::RuntimeError(
                        "received invalid callable statement".to_string(),
                    ),
                ),
            },
            _ => Err(
                LoxError::RuntimeError(
                    "received invalid callable statement".to_string(),
                ),
            ),
        }
    }
}
