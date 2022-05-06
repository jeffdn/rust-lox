use crate::environment::Environment;
use crate::errors::LoxError;
use crate::interpreter::Interpreter;
use crate::statements::Statement;
use crate::tokens::Literal;

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
                    name: _,
                    params,
                    body,
                } => {
                    let mut environment = Environment::new_literal();

                    for (idx, param) in params.iter().enumerate() {
                        environment.define(
                            param.lexeme.clone(),
                            arguments.get(idx).unwrap().clone(),
                        );
                    }

                    match interpreter.execute_block(body, environment, false) {
                        Ok(_) => Ok(Literal::Nil),
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
            _ => Err(
                LoxError::RuntimeError(
                    "received invalid callable statement".to_string(),
                ),
            ),
        }
    }
}
