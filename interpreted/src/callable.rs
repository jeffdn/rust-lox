use std::{
    cell::RefCell,
    fmt,
    rc::Rc,
};

use crate::{
    environment::{Environment, LoxEntity, LoxInstance},
    errors::LoxError,
    interpreter::Interpreter,
    statements::Statement,
    tokens::Literal,
};

#[derive(Clone, Debug, PartialEq)]
pub enum LoxCallable {
    Function {
        statement: Statement,
        environment: Rc<RefCell<Environment<String, LoxEntity>>>,
    },
    Class {
        class: LoxInstance,
    },

    // Built-in functions
    Len,
    Type,
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
            LoxCallable::Class { class } => format!("class '{}'", class.name.lexeme),
            LoxCallable::Len => "function 'len'".to_string(),
            LoxCallable::Type => "function 'type'".to_string(),
        };

        write!(f, "{}", output)
    }
}

impl LoxCallable {
    pub fn call(
        &mut self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxEntity>,
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
                            argument.clone(),
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
            LoxCallable::Class { class } => Ok(
                LoxEntity::Callable(
                    LoxCallable::Class {
                        class: class.clone(),
                    }
                ),
            ),
            LoxCallable::Len => {
                match &arguments[0] {
                    LoxEntity::List(list) => Ok(
                        LoxEntity::Literal(
                            Literal::Number(list.len() as f64)
                        )
                    ),
                    LoxEntity::Map(map) => Ok(
                        LoxEntity::Literal(
                            Literal::Number(map.len() as f64)
                        )
                    ),
                    LoxEntity::Literal(Literal::String(string)) => Ok(
                        LoxEntity::Literal(
                            Literal::Number(string.len() as f64)
                        )
                    ),
                    _ => Err(
                        LoxError::RuntimeError(
                            "len() only accepts lists, maps, and strings".to_string(),
                        ),
                    ),
                }
            },
            LoxCallable::Type => {
                match &arguments[0] {
                    LoxEntity::List(_) => Ok(
                        LoxEntity::Literal(
                            Literal::String("list".into())
                        )
                    ),
                    LoxEntity::Map(_) => Ok(
                        LoxEntity::Literal(
                            Literal::String("map".into())
                        )
                    ),
                    LoxEntity::Literal(literal) => match literal {
                        Literal::String(_) => Ok(
                            LoxEntity::Literal(
                                Literal::String("str".into())
                            )
                        ),
                        Literal::Number(_) => Ok(
                            LoxEntity::Literal(
                                Literal::String("num".into())
                            )
                        ),
                        Literal::Boolean(_) => Ok(
                            LoxEntity::Literal(
                                Literal::String("bool".into())
                            )
                        ),
                        Literal::Nil => Ok(
                            LoxEntity::Literal(
                                Literal::String("null".into())
                            )
                        ),
                        _ => return Err(
                            LoxError::RuntimeError(
                                "arguments not properly evaluated".to_string()
                            )
                        ),
                    },
                    LoxEntity::Callable(callable) => match callable {
                        LoxCallable::Len | LoxCallable::Type => Ok(
                            LoxEntity::Literal(
                                Literal::String("built-in function".into()),
                            )
                        ),
                        LoxCallable::Function { statement: _, environment: _, } => Ok(
                            LoxEntity::Literal(
                                Literal::String("function".into())
                            )
                        ),
                        LoxCallable::Class { class: _ } => Ok(
                            LoxEntity::Literal(
                                Literal::String("class".into())
                            )
                        ),
                    },
                }
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
            LoxCallable::Class { class: _ } => Ok(0),
            LoxCallable::Len => Ok(1),
            LoxCallable::Type => Ok(1),
        }
    }
}
