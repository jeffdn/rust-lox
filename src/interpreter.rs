use std::cell::RefCell;
use std::rc::Rc;

use crate::callable::LoxCallable;
use crate::environment::{LoxEntity, Environment};
use crate::errors::LoxError;
use crate::expressions::{Expression, ExpressionVisitor};
use crate::statements::{Statement, StatementVisitor};
use crate::tokens::{Literal, TokenType, Token};

pub struct Interpreter {
    globals: Rc<RefCell<Environment<String, LoxEntity>>>,
    pub environment: Rc<RefCell<Environment<String, LoxEntity>>>,
    pub locals: Rc<RefCell<Environment<Expression, usize>>>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let inner_globals: Environment<String, LoxEntity> = Environment::new(None);
        let globals = Rc::new(RefCell::new(inner_globals));

        Interpreter {
            globals: globals.clone(),
            environment: globals.clone(),
            locals: Rc::new(RefCell::new(Environment::new(None))),
        }
    }

    fn evaluate(&mut self, expr: &Expression) -> Result<LoxEntity, LoxError> {
        self.accept_expression(expr)
    }

    fn _is_truthy(&self, literal: &Literal) -> Result<bool, LoxError> {
        match literal {
            Literal::String(ref string) => Ok(string.len() > 0),
            Literal::Number(ref number) => Ok(*number != 0.0f64),
            Literal::Identifier(_) => Ok(false),
            Literal::Boolean(ref boolean) => Ok(*boolean),
            Literal::Nil => Ok(false),
        }
    }

    fn is_truthy(&mut self, expr: &Expression) -> Result<bool, LoxError> {
        match expr {
            Expression::Literal { value } => self._is_truthy(&value),
            Expression::Unary {
                operator: _,
                right: _,
            } | Expression::Binary {
                left: _,
                operator: _,
                right: _,
            } | Expression::Logical {
                left: _,
                operator: _,
                right: _,
            } | Expression::Variable {
                name: _,
            } | Expression::Grouping {
                expression: _,
            } => {
                match self.evaluate(expr)? {
                    LoxEntity::Literal(output) => self._is_truthy(&output),
                    LoxEntity::Callable(_) => Ok(true),
                }
            },
            _ => Err(LoxError::AstError),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Statement>) -> Result<(), LoxError> {
        for statement in statements.iter() {
            let _ = self.execute(statement)?;
        }

        Ok(())
    }

    pub fn execute(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        self.accept_statement(stmt)
    }

    pub fn resolve(&mut self, expr: &Expression, index: usize) -> Result<(), LoxError> {
        self.locals.borrow_mut().define(expr.clone(), index);

        Ok(())
    }

    fn look_up_variable(&mut self, name: &Token, expr: &Expression) -> Result<LoxEntity, LoxError> {
        let distance = self.locals.borrow().get(expr);

        match distance {
            Some(distance) => self.environment.borrow().get_at(&name.lexeme, distance),
            None => match self.globals.borrow().get(&name.lexeme) {
                Some(val) => Ok(val),
                None => Err(
                    LoxError::RuntimeError(
                        format!("undefined variable '{}'", name.lexeme)
                    )
                )
            },
        }
    }
}

impl ExpressionVisitor<LoxEntity> for Interpreter {
    fn visit_binary(&mut self, expr: &Expression) -> Result<LoxEntity, LoxError> {
        match expr {
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let eval_left = self.evaluate(&**left)?;
                let eval_right = self.evaluate(&**right)?;

                match (eval_left, eval_right) {
                    (LoxEntity::Literal(left), LoxEntity::Literal(right)) => {
                        match (left, right) {
                            (Literal::Number(left), Literal::Number(right)) => {
                                match operator.token_type {
                                    TokenType::Minus => Ok(
                                        LoxEntity::Literal(
                                            Literal::Number(left - right)
                                        )
                                    ),
                                    TokenType::Slash => Ok(
                                        LoxEntity::Literal(
                                            Literal::Number(left / right)
                                        )
                                    ),
                                    TokenType::Star => Ok(
                                        LoxEntity::Literal(
                                            Literal::Number(left * right)
                                        )
                                    ),
                                    TokenType::Plus => Ok(
                                        LoxEntity::Literal(
                                            Literal::Number(left + right)
                                        )
                                    ),
                                    TokenType::Greater => Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left > right)
                                        )
                                    ),
                                    TokenType::GreaterEqual => Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left >= right)
                                        )
                                    ),
                                    TokenType::Less => Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left < right)
                                        )
                                    ),
                                    TokenType::LessEqual => Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left <= right)
                                        )
                                    ),
                                    TokenType::BangEqual => Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left != right)
                                        )
                                    ),
                                    TokenType::EqualEqual => Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left == right)
                                        )
                                    ),
                                    _ => Err(LoxError::AstError),
                                }
                            },
                            (Literal::String(left), Literal::Number(right)) => {
                                match operator.token_type {
                                    TokenType::Plus => Ok(
                                        LoxEntity::Literal(
                                            Literal::String(
                                                format!("{}{}", left, right),
                                            )
                                        )
                                    ),
                                    _ => Err(LoxError::AstError),
                                }
                            },
                            (Literal::String(left), Literal::String(right)) => {
                                match operator.token_type {
                                    TokenType::Plus => Ok(
                                        LoxEntity::Literal(
                                            Literal::String(
                                                format!("{}{}", left, right),
                                            )
                                        )
                                    ),
                                    TokenType::BangEqual=> Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left != right)
                                        )
                                    ),
                                    TokenType::EqualEqual=> Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left == right)
                                        )
                                    ),
                                    _ => Err(LoxError::AstError),
                                }
                            },
                            (Literal::Boolean(left), Literal::Boolean(right)) => {
                                match operator.token_type {
                                    TokenType::BangEqual=> Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left != right)
                                        )
                                    ),
                                    TokenType::EqualEqual=> Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(left == right)
                                        )
                                    ),
                                    _ => Err(LoxError::AstError),
                                }
                            },
                            (Literal::Nil, Literal::Nil) => {
                                match operator.token_type {
                                    TokenType::BangEqual => Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(false)
                                        )
                                    ),
                                    TokenType::EqualEqual => Ok(
                                        LoxEntity::Literal(
                                            Literal::Boolean(true)
                                        )
                                    ),
                                    _ => Err(LoxError::AstError),
                                }
                            },
                            _ => Err(LoxError::AstError),
                        }
                    },
                    _ => Err(LoxError::AstError),
                }
            },
            _ => Err(LoxError::AstError)
        }
    }

    fn visit_call(&mut self, expr: &Expression) -> Result<LoxEntity, LoxError> {
        match expr {
            Expression::Call {
                callee,
                paren: _,
                arguments,
            } => {
                let mut real_arguments: Vec<Literal> = Vec::new();
                for arg in arguments.iter() {
                    match self.evaluate(arg)? {
                        LoxEntity::Literal(real_arg) => real_arguments.push(real_arg),
                        _ => return Err(LoxError::AstError),
                    };
                }

                let mut callable = match **callee {
                    Expression::Variable {
                        ref name,
                    } => match self.look_up_variable(name, expr)? {
                        LoxEntity::Callable(callable) => callable,
                        _ => return Err(LoxError::AstError),
                    },
                    _ => return Err(LoxError::AstError),
                };

                if real_arguments.len() != callable.arity()? {
                    return Err(
                        LoxError::RuntimeError(
                            format!(
                                "expected {} arguments but got {}",
                                callable.arity()?,
                                real_arguments.len(),
                            )
                        )
                    );
                }

                callable.call(self, real_arguments)
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_grouping(&mut self, expr: &Expression) -> Result<LoxEntity, LoxError> {
        match expr {
            Expression::Grouping { expression } => self.evaluate(&**expression),
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_literal(&mut self, expr: &Expression) -> Result<LoxEntity, LoxError> {
        match expr {
            Expression::Literal { value } => Ok(LoxEntity::Literal(value.clone())),
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_unary(&mut self, expr: &Expression) -> Result<LoxEntity, LoxError> {
        match expr {
            Expression::Unary {
                operator,
                right,
            } => match operator.token_type {
                TokenType::Minus => {
                    match self.evaluate(&**right)? {
                        LoxEntity::Literal(outer_right) => match outer_right {
                            Literal::Number(number) => {
                                Ok(
                                    LoxEntity::Literal(
                                        Literal::Number(-number)
                                    )
                                )
                            },
                            _ => Err(
                                LoxError::TypeError(
                                    format!("expected number, got {}", outer_right)
                                ),
                            ),
                        },
                        _ => Err(
                            LoxError::TypeError(
                                format!("expected number, got something else")
                            ),
                        ),
                    }
                },
                TokenType::Bang => {
                    match **right {
                        Expression::Literal { value: _ } => {
                            let truthy = self.is_truthy(&**right)?;
                            Ok(
                                LoxEntity::Literal(
                                    Literal::Boolean(truthy)
                                )
                            )
                        },
                        _ => Err(
                            LoxError::TypeError(
                                "expected value, got something else".to_string()
                            ),
                        ),
                    }
                },
                _ => Err(LoxError::AstError),
            },
            _ => Err(LoxError::AstError)
        }
    }

    fn visit_variable(&mut self, expr: &Expression) -> Result<LoxEntity, LoxError> {
        match expr {
            Expression::Variable { name } => {
                self.look_up_variable(&name, expr)
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_assignment(&mut self, expr: &Expression) -> Result<LoxEntity, LoxError> {
        match expr {
            Expression::Assignment {
                name,
                expression,
            } => {
                let value = self.evaluate(&**expression)?;
                let distance = self.locals.borrow().get(expr);

                match distance {
                    Some(distance) => self.environment.borrow_mut().assign_at(
                        name.lexeme.clone(),
                        value.clone(),
                        distance,
                    )?,
                    None => self.globals.borrow_mut().assign(
                        name.lexeme.clone(),
                        value.clone(),
                    )?,
                };

                Ok(value)
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_logical(&mut self, expr: &Expression) -> Result<LoxEntity, LoxError> {
        match expr {
            Expression::Logical {
                left,
                operator,
                right,
            } => {
                match self.evaluate(&**left)? {
                    LoxEntity::Literal(inner) => {
                        let truthy_left = self.is_truthy(
                            &Expression::Literal {
                                value: inner.clone(),
                            }
                        )?;

                        match (truthy_left, &operator.token_type) {
                            (true, TokenType::Or) => Ok(
                                LoxEntity::Literal(
                                    inner.clone()
                                )
                            ),
                            (false, TokenType::And) => Ok(
                                LoxEntity::Literal(
                                    inner.clone()
                                )
                            ),
                            _ => Ok(self.evaluate(right)?),
                        }
                    },
                    _ => return Err(LoxError::AstError),
                }
            },
            _ => Err(LoxError::AstError),
        }
    }
}

impl StatementVisitor for Interpreter {
    fn visit_expression(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Expression {
                expression,
            } => {
                let _ = self.evaluate(&**expression)?;
                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_function(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Function {
                name,
                params: _,
                body: _,
            } => {
                let callable = LoxCallable::Function {
                    statement: stmt.clone(),
                    environment: Rc::new(
                        RefCell::new(
                            Environment::new(
                                Some(self.environment.clone()),
                            )
                        )
                    ),
                };

                self.environment.borrow_mut().define(
                    name.lexeme.clone(),
                    LoxEntity::Callable(callable),
                );

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_print(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Print {
                expression,
            } => {
                let value = self.evaluate(&**expression)?;
                println!("{}", value);

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_return(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Return {
                keyword: _,
                value,
            } => {
                let value = self.evaluate(value)?;
                Err(
                    LoxError::FunctionReturn(
                        value
                    )
                )
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_while(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::While {
                condition,
                body,
            } => {
                while self.is_truthy(&**condition)? {
                    self.execute(body)?;
                }

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_var(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Var {
                name,
                initializer,
            } => {
                let value = match initializer {
                    Some(init) => self.evaluate(&**init)?,
                    None => LoxEntity::Literal(Literal::Nil),
                };

                self.environment.borrow_mut().define(
                    name.lexeme.clone(), value,
                );

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_if(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                match self.is_truthy(&**condition)? {
                    true => self.execute(then_branch)?,
                    false => match else_branch {
                        Some(eb) => {
                            self.execute(&**eb)?
                        },
                        None => {},
                    }
                };

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_block(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Block {
                statements,
            } => {
                let new_env = Rc::new(
                    RefCell::new(
                        Environment::new(Some(self.environment.clone()))
                    )
                );
                self.execute_block(statements, new_env)?;
            },
            _ => return Err(LoxError::AstError),
        };

        Ok(())
    }
}

impl Interpreter {
    pub fn execute_block(
        &mut self,
        statements: &[Statement],
        working_env: Rc<RefCell<Environment<String, LoxEntity>>>,
    ) -> Result<(), LoxError> {
        let previous = self.environment.clone();
        self.environment = working_env;

        let mut statement_iter = statements.iter();
        let mut output: Result<(), LoxError> = Ok(());

        loop {
            let next_statement = statement_iter.next();

            match next_statement {
                None => break,
                Some(statement) => match self.execute(statement) {
                    Ok(_) => continue,
                    Err(e) => {
                        output = Err(e);
                        break;
                    },
                },
            };
        }

        self.environment = previous;

        output
    }
}
