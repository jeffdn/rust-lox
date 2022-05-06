use std::boxed::Box;
use std::cell::RefCell;
use std::rc::Rc;

use crate::environment::Environment;
use crate::errors::LoxError;
use crate::expressions::{Expression, ExpressionVisitor};
use crate::statements::{Statement, StatementVisitor};
use crate::tokens::{Literal, TokenType};

pub struct Interpreter {
    environment: Rc<RefCell<Box<Environment>>>,
}

impl Interpreter {
    pub fn new(env: Option<Rc<RefCell<Box<Environment>>>>) -> Interpreter {
        Interpreter {
            environment: match env {
                Some(env) => env,
                None => Rc::new(RefCell::new(Box::new(Environment::new()))),
            }
        }
    }

    fn evaluate(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        self.accept_expression(expr)
    }

    fn is_truthy(&mut self, expr: &Expression) -> Result<bool, LoxError> {
        match expr {
            Expression::Literal { value } => match value {
                Literal::String(string) => Ok(string.len() > 0),
                Literal::Number(number) => Ok(*number != 0.0f64),
                Literal::Identifier(_todo) => Ok(false),
                Literal::Boolean(boolean) => Ok(*boolean),
                Literal::Nil => Ok(false),
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
}

impl ExpressionVisitor<Literal> for Interpreter {
    fn accept_expression(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Binary {
                left: _,
                operator: _,
                right: _,
            } => self.visit_binary(expr),
            Expression::Grouping {
                expression: _,
            } => self.visit_grouping(expr),
            Expression::Literal {
                value: _
            } => self.visit_literal(expr),
            Expression::Unary {
                operator: _,
                right: _,
            } => self.visit_unary(expr),
            Expression::Variable {
                name: _,
            } => self.visit_variable(expr),
            Expression::Assignment {
                name: _,
                expression: _,
            } => self.visit_assignment(expr),
        }
    }

    fn visit_binary(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(&**left)?;
                let right = self.evaluate(&**right)?;

                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => {
                        match operator.token_type {
                            TokenType::Minus => Ok(Literal::Number(left - right)),
                            TokenType::Slash => Ok(Literal::Number(left / right)),
                            TokenType::Star => Ok(Literal::Number(left * right)),
                            TokenType::Plus => Ok(Literal::Number(left + right)),
                            TokenType::Greater => Ok(Literal::Boolean(left > right)),
                            TokenType::GreaterEqual => Ok(Literal::Boolean(left >= right)),
                            TokenType::Less => Ok(Literal::Boolean(left < right)),
                            TokenType::LessEqual=> Ok(Literal::Boolean(left <= right)),
                            TokenType::BangEqual=> Ok(Literal::Boolean(left != right)),
                            TokenType::EqualEqual=> Ok(Literal::Boolean(left == right)),
                            _ => Err(LoxError::AstError),
                        }
                    },
                    (Literal::String(left), Literal::String(right)) => {
                        match operator.token_type {
                            TokenType::Plus => Ok(
                                Literal::String(
                                    format!("{}{}", left, right),
                                )
                            ),
                            TokenType::BangEqual=> Ok(Literal::Boolean(left != right)),
                            TokenType::EqualEqual=> Ok(Literal::Boolean(left == right)),
                            _ => Err(LoxError::AstError),
                        }
                    },
                    (Literal::Boolean(left), Literal::Boolean(right)) => {
                        match operator.token_type {
                            TokenType::BangEqual=> Ok(Literal::Boolean(left != right)),
                            TokenType::EqualEqual=> Ok(Literal::Boolean(left == right)),
                            _ => Err(LoxError::AstError),
                        }
                    },
                    (Literal::Nil, Literal::Nil) => {
                        match operator.token_type {
                            TokenType::BangEqual => Ok(Literal::Boolean(false)),
                            TokenType::EqualEqual => Ok(Literal::Boolean(true)),
                            _ => Err(LoxError::AstError),
                        }
                    },
                    _ => Err(LoxError::AstError),
                }
            },
            _ => Err(LoxError::AstError)
        }
    }

    fn visit_grouping(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Grouping { expression } => self.evaluate(&**expression),
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_literal(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Literal { value } => Ok(value.clone()),
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_unary(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Unary {
                operator,
                right,
            } => match operator.token_type {
                TokenType::Minus => {
                    let right = self.evaluate(&**right)?;
                    match right {
                        Literal::Number(number) => {
                            Ok(Literal::Number(-number))
                        },
                        _ => Err(
                            LoxError::TypeError(
                                format!("expected number, got {}", right)
                            ),
                        ),
                    }
                },
                TokenType::Bang => {
                    match **right {
                        Expression::Literal { value: _ } => {
                            let truthy = self.is_truthy(&**right)?;
                            Ok(Literal::Boolean(truthy))
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

    fn visit_variable(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Variable { name } => {
                (*(Rc::get_mut(&mut self.environment).unwrap().borrow_mut())).get(name.lexeme.clone())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_assignment(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Assignment {
                name,
                expression,
            } => {
                let value = self.evaluate(&**expression)?;
                (*(Rc::get_mut(&mut self.environment).unwrap().borrow_mut())).assign(name.lexeme.clone(), value.clone())?;

                Ok(value)
            },
            _ => Err(LoxError::AstError),
        }
    }
}

impl StatementVisitor for Interpreter {
    fn accept_statement(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Expression {
                expression: _,
            } => self.visit_expression(stmt),
            Statement::Print {
                expression: _,
            } => self.visit_print(stmt),
            Statement::Var {
                name: _,
                initializer: _,
            } => self.visit_var(stmt),
            Statement::Block {
                statements: _,
            } => self.visit_block(stmt),
        }
    }

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

    fn visit_var(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Var {
                name,
                initializer,
            } => {
                let value = match initializer {
                    Some(init) => self.evaluate(&**init)?,
                    None => Literal::Nil,
                };

                (*(Rc::get_mut(&mut self.environment).unwrap().borrow_mut())).define(name.lexeme.clone(), value);

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_block(
        &mut self,
        stmt: &Statement,
    ) -> Result<(), LoxError> {
        let mut tmp_environment = Rc::new(RefCell::new(Box::new(Environment::new())));
        (*(*Rc::get_mut(&mut tmp_environment).unwrap().borrow_mut())).set_enclosing(self.environment.clone());
        let mut tmp_interpreter = Interpreter::new(Some(tmp_environment));

        match stmt {
            Statement::Block {
                statements,
            } => {
                for statement in statements.iter() {
                    tmp_interpreter.execute(statement)?;
                }

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }
}
