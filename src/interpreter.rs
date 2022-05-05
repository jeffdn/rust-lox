use crate::errors::LoxError;
use crate::expressions::{Expression, Visitor};
use crate::tokens::{Literal, TokenType};

pub struct Interpreter { }

impl Interpreter {
    fn evaluate(&self, expr: Expression) -> Result<Literal, LoxError> {
        self.accept(expr)
    }

    fn is_truthy(&self, expr: Expression) -> Result<bool, LoxError> {
        match expr {
            Expression::Literal { value } => match value {
                Literal::String(string) => Ok(string.len() > 0),
                Literal::Number(number) => Ok(number != 0.0f64),
                Literal::Identifier(_todo) => Ok(false),
                Literal::Boolean(boolean) => Ok(boolean),
                Literal::Nil => Ok(false),
            },
            _ => Err(LoxError::AstError),
        }
    }

    pub fn interpret(&self, expr: Expression) -> Result<(), LoxError> {
        let output = self.evaluate(expr)?;
        println!("{}", output);

        Ok(())
    }
}

impl Visitor<Literal> for Interpreter {
    fn accept(&self, expr: Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Binary {
                left: _,
                operator: _,
                right: _,
            } => self.visit_binary_expression(expr),
            Expression::Grouping {
                expression: _,
            } => self.visit_grouping_expression(expr),
            Expression::Literal {
                value: _
            } => self.visit_literal_expression(expr),
            Expression::Unary {
                operator: _,
                right: _,
            } => self.visit_unary_expression(expr),
        }
    }

    fn visit_binary_expression(&self, expr: Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(*left)?;
                let right = self.evaluate(*right)?;

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

    fn visit_grouping_expression(&self, expr: Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Grouping { expression } => self.evaluate(*expression),
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_literal_expression(&self, expr: Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Literal { value } => Ok(value),
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_unary_expression(&self, expr: Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Unary {
                operator,
                right,
            } => match operator.token_type {
                TokenType::Minus => {
                    let right = self.evaluate(*right)?;
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
                    match *right {
                        Expression::Literal { value: _ } => {
                            let truthy = self.is_truthy(*right)?;
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
}
