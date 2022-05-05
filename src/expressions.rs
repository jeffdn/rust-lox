use std::boxed::Box;

use crate::errors::LoxError;
use crate::tokens;

pub trait Visitor<T> {
    fn accept(&self, expr: Expression) -> Result<T, LoxError>;
    fn visit_binary_expression(&self, expr: Expression) -> Result<T, LoxError>;
    fn visit_grouping_expression(&self, expr: Expression) -> Result<T, LoxError>;
    fn visit_literal_expression(&self, expr: Expression) -> Result<T, LoxError>;
    fn visit_unary_expression(&self, expr: Expression) -> Result<T, LoxError>;
}

#[derive(Clone, Debug)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        operator: tokens::Token,
        right: Box<Expression>,
    },
    Grouping {
        expression: Box<Expression>,
    },
    Literal {
        value: tokens::Literal,
    },
    Unary {
        operator: tokens::Token,
        right: Box<Expression>,
    },
}

pub struct AstPrinter { }

impl Visitor<String> for AstPrinter {
    fn accept(&self, expr: Expression) -> Result<String, LoxError> {
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

    fn visit_binary_expression(&self, expr: Expression) -> Result<String, LoxError> {
        match expr {
            Expression::Binary {
                left,
                operator,
                right,
            } => Ok(
                format!(
                    "({} {} {})",
                    operator.lexeme,
                    self.accept(*left)?,
                    self.accept(*right)?,
                )
            ),
            _ => Err(LoxError::AstError)
        }
    }

    fn visit_grouping_expression(&self, expr: Expression) -> Result<String, LoxError> {
        match expr {
            Expression::Grouping { expression } => Ok(
                format!("(group {})", self.accept(*expression)?)
            ),
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_literal_expression(&self, expr: Expression) -> Result<String, LoxError> {
        match expr {
            Expression::Literal { value } => {
                match value {
                    tokens::Literal::String(string) |
                    tokens::Literal::Identifier(string) => Ok(string.clone()),
                    tokens::Literal::Boolean(literal) => Ok(literal.to_string()),
                    tokens::Literal::Number(literal) => Ok(literal.to_string()),
                    tokens::Literal::Nil => Ok("null".to_string()),
                }
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_unary_expression(&self, expr: Expression) -> Result<String, LoxError> {
        match expr {
            Expression::Unary {
                operator,
                right,
            } => Ok(
                format!(
                    "({} {})",
                    operator.lexeme,
                    self.accept(*right)?,
                )
            ),
            _ => Err(LoxError::AstError)
        }
    }
}

/*
        let expression = Expression::Binary {
            left: Box::new(
                Expression::Unary {
                    operator: Token::new(TokenType::Minus, "-".to_string(), None, 1),
                    right: Box::new(
                        Expression::Literal {
                            value: Literal::Number(123f64),
                        },
                    ),
                },
            ),
            operator: Token::new(TokenType::Star, "*".to_string(), None, 1),
            right: Box::new(
                Expression::Grouping {
                    expression: Box::new(
                        Expression::Literal {
                            value: Literal::Number(45.67),
                        },
                    ),
                },
            ),
        };
*/
