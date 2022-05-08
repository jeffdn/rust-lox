use std::boxed::Box;

use crate::errors::LoxError;
use crate::tokens;

pub struct Sequence {
    current_value: usize,
}

impl Sequence {
    pub fn new() -> Sequence {
        Sequence {
            current_value: 0,
        }
    }

    pub fn next_value(&mut self) -> usize {
        self.current_value = self.current_value + 1;
        self.current_value
    }
}

pub trait ExpressionVisitor<T> {
    fn accept_expression(&mut self, expr: &Expression) -> Result<T, LoxError> {
        match expr {
            Expression::Binary {
                left: _,
                operator: _,
                right: _,
            } => self.visit_binary(expr),
            Expression::Call {
                callee: _,
                paren: _,
                arguments: _,
            } => self.visit_call(expr),
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
            Expression::Logical {
                left: _,
                operator: _,
                right: _,
            } => self.visit_logical(expr),
        }
    }

    fn visit_binary(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_call(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_grouping(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_literal(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_unary(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_variable(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_assignment(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_logical(&mut self, expr: &Expression) -> Result<T, LoxError>;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        operator: tokens::Token,
        right: Box<Expression>,
    },
    Call {
        callee: Box<Expression>,
        paren: tokens::Token,
        arguments: Vec<Expression>,
    },
    Grouping {
        expression: Box<Expression>,
    },
    Literal {
        value: tokens::Literal,
    },
    Logical {
        left: Box<Expression>,
        operator: tokens::Token,
        right: Box<Expression>,
    },
    Unary {
        operator: tokens::Token,
        right: Box<Expression>,
    },
    Variable {
        name: tokens::Token,
    },
    Assignment {
        name: tokens::Token,
        expression: Box<Expression>,
    },
}
