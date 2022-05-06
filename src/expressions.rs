use std::boxed::Box;

use crate::errors::LoxError;
use crate::tokens;

pub trait ExpressionVisitor<T> {
    fn accept_expression(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_binary(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_call(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_grouping(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_literal(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_unary(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_variable(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_assignment(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_logical(&mut self, expr: &Expression) -> Result<T, LoxError>;
}

#[derive(Clone, Debug)]
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
