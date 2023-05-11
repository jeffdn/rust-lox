use std::boxed::Box;
use std::collections::BTreeMap;

use crate::{
    errors::LoxError,
    tokens,
};

pub trait ExpressionVisitor<T> {
    fn accept_expression(&mut self, expr: &Expression) -> Result<T, LoxError> {
        match expr {
            Expression::Assignment { .. } => self.visit_assignment(expr),
            Expression::Binary { .. } => self.visit_binary(expr),
            Expression::Call { .. } => self.visit_call(expr),
            Expression::Get { .. } => self.visit_get(expr),
            Expression::Grouping { .. } => self.visit_grouping(expr),
            Expression::Index { .. } => self.visit_index(expr),
            Expression::IndexedAssignment { .. } => self.visit_indexed_assignment(expr),
            Expression::List { .. } => self.visit_list(expr),
            Expression::Literal { .. } => self.visit_literal(expr),
            Expression::Logical { .. } => self.visit_logical(expr),
            Expression::Map { .. } => self.visit_map(expr),
            Expression::Set { .. } => self.visit_set(expr),
            Expression::Unary { .. } => self.visit_unary(expr),
            Expression::Variable { .. } => self.visit_variable(expr),
        }
    }

    fn visit_assignment(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_binary(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_call(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_get(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_grouping(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_index(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_indexed_assignment(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_list(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_literal(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_logical(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_map(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_set(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_unary(&mut self, expr: &Expression) -> Result<T, LoxError>;
    fn visit_variable(&mut self, expr: &Expression) -> Result<T, LoxError>;
}

#[derive(Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub enum Expression {
    Assignment {
        name: tokens::Token,
        expression: Box<Expression>,
    },
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
    Get {
        name: tokens::Token,
        object: Box<Expression>,
    },
    Grouping {
        expression: Box<Expression>,
    },
    Index {
        item: Box<Expression>,
        index: Box<Expression>,
        slice: Option<Box<Expression>>,
    },
    IndexedAssignment {
        indexed_item: Box<Expression>,
        expression: Box<Expression>,
    },
    List {
        expressions: Vec<Expression>,
    },
    Literal {
        value: tokens::Literal,
    },
    Logical {
        left: Box<Expression>,
        operator: tokens::Token,
        right: Box<Expression>,
    },
    Map {
        expression_map: BTreeMap<Expression, Expression>,
    },
    Set {
        name: tokens::Token,
        object: Box<Expression>,
        value: Box<Expression>,
    },
    Unary {
        operator: tokens::Token,
        right: Box<Expression>,
    },
    Variable {
        name: tokens::Token,
    },
}
