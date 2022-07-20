use std::boxed::Box;
use std::collections::BTreeMap;

use crate::{
    errors::LoxError,
    tokens,
};

pub trait ExpressionVisitor<T> {
    fn accept_expression(&mut self, expr: &Expression) -> Result<T, LoxError> {
        match expr {
            Expression::Assignment {
                name: _,
                expression: _,
            } => self.visit_assignment(expr),
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
            Expression::Get { name: _, object: _ } => self.visit_get(expr),
            Expression::Grouping { expression: _ } => self.visit_grouping(expr),
            Expression::Index { item: _, index: _ } => self.visit_index(expr),
            Expression::IndexedAssignment {
                indexed_item: _,
                expression: _,
            } => self.visit_indexed_assignment(expr),
            Expression::List { expressions: _ } => self.visit_list(expr),
            Expression::Literal { value: _ } => self.visit_literal(expr),
            Expression::Logical {
                left: _,
                operator: _,
                right: _,
            } => self.visit_logical(expr),
            Expression::Map { expression_map: _ } => self.visit_map(expr),
            Expression::Set {
                name: _,
                object: _,
                value: _,
            } => self.visit_set(expr),
            Expression::Unary {
                operator: _,
                right: _,
            } => self.visit_unary(expr),
            Expression::Variable { name: _ } => self.visit_variable(expr),
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
