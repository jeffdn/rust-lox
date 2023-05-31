use std::boxed::Box;

use crate::{errors::LoxResult, tokens};

pub trait ExpressionVisitor {
    fn evaluate(&mut self, expr: &Expression) -> LoxResult<()> {
        match expr {
            Expression::Assignment { name, expression } => self.visit_assignment(name, expression),
            Expression::Binary {
                left,
                operator,
                right,
            } => self.visit_binary(left, operator, right),
            Expression::Call {
                callee,
                paren: _,
                arguments,
                invocation,
            } => self.visit_call(callee, arguments, invocation),
            Expression::Get { name, object } => self.visit_get(name, object),
            Expression::Grouping { expression } => self.visit_grouping(expression),
            Expression::Index {
                item,
                is_slice,
                left,
                right,
            } => self.visit_index(
                item,
                is_slice,
                left.as_ref().map(|i| &**i),
                right.as_ref().map(|i| &**i),
            ),
            Expression::IndexedAssignment {
                indexed_item,
                expression,
            } => self.visit_indexed_assignment(indexed_item, expression),
            Expression::List { expressions } => self.visit_list(expressions),
            Expression::Literal { value } => self.visit_literal(value),
            Expression::Logical {
                left,
                operator,
                right,
            } => self.visit_logical(left, operator, right),
            Expression::Map { expressions } => self.visit_map(expressions),
            Expression::Set {
                name,
                object,
                value,
            } => self.visit_set(name, object, value),
            Expression::Super { name, token } => self.visit_super(name, token),
            Expression::This { token } => self.visit_this(token),
            Expression::Unary { operator, right } => self.visit_unary(operator, right),
            Expression::Variable { name } => self.visit_variable(name),
        }
    }

    fn visit_assignment(&mut self, name: &tokens::Token, expression: &Expression) -> LoxResult<()>;
    fn visit_binary(
        &mut self,
        left: &Expression,
        operator: &tokens::Token,
        right: &Expression,
    ) -> LoxResult<()>;
    fn visit_call(
        &mut self,
        callee: &Expression,
        arguments: &[Expression],
        invocation: &bool,
    ) -> LoxResult<()>;
    fn visit_get(&mut self, name: &tokens::Token, object: &Expression) -> LoxResult<()>;
    fn visit_grouping(&mut self, expression: &Expression) -> LoxResult<()>;
    fn visit_index(
        &mut self,
        item: &Expression,
        is_slice: &bool,
        left: Option<&Expression>,
        right: Option<&Expression>,
    ) -> LoxResult<()>;
    fn visit_indexed_assignment(
        &mut self,
        indexed_item: &Expression,
        expression: &Expression,
    ) -> LoxResult<()>;
    fn visit_list(&mut self, expressions: &[Expression]) -> LoxResult<()>;
    fn visit_literal(&mut self, value: &tokens::Literal) -> LoxResult<()>;
    fn visit_logical(
        &mut self,
        left: &Expression,
        operator: &tokens::Token,
        right: &Expression,
    ) -> LoxResult<()>;
    fn visit_map(&mut self, expressions: &[Expression]) -> LoxResult<()>;
    fn visit_set(
        &mut self,
        name: &tokens::Token,
        object: &Expression,
        value: &Expression,
    ) -> LoxResult<()>;
    fn visit_super(&mut self, name: &tokens::Token, token: &tokens::Token) -> LoxResult<()>;
    fn visit_this(&mut self, token: &tokens::Token) -> LoxResult<()>;
    fn visit_unary(&mut self, operator: &tokens::Token, right: &Expression) -> LoxResult<()>;
    fn visit_variable(&mut self, name: &tokens::Token) -> LoxResult<()>;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
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
        arguments: Box<Vec<Expression>>,
        invocation: bool,
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
        is_slice: bool,
        left: Option<Box<Expression>>,
        right: Option<Box<Expression>>,
    },
    IndexedAssignment {
        indexed_item: Box<Expression>,
        expression: Box<Expression>,
    },
    List {
        expressions: Box<Vec<Expression>>,
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
        expressions: Box<Vec<Expression>>,
    },
    Set {
        name: tokens::Token,
        object: Box<Expression>,
        value: Box<Expression>,
    },
    Super {
        token: tokens::Token,
        name: tokens::Token,
    },
    This {
        token: tokens::Token,
    },
    Unary {
        operator: tokens::Token,
        right: Box<Expression>,
    },
    Variable {
        name: tokens::Token,
    },
}
