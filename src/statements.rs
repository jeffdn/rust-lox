use crate::environment::Environment;
use crate::errors::LoxError;
use crate::expressions::Expression;
use crate::tokens::Token;

pub trait StatementVisitor {
    fn accept_statement(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_expression(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_print(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_var(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_block(&mut self, stmt: &Statement) -> Result<(), LoxError>;
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expression {
        expression: Box<Expression>,
    },
    Print {
        expression: Box<Expression>,
    },
    Var {
        name: Token,
        initializer: Option<Box<Expression>>,
    },
    Block {
        statements: Vec<Statement>,
    },
}
