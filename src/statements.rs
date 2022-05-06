use crate::errors::LoxError;
use crate::expressions::Expression;
use crate::tokens::Token;

pub trait StatementVisitor {
    fn accept_statement(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_expression(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_function(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_if(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_print(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_return(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_var(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_while(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_block(&'_ mut self, stmt: &Statement) -> Result<(), LoxError>;
}

#[derive(Clone, Debug)]
pub enum Statement {
    Expression {
        expression: Box<Expression>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Statement>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Statement>,
        else_branch: Box<Option<Statement>>,
    },
    Print {
        expression: Box<Expression>,
    },
    Return {
        keyword: Token,
        value: Box<Expression>,
    },
    Var {
        name: Token,
        initializer: Option<Box<Expression>>,
    },
    While {
        condition: Box<Expression>,
        body: Box<Statement>,
    },
    Block {
        statements: Vec<Statement>,
    },
}
