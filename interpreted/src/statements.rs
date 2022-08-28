use crate::{
    errors::LoxError,
    expressions::Expression,
    tokens::Token,
};

pub trait StatementVisitor {
    fn accept_statement(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Block { .. } => self.visit_block(stmt),
            Statement::Class { .. } => self.visit_class(stmt),
            Statement::Expression { .. } => self.visit_expression(stmt),
            Statement::Foreach { .. } => self.visit_foreach(stmt),
            Statement::Function { .. } => self.visit_function(stmt),
            Statement::If { .. } => self.visit_if(stmt),
            Statement::Print { .. } => self.visit_print(stmt),
            Statement::Return { .. } => self.visit_return(stmt),
            Statement::While { .. } => self.visit_while(stmt),
            Statement::Var { .. } => self.visit_var(stmt),
        }
    }

    fn visit_block(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_class(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_expression(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_foreach(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_function(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_if(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_print(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_return(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_var(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_while(&mut self, stmt: &Statement) -> Result<(), LoxError>;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Block {
        statements: Vec<Statement>,
    },
    Class {
        name: Token,
        methods: Vec<Statement>,
    },
    Expression {
        expression: Box<Expression>,
    },
    Foreach {
        iterator: Token,
        iterable: Box<Expression>,
        body: Box<Statement>,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Statement>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
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
}
