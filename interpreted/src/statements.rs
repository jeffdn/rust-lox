use crate::errors::LoxError;
use crate::expressions::Expression;
use crate::tokens::Token;

pub trait StatementVisitor {
    fn accept_statement(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Block {
                statements: _,
            } => self.visit_block(stmt),
            Statement::Class {
                name: _,
                methods: _,
            } => self.visit_class(stmt),
            Statement::Expression {
                expression: _,
            } => self.visit_expression(stmt),
            Statement::Function {
                name: _,
                params: _,
                body: _,
            } => self.visit_function(stmt),
            Statement::If {
                condition: _,
                then_branch: _,
                else_branch: _,
            } => self.visit_if(stmt),
            Statement::Print {
                expression: _,
            } => self.visit_print(stmt),
            Statement::Return {
                keyword: _,
                value: _,
            } => self.visit_return(stmt),
            Statement::While {
                condition: _,
                body: _,
            } => self.visit_while(stmt),
            Statement::Var {
                name: _,
                initializer: _,
            } => self.visit_var(stmt),
        }
    }

    fn visit_block(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_class(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_expression(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_function(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_if(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_print(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_return(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_var(&mut self, stmt: &Statement) -> Result<(), LoxError>;
    fn visit_while(&mut self, stmt: &Statement) -> Result<(), LoxError>;
}

#[derive(Clone, Debug)]
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
