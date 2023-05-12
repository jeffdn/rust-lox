use crate::{
    errors::{LoxError, LoxResult},
    expressions::Expression,
    tokens::Token,
};

pub trait StatementVisitor {
    fn accept_statement(&mut self, stmt: &Statement) -> LoxResult<()> {
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

    fn visit_block(&mut self, stmt: &Statement) -> LoxResult<()>;
    fn visit_class(&mut self, stmt: &Statement) -> LoxResult<()>;
    fn visit_expression(&mut self, stmt: &Statement) -> LoxResult<()>;
    fn visit_foreach(&mut self, stmt: &Statement) -> LoxResult<()>;
    fn visit_function(&mut self, stmt: &Statement) -> LoxResult<()>;
    fn visit_if(&mut self, stmt: &Statement) -> LoxResult<()>;
    fn visit_print(&mut self, stmt: &Statement) -> LoxResult<()>;
    fn visit_return(&mut self, stmt: &Statement) -> LoxResult<()>;
    fn visit_var(&mut self, stmt: &Statement) -> LoxResult<()>;
    fn visit_while(&mut self, stmt: &Statement) -> LoxResult<()>;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Block {
        statements: Box<Vec<Statement>>,
    },
    Class {
        name: Token,
        methods: Box<Vec<Statement>>,
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
        params: Box<Vec<Token>>,
        body: Box<Vec<Statement>>,
    },
    If {
        condition: Box<Expression>,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    Print {
        newline: bool,
        expression: Box<Expression>,
    },
    Return {
        keyword: Token,
        value: Option<Box<Expression>>,
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
