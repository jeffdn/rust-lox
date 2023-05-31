use crate::{errors::LoxResult, expressions::Expression, tokens::Token};

pub trait StatementVisitor {
    fn execute(&mut self, stmt: &Statement) -> LoxResult<()> {
        match stmt {
            Statement::Assert {
                expression,
                message,
            } => self.visit_assert(&*expression, message.as_ref().map(|i| &**i)),
            Statement::Block { statements } => self.visit_block(&*statements),
            Statement::Break => self.visit_break(),
            Statement::Class {
                name,
                superclass,
                methods,
            } => self.visit_class(name, superclass.as_ref(), &*methods),
            Statement::Continue => self.visit_continue(),
            Statement::Delete { expression } => self.visit_delete(&*expression),
            Statement::Expression { expression } => self.visit_expression(&*expression),
            Statement::For {
                initializer,
                condition,
                increment,
                body,
            } => self.visit_for(
                initializer.as_ref().map(|i| &**i),
                condition.as_ref().map(|i| &**i),
                increment.as_ref().map(|i| &**i),
                &*body,
            ),
            Statement::Foreach {
                iterator,
                iterable,
                body,
            } => self.visit_foreach(iterator, &*iterable, &*body),
            Statement::Function { name, params, body } => {
                self.visit_function(name, &*params, &*body)
            },
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => self.visit_if(
                &*condition,
                &*then_branch,
                else_branch.as_ref().map(|i| &**i),
            ),
            Statement::Print {
                newline,
                expression,
            } => self.visit_print(newline, &*expression),
            Statement::Return { keyword: _, value } => {
                self.visit_return(value.as_ref().map(|i| &**i))
            },
            Statement::Var { name, initializer } => {
                self.visit_var(name, initializer.as_ref().map(|i| &**i))
            },
            Statement::While { condition, body } => self.visit_while(&*condition, &*body),
        }
    }

    fn visit_assert(
        &mut self,
        expression: &Expression,
        message: Option<&Expression>,
    ) -> LoxResult<()>;
    fn visit_block(&mut self, statements: &[Statement]) -> LoxResult<()>;
    fn visit_break(&mut self) -> LoxResult<()>;
    fn visit_class(
        &mut self,
        name: &Token,
        superclass: Option<&Token>,
        methods: &[Statement],
    ) -> LoxResult<()>;
    fn visit_continue(&mut self) -> LoxResult<()>;
    fn visit_delete(&mut self, expression: &Expression) -> LoxResult<()>;
    fn visit_expression(&mut self, expression: &Expression) -> LoxResult<()>;
    fn visit_for(
        &mut self,
        initializer: Option<&Statement>,
        condition: Option<&Expression>,
        increment: Option<&Expression>,
        body: &Statement,
    ) -> LoxResult<()>;
    fn visit_foreach(
        &mut self,
        iterator: &Token,
        iterable: &Expression,
        body: &Statement,
    ) -> LoxResult<()>;
    fn visit_function(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Statement],
    ) -> LoxResult<()>;
    fn visit_if(
        &mut self,
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) -> LoxResult<()>;
    fn visit_print(&mut self, newline: &bool, expression: &Expression) -> LoxResult<()>;
    fn visit_return(&mut self, value: Option<&Expression>) -> LoxResult<()>;
    fn visit_var(&mut self, name: &Token, initializer: Option<&Expression>) -> LoxResult<()>;
    fn visit_while(&mut self, condition: &Expression, body: &Statement) -> LoxResult<()>;
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Statement {
    Assert {
        expression: Box<Expression>,
        message: Option<Box<Expression>>,
    },
    Block {
        statements: Box<Vec<Statement>>,
    },
    Break,
    Class {
        name: Token,
        superclass: Option<Token>,
        methods: Box<Vec<Statement>>,
    },
    Continue,
    Delete {
        expression: Box<Expression>,
    },
    Expression {
        expression: Box<Expression>,
    },
    For {
        initializer: Option<Box<Statement>>,
        condition: Option<Box<Expression>>,
        increment: Option<Box<Expression>>,
        body: Box<Statement>,
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
