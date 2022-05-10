use std::collections::HashMap;

use crate::errors::LoxError;
use crate::expressions::{Expression, ExpressionVisitor};
use crate::interpreter::Interpreter;
use crate::statements::{Statement, StatementVisitor};
use crate::tokens::Token;

#[derive(Clone, Copy)]
pub enum FunctionType {
    Function,
    Method,
}

pub struct Resolver {
    pub interpreter: Interpreter,
    function_type: Option<FunctionType>,
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    pub fn new() -> Resolver {
        Resolver {
            interpreter: Interpreter::new(),
            function_type: None,
            scopes: Vec::new(),
        }
    }

    fn begin_scope(&mut self) -> Result<(), LoxError> {
        self.scopes.push(HashMap::new());

        Ok(())
    }

    fn end_scope(&mut self) -> Result<(), LoxError> {
        self.scopes.pop().unwrap();

        Ok(())
    }

    pub fn resolve(&mut self, statements: &[Statement]) -> Result<(), LoxError> {
        for statement in statements {
            self.resolve_statement(statement)?;
        }

        Ok(())
    }

    fn resolve_statement(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        self.accept_statement(stmt)
    }

    fn resolve_expression(&mut self, expr: &Expression) -> Result<(), LoxError> {
        self.accept_expression(expr)
    }

    fn declare(&mut self, name: &Token) -> Result<(), LoxError>  {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let scope = self.scopes.last_mut()
            .ok_or(
                LoxError::ResolutionError(
                    "can't declare a variable without a scope!".to_string()
                )
            )?;

        if scope.contains_key(&name.lexeme) {
            return Err(
                LoxError::ResolutionError(
                    "already a variable with this name in this scope".to_string()
                )
            );
        }

        scope.entry(name.lexeme.clone()).or_insert(false);

        Ok(())
    }

    fn define(&mut self, name: &Token) -> Result<(), LoxError> {
        if self.scopes.is_empty() {
            return Ok(());
        }

        let scope = self.scopes.last_mut()
            .ok_or(
                LoxError::ResolutionError(
                    "can't define a variable without a scope!".to_string()
                )
            )?;

        scope.entry(name.lexeme.clone()).and_modify(|v| *v = true);

        Ok(())
    }

    fn resolve_local(&mut self, expr: &Expression, name: &Token) -> Result<(), LoxError> {
        for (idx, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                self.interpreter.resolve(expr, self.scopes.len() - idx - 1)?;
                break;
            }
        }

        Ok(())
    }

    fn resolve_function(
        &mut self, stmt: &Statement, function_type: FunctionType,
    ) -> Result<(), LoxError> {
        match stmt {
            Statement::Function {
                name: _,
                params,
                body,
            } => {
                let enclosing_function: Option<FunctionType> = self.function_type;
                self.function_type = Some(function_type);

                self.begin_scope()?;

                for param in params.iter() {
                    self.declare(param)?;
                    self.define(param)?;
                }

                self.resolve(body)?;
                self.end_scope()?;

                self.function_type = enclosing_function;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to resolve a non-function statement".to_string(),
                )
            ),
        }
    }
}


impl ExpressionVisitor<()> for Resolver {
    fn visit_assignment(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Assignment {
                name,
                expression,
            } =>  {
                self.resolve_expression(&**expression)?;
                self.resolve_local(expr, name)?;
                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit assignment with a non-assignment expression".to_string()
                )
            ),
        }
    }

    fn visit_binary(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Binary {
                left,
                operator: _,
                right,
            } =>  {
                self.resolve_expression(&*left)?;
                self.resolve_expression(&*right)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit binary with a non-binary expression".to_string()
                )
            ),
        }
    }

    fn visit_call(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Call {
                callee,
                paren: _,
                arguments,
            } =>  {
                self.resolve_expression(&*callee)?;

                for argument in arguments.iter() {
                    self.resolve_expression(argument)?;
                }

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit call with a non-call expression".to_string()
                )
            ),
        }
    }

    fn visit_get(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Get {
                name: _,
                object,
            } =>  {
                self.resolve_expression(&*object)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit grouping with a non-grouping expression".to_string()
                )
            ),
        }
    }

    fn visit_grouping(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Grouping {
                expression,
            } =>  {
                self.resolve_expression(&*expression)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit grouping with a non-grouping expression".to_string()
                )
            ),
        }
    }

    fn visit_literal(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Literal { value: _ } => { Ok(()) },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit literal with a non-literal expression".to_string()
                )
            ),
        }
    }

    fn visit_logical(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Logical {
                left,
                operator: _,
                right,
            } =>  {
                self.resolve_expression(&*left)?;
                self.resolve_expression(&*right)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit logical with a non-logical expression".to_string()
                )
            ),
        }
    }

    fn visit_set(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Set {
                name: _,
                object,
                value,
            } =>  {
                self.resolve_expression(&*value)?;
                self.resolve_expression(&*object)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit grouping with a non-grouping expression".to_string()
                )
            ),
        }
    }

    fn visit_unary(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Unary {
                operator: _,
                right,
            } =>  {
                self.resolve_expression(&*right)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit unary with a non-unary expression".to_string()
                )
            ),
        }
    }

    fn visit_variable(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Variable {
                name,
            } => {
                if !self.scopes.is_empty() {
                    match self.scopes.last().unwrap().get(&name.lexeme) {
                        Some(false) => return Err(
                            LoxError::ResolutionError(
                                format!(
                                    "can't read local variable '{}' in its own initializer",
                                    name.lexeme,
                                )
                            )
                        ),
                        _ => {},
                    };
                }

                self.resolve_local(expr, name)
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit variable with a non-variable expression".to_string()
                )
            ),
        }
    }
}

impl StatementVisitor for Resolver {
    fn visit_block(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Block {
                statements,
            } => {
                self.begin_scope()?;
                self.resolve(statements)?;
                self.end_scope()?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit block with a non-block statement".to_string()
                )
            ),
        }
    }

    fn visit_class(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Class {
                name,
                methods,
            } => {
                self.declare(name)?;
                self.define(name)?;

                for method in methods.iter() {
                    self.resolve_function(method, FunctionType::Method)?;
                }

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit block with a non-block statement".to_string()
                )
            ),
        }
    }

    fn visit_expression(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Expression {
                expression,
            } =>  {
                self.resolve_expression(expression)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit expression with a non-expression statement".to_string()
                )
            ),
        }
    }

    fn visit_function(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Function {
                name,
                params: _,
                body: _,
            } =>  {
                self.declare(name)?;
                self.define(name)?;

                self.resolve_function(stmt, FunctionType::Function)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit function with a non-function statement".to_string()
                )
            ),
        }
    }

    fn visit_if(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } =>  {
                self.resolve_expression(condition)?;
                self.resolve_statement(then_branch)?;

                match else_branch {
                    Some(eb) => self.resolve_statement(eb)?,
                    None => {},
                };

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit if with a non-if statement".to_string()
                )
            ),
        }
    }

    fn visit_print(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Print {
                expression,
            } =>  {
                self.resolve_expression(expression)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit print with a non-print statement".to_string()
                )
            ),
        }
    }

    fn visit_return(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Return {
                keyword: _,
                value,
            } =>  {
                if let None = self.function_type {
                    return Err(
                        LoxError::ResolutionError(
                            "can't return from top-level code".to_string(),
                        )
                    );
                }

                self.resolve_expression(value)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit return with a non-return statement".to_string()
                )
            ),
        }
    }

    fn visit_var(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Var {
                name,
                initializer,
            } => {
                self.declare(name)?;

                match initializer {
                    Some(init) => self.resolve_expression(&**init)?,
                    None => {},
                };

                self.define(name)
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit var with a non-var statement".to_string()
                )
            ),
        }
    }

    fn visit_while(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::While {
                condition,
                body,
            } =>  {
                self.resolve_expression(condition)?;
                self.resolve_statement(body)?;

                Ok(())
            },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit while with a non-while statement".to_string()
                )
            ),
        }
    }
}
