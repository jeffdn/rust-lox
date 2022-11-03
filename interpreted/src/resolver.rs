use std::collections::HashMap;

use crate::{
    errors::LoxError,
    expressions::{Expression, ExpressionVisitor},
    interpreter::Interpreter,
    statements::{Statement, StatementVisitor},
    tokens::Token,
};

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
            .ok_or_else(|| LoxError::ResolutionError(
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
        &mut self,
        stmt: &Statement,
        function_type: FunctionType,
    ) -> Result<(), LoxError> {
        let (params, body) = match stmt {
            Statement::Function {
                name: _,
                params,
                body,
            } => (params, body),
            _ => return Err(
                LoxError::ResolutionError(
                    "attempted to resolve a non-function statement".to_string(),
                )
            ),
        };

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
    }
}


impl ExpressionVisitor<()> for Resolver {
    fn visit_assignment(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Assignment { name, expression } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit assignment with a non-assignment expression".to_string()
                )
            );
        };

        self.resolve_expression(&**expression)?;
        self.resolve_local(expr, name)?;

        Ok(())
    }

    fn visit_binary(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Binary { left, operator: _, right, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit binary with a non-binary expression".to_string()
                )
            );
        };

        self.resolve_expression(&*left)?;
        self.resolve_expression(&*right)?;

        Ok(())
    }

    fn visit_call(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Call { callee, paren: _, arguments, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit call with a non-call expression".to_string()
                )
            );
        };

        self.resolve_expression(&*callee)?;

        for argument in arguments.iter() {
            self.resolve_expression(argument)?;
        }

        Ok(())
    }

    fn visit_get(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Get { name: _, object, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit grouping with a non-grouping expression".to_string()
                )
            );
        };

        self.resolve_expression(&*object)?;

        Ok(())
    }

    fn visit_grouping(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Grouping { expression, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit grouping with a non-grouping expression".to_string()
                )
            );
        };

        self.resolve_expression(&*expression)?;

        Ok(())
    }

    fn visit_index(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Index { item, index, slice, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit index with a non-index expression".to_string()
                )
            );
        };

        self.resolve_expression(&*item)?;
        self.resolve_expression(&*index)?;

        if let Some(slice) = slice {
            self.resolve_expression(&*slice)?;
        }

        Ok(())
    }

    fn visit_indexed_assignment(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::IndexedAssignment { indexed_item, expression, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit index with a non-index expression".to_string()
                )
            );
        };

        self.resolve_expression(&*indexed_item)?;
        self.resolve_expression(&*expression)?;

        Ok(())
    }

    fn visit_list(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::List { expressions: items } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit list with a non-list expression".to_string()
                )
            );
        };

        for item in items.iter() {
            self.resolve_expression(item)?;
        }

        Ok(())
    }

    fn visit_literal(&mut self, expr: &Expression) -> Result<(), LoxError> {
        match expr {
            Expression::Literal { .. } => { Ok(()) },
            _ => Err(
                LoxError::ResolutionError(
                    "attempted to visit literal with a non-literal expression".to_string()
                )
            ),
        }
    }

    fn visit_logical(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Logical { left, operator: _, right, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit logical with a non-logical expression".to_string()
                )
            );
        };

        self.resolve_expression(&*left)?;
        self.resolve_expression(&*right)?;

        Ok(())
    }

    fn visit_map(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Map { expression_map, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit list with a non-list expression".to_string()
                )
            );
        };

        for (key, value) in expression_map.iter() {
            self.resolve_expression(key)?;
            self.resolve_expression(value)?;
        }

        Ok(())
    }

    fn visit_set(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Set { name: _, object, value, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit grouping with a non-grouping expression".to_string()
                )
            );
        };

        self.resolve_expression(&*value)?;
        self.resolve_expression(&*object)?;

        Ok(())
    }

    fn visit_unary(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Unary { operator: _, right, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit unary with a non-unary expression".to_string()
                )
            );
        };

        self.resolve_expression(&*right)?;

        Ok(())
    }

    fn visit_variable(&mut self, expr: &Expression) -> Result<(), LoxError> {
        let Expression::Variable { name, } = expr else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit variable with a non-variable expression".to_string()
                )
            );
        };

        if !self.scopes.is_empty() {
            if let Some(false) = self.scopes.last().unwrap().get(&name.lexeme) {
                return Err(
                    LoxError::ResolutionError(
                        format!(
                            "can't read local variable '{}' in its own initializer",
                            name.lexeme,
                        )
                    )
                )
            }
        }

        self.resolve_local(expr, name)
    }
}

impl StatementVisitor for Resolver {
    fn visit_block(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::Block { statements, } = stmt else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit block with a non-block statement".to_string()
                )
            );
        };

        self.begin_scope()?;
        self.resolve(statements)?;
        self.end_scope()?;

        Ok(())
    }

    fn visit_class(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::Class { name, methods, } = stmt else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit block with a non-block statement".to_string()
                )
            );
        };

        self.declare(name)?;
        self.define(name)?;

        for method in methods.iter() {
            self.resolve_function(method, FunctionType::Method)?;
        }

        Ok(())
    }

    fn visit_expression(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::Expression { expression, } = stmt else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit expression with a non-expression statement".to_string()
                )
            );
        };

        self.resolve_expression(expression)?;

        Ok(())
    }

    fn visit_foreach(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::Foreach { iterator, iterable, body, } = stmt else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit foreach with a non-foreach statement".to_string()
                )
            );
        };

        self.begin_scope()?;

        self.declare(iterator)?;
        self.resolve_expression(iterable)?;
        self.resolve_statement(body)?;

        self.end_scope()?;

        Ok(())
    }

    fn visit_function(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::Function { name, .. } = stmt else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit function with a non-function statement".to_string()
                )
            );
        };

        self.declare(name)?;
        self.define(name)?;

        self.resolve_function(stmt, FunctionType::Function)?;

        Ok(())
    }

    fn visit_if(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::If { condition, then_branch, else_branch, } = stmt else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit if with a non-if statement".to_string()
                )
            );
        };

        self.resolve_expression(condition)?;
        self.resolve_statement(then_branch)?;

        match else_branch {
            Some(eb) => self.resolve_statement(eb)?,
            None => {}
        };

        Ok(())
    }

    fn visit_print(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::Print { expression, } = stmt else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit print with a non-print statement".to_string()
                )
            );
        };

        self.resolve_expression(expression)?;

        Ok(())
    }

    fn visit_return(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::Return { keyword: _, value, } = stmt else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit return with a non-return statement".to_string()
                )
            );
        };

        if self.function_type.is_none() {
            return Err(
                LoxError::ResolutionError(
                    "can't return from top-level code".to_string(),
                )
            );
        }

        self.resolve_expression(value)?;

        Ok(())
    }

    fn visit_var(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::Var { name, initializer } = stmt else {
            return Err(LoxError::ResolutionError(
                "attempted to visit var with a non-var statement".to_string(),
            ));
        };

        self.declare(name)?;

        match initializer {
            Some(init) => self.resolve_expression(&**init)?,
            None => {}
        };

        self.define(name)
    }

    fn visit_while(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        let Statement::While { condition, body, } = stmt else {
            return Err(
                LoxError::ResolutionError(
                    "attempted to visit while with a non-while statement".to_string()
                )
            );
        };

        self.resolve_expression(condition)?;
        self.resolve_statement(body)?;

        Ok(())
    }
}
