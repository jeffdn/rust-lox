use crate::callable::LoxCallable;
use crate::environment::Environment;
use crate::errors::LoxError;
use crate::expressions::{Expression, ExpressionVisitor};
use crate::statements::{Statement, StatementVisitor};
use crate::tokens::{Literal, TokenType};

pub struct Interpreter {
    environment: Environment<Literal>,
    callables: Environment<LoxCallable>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new_literal(),
            callables: Environment::new_callable(),
        }
    }

    fn evaluate(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        self.accept_expression(expr)
    }

    fn _is_truthy(&self, literal: &Literal) -> Result<bool, LoxError> {
        match literal {
            Literal::String(ref string) => Ok(string.len() > 0),
            Literal::Number(ref number) => Ok(*number != 0.0f64),
            Literal::Identifier(_) => Ok(false),
            Literal::Boolean(ref boolean) => Ok(*boolean),
            Literal::Nil => Ok(false),
        }
    }

    fn is_truthy(&mut self, expr: &Expression) -> Result<bool, LoxError> {
        match expr {
            Expression::Literal { value } => self._is_truthy(&value),
            Expression::Unary {
                operator: _,
                right: _,
            } | Expression::Binary {
                left: _,
                operator: _,
                right: _,
            } | Expression::Logical {
                left: _,
                operator: _,
                right: _,
            } | Expression::Variable {
                name: _,
            } | Expression::Grouping {
                expression: _,
            } => {
                let output = self.evaluate(expr)?;
                self._is_truthy(&output)
            },
            _ => Err(LoxError::AstError),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Statement>) -> Result<(), LoxError> {
        for statement in statements.iter() {
            let _ = self.execute(statement)?;
        }

        Ok(())
    }

    pub fn execute(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        self.accept_statement(stmt)
    }
}

impl ExpressionVisitor<Literal> for Interpreter {
    fn accept_expression(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
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
            Expression::Grouping {
                expression: _,
            } => self.visit_grouping(expr),
            Expression::Literal {
                value: _
            } => self.visit_literal(expr),
            Expression::Unary {
                operator: _,
                right: _,
            } => self.visit_unary(expr),
            Expression::Variable {
                name: _,
            } => self.visit_variable(expr),
            Expression::Assignment {
                name: _,
                expression: _,
            } => self.visit_assignment(expr),
            Expression::Logical {
                left: _,
                operator: _,
                right: _,
            } => self.visit_logical(expr),
        }
    }

    fn visit_binary(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Binary {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(&**left)?;
                let right = self.evaluate(&**right)?;

                match (left, right) {
                    (Literal::Number(left), Literal::Number(right)) => {
                        match operator.token_type {
                            TokenType::Minus => Ok(Literal::Number(left - right)),
                            TokenType::Slash => Ok(Literal::Number(left / right)),
                            TokenType::Star => Ok(Literal::Number(left * right)),
                            TokenType::Plus => Ok(Literal::Number(left + right)),
                            TokenType::Greater => Ok(Literal::Boolean(left > right)),
                            TokenType::GreaterEqual => Ok(Literal::Boolean(left >= right)),
                            TokenType::Less => Ok(Literal::Boolean(left < right)),
                            TokenType::LessEqual=> Ok(Literal::Boolean(left <= right)),
                            TokenType::BangEqual=> Ok(Literal::Boolean(left != right)),
                            TokenType::EqualEqual=> Ok(Literal::Boolean(left == right)),
                            _ => Err(LoxError::AstError),
                        }
                    },
                    (Literal::String(left), Literal::String(right)) => {
                        match operator.token_type {
                            TokenType::Plus => Ok(
                                Literal::String(
                                    format!("{}{}", left, right),
                                )
                            ),
                            TokenType::BangEqual=> Ok(Literal::Boolean(left != right)),
                            TokenType::EqualEqual=> Ok(Literal::Boolean(left == right)),
                            _ => Err(LoxError::AstError),
                        }
                    },
                    (Literal::Boolean(left), Literal::Boolean(right)) => {
                        match operator.token_type {
                            TokenType::BangEqual=> Ok(Literal::Boolean(left != right)),
                            TokenType::EqualEqual=> Ok(Literal::Boolean(left == right)),
                            _ => Err(LoxError::AstError),
                        }
                    },
                    (Literal::Nil, Literal::Nil) => {
                        match operator.token_type {
                            TokenType::BangEqual => Ok(Literal::Boolean(false)),
                            TokenType::EqualEqual => Ok(Literal::Boolean(true)),
                            _ => Err(LoxError::AstError),
                        }
                    },
                    _ => Err(LoxError::AstError),
                }
            },
            _ => Err(LoxError::AstError)
        }
    }

    fn visit_call(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Call {
                callee,
                paren: _,
                arguments,
            } => {
                let mut real_arguments: Vec<Literal> = Vec::new();
                for arg in arguments.iter() {
                    let real_arg = self.evaluate(arg)?;
                    real_arguments.push(real_arg);
                }

                let mut callable = match **callee {
                    Expression::Variable {
                        ref name,
                    } => self.callables.get(&name.lexeme)?,
                    _ => return Err(LoxError::AstError),
                };

                if real_arguments.len() != callable.arity()? {
                    return Err(
                        LoxError::RuntimeError(
                            format!(
                                "expected {} arguments but got {}",
                                callable.arity()?,
                                real_arguments.len(),
                            )
                        )
                    );
                }

                callable.call(self, real_arguments)
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_grouping(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Grouping { expression } => self.evaluate(&**expression),
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_literal(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Literal { value } => Ok(value.clone()),
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_unary(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Unary {
                operator,
                right,
            } => match operator.token_type {
                TokenType::Minus => {
                    let right = self.evaluate(&**right)?;
                    match right {
                        Literal::Number(number) => {
                            Ok(Literal::Number(-number))
                        },
                        _ => Err(
                            LoxError::TypeError(
                                format!("expected number, got {}", right)
                            ),
                        ),
                    }
                },
                TokenType::Bang => {
                    match **right {
                        Expression::Literal { value: _ } => {
                            let truthy = self.is_truthy(&**right)?;
                            Ok(Literal::Boolean(truthy))
                        },
                        _ => Err(
                            LoxError::TypeError(
                                "expected value, got something else".to_string()
                            ),
                        ),
                    }
                },
                _ => Err(LoxError::AstError),
            },
            _ => Err(LoxError::AstError)
        }
    }

    fn visit_variable(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Variable { name } => {
                self.environment.get(&name.lexeme.clone())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_assignment(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Assignment {
                name,
                expression,
            } => {
                let value = self.evaluate(&**expression)?;
                self.environment.assign(name.lexeme.clone(), value.clone())?;

                Ok(value)
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_logical(&mut self, expr: &Expression) -> Result<Literal, LoxError> {
        match expr {
            Expression::Logical {
                left,
                operator,
                right,
            } => {
                let left = self.evaluate(&**left)?;
                let truthy_left = self.is_truthy(
                    &Expression::Literal {
                        value: left.clone(),
                    }
                )?;

                match (truthy_left, &operator.token_type) {
                    (true, TokenType::Or) => Ok(left.clone()),
                    (false, TokenType::And) => Ok(left.clone()),
                    _ => Ok(self.evaluate(right)?),
                }
            },
            _ => Err(LoxError::AstError),
        }
    }
}

impl StatementVisitor for Interpreter {
    fn accept_statement(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
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
            Statement::Block {
                statements: _,
            } => self.visit_block(stmt),
        }
    }

    fn visit_expression(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Expression {
                expression,
            } => {
                let _ = self.evaluate(&**expression)?;
                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_function(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Function {
                name,
                params: _,
                body: _,
            } => {
                let callable = LoxCallable::Function(stmt.clone());
                self.callables.define(name.lexeme.clone(), callable);

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_print(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Print {
                expression,
            } => {
                let value = self.evaluate(&**expression)?;
                println!("{}", value);

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_return(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Return {
                keyword: _,
                value,
            } => {
                Err(
                    LoxError::FunctionReturn(
                        self.evaluate(value)?
                    )
                )
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_while(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::While {
                condition,
                body,
            } => {
                while self.is_truthy(&**condition)? {
                    self.execute(body)?;
                }

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_var(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Var {
                name,
                initializer,
            } => {
                let value = match initializer {
                    Some(init) => self.evaluate(&**init)?,
                    None => Literal::Nil,
                };

                self.environment.define(name.lexeme.clone(), value);

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_if(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                match self.is_truthy(&**condition)? {
                    true => self.execute(then_branch)?,
                    false => match **else_branch {
                        Some(ref eb) => self.execute(eb)?,
                        None => {},
                    }
                };

                Ok(())
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_block(&mut self, stmt: &Statement) -> Result<(), LoxError> {
        match stmt {
            Statement::Block {
                statements,
            } => {
                let output_env = self.execute_block(statements, self.environment.clone(), true)?;
                self.environment = output_env;
            },
            _ => return Err(LoxError::AstError),
        };

        Ok(())
    }
}

impl Interpreter {
    pub fn execute_block(
        &mut self,
        statements: &[Statement],
        working_env: Environment<Literal>,
        link_env: bool,
    ) -> Result<Environment<Literal>, LoxError> {
        let mut current_env = working_env.clone();
        self.environment = current_env.clone();

        for statement in statements.iter() {
            self.execute(statement)?;
        }

        if link_env {
            for (key, value) in self.environment.values.iter() {
                if current_env.values.contains_key(key) {
                    current_env.assign(key.clone(), value.clone())?;
                }
            }
        }

        Ok(current_env)
    }
}
