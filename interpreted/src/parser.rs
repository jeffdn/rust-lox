use std::collections::BTreeMap;

use crate::{
    errors::LoxError,
    expressions::Expression,
    statements::Statement,
    tokens::{Literal, Token, TokenType},
};

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, LoxError> {
        let mut statements: Vec<Statement> = Vec::new();

        while !self.at_end() {
            let statement = self.declaration_statement()?;

            if let Some(stmt) = statement {
                statements.push(stmt);
            }
        }

        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expression, LoxError> {
        self.assignment()
    }

    fn statement(&mut self) -> Result<Statement, LoxError> {
        if self.token_type_matches(&[TokenType::For]) {
            return self.for_statement();
        } else if self.token_type_matches(&[TokenType::Foreach]) {
            return self.foreach_statement();
        } else if self.token_type_matches(&[TokenType::If]) {
            return self.if_statement();
        } else if self.token_type_matches(&[TokenType::Print]) {
            return self.print_statement();
        } else if self.token_type_matches(&[TokenType::Return]) {
            return self.return_statement();
        } else if self.token_type_matches(&[TokenType::While]) {
            return self.while_statement();
        } else if self.token_type_matches(&[TokenType::LeftBrace]) {
            return Ok(
                Statement::Block {
                    statements: self.block()?,
                }
            );
        }

        self.expression_statement()
    }

    fn block(&mut self) -> Result<Vec<Statement>, LoxError> {
        let mut statements: Vec<Statement> = Vec::new();

        while !self.token_type_matches(&[TokenType::RightBrace]) && !self.at_end() {
            match self.declaration_statement() {
                Ok(Some(stmt)) => statements.push(stmt),
                Ok(None) => continue,
                Err(e) => return Err(e),
            };
        }

        Ok(statements)
    }

    fn for_statement(&mut self) -> Result<Statement, LoxError> {
        self.consume(&TokenType::LeftParen, "expect '(' after 'for'".to_string())?;

        let initializer: Option<Statement> = match self.token_type_matches(&[TokenType::Semicolon]) {
            true => None,
            false => match self.token_type_matches(&[TokenType::Var]) {
                true => Some(self.var_declaration()?),
                false => Some(self.expression_statement()?),
            },
        };

        let condition: Expression = match self.check_current_token(&TokenType::Semicolon) {
            true => Expression::Literal {
                value: Literal::Boolean(true),
            },
            false => self.expression()?,
        };

        self.consume(&TokenType::Semicolon, "expect ';' after loop condition".to_string())?;

        let increment: Option<Expression> = match self.check_current_token(&TokenType::RightParen) {
            true => None,
            false => Some(self.expression()?),
        };

        self.consume(&TokenType::RightParen, "expect ')' after 'for' setup".to_string())?;

        let mut body = self.statement()?;

        body = match increment {
            Some(increment) => Statement::Block {
                statements: vec![
                    body,
                    Statement::Expression {
                        expression: Box::new(increment),
                    },
                ],
            },
            None => body,
        };

        body = Statement::While {
            condition: Box::new(condition),
            body: Box::new(body),
        };

        match initializer {
            None => Ok(body),
            Some(initializer) => Ok(
                Statement::Block {
                    statements: vec![
                        initializer,
                        body,
                    ],
                },
            ),
        }
    }

    fn foreach_statement(&mut self) -> Result<Statement, LoxError> {
        self.consume(&TokenType::LeftParen, "expect '(' after 'foreach'".to_string())?;
        self.consume(&TokenType::Var, "expect 'var' after 'foreach ('".to_string())?;

        let token: Token = self.consume(
            &TokenType::Identifier,
            "expected a variable name".to_string(),
        )?;

        self.consume(
            &TokenType::In,
            "expect 'in' after 'foreach (identifier'".to_string(),
        )?;

        let iterable = self.expression()?;

        self.consume(&TokenType::RightParen, "expect ')' after 'foreach' setup".to_string())?;

        let body = self.statement()?;

        Ok(
            Statement::Foreach {
                iterator: token,
                iterable: Box::new(iterable),
                body: Box::new(body),
            }
        )
    }

    fn if_statement(&mut self) -> Result<Statement, LoxError> {
        self.consume(&TokenType::LeftParen, "expect '(' after 'if'".to_string())?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "expect ')' after 'if' condition".to_string())?;

        let then_branch = self.statement()?;
        let else_branch = match self.token_type_matches(&[TokenType::Else]) {
            true => Some(self.statement()?),
            false => None,
        };

        Ok(
            Statement::If {
                condition: Box::new(condition),
                then_branch: Box::new(then_branch),
                else_branch: match else_branch {
                    Some(eb) => Some(Box::new(eb)),
                    None => None,
                },
            }
        )
    }

    fn while_statement(&mut self) -> Result<Statement, LoxError> {
        self.consume(&TokenType::LeftParen, "expect '(' after 'while'".to_string())?;
        let condition = self.expression()?;
        self.consume(&TokenType::RightParen, "expect ')' after 'while' condition".to_string())?;

        let body = self.statement()?;

        Ok(
            Statement::While {
                condition: Box::new(condition),
                body: Box::new(body),
            }
        )
    }

    fn declaration_statement(&mut self) -> Result<Option<Statement>, LoxError> {
        let statement = match self.token_type_matches(&[TokenType::Class]) {
            true => self.class_declaration(),
            false => match self.token_type_matches(&[TokenType::Fun]) {
                true => self.function("function".to_string()),
                false => match self.token_type_matches(&[TokenType::Var]) {
                    true => self.var_declaration(),
                    false => self.statement(),
                },
            },
        };

        match statement {
            Ok(stmt) => Ok(Some(stmt)),
            Err(_) => {
                self.synchronize();
                Ok(None)
            }
        }
    }

    fn class_declaration(&mut self) -> Result<Statement, LoxError> {
        let name = self.consume(&TokenType::Identifier, "expect class name".to_string())?;

        self.consume(
            &TokenType::LeftBrace,
            "expect '{' after class body".to_string(),
        )?;

        let mut methods: Vec<Statement> = Vec::new();

        while !self.check_current_token(&TokenType::RightBrace) && !self.at_end() {
            methods.push(self.function("method".to_string())?);
        }

        self.consume(
            &TokenType::RightBrace,
            "expect '}' after class body".to_string(),
        )?;

        Ok(Statement::Class { name, methods })
    }

    fn var_declaration(&mut self) -> Result<Statement, LoxError> {
        let token: Token = self.consume(
            &TokenType::Identifier,
            "expected a variable name".to_string(),
        )?;

        let initializer = match self.token_type_matches(&[TokenType::Equal]) {
            true => Some(self.expression()?),
            false => None,
        };

        self.consume(
            &TokenType::Semicolon,
            "expected ';' after variable declaration".to_string(),
        )?;

        Ok(
            Statement::Var {
                name: token,
                initializer: match initializer {
                    Some(expr) => Some(Box::new(expr.clone())),
                    None => None,
                },
            }
        )

    }

    fn expression_statement(&mut self) -> Result<Statement, LoxError> {
        let expr = self.expression()?;

        self.consume(
            &TokenType::Semicolon,
            "expected ';' after expression".to_string(),
        )?;

        Ok(Statement::Expression {
            expression: Box::new(expr.clone()),
        })
    }

    fn function(&mut self, kind: String) -> Result<Statement, LoxError> {
        let name = self.consume(&TokenType::Identifier, format!("expect {} name", kind))?;

        self.consume(
            &TokenType::LeftParen,
            format!("expect '(' after {} name", kind),
        )?;

        let mut parameters: Vec<Token> = Vec::new();

        if !self.check_current_token(&TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    let next_token = self.peek();
                    self.error(
                        &next_token,
                        "can't have more than 255 arguments".to_string(),
                    );
                }

                parameters.push(
                    self.consume(&TokenType::Identifier, "expect parameter name".to_string())?,
                );

                if !self.token_type_matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(
            &TokenType::RightParen,
            "expect ')' after parameter list".to_string(),
        )?;

        self.consume(
            &TokenType::LeftBrace,
            format!("expect '{{' before {} body", kind),
        )?;

        let statements: Vec<Statement> = self.block()?;

        Ok(
            Statement::Function {
                name,
                params: parameters,
                body: statements,
            }
        )
    }

    fn print_statement(&mut self) -> Result<Statement, LoxError> {
        let expr = self.expression()?;

        self.consume(
            &TokenType::Semicolon,
            "expected ';' after value".to_string(),
        )?;

        Ok(
            Statement::Print {
                expression: Box::new(expr.clone()),
            }
        )
    }

    fn return_statement(&mut self) -> Result<Statement, LoxError> {
        let keyword = self.previous();

        let value: Expression = match self.check_current_token(&TokenType::Semicolon) {
            false => self.expression()?,
            true => Expression::Literal {
                value: Literal::Nil,
            },
        };

        self.consume(
            &TokenType::Semicolon,
            "expected ';' after return value".to_string(),
        )?;

        Ok(
            Statement::Return {
                keyword,
                value: Box::new(value),
            }
        )
    }

    fn assignment(&mut self) -> Result<Expression, LoxError> {
        let expr = self.or()?;

        if self.token_type_matches(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expression::Variable {
                    name,
                } => {
                    return Ok(
                        Expression::Assignment {
                            name,
                            expression: Box::new(value.clone()),
                        }
                    )
                },
                Expression::Get {
                    name,
                    object,
                } => {
                    return Ok(
                        Expression::Set {
                            name,
                            object,
                            value: Box::new(value.clone()),
                        }
                    )
                },
                Expression::Index {
                    item,
                    index,
                    slice,
                } => {
                    return Ok(
                        Expression::IndexedAssignment {
                            indexed_item: Box::new(
                                Expression::Index {
                                    item,
                                    index,
                                    slice,
                                },
                            ),
                            expression: Box::new(value.clone()),
                        }
                    );
                },
                _ => return Err(
                    self.error(&equals, "invalid assignment target".to_string())
                ),
            }
        } else if self.token_type_matches(
            &[
                TokenType::MinusEqual,
                TokenType::PlusEqual,
            ]
        ) {
            let mut token = self.previous();
            token.token_type = match &token.token_type {
                TokenType::MinusEqual => TokenType::Minus,
                TokenType::PlusEqual => TokenType::Plus,
                _ => panic!("unreachable!"),
            };

            let value = self.assignment()?;

            match expr {
                Expression::Variable {
                    name,
                } => {
                    return Ok(
                        Expression::Assignment {
                            name: name.clone(),
                            expression: Box::new(
                                Expression::Binary {
                                    left: Box::new(
                                        Expression::Variable {
                                            name,
                                        },
                                    ),
                                    operator: token,
                                    right: Box::new(value.clone()),
                                },
                            ),
                        }
                    );
                },
                Expression::Get {
                    name,
                    object,
                } => {
                    return Ok(
                        Expression::Set {
                            name: name.clone(),
                            object: object.clone(),
                            value: Box::new(
                                Expression::Binary {
                                    left: Box::new(
                                        Expression::Get {
                                            name,
                                            object,
                                        }
                                    ),
                                    operator: token,
                                    right: Box::new(value.clone()),
                                },
                            ),
                        }
                    )
                },
                Expression::Index {
                    item,
                    index,
                    slice,
                } => {
                    return Ok(
                        Expression::IndexedAssignment {
                            indexed_item: Box::new(
                                Expression::Index {
                                    item: item.clone(),
                                    index: index.clone(),
                                    slice: slice.clone(),
                                },
                            ),
                            expression: Box::new(
                                Expression::Binary {
                                    left: Box::new(
                                        Expression::Index {
                                            item,
                                            index,
                                            slice,
                                        }
                                    ),
                                    operator: token,
                                    right: Box::new(value.clone()),
                                },
                            ),
                        }
                    );
                },
                _ => return Err(
                    self.error(&token, "invalid assignment target".to_string())
                ),
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expression, LoxError> {
        let mut expr = self.and()?;

        while self.token_type_matches(&[TokenType::Or]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expression::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expression, LoxError> {
        let mut expr = self.equality()?;

        while self.token_type_matches(&[TokenType::And]) {
            let operator = self.previous();
            let right = self.and()?;
            expr = Expression::Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression, LoxError> {
        let expr = self.comparison()?;

        if self.token_type_matches(
            &[
                TokenType::BangEqual,
                TokenType::EqualEqual,
                TokenType::In,
            ]
        ) {
            let operator = self.previous();
            let right = self.comparison()?;
            let new_expr = Expression::Binary {
                left: Box::new(expr.clone()),
                operator,
                right: Box::new(right.clone()),
            };

            Ok(new_expr)
        } else if self.token_type_matches(&[TokenType::NotIn]) {
            // This is a special case, with a two-token operator.
            let operator = self.previous();

            self.consume(
                &TokenType::In,
                "expect 'in' after 'not'".to_string(),
            )?;

            let right = self.comparison()?;

            let new_expr = Expression::Binary {
                left: Box::new(expr.clone()),
                operator,
                right: Box::new(right.clone()),
            };

            Ok(new_expr)
        } else {
            Ok(expr)
        }
    }

    fn comparison(&mut self) -> Result<Expression, LoxError> {
        let expr = self.term()?;

        if self.token_type_matches(
            &[
                TokenType::Greater,
                TokenType::GreaterEqual,
                TokenType::Less,
                TokenType::LessEqual,
            ]
        ) {
            let operator = self.previous();
            let right = self.term()?;
            let new_expr = Expression::Binary {
                left: Box::new(expr.clone()),
                operator,
                right: Box::new(right.clone()),
            };

            return Ok(new_expr);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expression, LoxError> {
        let expr = self.factor()?;

        if self.token_type_matches(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.term()?;
            let new_expr = Expression::Binary {
                left: Box::new(expr.clone()),
                operator,
                right: Box::new(right.clone()),
            };

            return Ok(new_expr);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expression, LoxError> {
        let expr = self.unary()?;

        if self.token_type_matches(
            &[
                TokenType::Percent,
                TokenType::Slash,
                TokenType::Star,
            ]
        ) {
            let operator = self.previous();
            let right = self.unary()?;

            return Ok(
                Expression::Binary {
                    left: Box::new(expr.clone()),
                    operator,
                    right: Box::new(right.clone()),
                }
            );
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, LoxError> {
        if self.token_type_matches(
            &[
                TokenType::Bang,
                TokenType::Minus,
            ]) {
            let operator = self.previous();
            let right = self.unary()?;
            let new_expr = Expression::Unary {
                operator,
                right: Box::new(right.clone()),
            };

            return Ok(new_expr);
        }

        self.call()
    }

    fn finish_call(&mut self, callee: Expression) -> Result<Expression, LoxError> {
        let mut arguments: Vec<Expression> = Vec::new();

        if !self.check_current_token(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    let next_token = self.peek();
                    self.error(
                        &next_token,
                        "can't have more than 255 arguments".to_string(),
                    );
                }

                arguments.push(self.expression()?);

                if !self.token_type_matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(
            &TokenType::RightParen,
            "expect ')' after arguments".to_string(),
        )?;

        Ok(
            Expression::Call {
                callee: Box::new(callee),
                paren,
                arguments,
            }
        )
    }

    fn finish_list(&mut self) -> Result<Expression, LoxError> {
        let mut list: Vec<Expression> = vec![];

        if !self.check_current_token(&TokenType::RightBracket) {
            loop {
                list.push(self.expression()?);

                if !self.token_type_matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(
            &TokenType::RightBracket,
            "expect ']' after list items".to_string(),
        )?;

        return Ok(
            Expression::List {
                expressions: list,
            }
        );
    }

    fn finish_map(&mut self) -> Result<Expression, LoxError> {
        let mut map: BTreeMap<Expression, Expression> = BTreeMap::new();

        if !self.check_current_token(&TokenType::RightBrace) {
            loop {
                let key = self.expression()?;

                self.consume(
                    &TokenType::Colon,
                    "use ':' to separate keys and values".to_string(),
                )?;

                let value = self.expression()?;

                map.insert(key, value);

                if !self.token_type_matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(
            &TokenType::RightBrace,
            "expect ']' after list items".to_string(),
        )?;

        return Ok(
            Expression::Map {
                expression_map: map,
            }
        );
    }

    fn call(&mut self) -> Result<Expression, LoxError> {
        let mut expr = self.primary()?;

        loop {
            if self.token_type_matches(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.token_type_matches(&[TokenType::Dot]) {
                let name = self.consume(
                    &TokenType::Identifier,
                    "expect property name after '.'".to_string(),
                )?;

                expr = Expression::Get {
                    name,
                    object: Box::new(expr),
                };
            } else if self.token_type_matches(&[TokenType::LeftBracket]) {
                let index = self.expression()?;

                let slice = if self.token_type_matches(&[TokenType::Colon]) {
                    Some(Box::new(self.expression()?))
                } else {
                    None
                };

                self.consume(
                    &TokenType::RightBracket,
                    "expect indexes to close  with a ']'".to_string(),
                )?;

                expr = Expression::Index {
                    item: Box::new(expr),
                    index: Box::new(index),
                    slice,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }


    fn primary(&mut self) -> Result<Expression, LoxError> {
        if self.token_type_matches(&[TokenType::False]) {
            return Ok(
                Expression::Literal {
                    value: Literal::Boolean(false),
                }
            );
        } else if self.token_type_matches(&[TokenType::True]) {
            return Ok(
                Expression::Literal {
                    value: Literal::Boolean(true),
                }
            );
        } else if self.token_type_matches(&[TokenType::Nil]) {
            return Ok(
                Expression::Literal {
                    value: Literal::Nil,
                }
            );
        } else if self.token_type_matches(
            &[
                TokenType::Number,
                TokenType::String,
            ]
        ) {
            return Ok(
                Expression::Literal {
                    value: self.previous().literal.clone().unwrap(),
                }
            );
        } else if self.token_type_matches(&[TokenType::LeftBracket]) {
            return self.finish_list();
        } else if self.token_type_matches(&[TokenType::LeftBrace]) {
            return self.finish_map();
        } else if self.token_type_matches(&[TokenType::Identifier]) {
            return Ok(
                Expression::Variable {
                    name: self.previous(),
                }
            );
        } else if self.token_type_matches(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            let _ = self.consume(
                &TokenType::RightParen,
                "expect ')' after an expression".to_string(),
            )?;
            return Ok(
                Expression::Grouping {
                    expression: Box::new(
                        expr.clone()
                    ),
                }
            );
        }

        let next_token = self.peek();

        Err(self.error(&next_token, "expect expression".to_string()))
    }

    fn error(&mut self, next_token: &Token, message: String) -> LoxError {
        if next_token.token_type == TokenType::Eof {
            return LoxError::ParseError(next_token.line, format!("at end {}", message));
        }

        LoxError::ParseError(
            next_token.line,
            format!("at '{}' {}", next_token.lexeme, message),
        )
    }

    fn check_current_token(&mut self, token_type: &TokenType) -> bool {
        if self.at_end() {
            return false;
        }

        &self.peek().token_type == token_type
    }

    fn consume(&mut self, token_type: &TokenType, message: String) -> Result<Token, LoxError> {
        if self.check_current_token(token_type) {
            return Ok(self.advance().clone());
        }

        let next_token = self.peek();

        Err(self.error(&next_token, message))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.at_end() {
            if &self.previous().token_type == &TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => {}
            };

            self.advance();
        }
    }

    fn token_type_matches(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check_current_token(token_type) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn peek(&mut self) -> Token {
        self.tokens.get(self.current).unwrap().clone()
    }

    fn previous(&mut self) -> Token {
        self.tokens.get(self.current - 1).unwrap().clone()
    }

    fn at_end(&mut self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn advance(&mut self) -> Token {
        if !self.at_end() {
            self.current += 1;
        }

        self.previous()
    }
}
