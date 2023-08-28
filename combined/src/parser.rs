use crate::{
    errors::{LoxError, LoxResult},
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
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> LoxResult<Vec<Statement>> {
        let mut statements: Vec<Statement> = Vec::new();

        while !self.at_end() {
            let statement = self.declaration_statement()?;

            if let Some(stmt) = statement {
                statements.push(stmt);
            }
        }

        Ok(statements)
    }

    fn expression(&mut self) -> LoxResult<Expression> {
        self.assignment()
    }

    fn statement(&mut self) -> LoxResult<Statement> {
        if self.token_type_matches(&[TokenType::For]) {
            return self.for_statement();
        } else if self.token_type_matches(&[TokenType::Foreach]) {
            return self.foreach_statement();
        } else if self.token_type_matches(&[TokenType::If]) {
            return self.if_statement();
        } else if self.token_type_matches(&[TokenType::Print]) {
            return self.print_statement(false);
        } else if self.token_type_matches(&[TokenType::Println]) {
            return self.print_statement(true);
        } else if self.token_type_matches(&[TokenType::Return]) {
            return self.return_statement();
        } else if self.token_type_matches(&[TokenType::While]) {
            return self.while_statement();
        } else if self.token_type_matches(&[TokenType::Break]) {
            self.consume(&TokenType::Semicolon, "expect ';' after 'break'".into())?;
            return Ok(Statement::Break);
        } else if self.token_type_matches(&[TokenType::Continue]) {
            self.consume(&TokenType::Semicolon, "expect ';' after 'continue'".into())?;
            return Ok(Statement::Continue);
        } else if self.token_type_matches(&[TokenType::Assert]) {
            return self.assert_statement();
        } else if self.token_type_matches(&[TokenType::Delete]) {
            return self.delete_statement();
        } else if self.token_type_matches(&[TokenType::LeftBrace]) {
            return Ok(Statement::Block {
                statements: Box::new(self.block()?),
            });
        }

        self.expression_statement()
    }

    fn block(&mut self) -> LoxResult<Vec<Statement>> {
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

    fn assert_statement(&mut self) -> LoxResult<Statement> {
        let expression = Box::new(self.expression()?);
        let mut message: Option<Box<Expression>> = None;

        if self.token_type_matches(&[TokenType::Comma]) {
            message = Some(Box::new(self.expression()?));
        }

        self.consume(&TokenType::Semicolon, "expected ';'".into())?;

        Ok(Statement::Assert {
            expression,
            message,
        })
    }

    fn delete_statement(&mut self) -> LoxResult<Statement> {
        let expression = Box::new(self.call()?);

        self.consume(&TokenType::Semicolon, "expected ';'".into())?;

        Ok(Statement::Delete { expression })
    }

    fn for_statement(&mut self) -> LoxResult<Statement> {
        self.consume(&TokenType::LeftParen, "expect '(' after 'for'".into())?;

        let initializer: Option<Box<Statement>> =
            match self.token_type_matches(&[TokenType::Semicolon]) {
                true => None,
                false => match self.token_type_matches(&[TokenType::Var]) {
                    true => Some(Box::new(self.var_declaration()?)),
                    false => Some(Box::new(self.expression_statement()?)),
                },
            };

        let condition: Option<Box<Expression>> =
            match self.check_current_token(&TokenType::Semicolon) {
                true => None,
                false => Some(Box::new(self.expression()?)),
            };

        self.consume(
            &TokenType::Semicolon,
            "expect ';' after loop condition".into(),
        )?;

        let increment: Option<Box<Expression>> =
            match self.check_current_token(&TokenType::RightParen) {
                true => None,
                false => Some(Box::new(self.expression()?)),
            };

        self.consume(
            &TokenType::RightParen,
            "expect ')' after 'for' setup".into(),
        )?;

        let body = Box::new(self.statement()?);

        Ok(Statement::For {
            initializer,
            condition,
            increment,
            body,
        })
    }

    fn foreach_statement(&mut self) -> LoxResult<Statement> {
        self.consume(&TokenType::LeftParen, "expect '(' after 'foreach'".into())?;
        self.consume(&TokenType::Var, "expect 'var' after 'foreach ('".into())?;

        let token: Token =
            self.consume(&TokenType::Identifier, "expected a variable name".into())?;

        self.consume(
            &TokenType::In,
            "expect 'in' after 'foreach (identifier'".into(),
        )?;

        let iterable = self.expression()?;

        self.consume(
            &TokenType::RightParen,
            "expect ')' after 'foreach' setup".into(),
        )?;

        let body = self.statement()?;

        Ok(Statement::Foreach {
            iterator: token,
            iterable: Box::new(iterable),
            body: Box::new(body),
        })
    }

    fn if_statement(&mut self) -> LoxResult<Statement> {
        self.consume(&TokenType::LeftParen, "expect '(' after 'if'".into())?;
        let condition = self.expression()?;
        self.consume(
            &TokenType::RightParen,
            "expect ')' after 'if' condition".into(),
        )?;

        let then_branch = self.statement()?;
        let else_branch = match self.token_type_matches(&[TokenType::Else]) {
            true => Some(self.statement()?),
            false => None,
        };

        Ok(Statement::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        })
    }

    fn while_statement(&mut self) -> LoxResult<Statement> {
        self.consume(&TokenType::LeftParen, "expect '(' after 'while'".into())?;
        let condition = self.expression()?;
        self.consume(
            &TokenType::RightParen,
            "expect ')' after 'while' condition".into(),
        )?;

        let body = self.statement()?;

        Ok(Statement::While {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn declaration_statement(&mut self) -> LoxResult<Option<Statement>> {
        let statement = match self.token_type_matches(&[TokenType::Class]) {
            true => self.class_declaration(),
            false => match self.token_type_matches(&[TokenType::Function]) {
                true => self.function("function".into()),
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
            },
        }
    }

    fn class_declaration(&mut self) -> LoxResult<Statement> {
        let name = self.consume(&TokenType::Identifier, "expect class name".into())?;
        let mut superclass: Option<Token> = None;

        if self.token_type_matches(&[TokenType::Less]) {
            superclass =
                Some(self.consume(&TokenType::Identifier, "expect superclass name".into())?);
            if name.lexeme == superclass.as_ref().unwrap().lexeme {
                return Err(self.error(&name, "a class cannot inherit from itself".into()));
            }
        }

        self.consume(&TokenType::LeftBrace, "expect '{' after class body".into())?;

        let mut methods: Vec<Statement> = Vec::new();

        while !self.check_current_token(&TokenType::RightBrace) && !self.at_end() {
            methods.push(self.function("method".into())?);
        }

        self.consume(&TokenType::RightBrace, "expect '}' after class body".into())?;

        Ok(Statement::Class {
            name,
            superclass,
            methods: Box::new(methods),
        })
    }

    fn var_declaration(&mut self) -> LoxResult<Statement> {
        let token: Token =
            self.consume(&TokenType::Identifier, "expected a variable name".into())?;

        let initializer = match self.token_type_matches(&[TokenType::Equal]) {
            true => Some(self.expression()?),
            false => None,
        };

        self.consume(
            &TokenType::Semicolon,
            "expected ';' after variable declaration".into(),
        )?;

        Ok(Statement::Var {
            name: token,
            initializer: initializer.map(Box::new),
        })
    }

    fn expression_statement(&mut self) -> LoxResult<Statement> {
        let expr = self.expression()?;

        self.consume(
            &TokenType::Semicolon,
            "expected ';' after expression".into(),
        )?;

        Ok(Statement::Expression {
            expression: Box::new(expr),
        })
    }

    fn function(&mut self, kind: String) -> LoxResult<Statement> {
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
                    self.error(&next_token, "can't have more than 255 arguments".into());
                }

                parameters
                    .push(self.consume(&TokenType::Identifier, "expect parameter name".into())?);

                if !self.token_type_matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(
            &TokenType::RightParen,
            "expect ')' after parameter list".into(),
        )?;

        self.consume(
            &TokenType::LeftBrace,
            format!("expect '{{' before {} body", kind),
        )?;

        let statements = self.block()?;

        Ok(Statement::Function {
            name,
            params: Box::new(parameters),
            body: Box::new(statements),
        })
    }

    fn print_statement(&mut self, newline: bool) -> LoxResult<Statement> {
        let expr = self.expression()?;

        self.consume(&TokenType::Semicolon, "expected ';' after value".into())?;

        Ok(Statement::Print {
            newline,
            expression: Box::new(expr),
        })
    }

    fn return_statement(&mut self) -> LoxResult<Statement> {
        let keyword = self.previous();

        let value = match self.check_current_token(&TokenType::Semicolon) {
            false => Some(Box::new(self.expression()?)),
            true => None,
        };

        self.consume(
            &TokenType::Semicolon,
            "expected ';' after return value".into(),
        )?;

        Ok(Statement::Return { keyword, value })
    }

    fn assignment(&mut self) -> LoxResult<Expression> {
        let expr = self.or()?;

        if self.token_type_matches(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expression::Variable { name } => {
                    return Ok(Expression::Assignment {
                        name,
                        expression: Box::new(value),
                    })
                },
                Expression::Get { name, object } => {
                    return Ok(Expression::Set {
                        name,
                        object,
                        value: Box::new(value),
                    })
                },
                Expression::Index {
                    item,
                    is_slice,
                    left,
                    right,
                } => {
                    return Ok(Expression::IndexedAssignment {
                        indexed_item: Box::new(Expression::Index {
                            item,
                            is_slice,
                            left,
                            right,
                        }),
                        expression: Box::new(value),
                    });
                },
                _ => return Err(self.error(&equals, "invalid assignment target".into())),
            }
        } else if self.token_type_matches(&[TokenType::MinusEqual, TokenType::PlusEqual]) {
            let mut token = self.previous();
            token.token_type = match &token.token_type {
                TokenType::MinusEqual => TokenType::Minus,
                TokenType::PlusEqual => TokenType::Plus,
                _ => panic!("unreachable!"),
            };

            let value = self.assignment()?;

            match expr {
                Expression::Variable { name } => {
                    return Ok(Expression::Assignment {
                        name: name.clone(),
                        expression: Box::new(Expression::Binary {
                            left: Box::new(Expression::Variable { name }),
                            operator: token,
                            right: Box::new(value),
                        }),
                    });
                },
                Expression::Get { name, object } => {
                    return Ok(Expression::Set {
                        name: name.clone(),
                        object: object.clone(),
                        value: Box::new(Expression::Binary {
                            left: Box::new(Expression::Get { name, object }),
                            operator: token,
                            right: Box::new(value),
                        }),
                    })
                },
                Expression::Index {
                    item,
                    is_slice,
                    left,
                    right,
                } => {
                    return Ok(Expression::IndexedAssignment {
                        indexed_item: Box::new(Expression::Index {
                            item: item.clone(),
                            is_slice,
                            left: left.clone(),
                            right: right.clone(),
                        }),
                        expression: Box::new(Expression::Binary {
                            left: Box::new(Expression::Index {
                                item,
                                is_slice,
                                left,
                                right,
                            }),
                            operator: token,
                            right: Box::new(value),
                        }),
                    });
                },
                _ => return Err(self.error(&token, "invalid assignment target".into())),
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> LoxResult<Expression> {
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

    fn and(&mut self) -> LoxResult<Expression> {
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

    fn equality(&mut self) -> LoxResult<Expression> {
        let expr = self.comparison()?;

        if self.token_type_matches(&[TokenType::BangEqual, TokenType::EqualEqual, TokenType::In]) {
            let operator = self.previous();
            let right = self.comparison()?;
            let new_expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };

            Ok(new_expr)
        } else if self.token_type_matches(&[TokenType::NotIn]) {
            // This is a special case, with a two-token operator.
            let operator = self.previous();

            self.consume(&TokenType::In, "expect 'in' after 'not'".into())?;

            let right = self.comparison()?;

            let new_expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };

            Ok(new_expr)
        } else {
            Ok(expr)
        }
    }

    fn comparison(&mut self) -> LoxResult<Expression> {
        let expr = self.term()?;

        if self.token_type_matches(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous();
            let right = self.term()?;
            let new_expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };

            return Ok(new_expr);
        }

        Ok(expr)
    }

    fn term(&mut self) -> LoxResult<Expression> {
        let expr = self.factor()?;

        if self.token_type_matches(&[TokenType::Minus, TokenType::Plus]) {
            let operator = self.previous();
            let right = self.term()?;
            let new_expr = Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };

            return Ok(new_expr);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> LoxResult<Expression> {
        let expr = self.unary()?;

        if self.token_type_matches(&[TokenType::Percent, TokenType::Slash, TokenType::Star]) {
            let operator = self.previous();
            let right = self.factor()?;

            return Ok(Expression::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn unary(&mut self) -> LoxResult<Expression> {
        if self.token_type_matches(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous();
            let right = self.unary()?;
            let new_expr = Expression::Unary {
                operator,
                right: Box::new(right),
            };

            return Ok(new_expr);
        }

        self.call()
    }

    fn finish_call(&mut self, callee: Expression) -> LoxResult<Expression> {
        let mut arguments: Vec<Expression> = Vec::new();
        let invocation = matches!(callee, Expression::Get { .. });

        if !self.check_current_token(&TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    let next_token = self.peek();
                    self.error(&next_token, "can't have more than 255 arguments".into());
                }

                arguments.push(self.expression()?);

                if !self.token_type_matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(&TokenType::RightParen, "expect ')' after arguments".into())?;

        Ok(Expression::Call {
            callee: Box::new(callee),
            paren,
            arguments: Box::new(arguments),
            invocation,
        })
    }

    fn finish_list(&mut self) -> LoxResult<Expression> {
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
            "expect ']' after list items".into(),
        )?;

        Ok(Expression::List {
            expressions: Box::new(list),
        })
    }

    fn finish_map(&mut self) -> LoxResult<Expression> {
        let mut items: Vec<Expression> = Vec::new();

        if !self.check_current_token(&TokenType::RightBrace) {
            loop {
                let key = self.expression()?;
                items.push(key);

                self.consume(
                    &TokenType::Colon,
                    "use ':' to separate keys and values".into(),
                )?;

                let value = self.expression()?;
                items.push(value);

                if !self.token_type_matches(&[TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(&TokenType::RightBrace, "expect ']' after list items".into())?;

        Ok(Expression::Map {
            expressions: Box::new(items),
        })
    }

    fn finish_super(&mut self) -> LoxResult<Expression> {
        let token = self.previous();
        self.consume(&TokenType::Dot, "expect '.' after 'super'".into())?;
        let name = self.consume(
            &TokenType::Identifier,
            "expect identifier after 'super.".into(),
        )?;

        Ok(Expression::Super { token, name })
    }

    fn call(&mut self) -> LoxResult<Expression> {
        let mut expr = self.primary()?;

        loop {
            if self.token_type_matches(&[TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.token_type_matches(&[TokenType::Dot]) {
                let name = self.consume(
                    &TokenType::Identifier,
                    "expect property name after '.'".into(),
                )?;

                expr = Expression::Get {
                    name,
                    object: Box::new(expr),
                };
            } else if self.token_type_matches(&[TokenType::LeftBracket]) {
                let (is_slice, left, right) = if self.token_type_matches(&[TokenType::Colon]) {
                    // this is a [:expr] slice
                    let is_slice = true;
                    let left = None;

                    let right = if self.check_current_token(&TokenType::RightBracket) {
                        None
                    } else {
                        Some(Box::new(self.expression()?))
                    };

                    (is_slice, left, right)
                } else {
                    let left = Some(Box::new(self.expression()?));
                    let (is_slice, right) = if self.token_type_matches(&[TokenType::Colon]) {
                        let is_slice = true;

                        let right = if self.check_current_token(&TokenType::RightBracket) {
                            None
                        } else {
                            Some(Box::new(self.expression()?))
                        };

                        (is_slice, right)
                    } else {
                        (false, None)
                    };

                    (is_slice, left, right)
                };

                self.consume(
                    &TokenType::RightBracket,
                    "expect indexes to close  with a ']'".into(),
                )?;

                expr = Expression::Index {
                    item: Box::new(expr),
                    is_slice,
                    left,
                    right,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn primary(&mut self) -> LoxResult<Expression> {
        if self.token_type_matches(&[TokenType::False]) {
            return Ok(Expression::Literal {
                value: Literal::Boolean(false),
            });
        } else if self.token_type_matches(&[TokenType::True]) {
            return Ok(Expression::Literal {
                value: Literal::Boolean(true),
            });
        } else if self.token_type_matches(&[TokenType::Nil]) {
            return Ok(Expression::Literal {
                value: Literal::Nil,
            });
        } else if self.token_type_matches(&[TokenType::Number, TokenType::String]) {
            return Ok(Expression::Literal {
                value: self.previous().literal.unwrap(),
            });
        } else if self.token_type_matches(&[TokenType::LeftBracket]) {
            return self.finish_list();
        } else if self.token_type_matches(&[TokenType::LeftBrace]) {
            return self.finish_map();
        } else if self.token_type_matches(&[TokenType::Identifier]) {
            return Ok(Expression::Variable {
                name: self.previous(),
            });
        } else if self.token_type_matches(&[TokenType::Super]) {
            return self.finish_super();
        } else if self.token_type_matches(&[TokenType::This]) {
            return Ok(Expression::This {
                token: self.previous(),
            });
        } else if self.token_type_matches(&[TokenType::LeftParen]) {
            let expr = self.expression()?;
            let _ = self.consume(
                &TokenType::RightParen,
                "expect ')' after an expression".into(),
            )?;
            return Ok(Expression::Grouping {
                expression: Box::new(expr),
            });
        }

        let next_token = self.peek();

        Err(self.error(&next_token, "expect expression".into()))
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

    fn consume(&mut self, token_type: &TokenType, message: String) -> LoxResult<Token> {
        if self.check_current_token(token_type) {
            return Ok(self.advance());
        }

        let next_token = self.peek();

        Err(self.error(&next_token, message))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.at_end() {
            if self.previous().token_type == TokenType::Semicolon {
                return;
            }

            match self.peek().token_type {
                TokenType::Class
                | TokenType::Continue
                | TokenType::Break
                | TokenType::Function
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                },
                _ => {},
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
