use crate::errors::LoxError;
use crate::expressions::Expression;
use crate::statements::Statement;
use crate::tokens::{Literal, Token, TokenType};

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
        if self.token_type_matches(&[TokenType::Print]) {
            return self.print_statement();
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

    fn declaration_statement(&mut self) -> Result<Option<Statement>, LoxError> {
        let statement = match self.token_type_matches(&[TokenType::Var]) {
            true => self.var_declaration(),
            false => self.statement(),
        };

        match statement {
            Ok(stmt) => Ok(Some(stmt)),
            Err(_) => {
                self.synchronize();
                Ok(None)
            },
        }
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

        Ok(
            Statement::Expression {
                expression: Box::new(expr.clone()),
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

    fn assignment(&mut self) -> Result<Expression, LoxError> {
        let expr = self.equality()?;

        if self.token_type_matches(&[TokenType::Equal]) {
            let equals = self.previous();
            let value = self.assignment()?;

            match expr {
                Expression::Variable { name } => {
                    return Ok(
                        Expression::Assignment {
                            name,
                            expression: Box::new(value.clone()),
                        }
                    )
                },
                _ => return Err(
                    self.error(&equals, "invalid assignment target".to_string())
                ),
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expression, LoxError> {
        let expr = self.comparison()?;

        if self.token_type_matches(
            &[
                TokenType::BangEqual,
                TokenType::EqualEqual,
            ]
        ) {
            let operator = self.previous();
            let right = self.comparison()?;
            let new_expr = Expression::Binary {
                left: Box::new(expr.clone()),
                operator,
                right: Box::new(right.clone()),
            };

            return Ok(new_expr);
        }

        Ok(expr)
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

        if self.token_type_matches(
            &[
                TokenType::Minus,
                TokenType::Plus,
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

    fn factor(&mut self) -> Result<Expression, LoxError> {
        let expr = self.unary()?;

        if self.token_type_matches(
            &[
                TokenType::Slash,
                TokenType::Star,
            ]
        ) {
            let operator = self.previous();
            let right = self.unary()?;
            let new_expr = Expression::Binary {
                left: Box::new(expr.clone()),
                operator,
                right: Box::new(right.clone()),
            };

            return Ok(new_expr);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expression, LoxError> {
        if self.token_type_matches(
            &[
                TokenType::Bang,
                TokenType::Minus,
            ]
        ) {
            let operator = self.previous();
            let right = self.unary()?;
            let new_expr = Expression::Unary {
                operator,
                right: Box::new(right.clone()),
            };

            return Ok(new_expr);
        }

        self.primary()
    }

    fn primary(&mut self) ->  Result<Expression, LoxError> {
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
            return LoxError::ParseError(next_token.line, format!("at end {}", message))
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

    fn consume(
        &mut self,
        token_type: &TokenType,
        message: String,
    ) -> Result<Token, LoxError> {
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
                TokenType::Class |
                TokenType::Fun |
                TokenType::Var |
                TokenType::For |
                TokenType::If |
                TokenType::While |
                TokenType::Print |
                TokenType::Return => {
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
