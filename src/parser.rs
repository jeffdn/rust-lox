use crate::errors::LoxError;
use crate::expressions::Expression;
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

    pub fn parse(&mut self) -> Result<Expression, LoxError> {
        // begin the recursive descent
        self.expression()
    }

    fn expression(&mut self) -> Result<Expression, LoxError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expression, LoxError> {
        let expr = self.comparison()?;

        if self.token_type_matches(
            &[
                TokenType::BangEqual,
                TokenType::EqualEqual,
            ]
        ) {
            let operator = self.previous().clone();
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
            let operator = self.previous().clone();
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
            let operator = self.previous().clone();
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
            let operator = self.previous().clone();
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
            let operator = self.previous().clone();
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

        Err(self.error("expect expression".to_string()))
    }

    fn error(&mut self, message: String) -> LoxError {
        let next_token = self.peek();

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

        Err(self.error(message))
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

    fn peek(&mut self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn previous(&mut self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn at_end(&mut self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn advance(&mut self) -> &Token {
        if !self.at_end() {
            self.current += 1;
        }

        self.previous()
    }
}
