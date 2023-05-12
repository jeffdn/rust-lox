use crate::{
    errors::LoxError,
    tokens::{Literal, Token, TokenType},
};

#[derive(Debug)]
pub struct Scanner {
    source: String,
    chars: Vec<char>,
    errors: Vec<LoxError>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source: source.clone(),
            chars: source.chars().collect(),
            errors: Vec::new(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(&mut self) -> (Vec<Token>, Vec<LoxError>) {
        while !self.at_end() {
            self.start = self.current;
            let next_char = self.advance();
            let possible_token = self.find(next_char);

            match possible_token {
                Ok(token_type) => match token_type {
                    TokenType::Skip => {},
                    _ => self.add_token(token_type),
                },
                Err(e) => self
                    .errors
                    .push(LoxError::SyntaxError(self.line, format!("{}", e))),
            };
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "".to_string(), None, self.line));

        (self.tokens.clone(), self.errors.clone())
    }

    fn at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn add_token(&mut self, token_type: TokenType) {
        self.tokens.push(Token::new(
            token_type,
            self.chars[self.start..self.current].iter().collect(),
            None,
            self.line,
        ));
    }

    pub fn advance(&mut self) -> char {
        let char_at = self.current;
        self.current += 1;

        self.chars[char_at]
    }

    fn find(&mut self, c: char) -> Result<TokenType, LoxError> {
        match c {
            '(' => Ok(TokenType::LeftParen),
            ')' => Ok(TokenType::RightParen),
            '{' => Ok(TokenType::LeftBrace),
            '}' => Ok(TokenType::RightBrace),
            '[' => Ok(TokenType::LeftBracket),
            ']' => Ok(TokenType::RightBracket),
            ':' => Ok(TokenType::Colon),
            ',' => Ok(TokenType::Comma),
            '.' => Ok(TokenType::Dot),
            '%' => Ok(TokenType::Percent),
            ';' => Ok(TokenType::Semicolon),
            '*' => Ok(TokenType::Star),
            '!' => match self.check_next('=') {
                true => Ok(TokenType::BangEqual),
                false => Ok(TokenType::Bang),
            },
            '=' => match self.check_next('=') {
                true => Ok(TokenType::EqualEqual),
                false => Ok(TokenType::Equal),
            },
            '<' => match self.check_next('=') {
                true => Ok(TokenType::LessEqual),
                false => Ok(TokenType::Less),
            },
            '>' => match self.check_next('=') {
                true => Ok(TokenType::GreaterEqual),
                false => Ok(TokenType::Greater),
            },
            '-' => match self.check_next('=') {
                true => Ok(TokenType::MinusEqual),
                false => Ok(TokenType::Minus),
            },
            '+' => match self.check_next('=') {
                true => Ok(TokenType::PlusEqual),
                false => Ok(TokenType::Plus),
            },
            '/' => match self.check_next('/') {
                true => {
                    while self.peek() != '\n' && !self.at_end() {
                        self.advance();
                    }

                    Ok(TokenType::Skip)
                },
                false => Ok(TokenType::Slash),
            },
            ' ' | '\r' | '\t' => Ok(TokenType::Skip),
            '\n' => {
                self.line += 1;
                Ok(TokenType::Skip)
            },
            '"' => self.add_string(),
            _ => {
                if c.is_ascii_digit() {
                    return self.add_number();
                } else if self.is_alpha(c) {
                    return self.add_identifier();
                }

                Err(LoxError::InputError("no matching token".to_string()))
            },
        }
    }

    fn find_identifier(&self, input_lexeme: &str) -> TokenType {
        match input_lexeme {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "foreach" => TokenType::Foreach,
            "fn" => TokenType::Function,
            "if" => TokenType::If,
            "in" => TokenType::In,
            "nil" => TokenType::Nil,
            "not" => TokenType::NotIn,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "println" => TokenType::Println,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        }
    }

    fn is_alpha(&self, c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')
    }

    fn add_identifier(&mut self) -> Result<TokenType, LoxError> {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }

        let output: String = self.chars[self.start..self.current].iter().collect();
        Ok(self.find_identifier(&output))
    }

    fn add_string(&mut self) -> Result<TokenType, LoxError> {
        while self.peek() != '"' && !self.at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.at_end() {
            return Err(LoxError::InputError("unterminated string".to_string()));
        }

        self.advance();

        let literal = Literal::String(
            self.chars[(self.start + 1)..(self.current - 1)]
                .iter()
                .collect(),
        );

        self.tokens.push(Token::new(
            TokenType::String,
            self.chars[self.start..self.current].iter().collect(),
            Some(literal),
            self.line,
        ));

        Ok(TokenType::Skip)
    }

    fn add_number(&mut self) -> Result<TokenType, LoxError> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_twice().is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        let string_literal: String = self.chars[self.start..self.current].iter().collect();
        let literal = Literal::Number(string_literal.parse().unwrap());

        self.tokens.push(Token::new(
            TokenType::Number,
            self.chars[self.start..self.current].iter().collect(),
            Some(literal),
            self.line,
        ));

        Ok(TokenType::Skip)
    }

    fn check_next(&mut self, expected: char) -> bool {
        if self.at_end() || self.chars[self.current] != expected {
            return false;
        }

        self.current += 1;

        true
    }

    fn peek(&self) -> char {
        if self.at_end() {
            return '\0';
        }

        self.chars[self.current]
    }

    fn peek_twice(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }

        self.chars[self.current + 1]
    }
}
