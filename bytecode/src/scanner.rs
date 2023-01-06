use crate::{
    errors::LoxError,
    tokens::{Token, TokenType},
};

pub struct Scanner {
    chars: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(source: &str) -> Scanner {
        Scanner {
            chars: source.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Result<Token, LoxError> {
        self.skip_whitespace();
        self.start = self.current;

        if self.at_end() {
            return Ok(Token {
                token_type: TokenType::Eof,
                start: self.start,
                length: 1,
                line: self.line,
            });
        }

        let next = self.advance();

        match next {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '[' => self.make_token(TokenType::LeftBracket),
            ']' => self.make_token(TokenType::RightBracket),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ':' => self.make_token(TokenType::Colon),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '%' => self.make_token(TokenType::Percent),
            '-' => match self.check_next('=') {
                true => self.make_token(TokenType::MinusEqual),
                false => self.make_token(TokenType::Minus),
            },
            '+' => match self.check_next('=') {
                true => self.make_token(TokenType::PlusEqual),
                false => self.make_token(TokenType::Plus),
            },
            '/' => match self.check_next('=') {
                true => self.make_token(TokenType::SlashEqual),
                false => self.make_token(TokenType::Slash),
            },
            '*' => match self.check_next('=') {
                true => self.make_token(TokenType::StarEqual),
                false => self.make_token(TokenType::Star),
            },
            '!' => match self.check_next('=') {
                true => self.make_token(TokenType::BangEqual),
                false => self.make_token(TokenType::Bang),
            },
            '=' => match self.check_next('=') {
                true => self.make_token(TokenType::EqualEqual),
                false => self.make_token(TokenType::Equal),
            },
            '<' => match self.check_next('=') {
                true => self.make_token(TokenType::LessEqual),
                false => self.make_token(TokenType::Less),
            },
            '>' => match self.check_next('=') {
                true => self.make_token(TokenType::GreaterEqual),
                false => self.make_token(TokenType::Greater),
            },
            '"' => self.add_string('"'),
            '\'' => self.add_string('\''),
            _ => {
                if next.is_ascii_digit() {
                    return self.add_number();
                } else if self.is_alpha(next) {
                    return self.add_identifier();
                }

                Err(LoxError::ParseError("no matching token".into()))
            }
        }
    }

    fn is_alpha(&self, c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        matches!(c, '0'..='9' | 'a'..='z' | 'A'..='Z' | '_')
    }

    fn find_identifier(&self, input_lexeme: &str) -> Result<Token, LoxError> {
        let token_type = match input_lexeme {
            "and" => TokenType::And,
            "as" => TokenType::As,
            "assert" => TokenType::Assert,
            "break" => TokenType::Break,
            "class" => TokenType::Class,
            "continue" => TokenType::Continue,
            "else" => TokenType::Else,
            "delete" => TokenType::Delete,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "foreach" => TokenType::Foreach,
            "fn" => TokenType::Function,
            "if" => TokenType::If,
            "import" => TokenType::Import,
            "in" => TokenType::In,
            "nil" => TokenType::Nil,
            "not" => TokenType::NotIn,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "this" => TokenType::This,
            "true" => TokenType::True,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            _ => TokenType::Identifier,
        };

        self.make_token(token_type)
    }

    fn add_identifier(&mut self) -> Result<Token, LoxError> {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }

        let output: String = self.chars[self.start..self.current].iter().collect();
        self.find_identifier(&output)
    }

    fn add_string(&mut self, end: char) -> Result<Token, LoxError> {
        while !self.at_end() && self.peek() != end {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.at_end() {
            return Err(LoxError::ParseError("unterminated string".into()));
        }

        self.advance();

        self.make_token(TokenType::String)
    }

    fn add_number(&mut self) -> Result<Token, LoxError> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        if self.peek() == '.' && self.peek_twice().is_ascii_digit() {
            self.advance();

            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => match self.peek_twice() == '/' {
                    true => {
                        while !self.at_end() && self.peek() != '\n' {
                            self.advance();
                        }
                    }
                    false => break,
                },
                _ => break,
            };
        }
    }

    fn peek(&self) -> char {
        if self.current == self.chars.len() - 1 {
            return '\0';
        }

        self.chars[self.current]
    }

    fn peek_twice(&self) -> char {
        if self.current + 1 >= self.chars.len() {
            return '\0';
        }

        self.chars[self.current + 1]
    }

    fn at_end(&self) -> bool {
        self.peek() == '\0'
    }

    fn check_next(&mut self, expected: char) -> bool {
        if self.at_end() || self.peek() != expected {
            return false;
        }

        self.current += 1;

        true
    }

    fn make_token(&self, token_type: TokenType) -> Result<Token, LoxError> {
        Ok(Token {
            token_type,
            start: self.start,
            length: self.current - self.start,
            line: self.line,
        })
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.chars[self.current - 1]
    }

    pub fn get_string(&self, token: &Token) -> String {
        self.chars[token.start..(token.start + token.length)]
            .iter()
            .collect()
    }

    pub fn get_string_literal(&self, token: &Token) -> String {
        self.chars[(token.start + 1)..(token.start + token.length - 1)]
            .iter()
            .collect()
    }
}
