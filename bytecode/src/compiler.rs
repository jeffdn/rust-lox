use crate::{
    chunk::{Chunk, OpCode},
    errors::LoxError,
    scanner::Scanner,
    tokens::{Token, TokenType},
    value::Value,
};

#[derive(Clone, Debug)]
enum Precedence {
    NoPrecedence,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn get_parent(&self) -> Self {
        match self {
            Precedence::NoPrecedence => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::NoPrecedence,
        }
    }

    fn for_token_type(token_type: &TokenType) -> Self {
        match token_type {
            TokenType::Minus => Precedence::Term,
            TokenType::Plus => Precedence::Term,
            TokenType::Slash => Precedence::Factor,
            TokenType::Star => Precedence::Factor,
            _ => Precedence::NoPrecedence,
        }
    }
}


pub struct Compiler {
    current: Token,
    previous: Token,
    scanner: Scanner,
    chunk: Chunk,
    panic_mode: bool,
}

impl Compiler {
    pub fn new(source: &String) -> Compiler {
        Compiler {
            current: Token {
                token_type: TokenType::Eof,
                start: 0,
                length: 0,
                line: 0,
            },
            previous: Token {
                token_type: TokenType::Eof,
                start: 0,
                length: 0,
                line: 0,
            },
            scanner: Scanner::new(source),
            chunk: Chunk::new(),
            panic_mode: false,
        }
    }

    pub fn compile(&mut self) -> Result<Chunk, LoxError> {
        self.advance()?;

        self.expression()?;

        self.consume(TokenType::Eof, "expect end of expression")?;
        self.emit_byte(OpCode::Return);

        #[cfg(feature = "debug")]
        if self.panic_mode {
            self.chunk.disassemble("code");
        }

        Ok(self.chunk.clone())
    }

    fn emit_byte(&mut self, byte: OpCode) {
        self.chunk.write(byte, self.previous.line);
    }

    fn expression(&mut self) -> Result<(), LoxError> {
        self.parse_precedence(&Precedence::Assignment)
    }

    fn binary(&mut self) -> Result<(), LoxError> {
        let operator_type = self.previous.token_type.clone();
        let precedence = Precedence::for_token_type(&operator_type);
        self.parse_precedence(&precedence.get_parent())?;

        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Subtract),
            TokenType::Star => self.emit_byte(OpCode::Multiply),
            TokenType::Slash => self.emit_byte(OpCode::Divide),
            _ => {},
        };

        Ok(())
    }

    fn grouping(&mut self) -> Result<(), LoxError> {
        self.expression()?;

        self.consume(TokenType::RightParen, "expect ')' after expression")
    }

    fn number(&mut self) -> Result<(), LoxError> {
        let value: f32 = match self.scanner.get_string(&self.previous).parse() {
            Ok(value) => value,
            Err(_) => return self.error("unable to extract number"),
        };

        let constant = self.make_constant(Value::Number(value))?;

        self.emit_byte(OpCode::Constant(constant));

        Ok(())
    }

    fn unary(&mut self) -> Result<(), LoxError> {
        let operator_type = self.previous.token_type.clone();

        self.parse_precedence(&Precedence::Unary)?;

        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            _ => {},
        };

        Ok(())
    }

    fn get_prefix_rule(&self, token_type: &TokenType) -> Option<fn(&mut Self) -> Result<(), LoxError>> {
        match token_type {
            &TokenType::LeftParen => Some(Compiler::grouping),
            &TokenType::Minus => Some(Compiler::unary),
            &TokenType::Number => Some(Compiler::number),
            _ => None,
        }
    }

    fn get_infix_rule(&self, token_type: &TokenType) -> Option<fn(&mut Self) -> Result<(), LoxError>> {
        match token_type {
            &TokenType::Minus |
            &TokenType::Plus |
            &TokenType::Star |
            &TokenType::Slash => Some(Compiler::binary),
            _ => None,
        }
    }

    fn parse_precedence(&mut self, precedence: &Precedence) -> Result<(), LoxError> {
        self.advance()?;

        let prefix_rule = match self.get_prefix_rule(&self.previous.token_type) {
            Some(rule) => rule,
            None => return self.error("expect expression -- prefix"),
        };

        prefix_rule(self)?;

        let precedence_val = precedence.clone() as u8;
        while precedence_val <= Precedence::for_token_type(&self.current.token_type) as u8 {
            self.advance()?;

            let infix_rule = match self.get_infix_rule(&self.previous.token_type) {
                Some(rule) => rule,
                None => break,
            };

            infix_rule(self)?;
        }

        Ok(())
    }

    fn make_constant(&mut self, value: Value) -> Result<usize, LoxError> {
        let constant = self.chunk.add_constant(value);

        if constant > usize::MAX {
            self.error("too many constants in one chunk")?;
        }

        Ok(constant)
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<(), LoxError> {
        if self.current.token_type == token_type {
            self.advance()?;
            return Ok(());
        }

        self.error_at_current(message.into())
    }

    fn advance(&mut self) -> Result<(), LoxError> {
        self.previous = self.current.clone();

        loop {
            self.current = self.scanner.scan_token()?;

            if self.current.token_type != TokenType::Error {
                break;
            }

            self.error_at_current("")?;
        }

        Ok(())
    }

    fn error_at(&mut self, token: Token, message: &str) -> Result<(), LoxError> {
        let output = match token.token_type {
            TokenType::Eof => "at end of file".into(),
            TokenType::Error => "unknown".into(),
            _ => format!("at {}", self.scanner.get_string(&token)),
        };

        self.panic_mode = true;

        Err(LoxError::CompileError(format!("{}: {}", output, message)))
    }

    fn error(&mut self, message: &str) -> Result<(), LoxError> {
        self.error_at(self.previous.clone(), message)
    }

    fn error_at_current(&mut self, message: &str) -> Result<(), LoxError> {
        self.error_at(self.current.clone(), message)
    }
}
