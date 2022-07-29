use std::boxed::Box;

use crate::{
    chunk::{Chunk, OpCode},
    errors::LoxError,
    object::Object,
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
            TokenType::BangEqual |
            TokenType::EqualEqual => Precedence::Equality,
            TokenType::Greater |
            TokenType::GreaterEqual |
            TokenType::Less |
            TokenType::LessEqual => Precedence::Comparison,
            TokenType::And => Precedence::And,
            TokenType::Or => Precedence::Or,
            _ => Precedence::NoPrecedence,
        }
    }
}

struct Local {
    name: Token,
    depth: Option<usize>,
}

pub struct Compiler {
    current: Token,
    previous: Token,
    scanner: Scanner,
    chunk: Chunk,
    panic_mode: bool,

    locals: Vec<Local>,
    local_count: usize,
    scope_depth: usize,
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

            locals: Vec::new(),
            local_count: 0,
            scope_depth: 0,
        }
    }

    pub fn compile(&mut self) -> Result<Chunk, LoxError> {
        self.advance()?;


        while !self.token_type_matches(&TokenType::Eof)? {
            self.declaration()?;
        }

        #[cfg(feature = "debug")]
        if self.panic_mode {
            self.chunk.disassemble("code");
        }

        Ok(self.chunk.clone())
    }

    fn begin_scope(&mut self) -> Result<(), LoxError> {
        self.scope_depth += 1;
        Ok(())
    }

    fn end_scope(&mut self) -> Result<(), LoxError> {
        self.scope_depth -= 1;

        let mut pop_count = self.locals.len();
        self.locals.retain(|x| x.depth.is_some() && x.depth.unwrap() <= self.scope_depth);

        pop_count -= self.locals.len();

        for _ in 0..pop_count {
            self.local_count -= 1;
            self.emit_byte(OpCode::Pop)?;
        }

        Ok(())
    }

    fn emit_byte(&mut self, byte: OpCode) -> Result<(), LoxError>{
        self.chunk.write(byte, self.previous.line);

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> Result<(), LoxError> {
        let offset = self.chunk.code.len() - loop_start;

        self.emit_byte(OpCode::Loop(offset))
    }

    fn emit_jump(&mut self, byte: OpCode) -> Result<usize, LoxError>{
        self.chunk.write(byte, self.previous.line);

        Ok(self.chunk.code.len())
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), LoxError> {
        let jump = self.chunk.code.len() - offset;

        self.chunk.code[offset - 1] = match self.chunk.code[offset - 1] {
            OpCode::Jump(_) => OpCode::Jump(jump),
            OpCode::JumpIfFalse(_) => OpCode::JumpIfFalse(jump),
            _ => return Err(LoxError::RuntimeError("unreachable".into())),
        };

        Ok(())

    }

    fn expression(&mut self) -> Result<(), LoxError> {
        self.parse_precedence(&Precedence::Assignment)
    }

    fn block(&mut self) -> Result<(), LoxError> {
        loop {
            match &self.current.token_type {
                &TokenType::RightBrace |
                &TokenType::Eof => break,
                _ => self.declaration()?
            };
        }

        self.consume(TokenType::RightBrace, "expect '}' after block")
    }

    fn var_declaration(&mut self) -> Result<(), LoxError> {
        let global: usize = self.parse_variable("expect variable name")?;

        match self.token_type_matches(&TokenType::Equal)? {
            true => self.expression()?,
            false => self.emit_byte(OpCode::Nil)?,
        };

        self.consume(TokenType::Semicolon, "expect ';' after variable declaration")?;

        self.define_variable(global)
    }

    fn expression_statement(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "expect ';' after value")?;

        self.emit_byte(OpCode::Pop)
    }

    fn for_statement(&mut self) -> Result<(), LoxError> {
        self.begin_scope()?;

        self.consume(TokenType::LeftParen, "expect '(' after 'for'")?;

        if self.token_type_matches(&TokenType::Semicolon)? {
            // No initializer
        } else if self.token_type_matches(&TokenType::Var)? {
            self.var_declaration()?;
        } else {
            self.expression_statement()?;
        }

        let mut loop_start = self.chunk.code.len();
        let mut exit_jump: Option<usize> = None;

        if !self.token_type_matches(&TokenType::Semicolon)? {
            self.expression()?;
            self.consume(TokenType::Semicolon, "expect ';' after loop condition")?;

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse(0))?);
            self.emit_byte(OpCode::Pop)?;
        }

        if !self.token_type_matches(&TokenType::RightParen)? {
            let body_jump = self.emit_jump(OpCode::Jump(0))?;
            let increment_start = self.chunk.code.len();

            self.expression()?;
            self.emit_byte(OpCode::Pop)?;

            self.consume(TokenType::RightParen, "expect ')' after for clauses")?;

            self.emit_loop(loop_start)?;
            loop_start = increment_start;
            self.patch_jump(body_jump)?;
        }

        self.statement()?;
        self.emit_loop(loop_start)?;

        match exit_jump {
            Some(exit_jump) => {
                self.patch_jump(exit_jump)?;
                self.emit_byte(OpCode::Pop)?;
            },
            None => {},
        };

        self.end_scope()
    }

    fn if_statement(&mut self) -> Result<(), LoxError> {
        self.consume(TokenType::LeftParen, "expect '(' after 'if'")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "expect ')' after if statement")?;

        let then_jump = self.emit_jump(OpCode::JumpIfFalse(0))?;
        self.emit_byte(OpCode::Pop)?;

        self.statement()?;

        let else_jump = self.emit_jump(OpCode::Jump(0))?;

        self.patch_jump(then_jump)?;
        self.emit_byte(OpCode::Pop)?;

        if self.token_type_matches(&TokenType::Else)? {
            self.statement()?;
        }

        self.patch_jump(else_jump)?;

        Ok(())
    }

    fn print_statement(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "expect ';' after value")?;
        self.emit_byte(OpCode::Print)
    }

    fn while_statement(&mut self) -> Result<(), LoxError> {
        let loop_start = self.chunk.code.len();

        self.consume(TokenType::LeftParen, "expect '(' after 'while'")?;
        self.expression()?;
        self.consume(TokenType::RightParen, "expect ')' after while statement")?;

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse(0))?;
        self.emit_byte(OpCode::Pop)?;
        self.statement()?;

        self.emit_loop(loop_start)?;

        self.patch_jump(exit_jump)?;
        self.emit_byte(OpCode::Pop)
    }

    fn synchronize(&mut self) -> Result<(), LoxError> {
        self.panic_mode = false;

        while !self.check_current_token(&TokenType::Eof) {
            if self.previous.token_type == TokenType::Semicolon {
                return Ok(());
            }

            match self.current.token_type {
                TokenType::Class |
                TokenType::Function |
                TokenType::Var |
                TokenType::For |
                TokenType::If |
                TokenType::While |
                TokenType::Print |
                TokenType::Return => break,
                _ => self.advance()?,
            };
        }

        Ok(())
    }

    fn declaration(&mut self) -> Result<(), LoxError> {
        match self.token_type_matches(&TokenType::Var)? {
            true => self.var_declaration()?,
            false => self.statement()?
        };

        if self.panic_mode {
            self.synchronize()?;
        }

        Ok(())
    }

    fn statement(&mut self) -> Result<(), LoxError> {
        if self.token_type_matches(&TokenType::Print)? {
            self.print_statement()
        } else if self.token_type_matches(&TokenType::For)? {
            self.for_statement()
        } else if self.token_type_matches(&TokenType::If)? {
            self.if_statement()
        } else if self.token_type_matches(&TokenType::While)? {
            self.while_statement()
        } else if self.token_type_matches(&TokenType::LeftBrace)? {
            self.begin_scope()?;
            self.block()?;
            self.end_scope()
        } else {
            self.expression_statement()
        }
    }

    fn binary(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let operator_type = self.previous.token_type.clone();
        let precedence = Precedence::for_token_type(&operator_type);
        self.parse_precedence(&precedence.get_parent())?;

        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::Add),
            TokenType::Minus => self.emit_byte(OpCode::Subtract),
            TokenType::Star => self.emit_byte(OpCode::Multiply),
            TokenType::Slash => self.emit_byte(OpCode::Divide),
            TokenType::BangEqual => {
                self.emit_byte(OpCode::Equal)?;
                self.emit_byte(OpCode::Not)
            },
            TokenType::EqualEqual => self.emit_byte(OpCode::Equal),
            TokenType::Greater => self.emit_byte(OpCode::Greater),
            TokenType::GreaterEqual => {
                self.emit_byte(OpCode::Less)?;
                self.emit_byte(OpCode::Not)
            },
            TokenType::Less => self.emit_byte(OpCode::Less),
            TokenType::LessEqual => {
                self.emit_byte(OpCode::Greater)?;
                self.emit_byte(OpCode::Not)
            },
            _ => self.error("unreachable"),
        }
    }

    fn literal(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        match &self.previous.token_type {
            &TokenType::Nil => self.emit_byte(OpCode::Nil),
            &TokenType::False => self.emit_byte(OpCode::False),
            &TokenType::True => self.emit_byte(OpCode::True),
            _ => self.error("unreachable"),
        }
    }

    fn grouping(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        self.expression()?;

        self.consume(TokenType::RightParen, "expect ')' after expression")
    }

    fn number(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let value: f32 = match self.scanner.get_string(&self.previous).parse() {
            Ok(value) => value,
            Err(_) => return self.error("unable to extract number"),
        };

        let constant = self.make_constant(Value::Number(value))?;

        self.emit_byte(OpCode::Constant(constant))
    }

    fn or_(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse(0))?;
        let and_jump = self.emit_jump(OpCode::Jump(0))?;

        self.patch_jump(else_jump)?;
        self.emit_byte(OpCode::Pop)?;

        self.parse_precedence(&Precedence::Or)?;
        self.patch_jump(and_jump)
    }

    fn string(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let value = self.scanner.get_string_literal(&self.previous);

        let constant = self.make_constant(Value::Object(Object::String(Box::new(value))))?;

        self.emit_byte(OpCode::Constant(constant))
    }

    fn named_variable(&mut self, token: &Token, can_assign: bool) -> Result<(), LoxError> {
        let (get_op, set_op) = match self.resolve_local(&token) {
            Ok(index) => (OpCode::GetLocal(index), OpCode::SetLocal(index)),
            Err(_) => {
                let index = self.identifier_constant(&token)?;
                (OpCode::GetGlobal(index), OpCode::SetGlobal(index))
            },
        };

        self.identifier_constant(token)?;

        if can_assign && self.token_type_matches(&TokenType::Equal)? {
            self.expression()?;
            self.emit_byte(set_op)
        } else {
            self.emit_byte(get_op)
        }
    }

    fn resolve_local(&mut self, token: &Token) -> Result<usize, LoxError> {
        match self.locals.iter().enumerate().rev().find(|(_, x)| {
            self.scanner.get_string(&x.name) == self.scanner.get_string(token)
        }) {
            Some((idx, local)) => match local.depth {
                Some(_) => Ok(idx),
                None => match self.error("can't read a local variable in its own initializer") {
                    Err(e) => Err(e),
                    _ => Err(LoxError::ResolutionError),
                },
            },
            None => Err(LoxError::ResolutionError),
        }
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), LoxError> {
        self.named_variable(&self.previous.clone(), can_assign)
    }

    fn unary(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let operator_type = self.previous.token_type.clone();

        self.parse_precedence(&Precedence::Unary)?;

        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            TokenType::Bang => self.emit_byte(OpCode::Not),
            _ => self.error("unreachable"),
        }
    }

    fn get_prefix_rule(&mut self, token_type: &TokenType) -> Option<fn(&mut Self, bool) -> Result<(), LoxError>> {
        match token_type {
            &TokenType::LeftParen => Some(Compiler::grouping),
            &TokenType::Minus |
            &TokenType::Bang => Some(Compiler::unary),
            &TokenType::Number => Some(Compiler::number),
            &TokenType::String => Some(Compiler::string),
            &TokenType::Nil |
            &TokenType::True |
            &TokenType::False => Some(Compiler::literal),
            &TokenType::Identifier => Some(Compiler::variable),
            _ => None,
        }
    }

    fn get_infix_rule(&mut self, token_type: &TokenType) -> Option<fn(&mut Self, bool) -> Result<(), LoxError>> {
        match token_type {
            &TokenType::Minus |
            &TokenType::Plus |
            &TokenType::Star |
            &TokenType::Slash |
            &TokenType::BangEqual |
            &TokenType::EqualEqual |
            &TokenType::Greater |
            &TokenType::GreaterEqual |
            &TokenType::Less |
            &TokenType::LessEqual => Some(Compiler::binary),
            &TokenType::And => Some(Compiler::and_),
            &TokenType::Or => Some(Compiler::or_),
            _ => None,
        }
    }

    fn parse_precedence(&mut self, precedence: &Precedence) -> Result<(), LoxError> {
        self.advance()?;

        let prefix_rule = match self.get_prefix_rule(&self.previous.token_type.clone()) {
            Some(rule) => rule,
            None => return self.error("expect expression -- prefix"),
        };

        let precedence_val = precedence.clone() as u8;
        let can_assign = precedence_val <= Precedence::Assignment as u8;

        prefix_rule(self, can_assign)?;

        while precedence_val <= Precedence::for_token_type(&self.current.token_type) as u8 {
            self.advance()?;

            let infix_rule = match self.get_infix_rule(&self.previous.token_type.clone()) {
                Some(rule) => rule,
                None => break,
            };

            infix_rule(self, can_assign)?;

            if can_assign && self.token_type_matches(&TokenType::Equal)? {
                self.error("invalid assignment target")?;
            }
        }

        Ok(())
    }

    fn identifier_constant(&mut self, token: &Token) -> Result<usize, LoxError> {
        self.make_constant(
            Value::Object(
                Object::String(
                    Box::new(
                        self.scanner.get_string(token)
                    )
                )
            )
        )
    }

    fn declare_variable(&mut self) -> Result<(), LoxError> {
        if self.scope_depth == 0 {
            return Ok(());
        }

        let local = self.locals.iter().find(|local| local.name == self.previous);

        if local.is_some() {
            self.error("already a variable with this name in this scope")?;
        }

        self.local_count += 1;
        self.locals.push(
            Local {
                name: self.previous.clone(),
                depth: None,
            }
        );

        Ok(())
    }

    fn parse_variable(&mut self, message: &str) -> Result<usize, LoxError> {
        self.consume(TokenType::Identifier, message)?;

        self.declare_variable()?;

        if self.scope_depth > 0 {
            return Ok(0);
        }

        self.identifier_constant(&self.previous.clone())
    }

    fn define_variable(&mut self, global: usize) -> Result<(), LoxError> {
        if self.scope_depth > 0 {
            match self.locals.last_mut() {
                Some(mut local) => local.depth = Some(self.scope_depth),
                None => self.error("this should never happen")?,
            };
            return Ok(());
        }

        self.emit_byte(OpCode::DefineGlobal(global))
    }

    fn and_(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse(0))?;

        self.emit_byte(OpCode::Pop)?;
        self.parse_precedence(&Precedence::And)?;

        self.patch_jump(end_jump)
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

    fn token_type_matches(&mut self, token_type: &TokenType) -> Result<bool, LoxError> {
        if !self.check_current_token(token_type) {
            return Ok(false);
        }

        self.advance()?;

        Ok(true)
    }

    fn check_current_token(&mut self, token_type: &TokenType) -> bool {
        &self.current.token_type == token_type
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
