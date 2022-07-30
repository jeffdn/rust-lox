use std::boxed::Box;

use crate::{
    chunk::{Chunk, OpCode},
    errors::LoxError,
    object::{Function, FunctionType, Object},
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
            TokenType::LeftParen => Precedence::Call,
            _ => Precedence::NoPrecedence,
        }
    }
}

struct Local {
    name: Token,
    depth: Option<usize>,
}

pub struct Compiler {
    compilers: Vec<Compiler>,
    functions: Vec<Function>,

    current: Token,
    previous: Token,
    scanner: Scanner,
    panic_mode: bool,

    locals: Vec<Local>,
    local_count: usize,
    scope_depth: usize,
}

impl Compiler {
    pub fn new(function_type: FunctionType) -> Compiler {
        let mut compiler = Compiler {
            compilers: Vec::new(),
            functions: Vec::new(),
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
            scanner: Scanner::new(&"".into()),
            panic_mode: false,

            locals: Vec::new(),
            local_count: 1,
            scope_depth: 0,
        };

        compiler.locals.push(
            Local {
                name: Token {
                    token_type: TokenType::Skip,
                    start: 0,
                    length: 0,
                    line: 0,
                },
                depth: Some(0),
            }
        );

        compiler.functions.push(
            Function {
                function_type,
                obj: None,
                arity: 0,
                chunk: Chunk::new(),
                name: "<script>".into(),
            }
        );


        compiler
    }

    fn init_compiler(&mut self, function_type: FunctionType) {
        let mut compiler = Compiler::new(function_type);
        compiler.functions.last_mut().unwrap().name = self.scanner.get_string(&self.previous);

        self.compilers.push(compiler);
    }

    fn compiler(&self) -> &Compiler {
        match self.compilers.is_empty() {
            true => self,
            false => match self.compilers.last() {
                Some(compiler) => compiler,
                None => panic!("unreachable"),
            }
        }
    }

    fn compiler_mut(&mut self) -> &mut Compiler {
        match self.compilers.is_empty() {
            true => self,
            false => match self.compilers.last_mut() {
                Some(compiler) => compiler,
                None => panic!("unreachable"),
            }
        }
    }

    fn current_function(&mut self) -> &mut Function {
        match self.compiler_mut().functions.last_mut() {
            Some(function) => function,
            None => panic!("unreachable"),
        }
    }

    fn chunk(&mut self) -> &mut Chunk {
        &mut self.compiler_mut().current_function().chunk
    }

    pub fn compile(&mut self, source: &String) -> Result<Function, LoxError> {
        self.scanner = Scanner::new(source);

        self.advance()?;

        while !self.token_type_matches(&TokenType::Eof)? {
            self.declaration()?;
        }

        self.emit_return()?;

        #[cfg(feature = "debug")]
        if self.panic_mode {
            self.chunk().disassemble("code");
        }

        Ok(self.functions.pop().unwrap())
    }

    fn end_compiler(&mut self) -> Result<Function, LoxError> {
        self.emit_return()?;

        #[cfg(feature = "debug")]
        if self.panic_mode {
            self.chunk().disassemble("code");
        }

        Ok(self.compilers.pop().unwrap().functions.pop().unwrap())
    }

    fn begin_scope(&mut self) -> Result<(), LoxError> {
        self.compiler_mut().scope_depth += 1;
        Ok(())
    }

    fn end_scope(&mut self) -> Result<(), LoxError> {
        self.compiler_mut().scope_depth -= 1;

        let mut pop_count = self.compiler().locals.len();
        let scope_depth = self.compiler().scope_depth;
        self.compiler_mut().locals.retain(|x| x.depth.is_some() && x.depth.unwrap() <= scope_depth);

        pop_count -= self.compiler().locals.len();

        for _ in 0..pop_count {
            self.compiler_mut().local_count -= 1;
            self.emit_byte(OpCode::Pop)?;
        }

        Ok(())
    }

    fn emit_byte(&mut self, byte: OpCode) -> Result<(), LoxError>{
        let previous_line = self.previous.line;
        self.chunk().write(byte, previous_line);

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> Result<(), LoxError> {
        let offset = self.chunk().code.len() - loop_start;

        self.emit_byte(OpCode::Loop(offset))
    }

    fn emit_jump(&mut self, byte: OpCode) -> Result<usize, LoxError>{
        let previous_line = self.previous.line;
        self.chunk().write(byte, previous_line);

        Ok(self.chunk().code.len())
    }

    fn emit_return(&mut self) -> Result<(), LoxError> {
        self.emit_byte(OpCode::Nil)?;
        self.emit_byte(OpCode::Return)
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), LoxError> {
        let jump = self.chunk().code.len() - offset;

        self.chunk().code[offset - 1] = match self.chunk().code[offset - 1] {
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

    fn function(&mut self, function_type: FunctionType) -> Result<(), LoxError> {
        self.init_compiler(function_type);

        self.begin_scope()?;

        self.consume(TokenType::LeftParen, "expect '(' after function name")?;

        if !self.check_current_token(&TokenType::RightParen) {
            loop {
                self.current_function().arity += 1;
                let constant = self.parse_variable("expect parameter name")?;
                self.define_variable(constant)?;

                if !self.token_type_matches(&TokenType::Comma)? {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "expect ')' after parameter list")?;
        self.consume(TokenType::LeftBrace, "expect '{' before function body")?;

        self.block()?;

        let function = self.end_compiler()?;
        let constant = self.make_constant(
            Value::Object(
                Object::Function(Box::new(function))
            )
        )?;

        self.emit_byte(OpCode::Constant(constant))
    }

    fn function_declaration(&mut self) -> Result<(), LoxError> {
        let global = self.parse_variable("expect function name")?;

        self.mark_initialized()?;

        self.function(FunctionType::Function)?;

        self.define_variable(global)
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

        let mut loop_start = self.chunk().code.len();
        let mut exit_jump: Option<usize> = None;

        if !self.token_type_matches(&TokenType::Semicolon)? {
            self.expression()?;
            self.consume(TokenType::Semicolon, "expect ';' after loop condition")?;

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse(0))?);
            self.emit_byte(OpCode::Pop)?;
        }

        if !self.token_type_matches(&TokenType::RightParen)? {
            let body_jump = self.emit_jump(OpCode::Jump(0))?;
            let increment_start = self.chunk().code.len();

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

    fn return_statement(&mut self) -> Result<(), LoxError> {
        match self.token_type_matches(&TokenType::Semicolon)? {
            true => self.emit_return(),
            false => {
                self.expression()?;
                self.consume(TokenType::Semicolon, "expect ';' after value")?;
                self.emit_byte(OpCode::Return)
            },
        }
    }

    fn while_statement(&mut self) -> Result<(), LoxError> {
        let loop_start = self.chunk().code.len();

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
        if self.token_type_matches(&TokenType::Function)? {
            self.function_declaration()?;
        } else if self.token_type_matches(&TokenType::Var)? {
            self.var_declaration()?;
        } else {
            self.statement()?
        }

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
        } else if self.token_type_matches(&TokenType::Return)? {
            self.return_statement()
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

    fn call(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let arg_count = self.argument_list()?;

        self.emit_byte(OpCode::Call(arg_count))?;

        Ok(())
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
        let value: f64 = match self.scanner.get_string(&self.previous).parse() {
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

    fn resolve_local(&self, token: &Token) -> Result<usize, LoxError> {
        match self.compiler().locals.iter().enumerate().rev().find(|(_, x)| {
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
            &TokenType::LeftParen => Some(Compiler::call),
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
        if self.compiler().scope_depth == 0 {
            return Ok(());
        }

        let local = self.compiler().locals.iter().find(|local| local.name == self.previous);

        if local.is_some() {
            self.error("already a variable with this name in this scope")?;
        }

        self.compiler_mut().local_count += 1;
        let local_name = self.previous.clone();
        self.compiler_mut().locals.push(
            Local {
                name: local_name,
                depth: None,
            }
        );

        Ok(())
    }

    fn parse_variable(&mut self, message: &str) -> Result<usize, LoxError> {
        self.consume(TokenType::Identifier, message)?;

        self.declare_variable()?;

        if self.compiler().scope_depth > 0 {
            return Ok(0);
        }

        self.identifier_constant(&self.previous.clone())
    }

    fn mark_initialized(&mut self) -> Result<(), LoxError> {
        match self.compiler().scope_depth {
            0 => Ok(()),
            _ => {
                let scope_depth = self.compiler().scope_depth;
                match self.compiler_mut().locals.last_mut() {
                    Some(mut local) => local.depth = Some(scope_depth),
                    None => self.error("this should never happen")?,
                };

                Ok(())
            },
        }
    }

    fn define_variable(&mut self, global: usize) -> Result<(), LoxError> {
        if self.compiler().scope_depth > 0 {
            self.mark_initialized()?;
            return Ok(());
        }

        self.emit_byte(OpCode::DefineGlobal(global))
    }

    fn argument_list(&mut self) -> Result<usize, LoxError> {
        let mut arg_count: usize = 0;

        if !self.check_current_token(&TokenType::RightParen) {
            loop {
                self.expression()?;
                arg_count += 1;

                if !self.token_type_matches(&TokenType::Comma)? {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "expect ')' after arguments")?;

        Ok(arg_count)
    }

    fn and_(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse(0))?;

        self.emit_byte(OpCode::Pop)?;
        self.parse_precedence(&Precedence::And)?;

        self.patch_jump(end_jump)
    }

    fn make_constant(&mut self, value: Value) -> Result<usize, LoxError> {
        let constant = self.chunk().add_constant(value);

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

    fn error_at(&self, token: Token, message: &str) -> Result<(), LoxError> {
        let output = match token.token_type {
            TokenType::Eof => "at end of file".into(),
            TokenType::Error => "unknown".into(),
            _ => format!("at {}", self.scanner.get_string(&token)),
        };

        Err(LoxError::CompileError(format!("{}: {}", output, message)))
    }

    fn error(&self, message: &str) -> Result<(), LoxError> {
        self.error_at(self.previous.clone(), message)
    }

    fn error_at_current(&self, message: &str) -> Result<(), LoxError> {
        self.error_at(self.current.clone(), message)
    }
}
