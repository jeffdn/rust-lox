use std::{boxed::Box, collections::HashMap};

use crate::{
    chunk::{Chunk, OpCode, UpValue},
    errors::LoxError,
    object::{Function, FunctionType, Object, ValueMap},
    scanner::Scanner,
    tokens::{Token, TokenType},
    value::Value,
};

#[derive(Clone, Debug)]
enum Precedence {
    None,
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
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => Precedence::None,
        }
    }

    fn for_token_type(token_type: &TokenType) -> Self {
        match token_type {
            TokenType::Minus => Precedence::Term,
            TokenType::Percent => Precedence::Factor,
            TokenType::Plus => Precedence::Term,
            TokenType::Slash => Precedence::Factor,
            TokenType::Star => Precedence::Factor,
            TokenType::BangEqual | TokenType::EqualEqual => Precedence::Equality,
            TokenType::In
            | TokenType::NotIn
            | TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual => Precedence::Comparison,
            TokenType::And => Precedence::And,
            TokenType::Or => Precedence::Or,
            TokenType::Dot | TokenType::LeftBracket | TokenType::LeftParen => Precedence::Call,
            _ => Precedence::None,
        }
    }
}

struct Local {
    name: Token,
    depth: Option<usize>,
    is_captured: bool,
}

#[derive(Default)]
struct ClassCompiler {
    pub has_superclass: bool,
}

pub struct Compiler {
    compilers: Vec<CompilerNode>,
    classes: Vec<ClassCompiler>,

    current: Token,
    previous: Token,
    scanner: Scanner,

    panic_mode: bool,
}

struct CompilerNode {
    locals: Vec<Local>,
    local_count: usize,
    upvalues: Vec<UpValue>,
    scope_depth: usize,
    function: Function,
}

type FixRule = fn(&mut Compiler, bool) -> Result<(), LoxError>;

impl CompilerNode {
    pub fn new(function_type: FunctionType) -> CompilerNode {
        let mut node = CompilerNode {
            locals: Vec::with_capacity(256),
            local_count: 1,
            upvalues: Vec::with_capacity(256),
            scope_depth: 0,
            function: Function {
                function_type: function_type.clone(),
                obj: None,
                arity: 0,
                upvalue_count: 0,
                chunk: Chunk::new(),
                name: "<script>".into(),
            },
        };

        let (token_type, length) = match function_type {
            FunctionType::Method | FunctionType::Initializer => (TokenType::This, 4),
            _ => (TokenType::Skip, 0),
        };

        node.locals.push(Local {
            name: Token {
                token_type,
                length,
                start: 0,
                line: 0,
            },
            depth: Some(0),
            is_captured: false,
        });

        node
    }
}

impl Compiler {
    pub fn new(source: &str) -> Compiler {
        Compiler {
            compilers: vec![CompilerNode::new(FunctionType::Script)],
            classes: Vec::new(),
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
            panic_mode: false,
        }
    }

    fn init_compiler(&mut self, function_type: FunctionType) {
        let mut compiler = CompilerNode::new(function_type);

        compiler.function.name = self.scanner.get_string(&self.previous);

        self.compilers.push(compiler);
    }

    fn compiler_at(&self, depth: usize) -> &CompilerNode {
        let at_index = self.compilers.len() - (1 + depth);
        &self.compilers[at_index]
    }

    fn compiler_at_mut(&mut self, depth: usize) -> &mut CompilerNode {
        let at_index = self.compilers.len() - (1 + depth);
        &mut self.compilers[at_index]
    }

    fn compiler(&self) -> &CompilerNode {
        self.compiler_at(0)
    }

    fn compiler_mut(&mut self) -> &mut CompilerNode {
        self.compiler_at_mut(0)
    }

    fn current_function(&mut self) -> &mut Function {
        &mut self.compiler_mut().function
    }

    fn chunk(&mut self) -> &mut Chunk {
        &mut self.current_function().chunk
    }

    pub fn compile(&mut self) -> Result<Function, LoxError> {
        self.advance()?;

        while !self.token_type_matches(&TokenType::Eof)? {
            self.declaration()?;
        }

        self.emit_return()?;

        #[cfg(feature = "debug")]
        if self.panic_mode {
            self.chunk().disassemble("code");
        }

        Ok(self.compilers.pop().unwrap().function)
    }

    fn end_compiler(&mut self) -> Result<CompilerNode, LoxError> {
        self.emit_return()?;

        #[cfg(feature = "debug")]
        if self.panic_mode {
            self.chunk().disassemble("code");
        }

        Ok(self.compilers.pop().unwrap())
    }

    fn begin_scope(&mut self) -> Result<(), LoxError> {
        self.compiler_mut().scope_depth += 1;
        Ok(())
    }

    fn end_scope(&mut self) -> Result<(), LoxError> {
        self.compiler_mut().scope_depth -= 1;

        loop {
            if self.compiler().local_count == 0 {
                break;
            }

            let idx = self.compiler().local_count - 1;
            if self.compiler().locals[idx].depth.is_some()
                && self.compiler().locals[idx].depth.unwrap() <= self.compiler().scope_depth
            {
                break;
            }

            let local = self.compiler_mut().locals.pop().unwrap();

            match local.is_captured {
                true => self.emit_byte(OpCode::CloseUpValue)?,
                false => self.emit_byte(OpCode::Pop)?,
            };

            self.compiler_mut().local_count -= 1;
        }

        Ok(())
    }

    fn emit_byte(&mut self, byte: OpCode) -> Result<(), LoxError> {
        let previous_line = self.previous.line;
        self.chunk().write(byte, previous_line);

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> Result<(), LoxError> {
        let last_code = self.chunk().code.len();

        self.emit_byte(OpCode::Loop(last_code - loop_start + 1))?;

        // Now, look through all instructions emitted between the start of the loop and
        // the loop instructions. Any that take the shape of either:
        //
        //     OpCode::Break(position, false)
        //     OpCode::Continue(position, false)
        //
        //  Will be replaced with, respectively:
        //
        //     OpCode::Break(end_of_loop, true)
        //     OpCode::Continue(start_of_loop, true)
        for code in self.chunk().code[loop_start..last_code].iter_mut() {
            match code {
                OpCode::Break(initial, false) => {
                    *code = OpCode::Break(last_code - *initial + 1, true)
                },
                OpCode::Continue(initial, false) => {
                    *code = OpCode::Continue(*initial - loop_start + 1, true)
                },
                _ => {},
            };
        }

        Ok(())
    }

    fn emit_jump(&mut self, byte: OpCode) -> Result<usize, LoxError> {
        let previous_line = self.previous.line;
        self.chunk().write(byte, previous_line);

        Ok(self.chunk().code.len())
    }

    fn emit_return(&mut self) -> Result<(), LoxError> {
        match self.current_function().function_type {
            FunctionType::Initializer => self.emit_byte(OpCode::GetLocal(0))?,
            _ => self.emit_byte(OpCode::Nil)?,
        };

        self.emit_byte(OpCode::Return)
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), LoxError> {
        let jump = self.chunk().code.len() - offset;

        self.chunk().code[offset - 1] = match self.chunk().code[offset - 1] {
            OpCode::Jump(_) => OpCode::Jump(jump),
            OpCode::JumpIfFalse(_) => OpCode::JumpIfFalse(jump),
            OpCode::IteratorNext(index, _) => OpCode::IteratorNext(index, jump),
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
                &TokenType::RightBrace | &TokenType::Eof => break,
                _ => self.declaration()?,
            };
        }

        self.consume(TokenType::RightBrace, "expect '}' after block")
    }

    fn function(&mut self, function_type: FunctionType) -> Result<(), LoxError> {
        self.init_compiler(function_type.clone());

        self.begin_scope()?;

        if function_type == FunctionType::Method || function_type == FunctionType::Initializer {
            self.named_variable(&self.synthetic_token(TokenType::This), false)?;
        }

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

        let compiler = self.end_compiler()?;
        let constant =
            self.make_constant(Value::Object(Object::Function(Box::new(compiler.function))))?;

        self.emit_byte(OpCode::Closure(constant, Box::new(compiler.upvalues)))
    }

    fn method(&mut self) -> Result<(), LoxError> {
        self.consume(TokenType::Identifier, "expect method name")?;

        let previous = self.previous.clone();
        let name_constant = self.identifier_constant(&previous)?;
        let name = self.scanner.get_string(&previous);

        let function_type = match name.as_str() {
            "init" => FunctionType::Initializer,
            _ => FunctionType::Method,
        };

        self.function(function_type)?;

        self.emit_byte(OpCode::Method(name_constant))?;

        Ok(())
    }

    fn class_declaration(&mut self) -> Result<(), LoxError> {
        self.consume(TokenType::Identifier, "expect class name")?;
        let class_name = self.previous.clone();
        let name_constant = self.identifier_constant(&class_name)?;
        self.declare_variable()?;

        self.emit_byte(OpCode::Class(name_constant))?;
        self.define_variable(name_constant)?;

        self.classes.push(ClassCompiler::default());

        if self.token_type_matches(&TokenType::Less)? {
            self.consume(TokenType::Identifier, "expect superclass name")?;
            self.variable(false)?;

            if self.previous == class_name {
                self.error("a class cannot inherit from itself")?;
            }

            self.begin_scope()?;
            self.add_local(Some(self.synthetic_token(TokenType::Super)))?;
            self.define_variable(0)?;

            self.named_variable(&class_name, false)?;
            self.emit_byte(OpCode::Inherit)?;
            self.classes.last_mut().unwrap().has_superclass = true;
        }

        self.named_variable(&class_name, false)?;
        self.consume(TokenType::LeftBrace, "expect '{' before class body")?;

        while !self.check_current_token(&TokenType::RightBrace)
            && !self.check_current_token(&TokenType::Eof)
        {
            self.method()?;
        }

        self.consume(TokenType::RightBrace, "expect '}' after class body")?;
        self.emit_byte(OpCode::Pop)?;

        if self.classes.last().unwrap().has_superclass {
            self.end_scope()?;
        }

        self.classes.pop();

        Ok(())
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

        self.consume(
            TokenType::Semicolon,
            "expect ';' after variable declaration",
        )?;

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

        // This is the loop condition -- if present, the loop instruction will return
        // to this instruction for evaluation on every iteration.
        if !self.token_type_matches(&TokenType::Semicolon)? {
            self.expression()?;
            self.consume(TokenType::Semicolon, "expect ';' after loop condition")?;

            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse(0))?);
            self.emit_byte(OpCode::Pop)?;
        }

        // This is the incrementer -- if present, this will be evaluated after the
        // body of the loop.
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

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump)?;
            self.emit_byte(OpCode::Pop)?;
        };

        self.end_scope()
    }

    fn foreach_statement(&mut self) -> Result<(), LoxError> {
        self.begin_scope()?;

        // This statement must take the shape of:
        //
        //     foreach (var iteration_var in iterable_object) { ... }
        self.consume(TokenType::LeftParen, "expect '(' after 'foreach'")?;
        self.consume(TokenType::Var, "expect 'var' after 'foreach ('")?;

        self.add_local(Some(self.synthetic_token(TokenType::Skip)))?;

        let global: usize = self.parse_variable("expect variable name")?;
        self.define_variable(global)?;
        let offset = self.compiler().locals.len() - 1;

        self.consume(TokenType::In, "expect 'in' after 'foreach (var iter'")?;
        self.expression()?;
        self.emit_byte(OpCode::DefineIterator)?;

        self.consume(TokenType::RightParen, "expect ')' after for clauses")?;

        let loop_start = self.chunk().code.len();

        self.emit_byte(OpCode::IteratorNext(offset, 0))?;

        self.statement()?;
        self.emit_loop(loop_start)?;

        self.patch_jump(loop_start + 1)?;
        self.emit_byte(OpCode::Pop)?;

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

    fn import_statement(&mut self) -> Result<(), LoxError> {
        let path = self.scanner.get_string_literal(&self.current);
        self.consume(
            TokenType::String,
            "expect '\"/path/to/file.lox\"' after 'import '",
        )?;

        self.consume(
            TokenType::As,
            "expect 'as' after 'import \"/path/to/file.lox\"'",
        )?;

        let global: usize = self.parse_variable("expect variable name")?;
        self.consume(
            TokenType::Semicolon,
            "expect ';' after 'import \"...\" as ...;'",
        )?;

        self.emit_byte(OpCode::Import(Box::new(path), global))
    }

    fn assert_statement(&mut self) -> Result<(), LoxError> {
        let mut has_message = false;

        self.expression()?;

        if self.token_type_matches(&TokenType::Comma)? {
            self.expression()?;
            has_message = true;
        }

        self.consume(TokenType::Semicolon, "expect ';' after value")?;
        self.emit_byte(OpCode::Assert(has_message))
    }

    fn print_statement(&mut self, newline: bool) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "expect ';' after value")?;
        self.emit_byte(OpCode::Print(newline))
    }

    fn return_statement(&mut self) -> Result<(), LoxError> {
        match self.token_type_matches(&TokenType::Semicolon)? {
            true => self.emit_return(),
            false => {
                if self.current_function().function_type == FunctionType::Initializer {
                    return Err(LoxError::ParseError(
                        "can't return from a class initializer".into(),
                    ));
                }

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
                TokenType::Assert
                | TokenType::Break
                | TokenType::Class
                | TokenType::Continue
                | TokenType::Delete
                | TokenType::For
                | TokenType::Foreach
                | TokenType::Function
                | TokenType::If
                | TokenType::Import
                | TokenType::Print
                | TokenType::Return
                | TokenType::Var
                | TokenType::While => break,
                _ => self.advance()?,
            };
        }

        Ok(())
    }

    fn declaration(&mut self) -> Result<(), LoxError> {
        if self.token_type_matches(&TokenType::Class)? {
            self.class_declaration()?;
        } else if self.token_type_matches(&TokenType::Function)? {
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
            self.print_statement(false)
        } else if self.token_type_matches(&TokenType::Println)? {
            self.print_statement(true)
        } else if self.token_type_matches(&TokenType::Assert)? {
            self.assert_statement()
        } else if self.token_type_matches(&TokenType::Delete)? {
            self.delete_statement()
        } else if self.token_type_matches(&TokenType::For)? {
            self.for_statement()
        } else if self.token_type_matches(&TokenType::Foreach)? {
            self.foreach_statement()
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
        } else if self.token_type_matches(&TokenType::Import)? {
            self.import_statement()
        } else {
            self.expression_statement()
        }
    }

    fn break_(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let current_len = self.chunk().code.len();
        self.emit_byte(OpCode::Break(current_len, false))
    }

    fn continue_(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let current_len = self.chunk().code.len();
        self.emit_byte(OpCode::Continue(current_len, false))
    }

    fn binary(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let operator_type = self.previous.token_type.clone();
        let precedence = Precedence::for_token_type(&operator_type);
        self.parse_precedence(&precedence.get_parent())?;

        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::Subtract),
            TokenType::Percent => self.emit_byte(OpCode::Modulo),
            TokenType::Plus => self.emit_byte(OpCode::Add),
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
            TokenType::In => self.emit_byte(OpCode::In),
            TokenType::NotIn => {
                self.emit_byte(OpCode::In)?;
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

    fn dot(&mut self, can_assign: bool) -> Result<(), LoxError> {
        self.consume(TokenType::Identifier, "expect property name after '.'")?;
        let name = self.identifier_constant(&self.previous.clone())?;

        if can_assign && self.token_type_matches(&TokenType::Equal)? {
            self.expression()?;
            self.emit_byte(OpCode::SetProperty(name))?;
        } else if self.token_type_matches(&TokenType::LeftParen)? {
            let arg_count = self.argument_list()?;
            self.emit_byte(OpCode::Invoke(name, arg_count))?;
        } else {
            self.emit_byte(OpCode::GetProperty(name))?;
        }

        Ok(())
    }

    fn finish_slice(&mut self, has_left: bool) -> Result<(), LoxError> {
        if self.token_type_matches(&TokenType::RightBracket)? {
            return self.emit_byte(OpCode::GetSlice(has_left, false));
        }

        self.expression()?;

        self.consume(
            TokenType::RightBracket,
            "expect ']' after slice expressions",
        )?;

        self.emit_byte(OpCode::GetSlice(has_left, true))
    }

    fn delete_statement(&mut self) -> Result<(), LoxError> {
        self.expression()?;
        self.consume(TokenType::Semicolon, "expect ';' after value")?;

        match self.chunk().code.last() {
            Some(OpCode::GetIndex) => {},
            _ => return Err(LoxError::CompileError("invalid 'delete' statement".into())),
        };

        let last_pos = self.chunk().code.len() - 1;
        self.chunk().code[last_pos] = OpCode::DeleteIndex;

        Ok(())
    }

    fn index(&mut self, can_assign: bool) -> Result<(), LoxError> {
        if self.token_type_matches(&TokenType::Colon)? {
            return self.finish_slice(false);
        }

        self.expression()?;

        if self.token_type_matches(&TokenType::Colon)? {
            return self.finish_slice(true);
        }

        self.consume(
            TokenType::RightBracket,
            "expect ']' after index expressions",
        )?;

        if can_assign && self.token_type_matches(&TokenType::Equal)? {
            self.expression()?;
            self.emit_byte(OpCode::SetIndex)
        } else {
            self.emit_byte(OpCode::GetIndex)
        }
    }

    fn list(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let mut item_count: usize = 0;

        if !self.check_current_token(&TokenType::RightBracket) {
            loop {
                self.expression()?;
                item_count += 1;

                if !self.token_type_matches(&TokenType::Comma)? {
                    break;
                }
            }
        }

        self.consume(TokenType::RightBracket, "expect list to end with ']'")?;

        match item_count {
            0 => {
                let constant = self.make_constant(Value::Object(Object::List(Box::new(
                    Vec::with_capacity(16),
                ))))?;
                self.emit_byte(OpCode::Constant(constant))
            },
            _ => self.emit_byte(OpCode::BuildList(item_count)),
        }
    }

    fn map(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        let mut item_count: usize = 0;

        if !self.check_current_token(&TokenType::RightBrace) {
            loop {
                self.expression()?;
                self.consume(TokenType::Colon, "expect ':' to separate key and value")?;
                self.expression()?;

                item_count += 2;

                if !self.token_type_matches(&TokenType::Comma)? {
                    break;
                }
            }
        }

        self.consume(TokenType::RightBrace, "expect map to end with '}'")?;

        match item_count {
            0 => {
                let constant =
                    self.make_constant(Value::Object(Object::Map(Box::new(ValueMap {
                        map: HashMap::with_capacity(16),
                    }))))?;
                self.emit_byte(OpCode::Constant(constant))
            },
            _ => self.emit_byte(OpCode::BuildMap(item_count)),
        }
    }

    fn literal(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        match self.previous.token_type {
            TokenType::Nil => self.emit_byte(OpCode::Nil),
            TokenType::False => self.emit_byte(OpCode::False),
            TokenType::True => self.emit_byte(OpCode::True),
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
        let (get_op, set_op) = match self.resolve_local(token, 0) {
            Ok(index) => (OpCode::GetLocal(index), OpCode::SetLocal(index)),
            Err(_) => match self.resolve_upvalue(token, 0) {
                Ok(index) => (OpCode::GetUpValue(index), OpCode::SetUpValue(index)),
                Err(_) => {
                    let index = self.identifier_constant(token)?;
                    (OpCode::GetGlobal(index), OpCode::SetGlobal(index))
                },
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

    fn resolve_local(&mut self, token: &Token, depth: usize) -> Result<usize, LoxError> {
        match self
            .compiler_at(depth)
            .locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, x)| self.scanner.get_string(&x.name) == self.scanner.get_string(token))
        {
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

    fn add_upvalue(
        &mut self,
        index: usize,
        is_local: bool,
        depth: usize,
    ) -> Result<usize, LoxError> {
        if let Some(uv) = self
            .compiler_at_mut(depth)
            .upvalues
            .iter()
            .find(|uv| uv.is_local == is_local && uv.index == index)
        {
            return Ok(uv.index);
        }

        self.compiler_at_mut(depth)
            .upvalues
            .push(UpValue { is_local, index });

        self.compiler_at_mut(depth).function.upvalue_count += 1;

        Ok(index)
    }

    fn resolve_upvalue(&mut self, token: &Token, depth: usize) -> Result<usize, LoxError> {
        if depth + 2 >= self.compilers.len() {
            return Err(LoxError::ResolutionError);
        }

        if let Ok(index) = self.resolve_local(token, depth + 1) {
            self.compiler_at_mut(depth + 1).locals[index].is_captured = true;
            return self.add_upvalue(index, true, depth);
        }

        if let Ok(index) = self.resolve_upvalue(token, depth + 1) {
            return self.add_upvalue(index, false, depth);
        }

        Err(LoxError::ResolutionError)
    }

    fn synthetic_token(&self, token_type: TokenType) -> Token {
        let length = match token_type {
            TokenType::Super => 5,
            TokenType::This => 4,
            _ => 0,
        };

        Token {
            token_type,
            length,
            start: 0,
            line: 0,
        }
    }

    fn variable(&mut self, can_assign: bool) -> Result<(), LoxError> {
        self.named_variable(&self.previous.clone(), can_assign)
    }

    fn super_(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        match self.classes.last() {
            Some(class) => {
                if !class.has_superclass {
                    self.error("can't use 'super' outside of a class")?;
                }
            },
            None => self.error("can't user 'super' inside a class without a superclass")?,
        };

        self.consume(TokenType::Dot, "expect '.' after 'super'")?;
        self.consume(TokenType::Identifier, "expect superclass method name'")?;
        let name = self.identifier_constant(&self.previous.clone())?;

        self.named_variable(&self.synthetic_token(TokenType::This), false)?;

        self.emit_byte(OpCode::GetSuper(name))
    }

    fn this_(&mut self, _can_assign: bool) -> Result<(), LoxError> {
        if self.classes.is_empty() {
            return Err(LoxError::ParseError(
                "can't use 'this' outside of classes".into(),
            ));
        }

        self.named_variable(&self.synthetic_token(TokenType::This), false)
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

    fn get_prefix_rule(&mut self, token_type: &TokenType) -> Option<FixRule> {
        match token_type {
            &TokenType::LeftBrace => Some(Compiler::map),
            &TokenType::LeftBracket => Some(Compiler::list),
            &TokenType::LeftParen => Some(Compiler::grouping),
            &TokenType::Minus | &TokenType::Bang => Some(Compiler::unary),
            &TokenType::Number => Some(Compiler::number),
            &TokenType::String => Some(Compiler::string),
            &TokenType::Nil | &TokenType::True | &TokenType::False => Some(Compiler::literal),
            &TokenType::Identifier => Some(Compiler::variable),
            &TokenType::This => Some(Compiler::this_),
            &TokenType::Break => Some(Compiler::break_),
            &TokenType::Continue => Some(Compiler::continue_),
            &TokenType::Super => Some(Compiler::super_),
            _ => None,
        }
    }

    fn get_infix_rule(&mut self, token_type: &TokenType) -> Option<FixRule> {
        match token_type {
            &TokenType::Minus
            | &TokenType::Percent
            | &TokenType::Plus
            | &TokenType::Slash
            | &TokenType::Star
            | &TokenType::BangEqual
            | &TokenType::EqualEqual
            | &TokenType::Greater
            | &TokenType::GreaterEqual
            | &TokenType::In
            | &TokenType::NotIn
            | &TokenType::Less
            | &TokenType::LessEqual => Some(Compiler::binary),
            &TokenType::Dot => Some(Compiler::dot),
            &TokenType::And => Some(Compiler::and_),
            &TokenType::Or => Some(Compiler::or_),
            &TokenType::LeftParen => Some(Compiler::call),
            &TokenType::LeftBracket => Some(Compiler::index),
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

            if self.previous.token_type == TokenType::NotIn
                && self.current.token_type == TokenType::In
            {
                self.skip()?;
            }

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
        let constant = match token.token_type {
            TokenType::This => "this".into(),
            TokenType::Super => "super".into(),
            _ => self.scanner.get_string(token),
        };

        self.make_constant(Value::Object(Object::String(Box::new(constant))))
    }

    fn add_local(&mut self, name: Option<Token>) -> Result<(), LoxError> {
        let local_name = match name {
            Some(name) => name,
            None => self.previous.clone(),
        };

        self.compiler_mut().local_count += 1;
        self.compiler_mut().locals.push(Local {
            name: local_name,
            depth: None,
            is_captured: false,
        });

        Ok(())
    }

    fn declare_variable(&mut self) -> Result<(), LoxError> {
        if self.compiler().scope_depth == 0 {
            return Ok(());
        }

        let local = self
            .compiler()
            .locals
            .iter()
            .find(|local| local.name == self.previous);

        if local.is_some() {
            self.error("already a variable with this name in this scope")?;
        }

        self.add_local(None)?;

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

        if constant == usize::MAX {
            self.error("too many constants in one chunk")?;
        }

        Ok(constant)
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<(), LoxError> {
        if self.current.token_type == token_type {
            self.advance()?;
            return Ok(());
        }

        self.error_at_current(message)
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

    fn advance_until_error(&mut self) -> Result<(), LoxError> {
        loop {
            self.current = self.scanner.scan_token()?;

            if self.current.token_type != TokenType::Error {
                break;
            }

            self.error_at_current("")?;
        }

        Ok(())
    }

    fn skip(&mut self) -> Result<(), LoxError> {
        self.advance_until_error()
    }

    fn advance(&mut self) -> Result<(), LoxError> {
        self.previous = self.current.clone();
        self.advance_until_error()
    }

    fn error_at(&self, token: Token, message: &str) -> Result<(), LoxError> {
        let output = match token.token_type {
            TokenType::Eof => "at end of file".into(),
            TokenType::Error => "unknown".into(),
            _ => format!(
                "at {} on line {}",
                self.scanner.get_string(&token),
                token.line
            ),
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        let input = "var foo = 123;\nprintln foo;\n";
        let mut compiler = Compiler::new(&input);
        let function = compiler.compile().unwrap();

        assert_eq!(
            function.chunk.code,
            [
                OpCode::Constant(1),
                OpCode::DefineGlobal(0),
                OpCode::GetGlobal(2),
                OpCode::Print(true),
                OpCode::Nil,
                OpCode::Return,
            ],
        );
    }

    #[test]
    fn test_scope() {
        let input = r#"
            fn bar() {
                var foo = 123;
                return foo;
            }
            var foo = bar();
            println foo;
        "#;
        let mut compiler = Compiler::new(&input);
        let function = compiler.compile().unwrap();

        assert_eq!(
            function.chunk.code,
            [
                OpCode::Closure(1, Box::new(vec![])),
                OpCode::DefineGlobal(0),
                OpCode::GetGlobal(3),
                OpCode::Call(0),
                OpCode::DefineGlobal(2),
                OpCode::GetGlobal(5),
                OpCode::Print(true),
                OpCode::Nil,
                OpCode::Return,
            ],
        );
    }

    #[test]
    fn test_build_list() {
        let input = r#"
            println [1, 2, 1 + 2];
            print [];
        "#;
        let mut compiler = Compiler::new(&input);
        let function = compiler.compile().unwrap();

        assert_eq!(
            function.chunk.code,
            [
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::Constant(2),
                OpCode::Constant(3),
                OpCode::Add,
                OpCode::BuildList(3),
                OpCode::Print(true),
                OpCode::Constant(4),
                OpCode::Print(false),
                OpCode::Nil,
                OpCode::Return,
            ]
        );
    }

    #[test]
    fn test_build_map() {
        let input = r#"
            println {'a': 1};
            print {};
        "#;
        let mut compiler = Compiler::new(&input);
        let function = compiler.compile().unwrap();

        assert_eq!(
            function.chunk.code,
            [
                OpCode::Constant(0),
                OpCode::Constant(1),
                OpCode::BuildMap(2),
                OpCode::Print(true),
                OpCode::Constant(2),
                OpCode::Print(false),
                OpCode::Nil,
                OpCode::Return,
            ]
        );
    }
}
