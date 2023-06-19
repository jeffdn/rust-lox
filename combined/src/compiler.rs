use std::collections::HashMap;

use crate::{
    chunk::{Chunk, OpCode, UpValue},
    errors::{LoxError, LoxResult},
    expressions::{Expression, ExpressionVisitor},
    object::{Function, FunctionType, Object, ValueMap},
    statements::{Statement, StatementVisitor},
    tokens::{Literal, Token, TokenType},
    value::Value,
};

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
    // panic_mode: bool,
}

struct CompilerNode {
    locals: Vec<Local>,
    upvalues: Vec<UpValue>,
    scope_depth: usize,
    function: Function,
}

impl CompilerNode {
    pub fn new(function_type: FunctionType) -> CompilerNode {
        let mut node = CompilerNode {
            locals: Vec::with_capacity(256),
            upvalues: Vec::with_capacity(256),
            scope_depth: 0,
            function: Function {
                function_type: function_type.clone(),
                arity: 0,
                upvalue_count: 0,
                chunk: Chunk::new(),
                name: "<script>".into(),
            },
        };

        let (token_type, lexeme) = match function_type {
            FunctionType::Method | FunctionType::Initializer => (TokenType::This, "this".into()),
            _ => (TokenType::Skip, "".into()),
        };

        node.locals.push(Local {
            name: Token {
                token_type,
                lexeme,
                literal: None,
                line: 0,
            },
            depth: Some(0),
            is_captured: false,
        });

        node
    }
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            compilers: vec![CompilerNode::new(FunctionType::Script)],
            classes: Vec::new(),
            // panic_mode: false,
        }
    }

    fn init_compiler(&mut self, name: &Token, function_type: FunctionType) {
        let mut compiler = CompilerNode::new(function_type);

        compiler.function.name = name.lexeme.clone();

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

    fn make_constant(&mut self, value: Value) -> LoxResult<usize> {
        let maybe_idx = self
            .chunk()
            .constants
            .iter()
            .enumerate()
            .find_map(|(idx, v)| {
                if *v.borrow() == value {
                    Some(idx)
                } else {
                    None
                }
            });

        match maybe_idx {
            Some(idx) => Ok(idx),
            None => {
                let constant = self.chunk().add_constant(value);

                if constant == usize::MAX {
                    panic!("too many constants in one chunk");
                }

                Ok(constant)
            },
        }
    }

    pub fn compile(&mut self, statements: Vec<Statement>) -> LoxResult<Function> {
        for stmt in statements.iter() {
            self.execute(stmt)?;
        }

        self.emit_return()?;

        // #[cfg(feature = "debug")]
        // if self.panic_mode {
        //     self.chunk().disassemble("code");
        // }

        Ok(self.compilers.pop().unwrap().function)
    }

    fn end_compiler(&mut self) -> LoxResult<CompilerNode> {
        self.emit_return()?;

        // #[cfg(feature = "debug")]
        // if self.panic_mode {
        //     self.chunk().disassemble("code");
        // }

        Ok(self.compilers.pop().unwrap())
    }

    fn begin_scope(&mut self) -> LoxResult<()> {
        self.compiler_mut().scope_depth += 1;
        Ok(())
    }

    fn end_scope(&mut self) -> LoxResult<()> {
        self.compiler_mut().scope_depth -= 1;

        loop {
            if self.compiler().locals.is_empty() {
                break;
            }

            let idx = self.compiler().locals.len() - 1;
            if self.compiler().locals[idx]
                .depth
                .is_some_and(|depth| depth <= self.compiler().scope_depth)
            {
                break;
            }

            let local = self.compiler_mut().locals.pop().unwrap();

            if local.is_captured {
                self.emit_byte(OpCode::CloseUpValue)?;
            } else {
                self.emit_byte(OpCode::Pop)?;
            }
        }

        if self.compiler().scope_depth == 0
            && self
                .chunk()
                .code
                .iter()
                .any(|code| matches!(code, OpCode::Break(_) | OpCode::Continue(_)))
        {
            self.error("ending scope without correcting a break or continue")?;
        }

        Ok(())
    }

    fn emit_byte(&mut self, byte: OpCode) -> LoxResult<()> {
        // let previous_line = self.previous.line;
        self.chunk().write(byte, 0); //previous_line);

        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> LoxResult<()> {
        let last_code = self.chunk().len();

        self.emit_byte(OpCode::Loop(last_code - loop_start + 1))?;

        // Now, look through all instructions emitted between the start of the loop and
        // the loop instructions. Any that take the shape of either:
        //
        //     OpCode::Break(position)
        //     OpCode::Continue(position)
        //
        //  Will be replaced with, respectively:
        //
        //     OpCode::Jump(end_of_loop)
        //     OpCode::Loop(start_of_loop)
        for code in self.chunk().code[loop_start..last_code].iter_mut() {
            match code {
                OpCode::Break(initial) => *code = OpCode::Jump(last_code - *initial + 1),
                OpCode::Continue(initial) => *code = OpCode::Loop(*initial - loop_start + 1),
                _ => {},
            };
        }

        Ok(())
    }

    fn emit_jump(&mut self, byte: OpCode) -> LoxResult<usize> {
        self.chunk().write(byte, 0);

        Ok(self.chunk().len())
    }

    fn emit_return(&mut self) -> LoxResult<()> {
        match self.current_function().function_type {
            FunctionType::Initializer => self.emit_byte(OpCode::GetLocal(0))?,
            _ => self.emit_byte(OpCode::Nil)?,
        };

        self.emit_byte(OpCode::Return)
    }

    fn patch_jump(&mut self, offset: usize) -> LoxResult<()> {
        let jump = self.chunk().len() - offset;

        self.chunk().code[offset - 1] = match self.chunk().code[offset - 1] {
            OpCode::Jump(_) => OpCode::Jump(jump),
            OpCode::JumpIfFalse(_) => OpCode::JumpIfFalse(jump),
            OpCode::IteratorNext(index, _) => OpCode::IteratorNext(index, jump),
            _ => return Err(LoxError::RuntimeError("unreachable".into())),
        };

        Ok(())
    }

    fn identifier_constant(&mut self, token: &Token) -> LoxResult<usize> {
        let constant = match token.token_type {
            TokenType::This => "this".into(),
            TokenType::Super => "super".into(),
            _ => token.lexeme.clone(),
        };

        self.make_constant(Value::Object(Object::String(Box::new(constant))))
    }

    fn parse_variable(&mut self, name: &Token) -> LoxResult<usize> {
        self.declare_variable(name)?;

        if self.compiler().scope_depth > 0 {
            return Ok(0);
        }

        self.identifier_constant(name)
    }

    fn add_local(&mut self, name: &Token) -> LoxResult<()> {
        self.compiler_mut().locals.push(Local {
            name: name.clone(),
            depth: None,
            is_captured: false,
        });

        Ok(())
    }

    fn declare_variable(&mut self, name: &Token) -> LoxResult<()> {
        if self.compiler().scope_depth == 0 {
            return Ok(());
        }

        let local = self
            .compiler()
            .locals
            .iter()
            .find(|local| local.name.lexeme == name.lexeme);

        if local.is_some() {
            self.error("already a variable with this name in this scope")?;
        }

        self.add_local(name)?;

        Ok(())
    }

    fn error(&self, message: &str) -> LoxResult<()> {
        Err(LoxError::ResolutionError(message.into()))
    }

    fn mark_initialized(&mut self) -> LoxResult<()> {
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

    fn define_variable(&mut self, global: usize) -> LoxResult<()> {
        if self.compiler().scope_depth > 0 {
            self.mark_initialized()?;
            return Ok(());
        }

        self.emit_byte(OpCode::DefineGlobal(global))
    }

    fn resolve_local(&mut self, token: &Token, depth: usize) -> LoxResult<usize> {
        match self
            .compiler_at(depth)
            .locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, x)| x.name.lexeme == token.lexeme)
        {
            Some((idx, local)) => {
                if local.depth.is_none() {
                    return Err(self
                        .error("can't read a local variable in its own initializer")
                        .expect_err(""));
                }

                Ok(idx)
            },
            None => Err(LoxError::ResolutionError(format!(
                "unable to find local variable named {}",
                token.lexeme
            ))),
        }
    }

    fn named_variable(
        &mut self,
        token: &Token,
        can_assign: bool,
        expr: Option<&Expression>,
    ) -> LoxResult<()> {
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

        match (can_assign, expr) {
            (true, Some(expr)) => {
                self.evaluate(expr)?;
                self.emit_byte(set_op)
            },
            _ => self.emit_byte(get_op),
        }
    }

    fn add_upvalue(&mut self, index: usize, is_local: bool, depth: usize) -> LoxResult<usize> {
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

    fn resolve_upvalue(&mut self, token: &Token, depth: usize) -> LoxResult<usize> {
        if depth + 2 >= self.compilers.len() {
            return Err(LoxError::ResolutionError(
                "unable to resolve upvalue: too shallow".into(),
            ));
        }

        if let Ok(index) = self.resolve_local(token, depth + 1) {
            self.compiler_at_mut(depth + 1).locals[index].is_captured = true;
            return self.add_upvalue(index, true, depth);
        }

        if let Ok(index) = self.resolve_upvalue(token, depth + 1) {
            return self.add_upvalue(index, false, depth);
        }

        Err(LoxError::ResolutionError(
            "unable to resolve upvalue".into(),
        ))
    }

    fn function(
        &mut self,
        function_type: FunctionType,
        name: &Token,
        params: &[Token],
        body: &[Statement],
    ) -> LoxResult<()> {
        self.init_compiler(name, function_type.clone());
        self.begin_scope()?;

        if function_type == FunctionType::Method || function_type == FunctionType::Initializer {
            self.named_variable(
                &self.synthetic_token(TokenType::This, name.line),
                false,
                None,
            )?;
        }

        for param in params.iter() {
            self.current_function().arity += 1;
            let constant = self.parse_variable(param)?;
            self.define_variable(constant)?;
        }

        for statement in body.iter() {
            self.execute(statement)?;
        }

        let compiler = self.end_compiler()?;
        let constant =
            self.make_constant(Value::Object(Object::Function(Box::new(compiler.function))))?;

        self.emit_byte(OpCode::Closure(constant, Box::new(compiler.upvalues)))
    }

    fn synthetic_token(&self, token_type: TokenType, line: usize) -> Token {
        let lexeme = match token_type {
            TokenType::Super => "super".into(),
            TokenType::This => "this".into(),
            TokenType::Skip => "".into(),
            _ => unreachable!(),
        };

        Token {
            token_type,
            lexeme,
            literal: None,
            line,
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

impl ExpressionVisitor for Compiler {
    fn visit_assignment(&mut self, name: &Token, expression: &Expression) -> LoxResult<()> {
        self.named_variable(name, true, Some(expression))
    }

    fn visit_binary(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
    ) -> LoxResult<()> {
        self.evaluate(left)?;
        self.evaluate(right)?;

        match operator.token_type {
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
            _ => unreachable!(),
        }
    }

    fn visit_call(
        &mut self,
        callee: &Expression,
        arguments: &[Expression],
        invocation: &bool,
    ) -> LoxResult<()> {
        self.evaluate(callee)?;

        let pos = self.chunk().code.len() - 1;

        for arg in arguments.iter() {
            self.evaluate(arg)?;
        }

        if *invocation {
            let OpCode::GetProperty(name) = self.chunk().code.remove(pos) else {
                unreachable!();
            };

            self.emit_byte(OpCode::Invoke(name, arguments.len()))
        } else {
            self.emit_byte(OpCode::Call(arguments.len()))
        }
    }

    fn visit_get(&mut self, name: &Token, object: &Expression) -> LoxResult<()> {
        self.evaluate(object)?;
        let name = self.identifier_constant(name)?;
        self.emit_byte(OpCode::GetProperty(name))
    }

    fn visit_grouping(&mut self, expression: &Expression) -> LoxResult<()> {
        self.evaluate(expression)
    }

    fn visit_index(
        &mut self,
        item: &Expression,
        is_slice: &bool,
        left: Option<&Expression>,
        right: Option<&Expression>,
    ) -> LoxResult<()> {
        self.evaluate(item)?;

        if !is_slice {
            self.evaluate(left.as_ref().unwrap())?;
            return self.emit_byte(OpCode::GetIndex);
        }

        if let Some(left) = left {
            self.evaluate(left)?;
        }

        if let Some(right) = right {
            self.evaluate(right)?;
        }

        self.emit_byte(OpCode::GetSlice(left.is_some(), right.is_some()))
    }

    fn visit_indexed_assignment(
        &mut self,
        indexed_item: &Expression,
        expression: &Expression,
    ) -> LoxResult<()> {
        let Expression::Index { item, is_slice: _, left, right: _ } = indexed_item else {
            unreachable!();
        };

        self.evaluate(item)?;
        self.evaluate(left.as_ref().unwrap())?;
        self.evaluate(expression)?;

        self.emit_byte(OpCode::SetIndex)
    }

    fn visit_list(&mut self, expressions: &[Expression]) -> LoxResult<()> {
        for expression in expressions.iter() {
            self.evaluate(expression)?;
        }

        match expressions.is_empty() {
            true => {
                let constant = self.make_constant(Value::Object(Object::List(Box::new(
                    Vec::with_capacity(16),
                ))))?;
                self.emit_byte(OpCode::Constant(constant))?;
            },
            false => self.emit_byte(OpCode::BuildList(expressions.len()))?,
        };

        Ok(())
    }

    fn visit_literal(&mut self, value: &Literal) -> LoxResult<()> {
        match value {
            Literal::Boolean(bool_val) => match bool_val {
                true => self.emit_byte(OpCode::True),
                false => self.emit_byte(OpCode::False),
            },
            Literal::Nil => self.emit_byte(OpCode::Nil),
            Literal::Number(num_val) => {
                let constant = self.make_constant(Value::Number(*num_val))?;
                self.emit_byte(OpCode::Constant(constant))
            },
            Literal::String(str_val) | Literal::Identifier(str_val) => {
                let constant =
                    self.make_constant(Value::Object(Object::String(Box::new(str_val.clone()))))?;
                self.emit_byte(OpCode::Constant(constant))
            },
        }
    }

    fn visit_logical(
        &mut self,
        left: &Expression,
        operator: &Token,
        right: &Expression,
    ) -> LoxResult<()> {
        self.evaluate(left)?;

        match operator.token_type {
            TokenType::And => {
                let end_jump = self.emit_jump(OpCode::JumpIfFalse(0))?;
                self.emit_byte(OpCode::Pop)?;
                self.evaluate(right)?;
                self.patch_jump(end_jump)
            },
            TokenType::Or => {
                let else_jump = self.emit_jump(OpCode::JumpIfFalse(0))?;
                let and_jump = self.emit_jump(OpCode::Jump(0))?;

                self.patch_jump(else_jump)?;
                self.emit_byte(OpCode::Pop)?;

                self.evaluate(right)?;
                self.patch_jump(and_jump)
            },
            _ => unreachable!(),
        }
    }

    fn visit_map(&mut self, expressions: &[Expression]) -> LoxResult<()> {
        for expression in expressions.iter() {
            self.evaluate(expression)?;
        }

        match expressions.is_empty() {
            true => {
                let constant =
                    self.make_constant(Value::Object(Object::Map(Box::new(ValueMap {
                        map: HashMap::with_capacity(16),
                    }))))?;
                self.emit_byte(OpCode::Constant(constant))?;
            },
            false => self.emit_byte(OpCode::BuildMap(expressions.len()))?,
        };

        Ok(())
    }

    fn visit_set(
        &mut self,
        name: &Token,
        object: &Expression,
        value: &Expression,
    ) -> LoxResult<()> {
        self.evaluate(object)?;
        self.evaluate(value)?;
        let name = self.identifier_constant(name)?;
        self.emit_byte(OpCode::SetProperty(name))
    }

    fn visit_super(&mut self, name: &Token, token: &Token) -> LoxResult<()> {
        match self.classes.last() {
            Some(class) => {
                if !class.has_superclass {
                    self.error("can't user 'super' inside a class without a superclass")?
                }
            },
            None => self.error("can't use 'super' outside of a class")?,
        };

        let name_constant = self.identifier_constant(name)?;
        self.named_variable(
            &self.synthetic_token(TokenType::This, token.line),
            false,
            None,
        )?;

        self.emit_byte(OpCode::GetSuper(name_constant))
    }

    fn visit_this(&mut self, token: &Token) -> LoxResult<()> {
        if self.classes.is_empty() {
            return Err(LoxError::ParseError(
                token.line,
                "can't use 'this' outside of classes".into(),
            ));
        }

        self.named_variable(
            &self.synthetic_token(TokenType::This, token.line),
            false,
            None,
        )
    }

    fn visit_unary(&mut self, operator: &Token, right: &Expression) -> LoxResult<()> {
        self.evaluate(right)?;

        match operator.token_type {
            TokenType::Minus => self.emit_byte(OpCode::Negate),
            TokenType::Bang => self.emit_byte(OpCode::Not),
            _ => unreachable!(),
        }
    }

    fn visit_variable(&mut self, name: &Token) -> LoxResult<()> {
        self.named_variable(name, false, None)
    }
}

impl StatementVisitor for Compiler {
    fn visit_assert(
        &mut self,
        expression: &Expression,
        message: Option<&Expression>,
    ) -> LoxResult<()> {
        self.evaluate(expression)?;

        if let Some(message) = message {
            self.evaluate(message)?;
            self.emit_byte(OpCode::Assert(true))
        } else {
            self.emit_byte(OpCode::Assert(false))
        }
    }

    fn visit_block(&mut self, statements: &[Statement]) -> LoxResult<()> {
        self.begin_scope()?;

        for statement in statements.iter() {
            self.execute(statement)?;
        }

        self.end_scope()
    }

    fn visit_break(&mut self) -> LoxResult<()> {
        let current_len = self.chunk().len();
        self.emit_byte(OpCode::Break(current_len))
    }

    fn visit_class(
        &mut self,
        name: &Token,
        superclass: Option<&Token>,
        methods: &[Statement],
    ) -> LoxResult<()> {
        let name_constant = self.identifier_constant(name)?;
        self.declare_variable(name)?;

        self.emit_byte(OpCode::Class(name_constant))?;
        self.define_variable(name_constant)?;

        self.classes.push(ClassCompiler::default());

        if let Some(superclass) = superclass {
            self.named_variable(superclass, false, None)?;

            self.begin_scope()?;
            self.add_local(&self.synthetic_token(TokenType::Super, superclass.line))?;
            self.define_variable(0)?;

            self.named_variable(name, false, None)?;
            self.emit_byte(OpCode::Inherit)?;
            self.classes.last_mut().unwrap().has_superclass = true;
        }

        self.named_variable(name, false, None)?;

        for method in methods.iter() {
            let Statement::Function { name, params, body } = method else {
                unreachable!();
            };

            let name_constant = self.identifier_constant(name)?;
            let function_type = match name.lexeme.as_str() {
                "init" => FunctionType::Initializer,
                _ => FunctionType::Method,
            };

            self.function(function_type, name, params, body)?;
            self.emit_byte(OpCode::Method(name_constant))?;
        }

        self.emit_byte(OpCode::Pop)?;

        if self.classes.last().unwrap().has_superclass {
            self.end_scope()?;
        }

        self.classes.pop();

        Ok(())
    }

    fn visit_continue(&mut self) -> LoxResult<()> {
        let current_len = self.chunk().len();
        self.emit_byte(OpCode::Continue(current_len))
    }

    fn visit_delete(&mut self, expression: &Expression) -> LoxResult<()> {
        let Expression::Index { item, is_slice: _, left, right: _ } = expression else {
            unreachable!();
        };

        self.evaluate(item)?;
        self.evaluate(left.as_ref().unwrap())?;

        self.emit_byte(OpCode::DeleteIndex)
    }

    fn visit_expression(&mut self, expression: &Expression) -> LoxResult<()> {
        self.evaluate(expression)?;
        self.emit_byte(OpCode::Pop)
    }

    fn visit_for(
        &mut self,
        initializer: Option<&Statement>,
        condition: Option<&Expression>,
        increment: Option<&Expression>,
        body: &Statement,
    ) -> LoxResult<()> {
        self.begin_scope()?;

        if let Some(initializer) = initializer {
            self.execute(initializer)?;
        }

        let mut loop_start = self.chunk().len();
        let mut exit_jump: Option<usize> = None;

        if let Some(condition) = condition {
            self.evaluate(condition)?;
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse(0))?);
            self.emit_byte(OpCode::Pop)?;
        }

        if let Some(increment) = increment {
            let body_jump = self.emit_jump(OpCode::Jump(0))?;
            let increment_start = self.chunk().len();

            self.evaluate(increment)?;
            self.emit_byte(OpCode::Pop)?;

            self.emit_loop(loop_start)?;
            loop_start = increment_start;
            self.patch_jump(body_jump)?;
        }

        self.execute(body)?;
        self.emit_loop(loop_start)?;

        if let Some(exit_jump) = exit_jump {
            self.patch_jump(exit_jump)?;
            self.emit_byte(OpCode::Pop)?;
        }

        self.end_scope()
    }

    fn visit_foreach(
        &mut self,
        iterator: &Token,
        iterable: &Expression,
        body: &Statement,
    ) -> LoxResult<()> {
        self.begin_scope()?;
        self.add_local(&self.synthetic_token(TokenType::Skip, iterator.line))?;

        let global = self.parse_variable(iterator)?;
        self.define_variable(global)?;
        let offset = self.compiler().locals.len() - 1;

        self.evaluate(iterable)?;
        self.emit_byte(OpCode::DefineIterator)?;

        let loop_start = self.chunk().len();

        self.emit_byte(OpCode::IteratorNext(offset, 0))?;

        self.execute(body)?;

        self.emit_loop(loop_start)?;
        self.patch_jump(loop_start + 1)?;
        self.emit_byte(OpCode::Pop)?;

        self.end_scope()
    }

    fn visit_function(
        &mut self,
        name: &Token,
        params: &[Token],
        body: &[Statement],
    ) -> LoxResult<()> {
        let global = self.parse_variable(name)?;
        self.mark_initialized()?;

        self.function(FunctionType::Function, name, params, body)?;
        self.define_variable(global)
    }

    fn visit_if(
        &mut self,
        condition: &Expression,
        then_branch: &Statement,
        else_branch: Option<&Statement>,
    ) -> LoxResult<()> {
        self.evaluate(condition)?;

        let then_jump = self.emit_jump(OpCode::JumpIfFalse(0))?;
        self.emit_byte(OpCode::Pop)?;

        self.execute(then_branch)?;

        let else_jump = self.emit_jump(OpCode::Jump(0))?;

        self.patch_jump(then_jump)?;
        self.emit_byte(OpCode::Pop)?;

        if let Some(else_stmt) = else_branch {
            self.execute(else_stmt)?;
        }

        self.patch_jump(else_jump)?;

        Ok(())
    }

    fn visit_print(&mut self, newline: &bool, expression: &Expression) -> LoxResult<()> {
        self.evaluate(expression)?;
        self.emit_byte(OpCode::Print(*newline))
    }

    fn visit_return(&mut self, value: Option<&Expression>) -> LoxResult<()> {
        match value {
            Some(expression) => {
                if self.current_function().function_type == FunctionType::Initializer {
                    return Err(LoxError::ParseError(
                        0,
                        "can't return from a class initializer".into(),
                    ));
                }

                self.evaluate(expression)?;
                self.emit_byte(OpCode::Return)
            },
            None => self.emit_return(),
        }
    }

    fn visit_var(&mut self, name: &Token, initializer: Option<&Expression>) -> LoxResult<()> {
        let global = self.parse_variable(name)?;

        match initializer {
            Some(init) => self.evaluate(init)?,
            None => self.emit_byte(OpCode::Nil)?,
        };

        self.define_variable(global)
    }

    fn visit_while(&mut self, condition: &Expression, body: &Statement) -> LoxResult<()> {
        let loop_start = self.chunk().len();
        self.evaluate(condition)?;
        let exit_jump = self.emit_jump(OpCode::JumpIfFalse(0))?;
        self.emit_byte(OpCode::Pop)?;
        self.execute(body)?;
        self.emit_loop(loop_start)?;
        self.patch_jump(exit_jump)?;
        self.emit_byte(OpCode::Pop)
    }
}
