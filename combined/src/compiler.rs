use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    chunk::{Chunk, OpCode},
    errors::{LoxError, LoxResult},
    expressions::{Expression, ExpressionVisitor},
    object::{Function, FunctionType, Object, UpValue},
    statements::{Statement, StatementVisitor},
    tokens::{Literal, TokenType},
    value::Value,
};

// struct Local {
//     name: Token,
//     depth: Option<usize>,
//     is_captured: bool,
// }

#[derive(Default)]
struct ClassCompiler {
    pub has_superclass: bool,
}

pub struct Compiler {
    compilers: Vec<CompilerNode>,
    classes: Vec<ClassCompiler>,

    panic_mode: bool,
}

struct CompilerNode {
    // locals: Vec<Local>,
    // local_count: usize,
    upvalues: Vec<UpValue>,
    scope_depth: usize,
    function: Function,
}

impl CompilerNode {
    pub fn new(function_type: FunctionType) -> CompilerNode {
        let mut node = CompilerNode {
            // locals: Vec::with_capacity(256),
            // local_count: 1,
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

        // node.locals.push(Local {
        //     name: Token {
        //         token_type,
        //         length,
        //         start: 0,
        //         line: 0,
        //     },
        //     depth: Some(0),
        //     is_captured: false,
        // });

        node
    }
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            compilers: vec![CompilerNode::new(FunctionType::Script)],
            classes: Vec::new(),
            panic_mode: false,
        }
    }

    // fn init_compiler(&mut self, function_type: FunctionType) {
    //     let mut compiler = CompilerNode::new(function_type);

    //     compiler.function.name = self.scanner.get_string(&self.previous).into();

    //     self.compilers.push(compiler);
    // }

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
        let constant = self.chunk().add_constant(value);

        if constant == usize::MAX {
            panic!("too many constants in one chunk");
        }

        Ok(constant)
    }

    pub fn compile(&mut self, statements: Vec<Statement>) -> LoxResult<Function> {
        for stmt in statements.iter() {
            self.execute(stmt)?;
        }

        self.emit_return()?;

        #[cfg(feature = "debug")]
        if self.panic_mode {
            self.chunk().disassemble("code");
        }

        Ok(self.compilers.pop().unwrap().function)
    }

    fn end_compiler(&mut self) -> LoxResult<CompilerNode> {
        self.emit_return()?;

        #[cfg(feature = "debug")]
        if self.panic_mode {
            self.chunk().disassemble("code");
        }

        Ok(self.compilers.pop().unwrap())
    }

    fn begin_scope(&mut self) -> LoxResult<()> {
        self.compiler_mut().scope_depth += 1;
        Ok(())
    }

    fn end_scope(&mut self) -> LoxResult<()> {
        self.compiler_mut().scope_depth -= 1;

        // loop {
        //     if self.compiler().local_count == 0 {
        //         break;
        //     }

        //     let idx = self.compiler().local_count - 1;
        //     if self.compiler().locals[idx].depth.is_some()
        //         && self.compiler().locals[idx].depth.unwrap() <= self.compiler().scope_depth
        //     {
        //         break;
        //     }

        //     let local = self.compiler_mut().locals.pop().unwrap();

        //     if local.is_captured {
        //         self.emit_byte(OpCode::CloseUpValue)?;
        //     } else {
        //         self.emit_byte(OpCode::Pop)?;
        //     }

        //     self.compiler_mut().local_count -= 1;
        // }

        // if self.compiler().scope_depth == 0
        //     && self
        //         .chunk()
        //         .code
        //         .iter()
        //         .any(|code| matches!(code, OpCode::Break(_) | OpCode::Continue(_)))
        // {
        //     self.error_at_current("ending scope without correcting a break or continue")?;
        // }

        Ok(())
    }

    fn emit_byte(&mut self, byte: OpCode) -> LoxResult<()> {
        // let previous_line = self.previous.line;
        self.chunk().write(byte, 0); //previous_line);

        Ok(())
    }

    fn emit_return(&mut self) -> LoxResult<()> {
        match self.current_function().function_type {
            FunctionType::Initializer => self.emit_byte(OpCode::GetLocal(0))?,
            _ => self.emit_byte(OpCode::Nil)?,
        };

        self.emit_byte(OpCode::Return)
    }

    fn execute(&mut self, stmt: &Statement) -> LoxResult<()> {
        self.accept_statement(stmt)
    }

    fn evaluate(&mut self, expr: &Expression) -> LoxResult<()> {
        self.accept_expression(expr)
    }
}

impl ExpressionVisitor<()> for Compiler {
    fn visit_assignment(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::Assignment { name, expression } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())
        // let value = self.evaluate(expression)?;
        // let distance = self.locals.borrow().get(expr);

        // match distance {
        //     Ok(_) => self
        //         .environment
        //         .borrow_mut()
        //         .assign(name.lexeme.clone(), value.clone())?,
        //     Err(_) => self
        //         .globals
        //         .borrow_mut()
        //         .assign(name.lexeme.clone(), value.clone())?,
        // };

        // Ok(value)
    }

    fn visit_binary(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::Binary { left, operator, right } = expr else {
            return Err(LoxError::AstError);
        };

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

    fn visit_call(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::Call { callee, paren: _, arguments } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // let mut real_arguments: Vec<LoxEntity> = Vec::new();
        // for arg in arguments.iter() {
        //     let evaluated = self.evaluate(arg)?;
        //     real_arguments.push(evaluated);
        // }

        // let mut callable = match **callee {
        //     Expression::Variable { ref name } => {
        //         match self.environment.borrow().get(&name.lexeme)? {
        //             LoxEntity::Callable(callable) => callable,
        //             _ => return Err(LoxError::AstError),
        //         }
        //     }
        //     Expression::Get { .. } => match self.evaluate(callee)? {
        //         LoxEntity::Callable(callable) => callable,
        //         _ => return Err(LoxError::AstError),
        //     },
        //     Expression::Index { .. } => match self.evaluate(callee)? {
        //         LoxEntity::Callable(callable) => callable,
        //         _ => return Err(LoxError::AstError),
        //     },
        //     _ => return Err(LoxError::AstError),
        // };

        // if real_arguments.len() != callable.arity()? {
        //     return Err(LoxError::RuntimeError(format!(
        //         "expected {} arguments but got {}",
        //         callable.arity()?,
        //         real_arguments.len(),
        //     )));
        // }

        // callable.call(self, real_arguments)
    }

    fn visit_get(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::Get { name, object } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // match self.evaluate(object)? {
        //     LoxEntity::Callable(callable) => match *callable {
        //         LoxCallable::Class { class } => class.get(&name.lexeme),
        //         _ => Err(LoxError::AstError),
        //     },
        //     _ => Err(LoxError::AstError),
        // }
    }

    fn visit_grouping(&mut self, expr: &Expression) -> LoxResult<()> {
        Ok(())
        // match expr {
        //     Expression::Grouping { expression } => self.evaluate(expression),
        //     _ => Err(LoxError::AstError),
        // }
    }

    fn visit_index(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::Index { item, index, slice } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // let item = self.evaluate(item)?;
        // let index = self.evaluate(index)?;

        // match (item, index, slice) {
        //     (LoxEntity::List(list), LoxEntity::Literal(Literal::Number(index)), None) => {
        //         let list_index = self._convert_index(list.len(), index);

        //         if list_index > list.len() - 1 {
        //             return Err(LoxError::AstError);
        //         }

        //         Ok(list[list_index].clone())
        //     }
        //     (LoxEntity::List(list), LoxEntity::Literal(Literal::Number(index)), Some(slice)) => {
        //         let slice = match self.evaluate(slice)? {
        //             LoxEntity::Literal(Literal::Number(slice)) => slice,
        //             _ => return Err(LoxError::AstError),
        //         };

        //         let list_len = list.len();
        //         let mut slice_start = self._convert_index(list_len, index);
        //         let mut slice_end = self._convert_index(list_len, slice);

        //         if slice_start > list_len - 1 {
        //             slice_start = list_len - 1;
        //         }

        //         if slice_end > list_len - 1 {
        //             slice_end = list_len - 1;
        //         }

        //         if slice_start >= slice_end {
        //             return Ok(LoxEntity::List(vec![]));
        //         }

        //         let output: Vec<LoxEntity> = list[slice_start..(slice_end + 1)].to_vec();

        //         Ok(LoxEntity::List(output))
        //     }
        //     (LoxEntity::Map(map), LoxEntity::Literal(index), None) => {
        //         if !map.contains_key(&index) {
        //             return Err(LoxError::AstError);
        //         }

        //         Ok(map[&index].clone())
        //     }
        //     _ => Err(LoxError::AstError),
        // }
    }

    fn visit_indexed_assignment(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::IndexedAssignment { indexed_item, expression } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // let Expression::Index { item, index, slice } = &**indexed_item else {
        //     return Err(LoxError::AstError);
        // };

        // if slice.is_some() {
        //     return Err(LoxError::AstError);
        // }

        // let name = match &**item {
        //     Expression::Variable { name } => name.clone(),
        //     _ => return Err(LoxError::AstError),
        // };

        // let distance = self.locals.borrow().get(expr);

        // let item = self.evaluate(item)?;
        // let index = self.evaluate(index)?;

        // match (item, index) {
        //     (LoxEntity::List(mut list), LoxEntity::Literal(Literal::Number(index))) => {
        //         let list_index = self._convert_index(list.len(), index);

        //         list[list_index] = self.evaluate(expression)?;

        //         match distance {
        //             Ok(_) => self
        //                 .environment
        //                 .borrow_mut()
        //                 .assign(name.lexeme, LoxEntity::List(list))?,
        //             Err(_) => self
        //                 .globals
        //                 .borrow_mut()
        //                 .assign(name.lexeme, LoxEntity::List(list))?,
        //         };
        //     }
        //     (LoxEntity::Map(mut map), LoxEntity::Literal(index)) => {
        //         let value = self.evaluate(expression)?;

        //         map.insert(index, value);

        //         match distance {
        //             Ok(_) => self
        //                 .environment
        //                 .borrow_mut()
        //                 .assign(name.lexeme, LoxEntity::Map(map))?,
        //             Err(_) => self
        //                 .globals
        //                 .borrow_mut()
        //                 .assign(name.lexeme, LoxEntity::Map(map))?,
        //         };
        //     }
        //     _ => return Err(LoxError::AstError),
        // };

        // Ok(LoxEntity::Literal(Literal::Nil))
    }

    fn visit_list(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::List { expressions } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // let mut list: Vec<LoxEntity> = vec![];

        // for expression in expressions.iter() {
        //     let value = self.evaluate(expression)?;
        //     list.push(value);
        // }

        // Ok(LoxEntity::List(list))
    }

    fn visit_literal(&mut self, expr: &Expression) -> LoxResult<()> {
        match expr {
            Expression::Literal { value } => match value {
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
                    let constant = self
                        .make_constant(Value::Object(Object::String(Box::new(str_val.clone()))))?;
                    self.emit_byte(OpCode::Constant(constant))
                },
            },
            _ => Err(LoxError::AstError),
        }
    }

    fn visit_logical(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::Logical { left, operator, right } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // match self.evaluate(left)? {
        //     LoxEntity::Literal(inner) => {
        //         let truthy_left = self.is_truthy(&Expression::Literal {
        //             value: inner.clone(),
        //         })?;

        //         match (truthy_left, &operator.token_type) {
        //             (true, TokenType::Or) | (false, TokenType::And) => {
        //                 Ok(LoxEntity::Literal(inner))
        //             }
        //             _ => Ok(self.evaluate(right)?),
        //         }
        //     }
        //     _ => Err(LoxError::AstError),
        // }
    }

    fn visit_map(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::Map { expression_map } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // let mut map: HashMap<Literal, LoxEntity> = HashMap::new();

        // for (key, value) in expression_map.iter() {
        //     let key = match self.evaluate(key)? {
        //         LoxEntity::Literal(literal) => literal,
        //         _ => return Err(LoxError::AstError),
        //     };
        //     let value = self.evaluate(value)?;

        //     map.insert(key, value);
        // }

        // Ok(LoxEntity::Map(map))
    }

    fn visit_set(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::Set { name, object, value } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // match (self.evaluate(object)?, &**object) {
        //     (
        //         LoxEntity::Callable(callable),
        //         Expression::Variable {
        //             name: instance_name,
        //         },
        //     ) => match *callable {
        //         LoxCallable::Class { mut class } => {
        //             let value = self.evaluate(value)?;
        //             class.set(name.lexeme.clone(), value.clone())?;

        //             // We must reassign this to update its internal state permanently.
        //             self.environment.borrow_mut().assign(
        //                 instance_name.lexeme.clone(),
        //                 LoxEntity::Callable(Box::new(LoxCallable::Class { class })),
        //             )?;

        //             Ok(value)
        //         }
        //         _ => Err(LoxError::AstError),
        //     },
        //     _ => Err(LoxError::AstError),
        // }
    }

    fn visit_unary(&mut self, expr: &Expression) -> LoxResult<()> {
        let Expression::Unary { operator, right } = expr else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // match operator.token_type {
        //     TokenType::Minus => match self.evaluate(right)? {
        //         LoxEntity::Literal(outer_right) => match outer_right {
        //             Literal::Number(number) => Ok(LoxEntity::Literal(Literal::Number(-number))),
        //             _ => Err(LoxError::TypeError(format!(
        //                 "expected number, got {}",
        //                 outer_right
        //             ))),
        //         },
        //         _ => Err(LoxError::TypeError(
        //             "expected number, got something else".into(),
        //         )),
        //     },
        //     TokenType::Bang => match self.evaluate(right)? {
        //         LoxEntity::Literal(right) => Ok(LoxEntity::Literal(Literal::Boolean(
        //             !self._is_truthy(&right)?,
        //         ))),
        //         _ => Err(LoxError::TypeError(
        //             "expected value, got something else".to_string(),
        //         )),
        //     },
        //     _ => Err(LoxError::AstError),
        // }
    }

    fn visit_variable(&mut self, expr: &Expression) -> LoxResult<()> {
        Ok(())
        // match expr {
        //     Expression::Variable { name } => self.look_up_variable(name, expr),
        //     _ => Err(LoxError::AstError),
        // }
    }
}

impl StatementVisitor for Compiler {
    fn visit_block(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::Block { statements } = stmt else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // let new_env = Rc::new(RefCell::new(Environment::new(Some(
        //     self.environment.clone(),
        // ))));

        // self.execute_block(statements, new_env)
    }

    fn visit_class(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::Class { name, methods } = stmt else {
            return Err(LoxError::AstError);
        };

        // self.environment
        //     .borrow_mut()
        //     .define(name.lexeme.clone(), LoxEntity::Literal(Literal::Nil));

        // let mut class_methods: HashMap<String, LoxCallable> = HashMap::new();

        // for method in methods.iter() {
        //     match method {
        //         Statement::Function {
        //             name: method_name, ..
        //         } => {
        //             class_methods.insert(
        //                 method_name.lexeme.clone(),
        //                 LoxCallable::Function {
        //                     statement: method.clone(),
        //                     environment: self.environment.clone(),
        //                 },
        //             );
        //         }
        //         _ => return Err(LoxError::AstError),
        //     };
        // }

        // let class = LoxEntity::Callable(Box::new(LoxCallable::Class {
        //     class: LoxInstance::new(name.clone(), stmt.clone(), class_methods),
        // }));

        // self.environment
        //     .borrow_mut()
        //     .assign(name.lexeme.clone(), class)?;

        Ok(())
    }

    fn visit_expression(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::Expression { expression } = stmt else {
            return Err(LoxError::AstError);
        };

        // let _ = self.evaluate(expression)?;
        Ok(())
    }

    fn visit_foreach(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::Foreach { iterator, iterable, body } = stmt else {
            return Err(LoxError::AstError);
        };

        Ok(())

        // match (&**body, self.evaluate(iterable)?) {
        //     (Statement::Block { statements }, LoxEntity::Literal(Literal::String(string))) => {
        //         for item in string.chars() {
        //             let new_env = Rc::new(RefCell::new(Environment::new(Some(
        //                 self.environment.clone(),
        //             ))));

        //             new_env.borrow_mut().define(
        //                 iterator.lexeme.clone(),
        //                 LoxEntity::Literal(Literal::String(item.to_string())),
        //             );

        //             self.execute_block(statements, new_env)?;
        //         }

        //         Ok(())
        //     }
        //     (Statement::Block { statements }, LoxEntity::List(list)) => {
        //         for item in list.iter() {
        //             let new_env = Rc::new(RefCell::new(Environment::new(Some(
        //                 self.environment.clone(),
        //             ))));

        //             new_env
        //                 .borrow_mut()
        //                 .define(iterator.lexeme.clone(), item.clone());

        //             self.execute_block(statements, new_env)?;
        //         }

        //         Ok(())
        //     }
        //     (Statement::Block { statements }, LoxEntity::Map(map)) => {
        //         for item in map.keys() {
        //             let new_env = Rc::new(RefCell::new(Environment::new(Some(
        //                 self.environment.clone(),
        //             ))));

        //             new_env
        //                 .borrow_mut()
        //                 .define(iterator.lexeme.clone(), LoxEntity::Literal(item.clone()));

        //             self.execute_block(statements, new_env)?;
        //         }

        //         Ok(())
        //     }
        //     _ => Err(LoxError::AstError),
        // }
    }

    fn visit_function(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::Function { name, .. } = stmt else {
            return Err(LoxError::AstError);
        };

        // let callable = LoxCallable::Function {
        //     statement: stmt.clone(),
        //     environment: Rc::new(RefCell::new(Environment::new(Some(
        //         self.environment.clone(),
        //     )))),
        // };

        // self.environment
        //     .borrow_mut()
        //     .define(name.lexeme.clone(), LoxEntity::Callable(Box::new(callable)));

        Ok(())
    }

    fn visit_if(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::If { condition, then_branch, else_branch } = stmt else {
            return Err(LoxError::AstError);
        };

        // match self.is_truthy(condition)? {
        //     true => self.execute(then_branch)?,
        //     false => match else_branch {
        //         Some(eb) => self.execute(eb)?,
        //         None => {}
        //     },
        // };

        Ok(())
    }

    fn visit_print(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::Print { expression } = stmt else {
            return Err(LoxError::AstError);
        };

        self.evaluate(expression)?;
        self.emit_byte(OpCode::Print(true))
    }

    fn visit_return(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::Return { keyword: _, value } = stmt else {
            return Err(LoxError::AstError);
        };

        self.emit_return()

        // Err(LoxError::FunctionReturn(Box::new(self.evaluate(value)?)))
    }

    fn visit_while(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::While { condition, body } = stmt else {
            return Err(LoxError::AstError);
        };

        // while self.is_truthy(condition)? {
        //     self.execute(body)?;
        // }

        Ok(())
    }

    fn visit_var(&mut self, stmt: &Statement) -> LoxResult<()> {
        let Statement::Var { name, initializer } = stmt else {
            return Err(LoxError::AstError)
        };

        // let value = match initializer {
        //     Some(init) => self.evaluate(init)?,
        //     None => LoxEntity::Literal(Literal::Nil),
        // };

        // self.environment
        //     .borrow_mut()
        //     .define(name.lexeme.clone(), value);

        Ok(())
    }
}

// impl Interpreter {
//     pub fn execute_block(
//         &mut self,
//         statements: &[Statement],
//         working_env: Rc<RefCell<Environment<String, LoxEntity>>>,
//     ) -> LoxResult<()> {
//         let previous = self.environment.clone();
//         self.environment = working_env;
//
//         let mut statement_iter = statements.iter();
//         let mut output: LoxResult<()> = Ok(());
//
//         loop {
//             let next_statement = statement_iter.next();
//
//             match next_statement {
//                 None => break,
//                 Some(statement) => match self.execute(statement) {
//                     Ok(_) => continue,
//                     Err(e) => {
//                         output = Err(e);
//                         break;
//                     }
//                 },
//             };
//         }
//
//         self.environment = previous;
//
//         output
//     }
// }
