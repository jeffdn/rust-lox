use std::collections::HashMap;

use crate::{
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    errors::LoxError,
    object::Object,
    value::Value,
};

static STACK_MAX: usize = 256;

pub struct VirtualMachine {
    chunks: Vec<Chunk>,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,

    pos: usize,
}


impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            chunks: Vec::new(),
            stack: Vec::with_capacity(STACK_MAX),
            globals: HashMap::new(),
            pos: 0,
        }
    }

    pub fn interpret(&mut self, input: String) -> Result<(), LoxError> {
        let mut compiler = Compiler::new(&input);
        let chunk = compiler.compile()?;

        self.chunks.push(chunk);

        self.run()
    }

    #[cfg(feature="debug")]
    fn dump(&self, chunk: &Chunk, idx: usize) {
        let stack_str: Vec<String> = self.stack.iter()
            .map(|v| format!("{}", v))
            .collect();

        println!("          [{}]", stack_str.join(", "));
        chunk.disassemble_instruction(&chunk.code[idx], idx);
    }

    fn pop_stack(&mut self) -> Result<Value, LoxError> {
        match self.stack.pop() {
            Some(item) => Ok(item),
            None => Err(LoxError::RuntimeError("stack empty".into())),
        }
    }

    pub fn run(&mut self) -> Result<(), LoxError> {
        let chunk = self.chunks.pop().unwrap();

        while self.pos < chunk.code.len() {
            #[cfg(feature="debug")]
            self.dump(&chunk, self.pos);

            match chunk.code[self.pos] {
                OpCode::Constant(index) => {
                    self.stack.push(chunk.constants.get(index).clone());
                },
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Pop => { self.pop_stack()?; },
                OpCode::GetLocal(index) => {
                    self.stack.push(self.stack[index].clone());
                },
                OpCode::SetLocal(index) => {
                    let val = match self.stack.last() {
                        Some(val) => val.clone(),
                        None =>
                            return Err(
                            LoxError::RuntimeError(
                                format!("empty stack")
                            )
                        ),
                    };
                    self.stack[index] = val;
                },
                OpCode::GetGlobal(index) => {
                    let key = match chunk.constants.get(index) {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => return Err(LoxError::RuntimeError("missing constant".into())),
                    };

                    let val = match self.globals.get(&key) {
                        Some(val) => val,
                        None => return Err(
                            LoxError::RuntimeError(
                                format!("undefined variable: {}", key)
                            )
                        ),
                    };

                    self.stack.push(val.clone());
                },
                OpCode::DefineGlobal(index) => {
                    let key = match chunk.constants.get(index) {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => return Err(LoxError::RuntimeError("missing constant".into())),
                    };

                    let val = self.pop_stack()?;

                    self.globals.insert(key, val);
                },
                OpCode::SetGlobal(index) => {
                    let key = match chunk.constants.get(index) {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => return Err(LoxError::RuntimeError("missing constant".into())),
                    };

                    if !self.globals.contains_key(&key) {
                        return Err(
                            LoxError::RuntimeError(
                                format!("undefined variable: {}", key)
                            )
                        )
                    }

                    let val = match self.stack.last() {
                        Some(val) => val.clone(),
                        None =>
                            return Err(
                            LoxError::RuntimeError(
                                format!("empty stack")
                            )
                        ),
                    };

                    self.globals.insert(key, val);
                },
                OpCode::Equal => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    self.stack.push(Value::Bool(self.values_equal(&left, &right)?));
                },
                OpCode::Greater => {
                    let (b, a) = match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => (b, a),
                        _ => return Err(
                            LoxError::RuntimeError(
                                "comparison '>' only operates on numbers".into()
                            )
                        ),
                    };
                    self.stack.push(Value::Bool(a > b));
                },
                OpCode::Less => {
                    let (b, a) = match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => (b, a),
                        _ => return Err(
                            LoxError::RuntimeError(
                                "comparison '<' only operates on numbers".into()
                            )
                        ),
                    };
                    self.stack.push(Value::Bool(a < b));
                },
                OpCode::Add => {
                    match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => {
                            self.stack.push(Value::Number(a + b));
                        },
                        (Value::Object(b), Value::Object(a)) => match (a, b) {
                            (Object::String(a), Object::String(b)) => {
                                self.stack.push(
                                    Value::Object(
                                        Object::String(
                                            Box::new(
                                                format!("{}{}", a, b)
                                            )
                                        )
                                    )
                                );
                            },
                        },
                        _ => return Err(
                            LoxError::RuntimeError(
                                "operation '+' only operates on numbers and strings".into()
                            )
                        ),
                    };
                },
                OpCode::Subtract => {
                    let (b, a) = match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => (b, a),
                        _ => return Err(
                            LoxError::RuntimeError(
                                "operation '-' only operates on numbers".into()
                            )
                        ),
                    };
                    self.stack.push(Value::Number(a - b));
                },
                OpCode::Multiply => {
                    let (b, a) = match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => (b, a),
                        _ => return Err(
                            LoxError::RuntimeError(
                                "operation '*' only operates on numbers".into()
                            )
                        ),
                    };
                    self.stack.push(Value::Number(a * b));
                },
                OpCode::Divide => {
                    let (b, a) = match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => (b, a),
                        _ => return Err(
                            LoxError::RuntimeError(
                                "operation '/' only operates on numbers".into()
                            )
                        ),
                    };
                    self.stack.push(Value::Number(a / b));
                },
                OpCode::Not => {
                    let item = self.pop_stack()?;
                    self.stack.push(Value::Bool(!self.truthy(&item)?));
                }
                OpCode::Negate => {
                    let item = match self.pop_stack()? {
                        Value::Number(number) => number,
                        _ => return Err(
                            LoxError::RuntimeError(
                                "negation only operates on numbers".into()
                            )
                        ),
                    };
                    self.stack.push(Value::Number(-item));
                },
                OpCode::Print => {
                    println!("{}", self.pop_stack()?);
                },
                OpCode::Jump(offset) => {
                    self.pos += offset;
                },
                OpCode::JumpIfFalse(offset) => {
                    if !self.truthy(self.stack.last().unwrap())? {
                        self.pos += offset;
                    }
                },
                OpCode::Loop(offset) => {
                    self.pos -= offset;
                    continue;
                }
                OpCode::Return => break,
            };

            self.pos += 1;
        }

        Ok(())
    }

    fn truthy(&self, item: &Value) -> Result<bool, LoxError> {
        match item {
            Value::Number(number) => Ok(*number != 0.0f32),
            Value::Nil => Ok(false),
            Value::Bool(boolean) => Ok(*boolean),
            Value::Object(object) => match object {
                Object::String(string) => Ok(!string.is_empty()),
            },
        }
    }

    fn values_equal(&self, left: &Value, right: &Value) -> Result<bool, LoxError> {
        match (left, right) {
            (Value::Number(left), Value::Number(right)) => Ok(left == right),
            (Value::Nil, Value::Nil) => Ok(true),
            (Value::Bool(left), Value::Bool(right)) => Ok(left == right),
            (Value::Object(left), Value::Object(right)) => match (left, right) {
                (Object::String(left), Object::String(right)) => Ok(left == right),
            },
            _ => Ok(false),
        }
    }
}
