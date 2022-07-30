use std::{
    collections::HashMap,
    time::SystemTime,
};

use crate::{
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    errors::LoxError,
    object::{
        Function,
        FunctionType,
        NativeFn,
        NativeFunction,
        Object,
    },
    value::Value,
};

static FRAMES_MAX: usize = 64;
static STACK_MAX: usize = 256;

pub struct CallFrame {
    function: Function,
    stack_offset: usize,
    pos: usize,
}

pub struct VirtualMachine {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

fn _built_in_time(_: &[Value]) -> Result<Value, LoxError> {
    match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(unix_now) => Ok(Value::Number(unix_now.as_secs_f64())),
        Err(_) => Err(LoxError::RuntimeError("time() failed, uh oh".into())),
    }
}

fn _built_in_str(input: &[Value]) -> Result<Value, LoxError> {
    match &input[0] {
        Value::Object(object) => match object {
            Object::String(string) => Ok(Value::Object(Object::String(Box::new(*string.clone())))),
            _ => Err(LoxError::RuntimeError("string() only accepts primitives".into())),
        },
        _ => Ok(Value::Object(Object::String(Box::new(input[0].clone().to_string())))),
    }
}

fn _built_in_len(input: &[Value]) -> Result<Value, LoxError> {
    match &input[0] {
        Value::Object(object) => match object {
            Object::String(string) => Ok(Value::Number(string.len() as f64)),
            _ => Err(LoxError::RuntimeError("len() only accepts strings and lists".into())),
        },
        _ => Ok(Value::Object(Object::String(Box::new(input[0].clone().to_string())))),
    }
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::with_capacity(STACK_MAX),
            globals: HashMap::new(),
        }
    }

    pub fn define_native(&mut self, name: String, function: NativeFn, arity: usize) -> Result<(), LoxError> {
        self.stack.push(Value::Object(Object::String(Box::new(name.clone()))));
        let function = Value::Object(
            Object::Native(
                Box::new(
                    NativeFunction {
                        function,
                        obj: None,
                        name: name.clone(),
                        arity,
                    }
                )
            )
        );
        self.stack.push(function.clone());
        self.globals.insert(
            name,
            function,
        );

        self.pop_stack()?;
        self.pop_stack()?;

        Ok(())
    }

    pub fn interpret(&mut self, input: String) -> Result<(), LoxError> {
        let mut compiler = Compiler::new(FunctionType::Script);
        let function = compiler.compile(&input)?;

        self.stack.push(
            Value::Object(
                Object::Function(
                    Box::new(
                        function.clone()
                    )
                )
            )
        );

        self.frames.push(
            CallFrame {
                function,
                stack_offset: 0,
                pos: 0,
            }
        );

        self.define_native("len".into(), _built_in_len, 1)?;
        self.define_native("time".into(), _built_in_time, 0)?;
        self.define_native("str".into(), _built_in_str, 1)?;

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

    pub fn frame(&self) -> &CallFrame {
        match self.frames.last() {
            Some(frame) => frame,
            None => panic!("unreachable"),
        }
    }

    pub fn frame_mut(&mut self) -> &mut CallFrame {
        match self.frames.last_mut() {
            Some(frame) => frame,
            None => panic!("unreachable"),
        }
    }

    pub fn run(&mut self) -> Result<(), LoxError> {
        while self.frame().pos < self.frame().function.chunk.code.len() {
            let pos = self.frame().pos;

            #[cfg(feature="debug")]
            self.dump(&self.frame().function.chunk, pos);

            match self.frame().function.chunk.code[pos] {
                OpCode::Constant(index) => {
                    let constant = self.frame().function.chunk.constants.get(index).clone();
                    self.stack.push(constant);
                },
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::Pop => { self.pop_stack()?; },
                OpCode::GetLocal(index) => {
                    let index = index + self.frame().stack_offset;
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
                    let index = index + self.frame().stack_offset;
                    self.stack[index] = val;
                },
                OpCode::GetGlobal(index) => {
                    let key = match self.frame().function.chunk.constants.get(index) {
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
                    let key = match self.frame().function.chunk.constants.get(index) {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => return Err(LoxError::RuntimeError("missing constant".into())),
                    };

                    let val = self.pop_stack()?;

                    self.globals.insert(key, val);
                },
                OpCode::SetGlobal(index) => {
                    let key = match self.frame().function.chunk.constants.get(index) {
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
                        (Value::Object(b), Value::Object(a)) => match (a.clone(), b.clone()) {
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
                            _ => return Err(
                                LoxError::RuntimeError(
                                    format!("operation '+' only operates on numbers and strings, got: {} + {}", a, b),
                                )
                            ),
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
                    self.frame_mut().pos += offset;
                },
                OpCode::JumpIfFalse(offset) => {
                    if !self.truthy(self.stack.last().unwrap())? {
                        self.frame_mut().pos += offset;
                    }
                },
                OpCode::Loop(offset) => {
                    self.frame_mut().pos -= offset;
                    continue;
                },
                OpCode::Call(arg_count) => {
                    self.call_value(arg_count)?;
                    continue;
                },
                OpCode::Return => {
                    let result = self.pop_stack()?;

                    let old_frame = self.frames.pop().unwrap();

                    if self.frames.is_empty() {
                        self.pop_stack()?;
                        return Ok(());
                    }

                    self.stack.truncate(old_frame.stack_offset);
                    self.stack.push(result);
                },
            };

            self.frame_mut().pos += 1;
        }

        Ok(())
    }

    fn check_arity(
        &self,
        name: &String,
        callee_arity: usize,
        arg_count: usize,
    ) -> Result<(), LoxError> {
        if callee_arity != arg_count {
            return Err(
                LoxError::RuntimeError(
                    format!("{}: expected {} arguments but {} were given", name, callee_arity, arg_count)
                )
            );
        }

        Ok(())
    }

    fn call(&self, callee: &Function, arg_count: usize) -> Result<CallFrame, LoxError> {
        Ok(
            CallFrame {
                function: callee.clone(),
                stack_offset: self.stack.len() - arg_count - 1,
                pos: 0,
            }
        )
    }

    fn call_value(&mut self, arg_count: usize) -> Result<bool, LoxError> {
        let offset = self.stack.len() - arg_count - 1;
        match &self.stack[offset] {
            Value::Object(object) => match object {
                Object::Function(function) => {
                    self.check_arity(&function.name, function.arity, arg_count)?;

                    let frame = self.call(function, arg_count)?;
                    self.frames.push(frame);
                    Ok(true)
                },
                Object::Native(native) => {
                    self.check_arity(&native.name, native.arity, arg_count)?;

                    let arg_range_start = self.stack.len() - arg_count;
                    let result = (native.function)(&self.stack[arg_range_start..])?;

                    self.stack.truncate(arg_range_start - 1);
                    self.stack.push(result);
                    self.frame_mut().pos += 1;

                    Ok(true)
                },
                _ => Err(LoxError::RuntimeError("can only call functions and classes".into()))
            },
            _ => Err(LoxError::RuntimeError("can only call functions and classes".into()))
        }
    }

    fn truthy(&self, item: &Value) -> Result<bool, LoxError> {
        match item {
            Value::Number(number) => Ok(*number != 0.0f64),
            Value::Nil => Ok(false),
            Value::Bool(boolean) => Ok(*boolean),
            Value::Object(object) => match object {
                Object::String(string) => Ok(!string.is_empty()),
                Object::Function(_) => Ok(true),
                Object::Native(_) => Ok(true),
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
                (Object::Function(left), Object::Function(right)) => Ok(left.name == right.name),
                _ => Ok(false),
            },
            _ => Ok(false),
        }
    }
}
