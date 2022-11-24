use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
    time::SystemTime,
};

use crate::{
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    errors::LoxError,
    object::{
        Closure,
        Function,
        NativeFn,
        NativeFunction,
        Object,
        ObjUpValue,
        ObjUpValuePtr,
    },
    value::{Value, ValuePtr},
};

static FRAMES_MAX: usize = 64;
static STACK_MAX: usize = 256;

pub struct CallFrame {
    closure: Closure,
    stack_offset: usize,
    pos: usize,
}

pub struct VirtualMachine {
    frames: Vec<CallFrame>,
    stack: Vec<ValuePtr>,
    globals: HashMap<String, ValuePtr>,
}

fn _built_in_time(_: &[ValuePtr]) -> Result<Value, LoxError> {
    match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(unix_now) => Ok(Value::Number(unix_now.as_secs_f64())),
        Err(_) => Err(LoxError::RuntimeError("time() failed, uh oh".into())),
    }
}

fn _built_in_str(input: &[ValuePtr]) -> Result<Value, LoxError> {
    match &*input[0].borrow() {
        Value::Object(object) => match object {
            Object::String(string) => Ok(Value::Object(Object::String(Box::new(*string.clone())))),
            _ => Err(LoxError::RuntimeError("string() only accepts primitives".into())),
        },
        _ => Ok(Value::Object(Object::String(Box::new((*input[0].borrow()).to_string())))),
    }
}

fn _built_in_len(input: &[ValuePtr]) -> Result<Value, LoxError> {
    let Value::Object(Object::String(string)) = &*input[0].borrow() else {
        return Err(LoxError::RuntimeError("len() only accepts strings and lists".into()));
    };
    Ok(Value::Number(string.len() as f64))
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self::new()
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
        self.stack_push_value(
            Value::Object(
                Object::String(
                    Box::new(name.clone())
                )
            )
        );
        let function = Rc::new(
            RefCell::new(
                Value::Object(
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
        let mut compiler = Compiler::new(&input);
        let function = compiler.compile()?;

        self.stack_push_value(
            Value::Object(
                Object::Function(
                    Box::new(
                        function.clone()
                    )
                )
            )
        );

        let closure = Closure {
            upvalues: Vec::with_capacity(function.upvalue_count),
            function,
            obj: None,
        };

        self.pop_stack()?;

        self.stack_push_value(
            Value::Object(
                Object::Closure(
                    Box::new(closure.clone())
                )
            )
        );

        self.call(&closure, 0)?;

        self.define_native("len".into(), _built_in_len, 1)?;
        self.define_native("time".into(), _built_in_time, 0)?;
        self.define_native("str".into(), _built_in_str, 1)?;

        self.run()
    }

    #[cfg(feature="debug")]
    fn dump(&self, chunk: &Chunk, idx: usize) {
        let stack_str: Vec<String> = self.stack.iter()
            .map(|v| format!("{}", (*v.borrow())))
            .collect();

        println!("          [{}]", stack_str.join(", "));
        chunk.disassemble_instruction(&chunk.code[idx], idx);
    }

    fn pop_stack(&mut self) -> Result<ValuePtr, LoxError> {
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

    pub fn function(&self) -> &Function {
        &self.frame().closure.function
    }

    fn stack_push_value(&mut self, value: Value) {
        self.stack.push(Rc::new(RefCell::new(value)));
    }

    pub fn run(&mut self) -> Result<(), LoxError> {
        macro_rules! binary_opcode {
            ( $op:tt, $op_char:expr, $output:expr ) => {
                {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    let (Value::Number(b), Value::Number(a)) = (&*right.borrow(), &*left.borrow()) else {
                        return Err(
                            LoxError::RuntimeError(
                                format!("comparison '{}' only operates on numbers", $op_char)
                            )
                        );
                    };

                    self.stack_push_value($output(a $op b));
                }
            };
        }

        while self.frame().pos < self.function().chunk.code.len() {
            let pos = self.frame().pos;

            #[cfg(feature="debug")]
            self.dump(&self.function().chunk, pos);

            match self.function().chunk.code[pos] {
                OpCode::Constant(index) => {
                    let constant = self.function().chunk.constants.get(index).clone();
                    self.stack.push(constant);
                },
                OpCode::False => self.stack_push_value(Value::Bool(false)),
                OpCode::True => self.stack_push_value(Value::Bool(true)),
                OpCode::Nil => self.stack_push_value(Value::Nil),
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
                            LoxError::RuntimeError("empty stack".to_string())
                        ),
                    };
                    let index = index + self.frame().stack_offset;
                    self.stack[index] = val;
                },
                OpCode::GetGlobal(index) => {
                    let key = match &*self.function().chunk.constants.get(index).borrow() {
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
                    let key = match &*self.function().chunk.constants.get(index).borrow() {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => return Err(LoxError::RuntimeError("missing constant".into())),
                    };

                    let val = self.pop_stack()?;

                    self.globals.insert(key, val);
                },
                OpCode::SetGlobal(index) => {
                    let key = match &*self.function().chunk.constants.get(index).borrow() {
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
                            LoxError::RuntimeError("empty stack".to_string())
                        ),
                    };

                    self.globals.insert(key, val);
                },
                OpCode::GetUpValue(index) => {
                    let upvalue = self.frame().closure.upvalues[index - 1].borrow().location.clone();
                    self.stack.push(upvalue);
                },
                OpCode::SetUpValue(index) => {
                    self.frame_mut().closure.upvalues[index - 1]
                        .borrow_mut().location = self.stack.last().unwrap().clone();
                },
                OpCode::Equal => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    self.stack_push_value(
                        Value::Bool(
                            self.values_equal(&left.borrow(), &right.borrow())?
                        )
                    );
                },
                OpCode::Greater => binary_opcode! { >, '>', Value::Bool },
                OpCode::Less => binary_opcode! { <, '<', Value::Bool },
                OpCode::Add => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    let right = right.borrow();
                    let left = left.borrow();

                    match (&*right, &*left) {
                        (Value::Number(b), Value::Number(a)) => {
                            self.stack_push_value(Value::Number(a + b));
                        },
                        (Value::Object(b), Value::Object(a)) => match (a, b) {
                            (Object::String(a), Object::String(b)) => {
                                self.stack_push_value(
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
                OpCode::Subtract => binary_opcode! { -, '-', Value::Number },
                OpCode::Multiply => binary_opcode! { *, '*', Value::Number },
                OpCode::Divide => binary_opcode! { /, '/', Value::Number },
                OpCode::Not => {
                    let item = self.pop_stack()?;
                    self.stack_push_value(Value::Bool(!self.truthy(&item)?));
                }
                OpCode::Negate => {
                    let item = match *(self.pop_stack()?).borrow() {
                        Value::Number(number) => number,
                        _ => return Err(
                            LoxError::RuntimeError(
                                "negation only operates on numbers".into()
                            )
                        ),
                    };
                    self.stack_push_value(Value::Number(-item));
                },
                OpCode::Print => {
                    println!("{}", *(self.pop_stack()?).borrow());
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
                OpCode::Closure(index, ref upvalues) => {
                    let constant = self.function().chunk.constants.get(index).clone();
                    let function = match &*constant.borrow() {
                        Value::Object(Object::Function(function)) => *function.clone(),
                        _ => panic!("unreachable"),
                    };

                    let mut captured: Vec<ObjUpValuePtr> = Vec::with_capacity(upvalues.len());
                    for uv in upvalues.iter() {
                        let new_upvalue = match uv.is_local {
                            true => self.capture_upvalue(&self.stack[self.frame().stack_offset + uv.index])?,
                            false => self.frame().closure.upvalues[uv.index - 1].clone(),
                        };

                        captured.push(new_upvalue);
                    }

                    self.stack_push_value(
                        Value::Object(
                            Object::Closure(
                                Box::new(
                                    Closure {
                                        obj: None,
                                        upvalues: captured,
                                        function,
                                    }
                                )
                            )
                        )
                    );
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

    fn call(&mut self, callee: &Closure, arg_count: usize) -> Result<(), LoxError> {
        let frame = CallFrame {
            closure: callee.clone(),
            stack_offset: self.stack.len() - arg_count - 1,
            pos: 0,
        };

        self.frames.push(frame);

        Ok(())
    }

    fn call_value(&mut self, arg_count: usize) -> Result<bool, LoxError> {
        let offset = self.stack.len() - arg_count - 1;
        let at_offset = self.stack[offset].clone();
        let at_offset = at_offset.borrow();

        match &*at_offset {
            Value::Object(object) => match object {
                Object::Closure(closure) => {
                    self.check_arity(&closure.function.name, closure.function.arity, arg_count)?;

                    self.call(closure, arg_count)?;
                    Ok(true)
                },
                Object::Native(native) => {
                    self.check_arity(&native.name, native.arity, arg_count)?;

                    let arg_range_start = self.stack.len() - arg_count;
                    let result = (native.function)(&self.stack[arg_range_start..])?;

                    self.stack.truncate(arg_range_start - 1);
                    self.stack_push_value(result);
                    self.frame_mut().pos += 1;

                    Ok(true)
                },
                _ => Err(LoxError::RuntimeError("can only call functions and classes".into()))
            },
            _ => Err(LoxError::RuntimeError("can only call functions and classes".into()))
        }
    }

    fn capture_upvalue(&self, item: &ValuePtr) -> Result<ObjUpValuePtr, LoxError> {
        Ok(
            Rc::new(
                RefCell::new(
                    ObjUpValue {
                        location: item.clone(),
                        obj: None,
                    }
                )
            )
        )
    }

    #[allow(clippy::only_used_in_recursion)]
    fn truthy(&self, item: &ValuePtr) -> Result<bool, LoxError> {
        match &*item.borrow() {
            Value::Number(number) => Ok(*number != 0.0f64),
            Value::Nil => Ok(false),
            Value::Bool(boolean) => Ok(*boolean),
            Value::Object(object) => match object {
                Object::String(string) => Ok(!string.is_empty()),
                Object::Function(_) => Ok(true),
                Object::Native(_) => Ok(true),
                Object::Closure(_) => Ok(true),
                Object::UpValue(value) => self.truthy(&value.location),
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
