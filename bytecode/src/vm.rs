use std::{
    cell::RefCell,
    collections::HashMap,
    rc::Rc,
};

use crate::{
    builtin,
    chunk::OpCode,
    compiler::Compiler,
    errors::LoxError,
    object::{
        BoundMethod,
        Class,
        Closure,
        Function,
        Instance,
        NativeFn,
        NativeFunction,
        Object,
        UpValue,
        UpValuePtr,
        ValueIter,
        ValueMap,
    },
    value::{
        Value,
        ValuePtr,
    },
};

#[cfg(feature="debug")]
use crate::chunk::Chunk;

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
    upvalues: Vec<UpValuePtr>,

    init_string: &'static str,
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! obj {
    ( $ot:tt, $val:expr ) => {
        Value::Object(Object::$ot(Box::new($val)))
    }
}

macro_rules! err {
    ( $msg:expr ) => {
        {
            return Err(LoxError::RuntimeError($msg.into()))
        }
    }
}

impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::with_capacity(STACK_MAX),
            globals: HashMap::with_capacity(STACK_MAX),
            upvalues: Vec::with_capacity(STACK_MAX),
            init_string: "init",
        }
    }

    pub fn define_native(&mut self, name: &str, function: NativeFn, arity: usize) -> Result<(), LoxError> {
        let function = ValuePtr::new(
            obj!(
                Native,
                NativeFunction {
                    function,
                    obj: None,
                    name: String::from(name),
                    arity,
                }
            )
        );
        self.globals.insert(String::from(name), function);

        Ok(())
    }

    pub fn interpret(&mut self, input: &str) -> Result<(), LoxError> {
        let mut compiler = Compiler::new(&input);
        let function = compiler.compile()?;

        let closure = Closure {
            upvalues: Vec::with_capacity(function.upvalue_count),
            function,
            obj: None,
        };

        self.stack_push_value(obj!(Closure, closure.clone()));
        self.call(&closure, 0)?;

        self.define_native("len", builtin::_len, 1)?;
        self.define_native("range", builtin::_range, 2)?;
        self.define_native("str", builtin::_str, 1)?;
        self.define_native("time", builtin::_time, 0)?;
        self.define_native("type", builtin::_type, 1)?;

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
            None => unreachable!(),
        }
    }

    pub fn frame_mut(&mut self) -> &mut CallFrame {
        match self.frames.last_mut() {
            Some(frame) => frame,
            None => unreachable!(),
        }
    }

    pub fn function(&self) -> &Function {
        &self.frame().closure.function
    }

    fn stack_push_value(&mut self, value: Value) {
        self.stack.push(ValuePtr::new(value));
    }

    fn _convert_index(&self, len: usize, number: i32) -> Result<usize, LoxError> {
        self._extract_index(len, number as f64)
    }

    fn _extract_index(&self, len: usize, number: f64) -> Result<usize, LoxError> {
        let index = match number < 0.0f64 {
            true => (len as f64 + number) as usize,
            false => number as usize,
        };

        match index > len {
            true => Ok(len),
            false => Ok(index),
        }
    }

    pub fn run(&mut self) -> Result<(), LoxError> {
        macro_rules! binary_opcode {
            ( $op:tt, $op_char:expr, $output:expr ) => {
                {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    let (Value::Number(b), Value::Number(a)) = (&*right.borrow(), &*left.borrow()) else {
                        err!(&format!("comparison '{}' only operates on numbers", $op_char));
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
                        None => err!("empty stack"),
                    };
                    let index = index + self.frame().stack_offset;
                    self.stack[index] = val;
                },
                OpCode::GetGlobal(index) => {
                    let key = match &*self.function().chunk.constants.get(index).borrow() {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => err!("missing constant"),
                    };

                    let val = match self.globals.get(&key) {
                        Some(val) => val,
                        None => err!(&format!("undefined variable: {}", key)),
                    };

                    self.stack.push(val.clone());
                },
                OpCode::DefineGlobal(index) => {
                    let key = match &*self.function().chunk.constants.get(index).borrow() {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => err!("missing constant"),
                    };

                    let val = self.pop_stack()?;

                    self.globals.insert(key, val);
                },
                OpCode::SetGlobal(index) => {
                    let key = match &*self.function().chunk.constants.get(index).borrow() {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => err!("missing constant"),
                    };

                    if !self.globals.contains_key(&key) {
                        err!(&format!("undefined variable: {}", key))
                    }

                    let val = match self.stack.last() {
                        Some(val) => val.clone(),
                        None => err!("empty stack"),
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
                OpCode::GetProperty(index) => {
                    let instance_ptr = self.stack.last().unwrap().clone();
                    let Value::Object(Object::Instance(instance)) = &*instance_ptr.borrow() else {
                        err!("not an instance");
                    };
                    let name_constant = self.function().chunk.constants.get(index).clone();
                    let Value::Object(Object::String(prop_name)) = &*name_constant.borrow() else {
                        unreachable!();
                    };

                    match instance.fields.get(&**prop_name) {
                        Some(prop) => {
                            self.pop_stack()?;
                            self.stack.push(prop.clone());
                        },
                        None => self.bind_method(instance.class.clone(), prop_name)?,
                    };
                },
                OpCode::SetProperty(index) => {
                    let instance_ptr = self.stack[self.stack.len() - 2].clone();
                    let Value::Object(Object::Instance(instance)) = &mut *instance_ptr.borrow_mut() else {
                        err!("not an instance");
                    };
                    let name_constant = self.function().chunk.constants.get(index).clone();
                    let Value::Object(Object::String(prop_name)) = &*name_constant.borrow() else {
                        unreachable!();
                    };

                    let value = self.pop_stack()?;
                    self.pop_stack()?;

                    instance.fields.insert(*prop_name.clone(), value.clone());
                    self.stack.push(value);
                },
                OpCode::GetIndex => {
                    let index = self.pop_stack()?;
                    let container = self.pop_stack()?;

                    let value = match &*container.borrow() {
                        Value::Object(Object::List(list)) => match &*index.borrow() {
                            Value::Number(number) => {
                                let index_usize = self._extract_index(list.len(), *number)?;
                                list[index_usize].clone()
                            },
                            _ => err!("lists can only be indexed with integers"),
                        },
                        Value::Object(Object::String(string)) => match &*index.borrow() {
                            Value::Number(number) => {
                                let index_usize = self._extract_index(string.len(), *number)?;

                                ValuePtr::new(
                                    obj!(String, string.chars().nth(index_usize).unwrap().to_string())
                                )
                            },
                            _ => err!("strings can only be indexed with integers"),
                        },
                        Value::Object(Object::Map(hmap)) => {
                            match hmap.map.get(&index) {
                                Some(value) => value.clone(),
                                None => err!(&format!("no entry for key {:?}", index)),
                            }
                        },
                        _ => err!("only lists and maps can be indexed into"),
                    };

                    self.stack.push(value);
                },
                OpCode::SetIndex => {
                    let value = self.pop_stack()?;
                    let index = self.pop_stack()?;
                    let container = self.pop_stack()?;

                    match &mut *container.borrow_mut() {
                        Value::Object(Object::List(list)) => match &*index.borrow() {
                            Value::Number(number) => {
                                let index_usize = self._extract_index(list.len(), *number)?;
                                list[index_usize] = value.clone();
                            },
                            _ => err!("lists can only be indexed with integers")
                        },
                        Value::Object(Object::Map(hmap)) => {
                            hmap.map.insert(index.clone(), value.clone());
                        },
                        _ => err!("only lists and maps can be indexed into")
                    };

                    self.stack.push(value);
                },
                OpCode::DeleteIndex => {
                    let index = self.pop_stack()?;
                    let container = self.pop_stack()?;

                    match &mut *container.borrow_mut() {
                        Value::Object(Object::List(list)) => match &*index.borrow() {
                            Value::Number(number) => {
                                let index_usize = self._extract_index(list.len(), *number)?;
                                list.remove(index_usize);
                            },
                            _ => err!("lists can only be indexed with integers")
                        },
                        Value::Object(Object::Map(hmap)) => {
                            hmap.map.remove(&index);
                        },
                        _ => err!("only lists and maps can be indexed into")
                    };
                },
                OpCode::GetSlice(has_left, has_right) => {
                    let right: Option<i32> = match has_right {
                        true => match &*self.pop_stack()?.borrow() {
                            Value::Number(number) => Some(*number as i32),
                            _ => err!("only numbers can be used to slice containers")
                        },
                        false => None,
                    };
                    let left: Option<i32> = match has_left {
                        true => match &*self.pop_stack()?.borrow() {
                            Value::Number(number) => Some(*number as i32),
                            _ => err!("only numbers can be used to slice containers"),
                        },
                        false => None,
                    };

                    let value = match &*self.pop_stack()?.borrow() {
                        Value::Object(Object::List(list)) => {
                            let left = match left {
                                Some(left) => self._convert_index(list.len(), left)?,
                                None => 0,
                            };
                            let right = match right {
                                Some(right) => self._convert_index(list.len(), right)?,
                                None => list.len(),
                            };

                            let tmp_value = match left > right {
                                true => vec![],
                                false => list[left..right].to_vec(),
                            };

                            obj!(List, tmp_value)
                        },
                        Value::Object(Object::String(string)) => {
                            let left = match left {
                                Some(left) => self._convert_index(string.len(), left)?,
                                None => 0,
                            };
                            let right = match right {
                                Some(right) => self._convert_index(string.len(), right)?,
                                None => string.len(),
                            };

                            let tmp_value = match left > right {
                                true => "".to_string(),
                                false => string[left..right].to_string(),
                            };

                            obj!(String, tmp_value)
                        },
                        _ => err!("only lists and strings can be sliced"),
                    };

                    self.stack_push_value(value);
                },
                OpCode::Equal => binary_opcode! { ==, "==", Value::Bool },
                OpCode::Greater => binary_opcode! { >, '>', Value::Bool },
                OpCode::Less => binary_opcode! { <, '<', Value::Bool },
                OpCode::In => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    let right = right.borrow();

                    match &*right {
                        Value::Object(Object::List(list)) => {
                            self.stack_push_value(
                                Value::Bool(
                                    list.contains(&left)
                                )
                            );
                        },
                        Value::Object(Object::Map(hmap)) => {
                            self.stack_push_value(
                                Value::Bool(
                                    hmap.map.contains_key(&left)
                                )
                            );
                        },
                        Value::Object(Object::String(string)) => match &*left.borrow() {
                            Value::Object(Object::String(substring)) =>  {
                                self.stack_push_value(
                                    Value::Bool(
                                        string.contains(&**substring)
                                    )
                                );
                            },
                            _ => err!("invalid 'in' check: strings can only contain other strings"),
                        },
                        _ => err!("'in' operator only functions on iterables"),
                    };
                },
                OpCode::Add => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    let right = right.borrow();
                    let left = left.borrow();

                    match (&*right, &*left) {
                        (
                            Value::Object(Object::List(b)),
                            Value::Object(Object::List(a)),
                        ) => {
                            let mut new_list: Vec<ValuePtr> = Vec::new();

                            for item in a.iter() {
                                new_list.push(item.clone());
                            }

                            for item in b.iter() {
                                new_list.push(item.clone());
                            }

                            self.stack_push_value(obj!(List, new_list))
                        },
                        (Value::Number(b), Value::Number(a)) => {
                            self.stack_push_value(Value::Number(a + b));
                        },
                        (Value::Number(b), Value::Object(Object::String(a))) => {
                            self.stack_push_value(obj!(String, format!("{}{}", a, b)));
                        },
                        (Value::Object(Object::String(b)), Value::Number(a)) => {
                            self.stack_push_value(obj!(String, format!("{}{}", a, b)));
                        },
                        (Value::Object(Object::String(b)), Value::Object(Object::String(a))) => {
                            self.stack_push_value(obj!(String, format!("{}{}", a, b)));
                        },
                        _ => err!("operation '+' only operates on numbers and strings"),
                    };
                },
                OpCode::Subtract => binary_opcode! { -, '-', Value::Number },
                OpCode::Modulo => binary_opcode! { %, '%', Value::Number },
                OpCode::Multiply => binary_opcode! { *, '*', Value::Number },
                OpCode::Divide => binary_opcode! { /, '/', Value::Number },
                OpCode::Not => {
                    let item = self.pop_stack()?;
                    self.stack_push_value(Value::Bool(!self.truthy(&item)?));
                }
                OpCode::Negate => {
                    let item = match *(self.pop_stack()?).borrow() {
                        Value::Number(number) => number,
                        _ => err!("negation only operates on numbers"),
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
                OpCode::Break(offset, fixed) => {
                    if !fixed {
                        err!("cannot break from outside a loop");
                    };

                    self.frame_mut().pos += offset + 1;
                },
                OpCode::Continue(offset, fixed) => {
                    if !fixed {
                        err!("cannot continue from outside a loop");
                    };

                    self.frame_mut().pos -= offset;
                    continue;
                },
                OpCode::DefineIterator => {
                    let to_iterate = self.pop_stack()?;
                    let value_iter = ValueIter::new(to_iterate)?;

                    self.stack_push_value(obj!(Iterator, value_iter));
                },
                OpCode::IteratorNext(index, jump) => {
                    let stack_index = index + self.frame().stack_offset;
                    let iterator_ptr = self.stack[index - 1].clone();
                    let Value::Object(Object::Iterator(iterator)) = &mut *iterator_ptr.borrow_mut() else {
                        unreachable!();
                    };

                    if iterator.next == 0 {
                        let constant = self.function().chunk.constants.get(index).clone();
                        self.stack.push(constant);
                    }

                    if iterator.next == iterator.items.len() {
                        self.frame_mut().pos += jump + 1;
                    } else {
                        self.stack[stack_index] = iterator.items[iterator.next].clone();
                        iterator.next += 1;
                    }
                },
                OpCode::BuildList(item_count) => {
                    let range_start = self.stack.len() - item_count;
                    let list: Vec<ValuePtr> = self.stack[range_start..].to_vec();

                    self.stack.truncate(range_start);
                    self.stack_push_value(obj!(List, list));
                },
                OpCode::BuildMap(item_count) => {
                    let range_start = self.stack.len() - item_count;
                    let mut map: HashMap<ValuePtr, ValuePtr> = HashMap::new();

                    while map.len() * 2 < item_count {
                        let val = self.pop_stack()?;
                        let key = self.pop_stack()?;

                        map.insert(key.clone(), val.clone());
                    }

                    self.stack.truncate(range_start);
                    self.stack_push_value(obj!(Map, ValueMap { map }));
                },
                OpCode::Call(arg_count) => {
                    self.call_value(arg_count)?;
                    continue;
                },
                OpCode::Closure(index, ref upvalues) => {
                    let constant = self.function().chunk.constants.get(index).clone();
                    let function = match &*constant.borrow() {
                        Value::Object(Object::Function(function)) => *function.clone(),
                        _ => unreachable!(),
                    };

                    let mut captured: Vec<UpValuePtr> = Vec::with_capacity(upvalues.len());
                    for uv in upvalues.clone().iter() {
                        let new_upvalue = match uv.is_local {
                            true => self.capture_upvalue(uv.index)?,
                            false => self.frame().closure.upvalues[uv.index - 1].clone(),
                        };

                        captured.push(new_upvalue);
                    }

                    self.stack_push_value(
                        obj!(
                            Closure,
                            Closure {
                                obj: None,
                                upvalues: captured,
                                function,
                            }
                        )
                    );
                },
                OpCode::CloseUpValue => {
                    let Some(value) = self.stack.last() else {
                        err!("can't close an upvalue without a stack!");
                    };

                    self.close_upvalues(value.clone());
                    self.pop_stack()?;
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
                OpCode::Class(index) => {
                    let class_name = self.read_string(index)?;

                    self.stack_push_value(obj!(Class, Class::new(class_name)));
                },
                OpCode::Method(index) => {
                    self.define_method(&self.read_string(index)?)?;
                },
            };

            self.frame_mut().pos += 1;
        }

        Ok(())
    }

    fn read_string(&self, index: usize) -> Result<String, LoxError> {
        let name_constant = self.function().chunk.constants.get(index).clone();
        let Value::Object(Object::String(name)) = &*name_constant.borrow() else {
            err!("tried to read a string and failed");
        };

        Ok(*name.clone())
    }

    fn check_arity(
        &self,
        name: &str,
        callee_arity: usize,
        arg_count: usize,
    ) -> Result<(), LoxError> {
        if callee_arity != arg_count {
            err!(&format!("{}: expected {} arguments but {} were given", name, callee_arity, arg_count));
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
        let borrowed_at_offset = at_offset.borrow();

        match &*borrowed_at_offset {
            Value::Object(object) => match object {
                Object::BoundMethod(bound_method) => {
                    let Value::Object(Object::Closure(closure)) = &*bound_method.closure.borrow() else {
                        unreachable!();
                    };
                    let arg_range_start = self.stack.len() - arg_count;
                    self.stack[arg_range_start - 1] = bound_method.receiver.clone();

                    self.call(closure, arg_count)?;

                    Ok(true)
                },
                Object::Class(class) => {
                    let position = self.stack.len() - arg_count - 1;
                    self.stack[position] = ValuePtr::new(
                        obj!(Instance, Instance::new(at_offset.clone()))
                    );

                    if let Some(init_ptr) = class.methods.get(self.init_string) {
                        let Value::Object(Object::Closure(init)) = &*init_ptr.borrow() else {
                            unreachable!();
                        };

                        self.call(init, arg_count)?;
                    } else if arg_count != 0 {
                        err!(&format!("expected no arguments on initialization but got {}", arg_count));
                    } else {
                        self.frame_mut().pos += 1;
                    }

                    Ok(true)
                },
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

    fn bind_method(&mut self, class_ptr: ValuePtr, name: &str) -> Result<(), LoxError> {
        let Value::Object(Object::Class(class)) = &*class_ptr.borrow() else {
            unreachable!();
        };

        match class.methods.get(name) {
            Some(method) => {
                let bound_method = obj!(
                    BoundMethod,
                    BoundMethod {
                        receiver: self.stack.last().unwrap().clone(),
                        closure: method.clone(),
                        obj: None,
                    }
                );

                self.pop_stack()?;
                self.stack_push_value(bound_method);

                Ok(())
            },
            None => Err(LoxError::RuntimeError(format!("'{}' not a valid property", name)))
        }
    }

    fn capture_upvalue(&mut self, upvalue_index: usize) -> Result<UpValuePtr, LoxError> {
        let item = &self.stack[self.frame().stack_offset + upvalue_index];

        for (idx, upvalue) in self.upvalues.iter().enumerate() {
            if idx == upvalue_index - 1 {
                return Ok(upvalue.clone());
            }
        }

        let new_upvalue = Rc::new(
            RefCell::new(
                UpValue {
                    location: item.clone(),
                    location_index: upvalue_index,
                    obj: None,
                }
            )
        );

        self.upvalues.push(new_upvalue.clone());

        Ok(new_upvalue)
    }

    fn close_upvalues(&mut self, value: ValuePtr) {
        for idx in (0..self.upvalues.len()).rev() {
            if self.upvalues[idx].borrow().location != value {
                break;
            }

            self.upvalues.remove(idx);
        }

        // let last = self.stack.last().unwrap();
        // println!("closing {}", upvalue_index);
        // while !self.upvalues.is_empty() && self.upvalues.last().unwrap().borrow().location_index >= upvalue_index {
        //     println!("popping {:?}", self.upvalues.last().unwrap());
        //     self.upvalues.pop();
        // }
    }

    fn define_method(&mut self, name: &str) -> Result<(), LoxError> {
        let method = self.stack.last().unwrap();
        let class_ptr = self.stack[self.stack.len() - 2].clone();
        let Value::Object(Object::Class(class)) = &mut *class_ptr.borrow_mut() else {
            return Err(LoxError::RuntimeError("no class on stack".into()));
        };

        class.methods.insert(String::from(name), method.clone());
        self.stack.pop();

        Ok(())
    }

    #[allow(clippy::only_used_in_recursion)]
    fn truthy(&self, item: &ValuePtr) -> Result<bool, LoxError> {
        match &*item.borrow() {
            Value::Bool(boolean) => Ok(*boolean),
            Value::Nil => Ok(false),
            Value::Number(number) => Ok(*number != 0.0f64),
            Value::Object(object) => match object {
                Object::BoundMethod(_) => Ok(true),
                Object::Class(_) => Ok(true),
                Object::Closure(_) => Ok(true),
                Object::Function(_) => Ok(true),
                Object::Instance(_) => Ok(true),
                Object::Iterator(_) => Ok(true),
                Object::List(list) => Ok(!list.is_empty()),
                Object::Map(hmap) => Ok(!hmap.map.is_empty()),
                Object::Native(_) => Ok(true),
                Object::String(string) => Ok(!string.is_empty()),
                Object::UpValue(value) => self.truthy(&value.location),
            },
        }
    }
}
