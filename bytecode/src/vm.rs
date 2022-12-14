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

    init_string: String,
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
            globals: HashMap::with_capacity(STACK_MAX),
            upvalues: Vec::with_capacity(STACK_MAX),
            init_string: "init".into(),
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
        let function = ValuePtr::new(
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

        self.define_native("len".into(), builtin::_len, 1)?;
        self.define_native("range".into(), builtin::_range, 2)?;
        self.define_native("str".into(), builtin::_str, 1)?;
        self.define_native("time".into(), builtin::_time, 0)?;
        self.define_native("type".into(), builtin::_type, 1)?;

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
                        None => return Err(LoxError::RuntimeError(format!("undefined variable: {}", key))),
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
                        return Err(LoxError::RuntimeError(format!("undefined variable: {}", key)))
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
                OpCode::GetProperty(index) => {
                    let instance_ptr = self.stack.last().unwrap().clone();
                    let Value::Object(Object::Instance(instance)) = &*instance_ptr.borrow() else {
                        return Err(LoxError::RuntimeError("not an instance".into()));
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
                        return Err(LoxError::RuntimeError("not an instance".into()));
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
                            _ => return Err(
                                LoxError::RuntimeError("lists can only be indexed with integers".into())
                            ),
                        },
                        Value::Object(Object::String(string)) => match &*index.borrow() {
                            Value::Number(number) => {
                                let index_usize = self._extract_index(string.len(), *number)?;

                                ValuePtr::new(
                                    Value::Object(
                                        Object::String(
                                            Box::new(
                                                string.chars().nth(index_usize).unwrap().to_string()
                                            )
                                        )
                                    )
                                )
                            },
                            _ => return Err(
                                LoxError::RuntimeError("strings can only be indexed with integers".into())
                            ),
                        },
                        Value::Object(Object::Map(hmap)) => {
                            match hmap.map.get(&index) {
                                Some(value) => value.clone(),
                                None => return Err(
                                    LoxError::RuntimeError(format!("no entry for key {:?}", index))
                                ),
                            }
                        },
                        _ => return Err(
                            LoxError::RuntimeError("only lists and maps can be indexed into".into())
                        ),
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
                            _ => return Err(
                                LoxError::RuntimeError("lists can only be indexed with integers".into())
                            ),
                        },
                        Value::Object(Object::Map(hmap)) => {
                            hmap.map.insert(index.clone(), value.clone());
                        },
                        _ => return Err(
                            LoxError::RuntimeError("only lists and maps can be indexed into".into())
                        ),
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
                            _ => return Err(
                                LoxError::RuntimeError("lists can only be indexed with integers".into())
                            ),
                        },
                        Value::Object(Object::Map(hmap)) => {
                            hmap.map.remove(&index);
                        },
                        _ => return Err(
                            LoxError::RuntimeError("only lists and maps can be indexed into".into())
                        ),
                    };
                },
                OpCode::GetSlice(has_left, has_right) => {
                    let right: Option<i32> = match has_right {
                        true => match &*self.pop_stack()?.borrow() {
                            Value::Number(number) => Some(*number as i32),
                            _ => return Err(
                                LoxError::RuntimeError(
                                    "only numbers can be used to slice containers".into(),
                                )
                            ),
                        },
                        false => None,
                    };
                    let left: Option<i32> = match has_left {
                        true => match &*self.pop_stack()?.borrow() {
                            Value::Number(number) => Some(*number as i32),
                            _ => return Err(
                                LoxError::RuntimeError(
                                    "only numbers can be used to slice containers".into(),
                                )
                            ),
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

                            Value::Object(
                                Object::List(
                                    Box::new(tmp_value)
                                )
                            )
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

                            Value::Object(
                                Object::String(
                                    Box::new(tmp_value)
                                )
                            )
                        },
                        _ => return Err(
                            LoxError::RuntimeError("only lists and strings can be sliced".into())
                        ),
                    };

                    self.stack_push_value(value);
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
                                        (&*string).contains(&**substring)
                                    )
                                );
                            },
                            _ => return Err(
                                LoxError::RuntimeError(
                                    "invalid 'in' check: strings can only contain other strings".into()
                                )
                            ),
                        },
                        _ => return Err(
                            LoxError::RuntimeError("'in' operator only functions on iterables".into())
                        ),
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

                            self.stack_push_value(
                                Value::Object(
                                    Object::List(
                                        Box::new(new_list)
                                    )
                                )
                            );
                        },
                        (Value::Number(b), Value::Number(a)) => {
                            self.stack_push_value(Value::Number(a + b));
                        },
                        (Value::Number(b), Value::Object(Object::String(a))) => {
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
                        (Value::Object(Object::String(b)), Value::Number(a)) => {
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
                        (Value::Object(Object::String(b)), Value::Object(Object::String(a))) => {
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
                                "operation '+' only operates on numbers and strings".into()
                            )
                        ),
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
                        _ => return Err(
                            LoxError::RuntimeError("negation only operates on numbers".into())
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
                OpCode::Break(offset, fixed) => {
                    if !fixed {
                        return Err(
                            LoxError::RuntimeError("cannot break from outside a loop".into())
                        );
                    };

                    self.frame_mut().pos += offset;
                },
                OpCode::Continue(offset, fixed) => {
                    if !fixed {
                        return Err(
                            LoxError::RuntimeError("cannot continue from outside a loop".into())
                        );
                    };

                    self.frame_mut().pos -= offset;
                    continue;
                },
                OpCode::DefineIterator => {
                    let to_iterate = self.pop_stack()?;
                    let value_iter = ValueIter::new(to_iterate)?;

                    self.stack_push_value(
                        Value::Object(
                            Object::Iterator(
                                Box::new(value_iter)
                            )
                        )
                    );
                },
                OpCode::IteratorNext(index, jump) => {
                    let iterator_ptr = self.pop_stack()?;
                    let Value::Object(Object::Iterator(iterator)) = &mut *iterator_ptr.borrow_mut() else {
                        unreachable!();
                    };

                    let constant = self.function().chunk.constants.get(index).clone();
                    self.stack.push(constant);

                    if iterator.next == iterator.items.len() {
                        self.frame_mut().pos += jump;
                    } else {
                        let index = index + self.frame().stack_offset;
                        self.stack[index] = iterator.items[iterator.next].clone();
                        iterator.next += 1;
                    }

                    self.stack.push(iterator_ptr.clone());
                },
                OpCode::BuildList(item_count) => {
                    let range_start = self.stack.len() - item_count;
                    let list: Vec<ValuePtr> = self.stack[range_start..].to_vec();

                    self.stack.truncate(range_start);
                    self.stack_push_value(
                        Value::Object(
                            Object::List(
                                Box::new(list)
                            )
                        )
                    );
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
                    self.stack_push_value(
                        Value::Object(
                            Object::Map(
                                Box::new(
                                    ValueMap {
                                        map,
                                    }
                                )
                            )
                        )
                    );
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
                OpCode::CloseUpValue => {
                    let Some(value) = self.stack.last() else {
                        return Err(
                            LoxError::RuntimeError("can't close an upvalue without a stack!".into())
                        );
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

                    self.stack_push_value(
                        Value::Object(
                            Object::Class(
                                Box::new(
                                    Class::new(class_name)
                                )
                            )
                        )
                    );
                },
                OpCode::Method(index) => {
                    self.define_method(self.read_string(index)?)?;
                },
            };

            self.frame_mut().pos += 1;
        }

        Ok(())
    }

    fn read_string(&self, index: usize) -> Result<String, LoxError> {
        let name_constant = self.function().chunk.constants.get(index).clone();
        let Value::Object(Object::String(name)) = &*name_constant.borrow() else {
            return Err(LoxError::RuntimeError("tried to read a string and failed".into()));
        };

        Ok(*name.clone())
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
                        Value::Object(
                            Object::Instance(
                                Box::new(
                                    Instance::new(at_offset.clone())
                                )
                            )
                        )
                    );

                    if let Some(init_ptr) = class.methods.get(&self.init_string) {
                        let Value::Object(Object::Closure(init)) = &*init_ptr.borrow() else {
                            unreachable!();
                        };

                        self.call(init, arg_count)?;
                    } else if arg_count != 0 {
                        return Err(
                            LoxError::RuntimeError(
                                format!("expected no arguments on initialization but got {}", arg_count)
                            )
                        );
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

    fn bind_method(&mut self, class_ptr: ValuePtr, name: &String) -> Result<(), LoxError> {
        let Value::Object(Object::Class(class)) = &*class_ptr.borrow() else {
            unreachable!();
        };

        match class.methods.get(name) {
            Some(method) => {
                let bound_method = Value::Object(
                    Object::BoundMethod(
                        Box::new(
                            BoundMethod {
                                receiver: self.stack.last().unwrap().clone(),
                                closure: method.clone(),
                                obj: None,
                            }
                        )
                    )
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

    fn define_method(&mut self, name: String) -> Result<(), LoxError> {
        let method = self.stack.last().unwrap();
        let class_ptr = self.stack[self.stack.len() - 2].clone();
        let Value::Object(Object::Class(class)) = &mut *class_ptr.borrow_mut() else {
            return Err(LoxError::RuntimeError("no class on stack".into()));
        };

        class.methods.insert(name, method.clone());
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

    fn values_equal(&self, left: &Value, right: &Value) -> Result<bool, LoxError> {
        match (left, right) {
            (Value::Bool(left), Value::Bool(right)) => Ok(left == right),
            (Value::Nil, Value::Nil) => Ok(true),
            (Value::Number(left), Value::Number(right)) => Ok(left == right),
            (Value::Object(left), Value::Object(right)) => match (left, right) {
                (Object::List(left), Object::List(right)) => Ok(left == right),
                (Object::Map(left), Object::Map(right)) => Ok(left == right),
                (Object::Function(left), Object::Function(right)) => Ok(left.name == right.name),
                (Object::String(left), Object::String(right)) => Ok(left == right),
                _ => Ok(false),
            },
            _ => Ok(false),
        }
    }
}
