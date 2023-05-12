use std::{cell::RefCell, collections::HashMap, fs, rc::Rc};

use crate::{
    builtin,
    chunk::OpCode,
    compiler::Compiler,
    errors::{LoxError, LoxResult},
    object::{
        BoundMethod, Class, Closure, Instance, Module, NativeFn, NativeFunction, Object, UpValue,
        UpValuePtr, ValueIter, ValueMap,
    },
    parser::Parser,
    scanner::Scanner,
    value::{Value, ValuePtr},
};

#[cfg(feature = "debug")]
use crate::chunk::Chunk;

#[cfg(feature = "debug")]
use crate::object::Function;

static FRAMES_MAX: usize = 64;
static STACK_MAX: usize = 256;

pub struct CallFrame {
    closure: ValuePtr,
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
    };
}

macro_rules! err {
    ( $msg:expr ) => {
        return Err(LoxError::RuntimeError($msg.into()))
    };
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

    fn build_native(&self, name: &str, function: NativeFn, arity: usize) -> ValuePtr {
        ValuePtr::new(obj!(
            Native,
            NativeFunction {
                function,
                obj: None,
                name: String::from(name),
                arity,
            }
        ))
    }

    fn define_native(&mut self, name: &str, function: NativeFn, arity: usize) {
        let function = self.build_native(name, function, arity);
        self.globals.insert(String::from(name), function);
    }

    pub fn interpret(&mut self, input: &str) -> LoxResult<()> {
        let mut scanner = Scanner::new(input.into());
        let (tokens, errors) = scanner.scan_tokens();
        let mut parser = Parser::new(tokens);
        let parse_output = parser.parse();

        match parse_output {
            Ok(output) => {
                let mut compiler = Compiler::new();

                let function = compiler.compile(output)?;

                let closure = ValuePtr::new(obj!(
                    Closure,
                    Closure {
                        upvalues: Vec::with_capacity(STACK_MAX),
                        function,
                        obj: None,
                    }
                ));

                self.stack.push(closure.clone());
                self.call(closure, 0)?;

                self.define_native("len", builtin::_len, 1);
                self.define_native("range", builtin::_range, 2);
                self.define_native("sqrt", builtin::_sqrt, 1);
                self.define_native("str", builtin::_str, 1);
                self.define_native("time", builtin::_time, 0);
                self.define_native("type", builtin::_type, 1);

                self.globals.insert("colors".into(), self.build_colors());

                self.run()?;
            },
            Err(e) => {
                println!("parse errors:");
                println!(" - {}", e);
            },
        };

        if !errors.is_empty() {
            println!("syntax errors:");
            for error in errors.iter() {
                println!(" - {}", error);
            }
        }

        Ok(())
    }

    fn build_colors(&self) -> ValuePtr {
        macro_rules! color_fn {
            ( $name:expr, $symbol:tt ) => {
                ($name.into(), self.build_native($name, builtin::$symbol, 1))
            };
        }

        let color_globals = HashMap::from_iter(
            vec![
                color_fn! { "black", _black },
                color_fn! { "red", _red },
                color_fn! { "green", _green },
                color_fn! { "yellow", _yellow },
                color_fn! { "blue", _blue },
                color_fn! { "magenta", _magenta },
                color_fn! { "cyan", _cyan },
                color_fn! { "white", _white },
            ]
            .into_iter(),
        );

        ValuePtr::new(obj!(
            Module,
            Module {
                name: "colors".into(),
                map: color_globals,
            }
        ))
    }

    #[cfg(feature = "debug")]
    fn dump(&self, chunk: &Chunk, idx: usize) {
        let stack_str: Vec<String> = self
            .stack
            .iter()
            .map(|v| format!("{}", (*v.borrow())))
            .collect();

        println!("          [{}]", stack_str.join(", "));
        chunk.disassemble_instruction(&chunk.code[idx], idx);
    }

    fn pop_stack(&mut self) -> LoxResult<ValuePtr> {
        self.stack
            .pop()
            .ok_or_else(|| LoxError::RuntimeError("stack empty".into()))
    }

    fn frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    #[cfg(feature = "debug")]
    fn function(&self) -> Function {
        match &*self.frame().closure.borrow() {
            Value::Object(Object::Closure(closure)) => closure.function.clone(),
            _ => unreachable!(),
        }
    }

    fn stack_push_value(&mut self, value: Value) {
        self.stack.push(ValuePtr::new(value));
    }

    fn _convert_index(&self, len: usize, number: i32) -> usize {
        self._extract_index(len, number as f64)
    }

    fn _get_slice_argument(&mut self, is_present: bool) -> LoxResult<Option<i32>> {
        if !is_present {
            return Ok(None);
        }

        let arg: Option<i32> = match &*self.pop_stack()?.borrow() {
            Value::Number(number) => Some(*number as i32),
            _ => err!("only numbers can be used to slice containers"),
        };

        Ok(arg)
    }

    fn _get_slice_range(
        &self,
        len: usize,
        left: Option<i32>,
        right: Option<i32>,
    ) -> (usize, usize) {
        let left = left.map_or_else(|| 0, |left| self._convert_index(len, left));
        let right = right.map_or_else(|| len, |right| self._convert_index(len, right));

        (left, right)
    }

    fn _extract_index(&self, len: usize, number: f64) -> usize {
        let index = if number < 0.0f64 {
            (len as f64 + number) as usize
        } else {
            number as usize
        };

        index.min(len)
    }

    pub fn run(&mut self) -> LoxResult<()> {
        macro_rules! binary {
            ( $op:tt, $op_char:expr, $output:expr ) => {
                {
                    let right = self.pop_stack()?;
                    let left = self.stack.last_mut().unwrap();

                    let result = match (&*right.borrow(), &*left.borrow()) {
                        (Value::Number(b), Value::Number(a)) => a $op b,
                        _ => err!(&format!("comparison '{}' only operates on numbers", $op_char)),
                    };

                    *left = ValuePtr::new($output(result));
                }
            };
        }

        macro_rules! global {
            ( $pos:expr ) => {{
                let Value::Object(Object::Closure(closure)) = &*self.frame().closure.borrow() else {
                                                                                unreachable!();
                                                                            };

                closure.function.chunk.constants.get($pos).clone()
            }};
        }

        macro_rules! truthy {
            ( $item:expr ) => {{
                match &*$item.borrow() {
                    Value::Bool(boolean) => Ok(*boolean),
                    Value::Nil => Ok(false),
                    Value::Number(number) => Ok(*number != 0.0f64),
                    Value::Object(object) => match object {
                        Object::List(list) => Ok(!list.is_empty()),
                        Object::Map(hmap) => Ok(!hmap.map.is_empty()),
                        Object::String(string) => Ok(!string.is_empty()),
                        _ => Ok(true),
                    },
                }
            }};
        }

        loop {
            let code = {
                let frame = self.frames.last().unwrap();

                let Value::Object(Object::Closure(closure)) = &*frame.closure.borrow() else {
                    unreachable!();
                };

                if frame.pos >= closure.function.chunk.code.len() {
                    break;
                }

                #[cfg(feature = "debug")]
                self.dump(&self.function().chunk, frame.pos);

                closure.function.chunk.code[frame.pos].clone()
            };

            match code {
                OpCode::Constant(index) => self.stack.push(global!(index)),
                OpCode::False => self.stack_push_value(Value::Bool(false)),
                OpCode::True => self.stack_push_value(Value::Bool(true)),
                OpCode::Nil => self.stack_push_value(Value::Nil),
                OpCode::Pop => {
                    self.pop_stack()?;
                },
                OpCode::GetLocal(index) => {
                    let index = index + self.frame().stack_offset;
                    self.stack.push(self.stack[index].clone());
                },
                OpCode::SetLocal(index) => {
                    let val = self.stack.last().unwrap().clone();
                    let index = index + self.frame().stack_offset;
                    self.stack[index] = val;
                },
                OpCode::GetGlobal(index) => {
                    let val = match &*global!(index).borrow() {
                        Value::Object(Object::String(string)) => {
                            self.globals.get(&**string).ok_or_else(|| {
                                LoxError::RuntimeError(format!("undefined variable: {}", &**string))
                            })?
                        },
                        _ => unreachable!(),
                    };

                    self.stack.push(val.clone());
                },
                OpCode::DefineGlobal(index) => {
                    let key = match &*global!(index).borrow() {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => unreachable!(),
                    };

                    let val = self.pop_stack()?;

                    self.globals.insert(key, val);
                },
                OpCode::SetGlobal(index) => {
                    let key = match &*global!(index).borrow() {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => unreachable!(),
                    };

                    if !self.globals.contains_key(&key) {
                        err!(&format!("undefined variable: {key}"))
                    }

                    let val = self.stack.last().unwrap().clone();

                    self.globals.insert(key, val);
                },
                OpCode::GetUpValue(index) => {
                    let upvalue = match &*self.frame().closure.borrow() {
                        Value::Object(Object::Closure(closure)) => {
                            closure.upvalues[index - 1].borrow().location.clone()
                        },
                        _ => unreachable!(),
                    };

                    self.stack.push(upvalue);
                },
                OpCode::SetUpValue(index) => {
                    let last_stack = self.stack.last().unwrap().clone();
                    match &*self.frame_mut().closure.borrow_mut() {
                        Value::Object(Object::Closure(closure)) => {
                            closure.upvalues[index - 1].borrow_mut().location = last_stack;
                        },
                        _ => unreachable!(),
                    }
                },
                OpCode::GetProperty(index) => {
                    let name_constant = global!(index);
                    let Value::Object(Object::String(prop_name)) = &*name_constant.borrow() else {
                        unreachable!();
                    };

                    let last = self.stack.last().unwrap().clone();
                    match &*last.borrow() {
                        Value::Object(Object::Instance(instance)) => {
                            match instance.fields.get(&**prop_name) {
                                Some(prop) => {
                                    *self.stack.last_mut().unwrap() = prop.clone();
                                },
                                None => self.bind_method(&instance.class, prop_name)?,
                            };
                        },
                        Value::Object(Object::Module(module)) => {
                            match module.map.get(&**prop_name) {
                                Some(prop) => {
                                    *self.stack.last_mut().unwrap() = prop.clone();
                                },
                                None => err!(&format!(
                                    "module '{}' has no property '{}'",
                                    module.name, prop_name
                                )),
                            };
                        },
                        _ => err!("only modules and instances have properties"),
                    };
                },
                OpCode::SetProperty(index) => {
                    let instance_ptr = self.stack[self.stack.len() - 2].clone();
                    let Value::Object(Object::Instance(instance)) = &mut *instance_ptr.borrow_mut() else {
                        err!("not an instance");
                    };
                    let name_constant = global!(index);
                    let Value::Object(Object::String(prop_name)) = &*name_constant.borrow() else {
                        unreachable!();
                    };

                    let value = self.pop_stack()?;

                    instance.fields.insert(*prop_name.clone(), value.clone());
                    *self.stack.last_mut().unwrap() = value;
                },
                OpCode::GetSuper(index) => {
                    let instance_ptr = self.pop_stack()?;
                    let Value::Object(Object::Instance(instance)) = &*instance_ptr.borrow() else {
                        err!("not an instance");
                    };
                    let name_constant = global!(index);
                    let Value::Object(Object::String(prop_name)) = &*name_constant.borrow() else {
                        unreachable!();
                    };

                    let Value::Object(Object::Class(class)) = &*instance.class.borrow() else {
                        unreachable!();
                    };

                    let Some(super_class) = &class.parent else {
                        err!("can't use super. on a base class!");
                    };

                    self.bind_method(super_class, prop_name)?;
                },
                OpCode::GetIndex => {
                    let index = self.pop_stack()?;
                    let container = self.pop_stack()?;

                    let value = match &*container.borrow() {
                        Value::Object(Object::List(list)) => match &*index.borrow() {
                            Value::Number(number) => {
                                let index_usize = self._extract_index(list.len(), *number);
                                list[index_usize].clone()
                            },
                            _ => err!("lists can only be indexed with integers"),
                        },
                        Value::Object(Object::String(string)) => match &*index.borrow() {
                            Value::Number(number) => {
                                let index_usize = self._extract_index(string.len(), *number);

                                ValuePtr::new(obj!(
                                    String,
                                    string.chars().nth(index_usize).unwrap().to_string()
                                ))
                            },
                            _ => err!("strings can only be indexed with integers"),
                        },
                        Value::Object(Object::Map(hmap)) => match hmap.map.get(&index) {
                            Some(value) => value.clone(),
                            None => err!(&format!("no entry for key {index:?}")),
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
                                let index_usize = self._extract_index(list.len(), *number);
                                list[index_usize] = value.clone();
                            },
                            _ => err!("lists can only be indexed with integers"),
                        },
                        Value::Object(Object::Map(hmap)) => {
                            hmap.map.insert(index, value.clone());
                        },
                        _ => err!("only lists and maps can be indexed into"),
                    };

                    self.stack.push(value);
                },
                OpCode::DeleteIndex => {
                    let index = self.pop_stack()?;
                    let container = self.pop_stack()?;

                    match &mut *container.borrow_mut() {
                        Value::Object(Object::List(list)) => match &*index.borrow() {
                            Value::Number(number) => {
                                let index_usize = self._extract_index(list.len(), *number);
                                list.remove(index_usize);
                            },
                            _ => err!("lists can only be indexed with integers"),
                        },
                        Value::Object(Object::Map(hmap)) => {
                            hmap.map.remove(&index);
                        },
                        _ => err!("only lists and maps can be indexed into"),
                    };
                },
                OpCode::GetSlice(has_left, has_right) => {
                    let right = self._get_slice_argument(has_right)?;
                    let left = self._get_slice_argument(has_left)?;

                    let value = match &*self.pop_stack()?.borrow() {
                        Value::Object(Object::List(list)) => {
                            let (left, right) = self._get_slice_range(list.len(), left, right);
                            let tmp_value = if left > right {
                                vec![]
                            } else {
                                list[left..right].to_vec()
                            };

                            obj!(List, tmp_value)
                        },
                        Value::Object(Object::String(string)) => {
                            let (left, right) = self._get_slice_range(string.len(), left, right);
                            let tmp_value = if left > right {
                                "".to_string()
                            } else {
                                string[left..right].to_string()
                            };

                            obj!(String, tmp_value)
                        },
                        _ => err!("only lists and strings can be sliced"),
                    };

                    self.stack_push_value(value);
                },
                OpCode::Equal => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    self.stack_push_value(Value::Bool(*right.borrow() == *left.borrow()));
                },
                OpCode::Greater => binary! { >, '>', Value::Bool },
                OpCode::Less => binary! { <, '<', Value::Bool },
                OpCode::In => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    let right = right.borrow();

                    match &*right {
                        Value::Object(Object::List(list)) => {
                            self.stack_push_value(Value::Bool(list.contains(&left)));
                        },
                        Value::Object(Object::Map(hmap)) => {
                            self.stack_push_value(Value::Bool(hmap.map.contains_key(&left)));
                        },
                        Value::Object(Object::String(string)) => match &*left.borrow() {
                            Value::Object(Object::String(substring)) => {
                                self.stack_push_value(Value::Bool(string.contains(&**substring)));
                            },
                            _ => err!("invalid 'in' check: strings can only contain other strings"),
                        },
                        _ => err!("'in' operator only functions on iterables"),
                    };
                },
                OpCode::Add => {
                    let right = self.pop_stack()?;
                    let left = self.stack.last_mut().unwrap();

                    let output = match (&*right.borrow(), &*left.borrow()) {
                        (Value::Object(Object::List(b)), Value::Object(Object::List(a))) => {
                            let mut new_list: Vec<ValuePtr> = *a.clone();
                            new_list.extend_from_slice(b);

                            obj!(List, new_list)
                        },
                        (Value::Number(b), Value::Number(a)) => Value::Number(a + b),
                        (Value::Number(b), Value::Object(Object::String(a))) => {
                            obj!(String, format!("{a}{b}"))
                        },
                        (Value::Object(Object::String(b)), Value::Number(a)) => {
                            obj!(String, format!("{a}{b}"))
                        },
                        (Value::Object(Object::String(b)), Value::Object(Object::String(a))) => {
                            obj!(String, format!("{a}{b}"))
                        },
                        _ => err!("operation '+' only operates on numbers and strings"),
                    };

                    *left = ValuePtr::new(output);
                },
                OpCode::Subtract => binary! { -, '-', Value::Number },
                OpCode::Modulo => binary! { %, '%', Value::Number },
                OpCode::Multiply => binary! { *, '*', Value::Number },
                OpCode::Divide => binary! { /, '/', Value::Number },
                OpCode::Not => {
                    let item = self.pop_stack()?;
                    self.stack_push_value(Value::Bool(!truthy!(&item)?));
                },
                OpCode::Negate => {
                    let item = match *(self.pop_stack()?).borrow() {
                        Value::Number(number) => number,
                        _ => err!("negation only operates on numbers"),
                    };
                    self.stack_push_value(Value::Number(-item));
                },
                OpCode::Print(newline) => {
                    let value = self.pop_stack()?;

                    if newline {
                        println!("{}", *value.borrow())
                    } else {
                        print!("{}", *value.borrow())
                    }
                },
                OpCode::Jump(offset) => self.frame_mut().pos += offset,
                OpCode::JumpIfFalse(offset) => {
                    if !truthy!(self.stack.last().unwrap())? {
                        self.frame_mut().pos += offset;
                    }
                },
                OpCode::Loop(offset) => self.frame_mut().pos -= offset,
                OpCode::Break(_) => unreachable!(),
                OpCode::Continue(_) => unreachable!(),
                OpCode::DefineIterator => {
                    let to_iterate = self.pop_stack()?;
                    let value_iter = ValueIter::new(to_iterate)?;

                    self.stack_push_value(obj!(Iterator, value_iter));
                },
                OpCode::IteratorNext(index, jump) => {
                    let stack_index = index + self.frame().stack_offset;
                    let iterator_ptr = self.stack[stack_index - 1].clone();
                    let Value::Object(Object::Iterator(iterator)) = &mut *iterator_ptr.borrow_mut() else {
                        unreachable!();
                    };

                    if iterator.next == 0 {
                        let constant = global!(index);
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
                    let list: Vec<ValuePtr> = self.stack.split_off(range_start);

                    self.stack_push_value(obj!(List, list));
                },
                OpCode::BuildMap(item_count) => {
                    let range_start = self.stack.len() - item_count;
                    let mut items = self.stack.split_off(range_start).into_iter();

                    let map: HashMap<ValuePtr, ValuePtr> = HashMap::from_iter(
                        (0..(item_count / 2))
                            .map(|_| (items.next().unwrap(), items.next().unwrap())),
                    );

                    self.stack_push_value(obj!(Map, ValueMap { map }));
                },
                OpCode::Call(arg_count) => {
                    self.call_value(arg_count)?;
                    continue;
                },
                OpCode::Invoke(index, arg_count) => {
                    let name_constant = global!(index);
                    let Value::Object(Object::String(method_name)) = &*name_constant.borrow() else {
                        unreachable!();
                    };

                    self.invoke(method_name, arg_count)?;
                    continue;
                },
                OpCode::Closure(index, ref upvalues) => {
                    let constant = global!(index);
                    let function = match &*constant.borrow() {
                        Value::Object(Object::Function(function)) => *function.clone(),
                        _ => unreachable!(),
                    };

                    let mut captured: Vec<UpValuePtr> = Vec::with_capacity(upvalues.len());
                    for uv in upvalues.iter() {
                        let new_upvalue = if uv.is_local {
                            self.capture_upvalue(uv.index)?
                        } else {
                            match &*self.frame().closure.borrow() {
                                Value::Object(Object::Closure(closure)) => {
                                    closure.upvalues[index - 1].clone()
                                },
                                _ => unreachable!(),
                            }
                        };

                        captured.push(new_upvalue);
                    }

                    self.stack_push_value(obj!(
                        Closure,
                        Closure {
                            obj: None,
                            upvalues: captured,
                            function,
                        }
                    ));
                },
                OpCode::CloseUpValue => {
                    if self.stack.is_empty() {
                        err!("can't close an upvalue without a stack!");
                    };

                    let value = self.pop_stack()?;
                    self.close_upvalues(value);
                },
                OpCode::Assert(has_message) => {
                    let message_ptr = has_message.then(|| self.pop_stack().ok()).flatten();
                    let assertion_ptr = self.pop_stack()?;

                    if !truthy!(&assertion_ptr)? {
                        let message = if has_message {
                            match &*message_ptr.unwrap().borrow() {
                                Value::Object(Object::String(message)) => format!("{message}"),
                                _ => err!("assert message must be a string"),
                            }
                        } else {
                            format!("{} is not true", assertion_ptr.borrow())
                        };

                        err!(&format!("assertion failed: {message}"))
                    }
                },
                OpCode::Import(ref path, index) => {
                    let path = *path.clone();
                    let source = fs::read_to_string(&path).ok().ok_or_else(|| {
                        LoxError::RuntimeError(format!("unable to import file '{path}'"))
                    })?;

                    let mut vm = VirtualMachine::new();
                    vm.interpret(&source)?;

                    let key = match &*global!(index).borrow() {
                        Value::Object(Object::String(string)) => *string.clone(),
                        _ => unreachable!(),
                    };

                    for (key, val) in vm.globals.clone() {
                        self.globals.insert(key, val);
                    }

                    self.globals.insert(
                        key.clone(),
                        ValuePtr::new(obj!(
                            Module,
                            Module {
                                name: key,
                                map: vm.globals,
                            }
                        )),
                    );
                },
                OpCode::Return => {
                    let result = self.pop_stack()?;

                    let old_frame = self.frames.pop().unwrap();

                    if self.frames.is_empty() {
                        self.pop_stack()?;
                        return Ok(());
                    }

                    self.stack.truncate(old_frame.stack_offset + 1);
                    *self.stack.last_mut().unwrap() = result;
                },
                OpCode::Class(index) => {
                    let class_name = self.read_string(index)?;

                    self.stack_push_value(obj!(Class, Class::new(class_name)));
                },
                OpCode::Inherit => {
                    let sub_class_ptr = self.pop_stack()?;
                    let super_class_ptr = self.stack.last_mut().unwrap();

                    let methods = match &*super_class_ptr.borrow() {
                        Value::Object(Object::Class(super_class)) => super_class.methods.clone(),
                        _ => err!("super class must be a class"),
                    };

                    match &mut *sub_class_ptr.borrow_mut() {
                        Value::Object(Object::Class(sub_class)) => {
                            sub_class.parent = Some(super_class_ptr.clone());
                            sub_class.methods = methods;
                        },
                        _ => unreachable!(),
                    };
                },
                OpCode::Method(index) => {
                    self.define_method(&self.read_string(index)?)?;
                },
            };

            self.frame_mut().pos += 1;
        }

        Ok(())
    }

    fn read_string(&self, index: usize) -> LoxResult<String> {
        match &*self.frame().closure.borrow() {
            Value::Object(Object::Closure(closure)) => {
                match &*closure.function.chunk.constants.get(index).borrow() {
                    Value::Object(Object::String(name)) => Ok(*name.clone()),
                    _ => err!("tried to read a string and failed"),
                }
            },
            _ => unreachable!(),
        }
    }

    fn check_arity(&self, name: &str, callee_arity: usize, arg_count: usize) -> LoxResult<()> {
        if callee_arity != arg_count {
            err!(&format!(
                "{name}: expected {callee_arity} arguments but {arg_count} were given",
            ));
        }

        Ok(())
    }

    fn call(&mut self, callee: ValuePtr, arg_count: usize) -> LoxResult<()> {
        let frame = CallFrame {
            closure: callee,
            stack_offset: self.stack.len() - arg_count - 1,
            pos: 0,
        };

        self.frames.push(frame);

        Ok(())
    }

    fn call_value(&mut self, arg_count: usize) -> LoxResult<()> {
        let offset = self.stack.len() - arg_count - 1;
        let at_offset = self.stack[offset].clone();

        match &*at_offset.borrow() {
            Value::Object(object) => match object {
                Object::Closure(closure) => {
                    self.check_arity(&closure.function.name, closure.function.arity, arg_count)?;
                    self.call(at_offset.clone(), arg_count)?;
                },
                Object::BoundMethod(bound_method) => {
                    let arg_range_start = self.stack.len() - arg_count;
                    self.stack[arg_range_start - 1] = bound_method.receiver.clone();

                    self.call(bound_method.closure.clone(), arg_count)?;
                },
                Object::Class(class) => {
                    let position = self.stack.len() - arg_count - 1;
                    self.stack[position] =
                        ValuePtr::new(obj!(Instance, Instance::new(at_offset.clone())));

                    if let Some(init_ptr) = class.methods.get(self.init_string) {
                        self.call(init_ptr.clone(), arg_count)?;
                    } else if arg_count != 0 {
                        err!(&format!(
                            "expected no arguments on initialization but got {arg_count}",
                        ));
                    } else {
                        self.frame_mut().pos += 1;
                    }
                },
                Object::Native(native) => {
                    self.check_arity(&native.name, native.arity, arg_count)?;

                    let arg_range_start = self.stack.len() - arg_count;
                    let result = (native.function)(&self.stack.split_off(arg_range_start))?;

                    self.pop_stack()?;
                    self.stack_push_value(result);
                    self.frame_mut().pos += 1;
                },
                _ => err!("can only call functions and classes"),
            },
            _ => unreachable!(),
        };

        Ok(())
    }

    fn invoke(&mut self, method_name: &str, arg_count: usize) -> LoxResult<()> {
        let offset = self.stack.len() - arg_count - 1;
        let at_offset = self.stack[offset].clone();
        let mut at_offset_borrowed = at_offset.borrow_mut();

        match &mut *at_offset_borrowed {
            Value::Object(Object::Instance(instance)) => match instance.fields.get(method_name) {
                Some(prop) => {
                    self.stack[offset] = prop.clone();
                    self.call_value(arg_count)
                },
                None => {
                    let Value::Object(Object::Class(class)) = &*instance.class.borrow() else {
                        unreachable!();
                    };

                    match class.methods.get(method_name) {
                        Some(prop) => self.call(prop.clone(), arg_count),
                        None => err!(&format!(
                            "no method '{}' on class '{}'",
                            method_name, class.name
                        )),
                    }
                },
            },
            Value::Object(Object::Module(module)) => match module.map.get(method_name) {
                Some(prop) => {
                    self.stack[offset] = prop.clone();
                    self.call_value(arg_count)
                },
                None => err!(&format!(
                    "module '{}' has no property '{}'",
                    module.name, method_name
                )),
            },
            Value::Object(Object::Map(hmap)) => {
                match (method_name, arg_count) {
                    ("clear", 0) => hmap.map.clear(),
                    ("keys", 0) => {
                        self.stack[offset] =
                            ValuePtr::new(obj!(List, hmap.map.keys().cloned().collect()));
                    },
                    ("values", 0) => {
                        self.stack[offset] =
                            ValuePtr::new(obj!(List, hmap.map.values().cloned().collect()));
                    },
                    _ => err!(&format!(
                        "map does not have the method '{method_name}' with {arg_count} argument(s)",
                    )),
                };

                self.frame_mut().pos += 1;
                Ok(())
            },
            Value::Object(Object::List(list)) => {
                match (method_name, arg_count) {
                    ("clear", 0) => list.clear(),
                    _ => err!(&format!(
                        "list does not have the method '{method_name}' with {arg_count} argument(s)",
                    )),
                };

                self.frame_mut().pos += 1;
                Ok(())
            },
            Value::Object(Object::String(string)) => {
                match (method_name, arg_count) {
                    ("clear", 0) => string.clear(),
                    _ => err!(&format!(
                        "string does not have the method '{method_name}' with {arg_count} argument(s)",
                    )),
                };

                self.frame_mut().pos += 1;
                Ok(())
            },
            _ => err!("only instances have methods"),
        }
    }

    fn bind_method(&mut self, class_ptr: &ValuePtr, name: &str) -> LoxResult<()> {
        let Value::Object(Object::Class(class)) = &*class_ptr.borrow() else {
            unreachable!();
        };

        match class.methods.get(name) {
            Some(method) => {
                let receiver = self.pop_stack()?;

                let bound_method = obj!(
                    BoundMethod,
                    BoundMethod {
                        receiver,
                        closure: method.clone(),
                        obj: None,
                    }
                );

                self.stack_push_value(bound_method);

                Ok(())
            },
            None => Err(LoxError::RuntimeError(format!(
                "'{name}' not a valid property",
            ))),
        }
    }

    fn capture_upvalue(&mut self, upvalue_index: usize) -> LoxResult<UpValuePtr> {
        let item = &self.stack[self.frame().stack_offset + upvalue_index];

        for (idx, upvalue) in self.upvalues.iter().enumerate() {
            if idx == upvalue_index - 1 {
                return Ok(upvalue.clone());
            }
        }

        let new_upvalue = Rc::new(RefCell::new(UpValue {
            location: item.clone(),
            location_index: upvalue_index,
            obj: None,
        }));

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

    fn define_method(&mut self, name: &str) -> LoxResult<()> {
        let method = self.pop_stack()?;
        let class_ptr = self.stack[self.stack.len() - 1].clone();
        let Value::Object(Object::Class(class)) = &mut *class_ptr.borrow_mut() else {
            return Err(LoxError::RuntimeError("no class on stack".into()));
        };

        class.methods.insert(String::from(name), method);

        Ok(())
    }
}
