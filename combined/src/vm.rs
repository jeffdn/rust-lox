use core::hash::BuildHasherDefault;
use std::fs;

use rustc_hash::{FxHashMap as HashMap, FxHasher};

type LoxBuildHasher = BuildHasherDefault<FxHasher>;

use crate::{
    builtin,
    chunk::OpCode,
    compiler::Compiler,
    errors::{LoxError, LoxResult},
    gc::{Heap, ObjPtr, Trace},
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
    frame: *mut CallFrame,
    stack: Vec<ValuePtr>,
    globals: HashMap<String, ValuePtr>,
    upvalues: Vec<UpValuePtr>,
    heap: Heap,

    init_string: &'static str,
}

impl Default for VirtualMachine {
    fn default() -> Self {
        Self::new()
    }
}

macro_rules! obj {
    ( $self:expr, $ot:tt, $val:expr ) => {
        Value::Obj($self.allocate(Object::$ot(Box::new($val))))
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
            frame: std::ptr::null_mut(),
            stack: Vec::with_capacity(STACK_MAX),
            globals: HashMap::with_capacity_and_hasher(STACK_MAX, LoxBuildHasher::default()),
            upvalues: Vec::with_capacity(STACK_MAX),
            heap: Heap::new(),
            init_string: "init",
        }
    }

    pub fn allocate(&mut self, object: Object) -> ObjPtr {
        if self.heap.bytes_allocated > self.heap.next_gc {
            self.collect_garbage();
        }
        self.heap.allocate(object)
    }

    pub fn collect_garbage(&mut self) {
        self.mark_roots();
        self.heap.trace_references();
        self.heap.sweep();

        // Adjust next_gc
        self.heap.next_gc = self.heap.bytes_allocated * 2;
    }

    fn mark_roots(&mut self) {
        // Stack
        for value in &self.stack {
            value.trace(&mut self.heap);
        }

        // Globals
        for value in self.globals.values() {
            value.trace(&mut self.heap);
        }

        // Upvalues
        for upvalue in &self.upvalues {
            self.heap.mark_object(*upvalue);
        }

        // Frames (closures)
        for frame in &self.frames {
            frame.closure.trace(&mut self.heap);
        }

        // Current frame
        if !self.frame.is_null() {
            unsafe {
                (*self.frame).closure.trace(&mut self.heap);
            }
        }
    }

    fn build_native(&mut self, name: &str, function: NativeFn, arity: usize) -> ValuePtr {
        Value::new(obj!(
            self,
            Native,
            NativeFunction {
                function,
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
                let mut compiler = Compiler::new(&mut self.heap);

                let function = compiler.compile(output)?;

                let closure = Value::new(obj!(
                    self,
                    Closure,
                    Closure {
                        upvalues: Vec::with_capacity(STACK_MAX),
                        function,
                    }
                ));

                self.stack.push(closure);
                self.call(closure, 0)?;

                self.define_native("len", builtin::_len, 1);
                self.define_native("pow", builtin::_pow, 2);
                self.define_native("range", builtin::_range, 2);
                self.define_native("sqrt", builtin::_sqrt, 1);
                self.define_native("str", builtin::_str, 1);
                self.define_native("time", builtin::_time, 0);
                self.define_native("type", builtin::_type, 1);

                let colors_module = self.build_colors();
                self.globals.insert("colors".into(), colors_module);

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

    fn build_colors(&mut self) -> ValuePtr {
        macro_rules! color_fn {
            ( $name:expr, $symbol:tt ) => {
                ($name.into(), self.build_native($name, builtin::$symbol, 1))
            };
        }

        let color_globals = HashMap::from_iter(vec![
            color_fn! { "black", _black },
            color_fn! { "red", _red },
            color_fn! { "green", _green },
            color_fn! { "yellow", _yellow },
            color_fn! { "blue", _blue },
            color_fn! { "magenta", _magenta },
            color_fn! { "cyan", _cyan },
            color_fn! { "white", _white },
        ]);

        Value::new(obj!(
            self,
            Module,
            Module {
                name: "colors".into(),
                map: color_globals,
            }
        ))
    }

    #[cfg(feature = "debug")]
    fn dump(&self, chunk: &Chunk, idx: usize) {
        let stack_str: Vec<String> = self.stack.iter().map(|v| format!("{}", v)).collect();

        println!("          [{}]", stack_str.join(", "));
        chunk.disassemble_instruction(&chunk.code[idx], idx);
    }

    fn pop_stack(&mut self) -> LoxResult<ValuePtr> {
        self.stack
            .pop()
            .ok_or_else(|| LoxError::RuntimeError("stack empty".into()))
    }

    #[inline(always)]
    fn frame(&self) -> &CallFrame {
        unsafe { &*self.frame }
    }

    #[inline(always)]
    fn frame_mut(&mut self) -> &mut CallFrame {
        unsafe { &mut *self.frame }
    }

    #[cfg(feature = "debug")]
    fn function(&self) -> Function {
        match &self.frame().closure {
            Value::Obj(closure_ptr) => match &closure_ptr.obj {
                Object::Closure(closure) => closure.function.clone(),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn stack_push_value(&mut self, value: Value) {
        self.stack.push(Value::new(value));
    }

    fn _convert_index(&self, len: usize, number: i32) -> usize {
        self._extract_index(len, number as f64)
    }

    fn _get_slice_argument(&mut self, is_present: bool) -> LoxResult<Option<i32>> {
        if !is_present {
            return Ok(None);
        }

        let arg: Option<i32> = match self.pop_stack()? {
            Value::Number(number) => Some(number as i32),
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

                    let result = match (&right, &*left) {
                        (Value::Number(b), Value::Number(a)) => a $op b,
                        _ => err!(&format!("comparison '{}' only operates on numbers", $op_char)),
                    };

                    *left = Value::new($output(result));
                }
            };
        }

        macro_rules! integer_binary {
            ( $op:tt, $op_char:expr, $output:expr ) => {
                {
                    let right = self.pop_stack()?;
                    let left = self.stack.last_mut().unwrap();

                    let result = match (&right, &*left) {
                        (Value::Number(b), Value::Number(a)) => (*a as i64) $op (*b as i64),
                        _ => err!(&format!("comparison '{}' only operates on numbers", $op_char)),
                    };

                    *left = Value::new($output(result as f64));
                }
            };
        }

        macro_rules! bit_ops {
            ( $op:tt, $op_char:expr ) => {
                {
                    let right = self.pop_stack()?;
                    let left = self.stack.last_mut().unwrap();

                    let result = match (&right, &*left) {
                        (Value::Number(b), Value::Number(a)) => Value::Number(((*a as i64) $op (*b as i64)) as f64),
                        (Value::Bool(b), Value::Bool(a)) => Value::Bool(*a $op *b),
                        _ => err!(&format!("comparison '{}' only operates on integers and bools", $op_char)),
                    };

                    *left = Value::new(result);
                }
            };
        }

        macro_rules! global {
            ( $pos:expr ) => {{
                let &Value::Obj(mut closure_ptr) = &self.frame().closure else {
                    unreachable!()
                };

                let Object::Closure(closure) = &mut closure_ptr.obj else {
                    unreachable!()
                };

                unsafe { &*closure.function.chunk.constants.as_ptr().add($pos) }
            }};
        }

        macro_rules! truthy {
            ( $item:expr ) => {{
                match $item {
                    Value::Bool(boolean) => Ok(*boolean),
                    Value::Nil => Ok(false),
                    Value::Number(number) => Ok(*number != 0.0f64),
                    Value::Obj(obj_ptr) => match &(*obj_ptr).obj {
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
                let frame = self.frame();

                let &Value::Obj(mut closure_ptr) = &frame.closure else {
                    unreachable!()
                };

                let Object::Closure(closure) = &mut closure_ptr.obj else {
                    unreachable!();
                };

                if frame.pos >= closure.function.chunk.code.len() {
                    break;
                }

                #[cfg(feature = "debug")]
                self.dump(&self.function().chunk, frame.pos);

                unsafe { &*closure.function.chunk.code.as_ptr().add(frame.pos) }
            };

            match code {
                OpCode::Constant(index) => self.stack.push(*global!(*index)),
                OpCode::False => self.stack_push_value(Value::Bool(false)),
                OpCode::True => self.stack_push_value(Value::Bool(true)),
                OpCode::Nil => self.stack_push_value(Value::Nil),
                OpCode::Pop => {
                    self.pop_stack()?;
                },
                OpCode::GetLocal(index) => {
                    let index = index + self.frame().stack_offset;
                    self.stack.push(self.stack[index]);
                },
                OpCode::SetLocal(index) => {
                    let val = *self.stack.last().unwrap();
                    let index = index + self.frame().stack_offset;
                    self.stack[index] = val;
                },
                OpCode::DefineGlobal(index) => {
                    let key = match global!(*index) {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::String(string) => *string.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                    let val = self.pop_stack()?;
                    self.globals.insert(key, val);
                },
                OpCode::GetGlobal(index) => {
                    let key = match global!(*index) {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::String(string) => *string.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    match self.globals.get(&key) {
                        Some(val) => self.stack_push_value(*val),
                        None => err!(&format!("undefined variable: {key}")),
                    }
                },
                OpCode::SetGlobal(index) => {
                    let key = match global!(*index) {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::String(string) => *string.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    if !self.globals.contains_key(&key) {
                        err!(&format!("undefined variable: {key}"))
                    }

                    let val = *self.stack.last().unwrap();

                    self.globals.insert(key, val);
                },
                OpCode::GetUpValue(index) => {
                    let upvalue_ptr = match &self.frame().closure {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::Closure(closure) => closure.upvalues[*index],
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let val = {
                        let uv = &*upvalue_ptr;
                        let upvalue = uv.obj.as_upvalue().unwrap();
                        if upvalue.location_index != usize::MAX {
                            self.stack[upvalue.location_index]
                        } else {
                            upvalue.location
                        }
                    };
                    self.stack_push_value(val);
                },
                OpCode::SetUpValue(index) => {
                    let val = *self.stack.last().unwrap();
                    let mut upvalue_ptr = match &self.frame().closure {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::Closure(closure) => closure.upvalues[*index],
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let uv = &mut *upvalue_ptr;
                    let upvalue = match &mut uv.obj {
                        Object::UpValue(uv) => uv,
                        _ => unreachable!(),
                    };
                    if upvalue.location_index != usize::MAX {
                        self.stack[upvalue.location_index] = val;
                    } else {
                        upvalue.location = val;
                    }
                },
                OpCode::GetProperty(index) => {
                    let instance = *self.stack.last().unwrap();
                    let name_constant = global!(*index);
                    let prop_name = match name_constant {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::String(prop_name) => prop_name.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    match instance {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::Instance(instance) => {
                                match instance.fields.get(&*prop_name) {
                                    Some(value) => {
                                        let value = *value;
                                        self.pop_stack()?;
                                        self.stack_push_value(value);
                                    },
                                    None => {
                                        let _class = match &instance.class {
                                            Value::Obj(class_ptr) => match &class_ptr.obj {
                                                Object::Class(class) => class.clone(),
                                                _ => unreachable!(),
                                            },
                                            _ => unreachable!(),
                                        };
                                        self.bind_method(&instance.class, &prop_name)?;
                                    },
                                };
                            },
                            Object::Module(module) => match module.map.get(&*prop_name) {
                                Some(value) => {
                                    let value = *value;
                                    self.pop_stack()?;
                                    self.stack_push_value(value);
                                },
                                None => err!(&format!("undefined property '{}'", prop_name)),
                            },
                            _ => err!("only instances have properties"),
                        },
                        _ => err!("only instances have properties"),
                    }
                },
                OpCode::SetProperty(index) => {
                    let name_constant = global!(*index);
                    let prop_name = match name_constant {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::String(prop_name) => prop_name.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let value = self.pop_stack()?;
                    let instance_ptr = *self.stack.last().unwrap();

                    match instance_ptr {
                        Value::Obj(mut ptr) => match &mut ptr.obj {
                            Object::Instance(instance) => {
                                instance.fields.insert(*prop_name, value);
                                *self.stack.last_mut().unwrap() = value;
                            },
                            _ => err!("only instances have fields"),
                        },
                        _ => err!("only instances have fields"),
                    }
                },
                OpCode::GetSuper(index) => {
                    let name_constant = global!(*index);
                    let prop_name = match name_constant {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::String(prop_name) => prop_name.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    let super_class = self.pop_stack()?;
                    let _instance = *self.stack.last().unwrap();

                    self.bind_method(&super_class, &prop_name)?;
                },
                OpCode::GetIndex => {
                    let index = self.pop_stack()?;
                    let callee = self.pop_stack()?;

                    match callee {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::List(list) => {
                                let index = match index {
                                    Value::Number(number) => {
                                        self._extract_index(list.len(), number)
                                    },
                                    _ => err!("only numbers can be used to index lists"),
                                };
                                match list.get(index) {
                                    Some(item) => self.stack_push_value(*item),
                                    None => self.stack_push_value(Value::Nil),
                                }
                            },
                            Object::Map(hmap) => match hmap.map.get(&index) {
                                Some(item) => self.stack_push_value(*item),
                                None => self.stack_push_value(Value::Nil),
                            },
                            Object::String(string) => {
                                let index = match index {
                                    Value::Number(number) => {
                                        self._extract_index(string.len(), number)
                                    },
                                    _ => err!("only numbers can be used to index strings"),
                                };
                                match string.chars().nth(index) {
                                    Some(c) => {
                                        let char_obj = obj!(self, String, c.to_string());
                                        self.stack_push_value(Value::new(char_obj));
                                    },
                                    None => self.stack_push_value(Value::Nil),
                                }
                            },
                            _ => err!("only lists, maps, and strings can be indexed"),
                        },
                        _ => err!("only lists, maps, and strings can be indexed"),
                    }
                },
                OpCode::SetIndex => {
                    let value = self.pop_stack()?;
                    let index = self.pop_stack()?;
                    let container = *self.stack.last().unwrap();

                    match container {
                        Value::Obj(mut ptr) => match &mut ptr.obj {
                            Object::List(list) => match index {
                                Value::Number(number) => {
                                    let index_usize = self._extract_index(list.len(), number);
                                    list[index_usize] = value;
                                },
                                _ => err!("lists can only be indexed with integers"),
                            },
                            Object::Map(hmap) => {
                                hmap.map.insert(index, value);
                            },
                            _ => err!("only lists and maps can be indexed into"),
                        },
                        _ => err!("only lists and maps can be indexed into"),
                    }
                },
                OpCode::DeleteIndex => {
                    let index = self.pop_stack()?;
                    let container = self.pop_stack()?;

                    match container {
                        Value::Obj(mut ptr) => match &mut ptr.obj {
                            Object::List(list) => match index {
                                Value::Number(number) => {
                                    let index_usize = self._extract_index(list.len(), number);
                                    list.remove(index_usize);
                                },
                                _ => err!("lists can only be indexed with integers"),
                            },
                            Object::Map(hmap) => {
                                hmap.map.remove(&index);
                            },
                            _ => err!("only lists and maps can be indexed into"),
                        },
                        _ => err!("only lists and maps can be indexed into"),
                    }
                },
                OpCode::GetSlice(has_left, has_right) => {
                    let right = self._get_slice_argument(*has_right)?;
                    let left = self._get_slice_argument(*has_left)?;

                    let value = match self.pop_stack()? {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::List(list) => {
                                let (left, right) = self._get_slice_range(list.len(), left, right);
                                let tmp_value = if left > right {
                                    vec![]
                                } else {
                                    list[left..right].to_vec()
                                };

                                obj!(self, List, tmp_value)
                            },
                            Object::String(string) => {
                                let (left, right) =
                                    self._get_slice_range(string.len(), left, right);
                                let tmp_value = if left > right {
                                    "".to_string()
                                } else {
                                    string[left..right].to_string()
                                };

                                obj!(self, String, tmp_value)
                            },
                            _ => err!("only lists and strings can be sliced"),
                        },
                        _ => err!("only lists and strings can be sliced"),
                    };

                    self.stack_push_value(value);
                },
                OpCode::Equal => {
                    let right = self.pop_stack()?;
                    let left = self.stack.last_mut().unwrap();
                    *left = Value::Bool(right == *left);
                },
                OpCode::Greater => binary! { >, '>', Value::Bool },
                OpCode::Less => binary! { <, '<', Value::Bool },
                OpCode::In => {
                    let right = self.pop_stack()?;
                    let left = self.pop_stack()?;

                    match right {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::List(list) => {
                                self.stack_push_value(Value::Bool(list.contains(&left)));
                            },
                            Object::Map(hmap) => {
                                self.stack_push_value(Value::Bool(hmap.map.contains_key(&left)));
                            },
                            Object::String(string) => match left {
                                Value::Obj(sub_ptr) => match &sub_ptr.obj {
                                    Object::String(substring) => {
                                        self.stack_push_value(Value::Bool(
                                            string.contains(&**substring),
                                        ));
                                    },
                                    _ => err!(
                                        "invalid 'in' check: strings can only contain other strings"
                                    ),
                                },
                                _ => err!(
                                    "invalid 'in' check: strings can only contain other strings"
                                ),
                            },
                            _ => err!("'in' operator only functions on iterables"),
                        },
                        _ => err!("'in' operator only functions on iterables"),
                    };
                },
                OpCode::Add => {
                    let right = self.pop_stack()?;
                    let left = self.stack.last_mut().unwrap();

                    match (&right, &*left) {
                        (Value::Number(b), Value::Number(a)) => *left = Value::Number(a + b),
                        (Value::Obj(b_ptr), Value::Number(a)) => match &b_ptr.obj {
                            Object::String(b) => {
                                let s = format!("{a}{b}");
                                let val = Value::new(obj!(self, String, s));
                                let left = self.stack.last_mut().unwrap();
                                *left = val;
                            },
                            _ => {
                                err!("operands must be two numbers, two strings, or two lists")
                            },
                        },
                        (Value::Number(b), Value::Obj(a_ptr)) => match &a_ptr.obj {
                            Object::String(a) => {
                                let s = format!("{a}{b}");
                                let val = Value::new(obj!(self, String, s));
                                let left = self.stack.last_mut().unwrap();
                                *left = val;
                            },
                            _ => {
                                err!("operands must be two numbers, two strings, or two lists")
                            },
                        },
                        (Value::Obj(b_ptr), Value::Obj(a_ptr)) => match (&b_ptr.obj, &a_ptr.obj) {
                            (Object::String(b), Object::String(a)) => {
                                let s = format!("{}{}", a, b);
                                let val = Value::new(obj!(self, String, s));
                                let left = self.stack.last_mut().unwrap();
                                *left = val;
                            },
                            (Object::List(b), Object::List(a)) => {
                                let mut new_list = (**a).clone();
                                new_list.extend(b.iter().cloned());
                                let val = Value::new(obj!(self, List, new_list));
                                let left = self.stack.last_mut().unwrap();
                                *left = val;
                            },
                            _ => {
                                err!("operands must be two numbers, two strings, or two lists")
                            },
                        },
                        _ => err!("operands must be two numbers, two strings, or two lists"),
                    }
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
                    let item = match self.pop_stack()? {
                        Value::Number(number) => number,
                        _ => err!("negation only operates on numbers"),
                    };
                    self.stack_push_value(Value::Number(-item));
                },
                OpCode::ShiftLeft => integer_binary! { <<, "<<", Value::Number },
                OpCode::ShiftRight => integer_binary! { >>, ">>", Value::Number },
                OpCode::BitAnd => bit_ops! { &, '&' },
                OpCode::BitOr => bit_ops! { |, '|' },
                OpCode::BitXor => bit_ops! { ^, '^' },
                OpCode::Print(newline) => {
                    let value = self.pop_stack()?;
                    if *newline {
                        println!("{}", value)
                    } else {
                        print!("{}", value)
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
                    let value_iter = ValueIter::new(&mut self.heap, to_iterate)?;

                    let iterator_obj = obj!(self, Iterator, value_iter);
                    self.stack_push_value(Value::new(iterator_obj));
                },
                OpCode::IteratorNext(index, jump) => {
                    let iterator_ptr = self.stack[*index];

                    match iterator_ptr {
                        Value::Obj(mut ptr) => match &mut ptr.obj {
                            Object::Iterator(iterator) => {
                                if iterator.next < iterator.items.len() {
                                    let item = iterator.items[iterator.next];
                                    iterator.next += 1;
                                    self.stack_push_value(item);
                                } else {
                                    self.stack_push_value(Value::Nil);
                                    self.frame_mut().pos += jump;
                                }
                            },
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    }
                },
                OpCode::BuildList(item_count) => {
                    let range_start = self.stack.len() - item_count;
                    let list: Vec<ValuePtr> = self.stack.split_off(range_start);

                    let list_obj = obj!(self, List, list);
                    self.stack_push_value(Value::new(list_obj));
                },
                OpCode::BuildMap(item_count) => {
                    let range_start = self.stack.len() - item_count;
                    let mut items = self.stack.split_off(range_start).into_iter();

                    let map: HashMap<ValuePtr, ValuePtr> = HashMap::from_iter(
                        (0..(item_count / 2))
                            .map(|_| (items.next().unwrap(), items.next().unwrap())),
                    );

                    let map_obj = obj!(self, Map, ValueMap { map });
                    self.stack_push_value(Value::new(map_obj));
                },
                OpCode::Call(arg_count) => {
                    self.call_value(*arg_count)?;
                    continue;
                },
                OpCode::Invoke(index, arg_count) => {
                    let name_constant = global!(*index);
                    let method_name = match name_constant {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::String(method_name) => method_name.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    self.invoke(&method_name, *arg_count)?;
                    continue;
                },
                OpCode::Closure(index, upvalues) => {
                    let prototype = {
                        let frame = self.frame();
                        let closure_val = &frame.closure;
                        match closure_val {
                            Value::Obj(ptr) => match &ptr.obj {
                                Object::Closure(closure) => {
                                    match &closure.function.chunk.constants[*index] {
                                        Value::Obj(proto_ptr) => match &proto_ptr.obj {
                                            Object::Function(function) => function.clone(),
                                            _ => unreachable!(),
                                        },
                                        _ => unreachable!(),
                                    }
                                },
                                _ => unreachable!(),
                            },
                            _ => unreachable!(),
                        }
                    };

                    let mut closure = Closure {
                        function: *prototype,
                        upvalues: Vec::new(),
                    };

                    let stack_offset = self.frame().stack_offset;
                    let current_closure_upvalues = match &self.frame().closure {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::Closure(c) => c.upvalues.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    for uv in upvalues.iter() {
                        let is_local = uv.is_local;
                        let index = uv.index;
                        if is_local {
                            let location = self.capture_upvalue(stack_offset + index)?;
                            closure.upvalues.push(location);
                        } else {
                            closure.upvalues.push(current_closure_upvalues[index]);
                        }
                    }

                    let closure_obj = obj!(self, Closure, closure);
                    self.stack_push_value(Value::new(closure_obj));
                },
                OpCode::CloseUpValue => {
                    if self.stack.is_empty() {
                        err!("can't close an upvalue without a stack!");
                    };

                    self.close_upvalues(self.stack.len() - 1);
                    self.stack.pop();
                },
                OpCode::Assert(has_message) => {
                    let message_ptr = has_message.then(|| self.pop_stack().ok()).flatten();
                    let assertion_ptr = self.pop_stack()?;

                    if !truthy!(&assertion_ptr)? {
                        let message = if *has_message {
                            match message_ptr.unwrap() {
                                Value::Obj(ptr) => match &ptr.obj {
                                    Object::String(message) => format!("{message}"),
                                    _ => err!("assert message must be a string"),
                                },
                                _ => err!("assert message must be a string"),
                            }
                        } else {
                            format!("{} is not true", assertion_ptr)
                        };

                        err!(&format!("assertion failed: {message}"))
                    }
                },
                OpCode::Import(path, index) => {
                    let path = *path.clone();
                    let source = fs::read_to_string(&path).ok().ok_or_else(|| {
                        LoxError::RuntimeError(format!("unable to import file '{path}'"))
                    })?;

                    let mut vm = VirtualMachine::new();
                    vm.interpret(&source)?;

                    let key = match global!(*index) {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::String(string) => string.clone(),
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };

                    for (key, val) in vm.globals.clone() {
                        self.globals.insert(key, val);
                    }

                    let module_obj = obj!(
                        self,
                        Module,
                        Module {
                            name: *key.clone(),
                            map: vm.globals,
                        }
                    );
                    self.globals.insert(*key, Value::new(module_obj));
                },
                OpCode::Return => {
                    let result = self.pop_stack()?;
                    let old_frame = self.pop_frame()?;
                    self.close_upvalues(old_frame.stack_offset);

                    if self.frames.is_empty() {
                        self.pop_stack()?;
                        return Ok(());
                    }

                    self.stack.truncate(old_frame.stack_offset + 1);
                    *self.stack.last_mut().unwrap() = result;
                },
                OpCode::Class(index) => {
                    let class_name = self.read_string(*index)?;

                    let class_obj = obj!(self, Class, Class::new(class_name));
                    self.stack_push_value(Value::new(class_obj));
                },
                OpCode::Inherit => {
                    let sub_class_ptr = self.pop_stack()?;
                    let super_class_ptr = self.stack.last_mut().unwrap();

                    let methods = match super_class_ptr {
                        Value::Obj(ptr) => match &ptr.obj {
                            Object::Class(super_class) => super_class.methods.clone(),
                            _ => err!("super class must be a class"),
                        },
                        _ => err!("super class must be a class"),
                    };

                    match sub_class_ptr {
                        Value::Obj(mut ptr) => match &mut ptr.obj {
                            Object::Class(sub_class) => {
                                sub_class.parent = Some(*super_class_ptr);
                                sub_class.methods = methods;
                            },
                            _ => unreachable!(),
                        },
                        _ => unreachable!(),
                    };
                },
                OpCode::Method(index) => {
                    self.define_method(&self.read_string(*index)?)?;
                },
            };

            self.frame_mut().pos += 1;
        }

        Ok(())
    }

    fn read_string(&self, index: usize) -> LoxResult<String> {
        match &self.frame().closure {
            Value::Obj(closure_ptr) => match &closure_ptr.obj {
                Object::Closure(closure) => match &closure.function.chunk.constants[index] {
                    Value::Obj(ptr) => match &ptr.obj {
                        Object::String(name) => Ok(*name.clone()),
                        _ => err!("tried to read a string and failed"),
                    },
                    _ => err!("tried to read a string and failed"),
                },
                _ => unreachable!(),
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

    fn pop_frame(&mut self) -> LoxResult<CallFrame> {
        let frame = self.frames.pop().unwrap();

        self.frame = unsafe {
            match self.frames.is_empty() {
                true => std::ptr::null_mut(),
                false => self.frames.as_mut_ptr().add(self.frames.len() - 1),
            }
        };

        Ok(frame)
    }

    fn push_frame(&mut self, frame: CallFrame) {
        self.frames.push(frame);
        self.frame = unsafe { self.frames.as_mut_ptr().add(self.frames.len() - 1) };
    }

    fn call(&mut self, callee: ValuePtr, arg_count: usize) -> LoxResult<()> {
        let frame = CallFrame {
            closure: callee,
            stack_offset: self.stack.len() - arg_count - 1,
            pos: 0,
        };

        self.push_frame(frame);

        Ok(())
    }

    fn call_value(&mut self, arg_count: usize) -> LoxResult<()> {
        let offset = self.stack.len() - arg_count - 1;
        let at_offset = unsafe { &*self.stack.as_ptr().add(offset) };

        match at_offset {
            Value::Obj(ptr) => match &ptr.obj {
                Object::Closure(closure) => {
                    self.check_arity(&closure.function.name, closure.function.arity, arg_count)?;
                    self.call(*at_offset, arg_count)?;
                },
                Object::BoundMethod(bound_method) => {
                    let arg_range_start = self.stack.len() - arg_count;
                    self.stack[arg_range_start - 1] = bound_method.receiver;

                    self.call(bound_method.closure, arg_count)?;
                },
                Object::Class(class) => {
                    let position = self.stack.len() - arg_count - 1;
                    self.stack[position] =
                        Value::new(obj!(self, Instance, Instance::new(*at_offset)));

                    if let Some(init_ptr) = class.methods.get(self.init_string) {
                        self.call(*init_ptr, arg_count)?;
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

                    let args = &self.stack[self.stack.len() - arg_count..];
                    let result = (native.function)(&mut self.heap, args)?;
                    self.stack.truncate(self.stack.len() - arg_count - 1);
                    self.stack_push_value(result);
                    self.frame_mut().pos += 1;
                },
                _ => err!("can only call functions and classes"),
            },
            _ => err!("can only call functions and classes"),
        };

        Ok(())
    }

    fn invoke(&mut self, method_name: &str, arg_count: usize) -> LoxResult<()> {
        let offset = self.stack.len() - arg_count - 1;
        let at_offset = unsafe { &*self.stack.as_ptr().add(offset) };

        match at_offset {
            &Value::Obj(mut ptr) => match &mut ptr.obj {
                Object::Instance(instance) => match instance.fields.get(method_name) {
                    Some(prop) => {
                        self.stack[offset] = *prop;
                        self.call_value(arg_count)
                    },
                    None => {
                        let Value::Obj(class_ptr) = &instance.class else {
                            unreachable!();
                        };
                        let Object::Class(class) = &class_ptr.obj else {
                            unreachable!();
                        };

                        match class.methods.get(method_name) {
                            Some(prop) => self.call(*prop, arg_count),
                            None => err!(&format!(
                                "no method '{}' on class '{}'",
                                method_name, class.name
                            )),
                        }
                    },
                },
                Object::Module(module) => match module.map.get(method_name) {
                    Some(prop) => {
                        self.stack[offset] = *prop;
                        self.call_value(arg_count)
                    },
                    None => err!(&format!(
                        "module '{}' has no property '{}'",
                        module.name, method_name
                    )),
                },
                Object::Map(hmap) => {
                    match (method_name, arg_count) {
                        ("clear", 0) => hmap.map.clear(),
                        ("keys", 0) => {
                            self.stack[offset] =
                                Value::new(obj!(self, List, hmap.map.keys().cloned().collect()));
                        },
                        ("values", 0) => {
                            self.stack[offset] =
                                Value::new(obj!(self, List, hmap.map.values().cloned().collect()));
                        },
                        _ => err!(&format!(
                            "map does not have the method '{method_name}' with {arg_count} argument(s)",
                        )),
                    };

                    self.frame_mut().pos += 1;
                    Ok(())
                },
                Object::List(list) => {
                    match (method_name, arg_count) {
                        ("clear", 0) => list.clear(),
                        _ => err!(&format!(
                            "list does not have the method '{method_name}' with {arg_count} argument(s)",
                        )),
                    };

                    self.frame_mut().pos += 1;
                    Ok(())
                },
                Object::String(string) => {
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
            },
            _ => err!("only instances have methods"),
        }
    }

    fn bind_method(&mut self, class_ptr: &ValuePtr, name: &str) -> LoxResult<()> {
        let Value::Obj(class_ptr) = class_ptr else {
            unreachable!();
        };

        let Object::Class(class) = &class_ptr.obj else {
            unreachable!();
        };

        match class.methods.get(name) {
            Some(method) => {
                let receiver = self.pop_stack()?;

                let bound_method = obj!(
                    self,
                    BoundMethod,
                    BoundMethod {
                        receiver,
                        closure: *method,
                    }
                );

                self.stack_push_value(Value::new(bound_method));

                Ok(())
            },
            None => Err(LoxError::RuntimeError(format!(
                "'{name}' not a valid property",
            ))),
        }
    }

    fn capture_upvalue(&mut self, upvalue_index: usize) -> LoxResult<UpValuePtr> {
        let item = &self.stack[upvalue_index];

        if let Some(uv) = self.upvalues.iter().find(|uv| {
            let Object::UpValue(uv) = &uv.obj else {
                unreachable!();
            };

            uv.location_index == upvalue_index
        }) {
            return Ok(*uv);
        }

        let new_upvalue = self.heap.allocate(Object::UpValue(Box::new(UpValue {
            location: *item,
            location_index: upvalue_index,
        })));

        self.upvalues.push(new_upvalue);

        Ok(new_upvalue)
    }

    fn close_upvalues(&mut self, position: usize) {
        // We need to iterate and modify, so we can't use retain directly if we want to modify the dropped ones.
        // Instead, we can partition or iterate.
        // Since `upvalues` contains ObjPtr, we can clone the ptrs we want to close.

        let mut i = 0;
        while i < self.upvalues.len() {
            let should_close = {
                let Object::UpValue(uv) = &self.upvalues[i].obj else {
                    unreachable!();
                };

                uv.location_index >= position
            };

            if should_close {
                let mut uv_ptr = self.upvalues.remove(i);
                let uv = &mut uv_ptr.obj;
                let upvalue = match uv {
                    Object::UpValue(uv) => uv,
                    _ => unreachable!(),
                };
                // Move value from stack to heap (location)
                upvalue.location = self.stack[upvalue.location_index];
                upvalue.location_index = usize::MAX; // Mark as closed
            } else {
                i += 1;
            }
        }
    }

    fn define_method(&mut self, name: &str) -> LoxResult<()> {
        let method = self.pop_stack()?;
        let class_ptr = self.stack[self.stack.len() - 1];

        match class_ptr {
            Value::Obj(mut ptr) => match &mut ptr.obj {
                Object::Class(class) => {
                    class.methods.insert(String::from(name), method);
                },
                _ => return Err(LoxError::RuntimeError("no class on stack".into())),
            },
            _ => return Err(LoxError::RuntimeError("no class on stack".into())),
        }

        Ok(())
    }
}
