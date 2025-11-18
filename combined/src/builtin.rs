use std::time::SystemTime;

use crate::{
    errors::{LoxError, LoxResult},
    gc::Heap,
    object::Object,
    value::{Value, ValuePtr},
};

pub fn _range(heap: &mut Heap, input: &[ValuePtr]) -> LoxResult<Value> {
    match (&input[0], &input[1]) {
        (Value::Number(start), Value::Number(stop)) => {
            let start = *start as i32;
            let stop = *stop as i32;

            let range_iter = match stop > start {
                true => start..stop,
                false => stop..start,
            };

            Ok(Value::Obj(heap.allocate(Object::List(Box::new(
                range_iter.map(|x| Value::Number(x.into())).collect(),
            )))))
        },
        _ => Err(LoxError::RuntimeError(
            "range(start, stop) takes two numbers".into(),
        )),
    }
}

pub fn _time(_: &mut Heap, _: &[ValuePtr]) -> LoxResult<Value> {
    match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
        Ok(unix_now) => Ok(Value::Number(unix_now.as_secs_f64())),
        Err(_) => Err(LoxError::RuntimeError("time() failed, uh oh".into())),
    }
}

pub fn _str(heap: &mut Heap, input: &[ValuePtr]) -> LoxResult<Value> {
    match &input[0] {
        Value::Obj(ptr) => unsafe {
            match &ptr.deref().obj {
                Object::String(string) => Ok(Value::Obj(
                    heap.allocate(Object::String(Box::new(*string.clone()))),
                )),
                _ => Err(LoxError::RuntimeError(
                    "string() only accepts primitives".into(),
                )),
            }
        },
        _ => Ok(Value::Obj(
            heap.allocate(Object::String(Box::new(input[0].to_string()))),
        )),
    }
}

pub fn _len(_: &mut Heap, input: &[ValuePtr]) -> LoxResult<Value> {
    match &input[0] {
        Value::Obj(ptr) => unsafe {
            match &ptr.deref().obj {
                Object::List(list) => Ok(Value::Number(list.len() as f64)),
                Object::Map(hmap) => Ok(Value::Number(hmap.map.len() as f64)),
                Object::String(string) => Ok(Value::Number(string.len() as f64)),
                _ => Err(LoxError::RuntimeError(
                    "len() only accepts strings, lists, and maps".into(),
                )),
            }
        },
        _ => Err(LoxError::RuntimeError(
            "len() only accepts strings, lists, and maps".into(),
        )),
    }
}

pub fn _type(heap: &mut Heap, input: &[ValuePtr]) -> LoxResult<Value> {
    let output: &str = match &input[0] {
        Value::Bool(_) => "bool",
        Value::Nil => "nil",
        Value::Number(_) => "number",
        Value::Obj(ptr) => unsafe {
            match &ptr.deref().obj {
                Object::BoundMethod(_) => "method",
                Object::Class(_) => "class",
                Object::Closure(_) => "closure",
                Object::Function(_) => "function",
                Object::Instance(_) => "object",
                Object::Iterator(_) => "iterator",
                Object::List(_) => "list",
                Object::Map(_) => "map",
                Object::Module(_) => "module",
                Object::Native(_) => "function",
                Object::String(_) => "string",
                Object::UpValue(uv) => return _type(heap, &[uv.location]),
            }
        },
    };

    Ok(Value::Obj(
        heap.allocate(Object::String(Box::new(output.into()))),
    ))
}

pub fn _sqrt(_: &mut Heap, input: &[ValuePtr]) -> LoxResult<Value> {
    match &input[0] {
        Value::Number(number) => Ok(Value::Number(number.sqrt())),
        _ => Err(LoxError::RuntimeError(
            "sqrt() only operates on numbers".into(),
        )),
    }
}

pub fn _pow(_: &mut Heap, input: &[ValuePtr]) -> LoxResult<Value> {
    match (&input[0], &input[1]) {
        (Value::Number(number), Value::Number(exp)) => Ok(Value::Number(number.powf(*exp))),
        _ => Err(LoxError::RuntimeError(
            "sqrt() only operates on numbers".into(),
        )),
    }
}

macro_rules! color_fn {
    ( $name:tt, $code:expr ) => {
        pub fn $name(heap: &mut Heap, input: &[ValuePtr]) -> LoxResult<Value> {
            Ok(Value::Obj(heap.allocate(Object::String(Box::new(
                format!("\x1b[{}m{}\x1b[0m", $code, input[0]),
            )))))
        }
    };
}

color_fn! { _black, "30" }
color_fn! { _red, "31" }
color_fn! { _green, "32" }
color_fn! { _yellow, "33" }
color_fn! { _blue, "34" }
color_fn! { _magenta, "35" }
color_fn! { _cyan, "36" }
color_fn! { _white, "37" }
