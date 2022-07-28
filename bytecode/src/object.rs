use std::{
    boxed::Box,
    fmt,
};

#[derive(Clone, Debug)]
pub enum Object {
    String(Box<String>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match self {
            Object::String(string) => *string.clone(),
        };

        write!(f, "'{}'", output)
    }
}
