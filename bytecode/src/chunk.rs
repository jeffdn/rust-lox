use std::{
    boxed::Box,
    fmt,
    iter::Iterator,
};

use crate::value::{Value, ValueSet};

#[derive(Clone, Debug, PartialEq)]
pub struct UpValue {
    pub is_local: bool,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub enum OpCode {
    Constant(usize),
    False,
    True,
    Nil,
    Pop,
    GetLocal(usize),
    SetLocal(usize),
    GetGlobal(usize),
    DefineGlobal(usize),
    SetGlobal(usize),
    GetUpValue(usize),
    SetUpValue(usize),
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Jump(usize),
    JumpIfFalse(usize),
    Loop(usize),
    Call(usize),
    Closure(usize, Box<Vec<UpValue>>),
    Return,
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let as_string = match self {
            OpCode::Constant(_) => "op_constant",
            OpCode::False => "op_false",
            OpCode::True => "op_true",
            OpCode::Nil => "op_nil",
            OpCode::Pop => "op_pop",
            OpCode::GetLocal(_) => "op_get_local",
            OpCode::SetLocal(_) => "op_set_local",
            OpCode::GetGlobal(_) => "op_get_global",
            OpCode::DefineGlobal(_) => "op_define_global",
            OpCode::SetGlobal(_) => "op_set_global",
            OpCode::GetUpValue(_) => "op_get_upvalue",
            OpCode::SetUpValue(_) => "op_set_upvalue",
            OpCode::Equal => "op_equal",
            OpCode::Greater => "op_greater",
            OpCode::Less => "op_less",
            OpCode::Add => "op_add",
            OpCode::Subtract => "op_subtract",
            OpCode::Multiply => "op_multiply",
            OpCode::Divide => "op_divide",
            OpCode::Not => "op_not",
            OpCode::Negate => "op_negate",
            OpCode::Print => "op_print",
            OpCode::Jump(_) => "op_jump",
            OpCode::JumpIfFalse(_) => "op_jump_if_false",
            OpCode::Loop(_) => "op_loop",
            OpCode::Call(_) => "op_call",
            OpCode::Closure(_, _) => "op_closure",
            OpCode::Return => "op_return",
        };

        write!(f, "{:<20}", as_string)
    }
}

#[derive(Clone, Debug)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: ValueSet,
    pub lines: Vec<usize>,
}



impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: ValueSet::new(),
            lines: Vec::new(),
        }
    }

    pub fn write(&mut self, opcode: OpCode, line: usize) {
        self.code.push(opcode);
        self.lines.push(line);
    }

    pub fn iter(&self) -> impl Iterator<Item = &OpCode> {
        self.code.iter()
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.write(value)
    }

    #[cfg(feature="debug")]
    pub fn disassemble(&self, name: &str) {
        println!("=== {} ===", name);

        for (idx, chunk) in self.iter().enumerate() {
            self.disassemble_instruction(chunk, idx);
        }
    }

    #[cfg(feature="debug")]
    pub fn disassemble_instruction(&self, code: &OpCode, offset: usize) {
        let line_str = if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            "   |".into()
        } else {
            format!("{:>4}", self.lines[offset])
        };

        let output = match code {
            OpCode::Constant(index) => format!(
                "{:<20} {}", "op_constant", *index, //self.constants.get(*index),
            ),
            OpCode::GetLocal(index) => format!(
                "{:<20} {}", "op_get_local", *index, //self.constants.get(*index),
            ),
            OpCode::SetLocal(index) => format!(
                "{:<20} {}", "op_set_local", *index, //self.constants.get(*index),
            ),
            OpCode::GetGlobal(index) => format!(
                "{:<20} {}", "op_get_global", *index, //self.constants.get(*index),
            ),
            OpCode::DefineGlobal(index) => format!(
                "{:<20} {}", "op_define_global", *index, //self.constants.get(*index),
            ),
            OpCode::SetGlobal(index) => format!(
                "{:<20} {}", "op_set_global", *index, //self.constants.get(*index),
            ),
            OpCode::GetUpValue(index) => format!(
                "{:<20} {}", "op_get_upvalue", *index, //self.constants.get(*index),
            ),
            OpCode::SetUpValue(index) => format!(
                "{:<20} {}", "op_set_upvalue", *index, //self.constants.get(*index),
            ),
            OpCode::Jump(offset) => format!(
                "{:<20} {}", "op_jump", offset,
            ),
            OpCode::JumpIfFalse(offset) => format!(
                "{:<20} {}", "op_jump_if_false", offset,
            ),
            OpCode::Loop(offset) => format!(
                "{:<20} {}", "op_loop", offset,
            ),
            OpCode::Call(offset) => format!(
                "{:<20} {}", "op_call", offset,
            ),
            OpCode::Closure(index, _) => format!(
                "{:<20} {}", "op_closure", index,
            ),
            _ => code.to_string(),
        };

        println!("{} {:0>4} {}", line_str, offset, output);
    }
}
