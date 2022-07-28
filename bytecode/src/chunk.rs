use std::{
    fmt,
    iter::Iterator,
};

use crate::value::{Value, ValueSet};

#[derive(Clone, Debug)]
pub enum OpCode {
    Return,
    Constant(usize),
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

impl fmt::Display for OpCode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let as_string = match self {
            OpCode::Return => "op_return",
            OpCode::Constant(_) => "op_constant",
            OpCode::Negate => "op_negate",
            OpCode::Add => "op_add",
            OpCode::Subtract => "op_subtract",
            OpCode::Multiply => "op_multiply",
            OpCode::Divide => "op_divide",
        };

        write!(f, "{:<16}", as_string)
    }
}

#[derive(Clone)]
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
                "{:<16} {}", "op_constant", self.constants.get(*index),
            ),
            _ => code.to_string(),
        };

        println!("{} {:0>4} {}", line_str, offset, output);
    }
}
