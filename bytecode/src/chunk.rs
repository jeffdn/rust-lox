use std::iter::Iterator;

use crate::value::ValueSet;

pub enum OpCode {
    Return,
    Constant(usize),
}

pub struct Chunk {
    code: Vec<OpCode>,
    constants: ValueSet,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: ValueSet::new(),
        }
    }

    pub fn write(&mut self, opcode: OpCode) {
        self.code.push(opcode);
    }

    pub fn iter(&self) -> impl Iterator<Item = &OpCode> {
        self.code.iter()
    }
}

// debugging
impl Chunk {
    fn disassemble(&self, name: &str) {
        println!("== {} ==", name);

        for chunk in self.iter() {
            self.disassemble_instruction(chunk);
        }
    }

    fn disassemble_instruction(&self, code: &OpCode) {
        match code {
            OpCode::Return => println!("op_return"),
            OpCode::Constant(index) => println!("op_constant {}", self.constants.get(index)),
        };
    }
}
