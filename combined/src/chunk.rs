use std::{boxed::Box, fmt, iter::Iterator};

use crate::value::{Value, ValuePtr};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UpValue {
    pub is_local: bool,
    pub index: usize,
}

#[derive(Clone, Debug, PartialEq, Eq)]
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
    GetProperty(usize),
    SetProperty(usize),
    GetSuper(usize),
    GetIndex,
    SetIndex,
    DeleteIndex,
    GetSlice(bool, bool),
    Add,
    Divide,
    Equal,
    Greater,
    Less,
    In,
    Modulo,
    Multiply,
    Subtract,
    ShiftLeft,
    ShiftRight,
    BitAnd,
    BitOr,
    BitXor,
    Not,
    Negate,
    Print(bool),
    Assert(bool),
    Jump(usize),
    JumpIfFalse(usize),
    Loop(usize),
    Break(usize),
    Continue(usize),
    DefineIterator,
    IteratorNext(usize, usize),
    BuildList(usize),
    BuildMap(usize),
    Call(usize),
    Invoke(usize, usize),
    Closure(usize, Box<Vec<UpValue>>),
    CloseUpValue,
    Import(Box<String>, usize),
    Return,
    Class(usize),
    Inherit,
    Method(usize),
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
            OpCode::GetProperty(_) => "op_get_property",
            OpCode::SetProperty(_) => "op_set_property",
            OpCode::GetSuper(_) => "op_get_super",
            OpCode::GetIndex => "op_get_index",
            OpCode::SetIndex => "op_set_index",
            OpCode::DeleteIndex => "op_delete_index",
            OpCode::GetSlice(_, _) => "op_get_slice",
            OpCode::Add => "op_add",
            OpCode::Divide => "op_divide",
            OpCode::Equal => "op_equal",
            OpCode::Greater => "op_greater",
            OpCode::Less => "op_less",
            OpCode::In => "op_in",
            OpCode::Modulo => "op_modulo",
            OpCode::Multiply => "op_multiply",
            OpCode::Subtract => "op_subtract",
            OpCode::ShiftLeft => "op_shiftleft",
            OpCode::ShiftRight => "op_shiftright",
            OpCode::BitAnd => "op_bitand",
            OpCode::BitOr => "op_bitor",
            OpCode::BitXor => "op_bitxor",
            OpCode::Not => "op_not",
            OpCode::Negate => "op_negate",
            OpCode::Print(_) => "op_print",
            OpCode::Assert(_) => "op_assert",
            OpCode::Jump(_) => "op_jump",
            OpCode::JumpIfFalse(_) => "op_jump_if_false",
            OpCode::Loop(_) => "op_loop",
            OpCode::Break(_) => "op_break",
            OpCode::Continue(_) => "op_continue",
            OpCode::DefineIterator => "op_define_iterator",
            OpCode::IteratorNext(_, _) => "op_iterator_next",
            OpCode::BuildList(_) => "op_build_list",
            OpCode::BuildMap(_) => "op_build_list",
            OpCode::Call(_) => "op_call",
            OpCode::Invoke(_, _) => "op_invoke",
            OpCode::Closure(_, _) => "op_closure",
            OpCode::CloseUpValue => "op_close_upvalue",
            OpCode::Import(_, _) => "op_import",
            OpCode::Return => "op_return",
            OpCode::Class(_) => "op_class",
            OpCode::Inherit => "op_inherit",
            OpCode::Method(_) => "op_method",
        };

        write!(f, "{as_string:<20}")
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<ValuePtr>,
    pub lines: Vec<usize>,
}

impl Default for Chunk {
    fn default() -> Self {
        Self::new()
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
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

    pub fn len(&self) -> usize {
        self.code.len()
    }

    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    pub fn add_constant(&mut self, value: Value) -> usize {
        self.constants.push(ValuePtr::new(value));
        self.constants.len() - 1
    }

    #[cfg(feature = "debug")]
    pub fn disassemble(&self, name: &str) {
        println!("=== {name} ===");

        for (idx, chunk) in self.iter().enumerate() {
            self.disassemble_instruction(chunk, idx);
        }
    }

    #[cfg(feature = "debug")]
    pub fn disassemble_instruction(&self, code: &OpCode, offset: usize) {
        let line_str = if offset > 0 && self.lines[offset] == self.lines[offset - 1] {
            "   |".into()
        } else {
            format!("{:>4}", self.lines[offset])
        };

        let output = match code {
            OpCode::Constant(index) => format!("{:<20} {}", "op_constant", *index),
            OpCode::GetLocal(index) => format!("{:<20} {}", "op_get_local", *index),
            OpCode::SetLocal(index) => format!("{:<20} {}", "op_set_local", *index),
            OpCode::GetGlobal(index) => format!("{:<20} {}", "op_get_global", *index),
            OpCode::DefineGlobal(index) => format!("{:<20} {}", "op_define_global", *index),
            OpCode::SetGlobal(index) => format!("{:<20} {}", "op_set_global", *index),
            OpCode::GetUpValue(index) => format!("{:<20} {}", "op_get_upvalue", *index),
            OpCode::SetUpValue(index) => format!("{:<20} {}", "op_set_upvalue", *index),
            OpCode::GetProperty(index) => format!("{:<20} {}", "op_get_property", *index),
            OpCode::SetProperty(index) => format!("{:<20} {}", "op_set_property", *index),
            OpCode::GetSuper(index) => format!("{:<20} {}", "op_get_super", *index),
            OpCode::GetSlice(left, right) => format!("{:<20} {left} {right}", "op_get_slice"),
            OpCode::Assert(has_message) => format!("{:<20} {has_message}", "op_assert"),
            OpCode::Jump(offset) => format!("{:<20} {offset}", "op_jump"),
            OpCode::JumpIfFalse(offset) => format!("{:<20} {offset}", "op_jump_if_false"),
            OpCode::Loop(offset) => format!("{:<20} {offset}", "op_loop"),
            OpCode::Break(offset) => format!("{:<20} {offset}", "op_break"),
            OpCode::Continue(offset) => {
                format!("{:<20} {offset}", "op_continue")
            },
            OpCode::IteratorNext(index, jump) => {
                format!("{:<20} {index} {jump}", "op_iterator_next")
            },
            OpCode::BuildList(item_count) => format!("{:<20} {item_count}", "op_build_list"),
            OpCode::BuildMap(item_count) => format!("{:<20} {item_count}", "op_build_map"),
            OpCode::Call(offset) => format!("{:<20} {offset}", "op_call"),
            OpCode::Invoke(index, offset) => format!("{:<20} {index} {offset}", "op_invoke"),
            OpCode::Closure(index, _) => format!("{:<20} {index}", "op_closure"),
            OpCode::Import(path, index) => format!("{:<20} {path} {index}", "op_import"),
            OpCode::Class(index) => format!("{:<20} {index}", "op_class"),
            OpCode::Method(index) => format!("{:<20} {index}", "op_method"),
            _ => code.to_string(),
        };

        println!("{line_str} {offset:0>4} {output}");
    }
}
