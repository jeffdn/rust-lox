use crate::{
    chunk::{Chunk, OpCode},
    compiler::Compiler,
    errors::LoxError,
    value::Value,
};

static STACK_MAX: usize = 256;

pub struct VirtualMachine {
    chunks: Vec<Chunk>,
    stack: Vec<Value>,
}


impl VirtualMachine {
    pub fn new() -> VirtualMachine {
        VirtualMachine {
            chunks: Vec::new(),
            stack: Vec::with_capacity(STACK_MAX),
        }
    }

    pub fn interpret(&mut self, input: String) -> Result<(), LoxError> {
        let mut compiler = Compiler::new(&input);
        let chunk = compiler.compile()?;

        self.chunks.push(chunk);

        self.run()
    }

    #[cfg(feature="debug")]
    fn dump(&self, chunk: &Chunk, idx: usize) {
        let stack_str: Vec<String> = self.stack.iter()
            .map(|v| format!("{}", v))
            .collect();

        println!("          [{}]", stack_str.join(", "));
        chunk.disassemble_instruction(&chunk.code[idx], idx);
    }

    fn pop_stack(&mut self) -> Result<Value, LoxError> {
        match self.stack.pop() {
            Some(item) => Ok(item),
            None => Err(LoxError::RuntimeError),
        }
    }

    pub fn run(&mut self) -> Result<(), LoxError> {
        let chunk = self.chunks.pop().unwrap();

        for (idx, code) in chunk.iter().enumerate() {
            #[cfg(feature="debug")]
            self.dump(&chunk, idx);

            match code {
                OpCode::Return => {
                    let value = self.pop_stack()?;

                    println!("{}", value);

                    return Ok(());
                },
                OpCode::Constant(index) => {
                    self.stack.push(chunk.constants.get(*index).clone());
                },
                OpCode::Negate => {
                    let item = match self.pop_stack()? {
                        Value::Number(number) => number,
                        _ => return Err(LoxError::RuntimeError),
                    };
                    self.stack.push(Value::Number(-item));
                },
                OpCode::Add => {
                    let (b, a) = match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => (b, a),
                        _ => return Err(LoxError::RuntimeError),
                    };
                    self.stack.push(Value::Number(a + b));
                },
                OpCode::Subtract => {
                    let (b, a) = match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => (b, a),
                        _ => return Err(LoxError::RuntimeError),
                    };
                    self.stack.push(Value::Number(a - b));
                },
                OpCode::Multiply => {
                    let (b, a) = match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => (b, a),
                        _ => return Err(LoxError::RuntimeError),
                    };
                    self.stack.push(Value::Number(a * b));
                },
                OpCode::Divide => {
                    let (b, a) = match (self.pop_stack()?, self.pop_stack()?) {
                        (Value::Number(b), Value::Number(a)) => (b, a),
                        _ => return Err(LoxError::RuntimeError),
                    };
                    self.stack.push(Value::Number(a / b));
                },
            };
        }

        Ok(())
    }
}
