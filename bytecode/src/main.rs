pub mod common;
pub mod chunk;
pub mod value;

use crate::chunk::{Chunk, OpCode};

fn main() {
    let mut chunk = Chunk::new();

    chunk.write(OpCode::Return);
}
