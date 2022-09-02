use scad_bytecode::{Chunk, Instruction};

use crate::Stack;

pub trait Callbacks {
    /// A callback that is fired immediately *before* executing the next
    /// instruction.
    ///
    /// At this point, the instruction pointer is still pointing at the current
    /// instruction.
    #[inline]
    fn before_execute(
        &mut self,
        _current_chunk: &Chunk,
        _instruction_pointer: usize,
        _instruction: Instruction,
        _stack: &Stack,
    ) {
    }
}
