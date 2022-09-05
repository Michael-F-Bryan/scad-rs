use scad_bytecode::{Chunk, Instruction};

use crate::{Geometry, Stack};

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

    #[inline]
    fn consume_geometry(&mut self, _geometry: Geometry) {}
}

macro_rules! defer_callbacks_impl {
    ($ty:ty) => {
        impl<C: Callbacks + ?Sized> Callbacks for $ty {
            fn before_execute(
                &mut self,
                current_chunk: &Chunk,
                instruction_pointer: usize,
                instruction: Instruction,
                stack: &Stack,
            ) {
                (**self).before_execute(current_chunk, instruction_pointer, instruction, stack);
            }

            #[inline]
            fn consume_geometry(&mut self, geometry: Geometry) {
                (**self).consume_geometry(geometry);
            }
        }
    };
}

defer_callbacks_impl!(&'_ mut C);
defer_callbacks_impl!(Box<C>);
