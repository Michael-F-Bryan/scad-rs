use scad_bytecode::Program;

use crate::{lowering::Lowering, Diagnostics};

pub(crate) fn compile(_db: &dyn Lowering) -> (Program, Diagnostics) {
    todo!();
}
