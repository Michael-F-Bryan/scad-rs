use std::ops::Index;

use im::{OrdMap, Vector};

use crate::{hir, Text};

pub fn execute(pkg: hir::Package) -> Result<MachineState, ExecutionError> {
    let mut state = MachineState::default();

    let root_frame = state.push_frame();

    for (id, c) in &pkg.constants {
        let name = pkg.global_namespace.get_name(id).unwrap();
        let value = match c.value {
            hir::Expr::FloatLiteral(f) => Value::Number(f.value()),
            hir::Expr::IntegerLiteral(i) => Value::Number(i as f64),
            _ => todo!(),
        };

        root_frame.variables.insert(name.clone(), value);
    }

    evaluate_stack_frame(&mut state, &pkg.script)?;

    Ok(state)
}

fn evaluate_stack_frame(
    state: &mut MachineState,
    script: &hir::ControlFlowGraph,
) -> Result<(), ExecutionError> {
    let hir::ControlFlowGraph { entry_point, nodes } = script;

    let mut current_node = &nodes[*entry_point];

    loop {
        for instruction in &current_node.instructions {
            evaluate_instruction(state, instruction)?;
        }

        match current_node.exit {
            hir::Continuation::Return => return Ok(()),
            hir::Continuation::Jump(next) => current_node = &nodes[next],
        }
    }
}

fn evaluate_instruction(
    _state: &mut MachineState,
    instruction: &hir::Instruction,
) -> Result<(), ExecutionError> {
    match *instruction {}
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct MachineState {
    stack: Vector<StackFrame>,
}

impl MachineState {
    pub fn lookup(&self, variable_name: &str) -> Option<&Value> {
        self.stack
            .iter()
            .find_map(|frame| frame.variables.get(variable_name))
    }

    pub fn push_frame(&mut self) -> &mut StackFrame {
        self.stack.push_front(StackFrame::default());
        &mut self.stack[0]
    }
}

impl Index<&str> for MachineState {
    type Output = Value;

    #[track_caller]
    fn index(&self, index: &str) -> &Self::Output {
        self.lookup(index).unwrap()
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct StackFrame {
    variables: OrdMap<Text, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    String(Text),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExecutionError;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lowering::Lower, syntax::Parse};

    #[derive(Default)]
    #[salsa::database(crate::syntax::ParsingStorage, crate::lowering::LoweringStorage)]
    struct Db {
        storage: salsa::Storage<Self>,
    }

    impl salsa::Database for Db {}

    fn load_package(src: &str) -> hir::Package {
        let mut db = Db::default();
        db.set_source_code(src.into());
        let (pkg, diags) = db.hir_package();
        assert!(!diags.has_warnings());
        pkg
    }

    #[test]
    fn basic_program() {
        let src = "x = 42;";
        let pkg = load_package(src);

        let state = execute(pkg).unwrap();

        assert_eq!(state["x"], Value::Number(42.0));
    }
}
