use scad_bytecode::Program;
use scad_compiler::{lowering::Lowering, parsing::Parsing, Database};
use scad_runtime::{Geometry, RuntimeError, Value, VirtualMachine};

#[test]
fn run_pass() {
    insta::glob!("run-pass/*.scad", |path| {
        let src = std::fs::read_to_string(path).unwrap();
        let program = compile(&src);

        let mut output = Output::default();
        let mut vm = VirtualMachine::load(program);
        vm.run(Callbacks(&mut output)).expect("Execution failed");

        insta::assert_debug_snapshot!(output);
    });
}

#[test]
fn run_fail() {
    insta::glob!("run-fail/*.scad", |path| {
        let src = std::fs::read_to_string(path).unwrap();
        let program = compile(&src);

        let mut output = Output::default();
        let mut vm = VirtualMachine::load(program);
        let error = vm
            .run(Callbacks(&mut output))
            .expect_err("Execution should have failed");

        #[derive(Debug)]
        #[allow(dead_code)]
        struct Result {
            output: Output,
            error: RuntimeError,
        }

        insta::assert_debug_snapshot!(Result { output, error });
    });
}

fn compile(src: &str) -> Program {
    let mut db = Database::default();
    db.set_src(src.into());
    let (program, diags) = db.compile();

    if diags.has_warnings() {
        panic!("The program didn't compile cleanly: {diags:#?}");
    }

    program
}

struct Callbacks<'a>(&'a mut Output);

impl scad_runtime::Callbacks for Callbacks<'_> {
    fn consume_geometry(&mut self, geometry: Geometry) {
        self.0.geometry.push(geometry);
    }

    fn print(&mut self, values: &[Value]) -> Result<(), scad_runtime::RuntimeError> {
        self.0.printed.push(values.to_vec());
        Ok(())
    }
}

#[derive(Debug, Default, Clone)]
pub struct Output {
    geometry: Vec<Geometry>,
    printed: Vec<Vec<Value>>,
}
