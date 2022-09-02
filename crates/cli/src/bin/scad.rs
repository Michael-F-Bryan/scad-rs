use clap::Parser;
use scad_cli::{Disassemble, Run};

fn main() -> Result<(), anyhow::Error> {
    let cmd = Cmd::from_args();

    match cmd {
        Cmd::Run(r) => r.execute(),
        Cmd::Disassemble(d) => d.execute(),
    }
}

#[derive(Debug, Parser)]
enum Cmd {
    /// Run a compiled program.
    Run(Run),
    /// Disassemble a bytecode file.
    #[clap(aliases = &["dis", "d"])]
    Disassemble(Disassemble),
}
