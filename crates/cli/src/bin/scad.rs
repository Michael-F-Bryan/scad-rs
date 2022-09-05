use clap::Parser;
use scad_cli::{Compile, Disassemble, Run};

fn main() -> Result<(), anyhow::Error> {
    let cmd = Cmd::from_args();

    match cmd {
        Cmd::Run(r) => r.execute(),
        Cmd::Disassemble(d) => d.execute(),
        Cmd::Compile(c) => c.execute(),
    }
}

#[derive(Debug, Parser)]
enum Cmd {
    /// Run a compiled program.
    #[clap(aliases = &["r"])]
    Run(Run),
    /// Disassemble a bytecode file.
    #[clap(aliases = &["dis", "d"])]
    Disassemble(Disassemble),
    #[clap(aliases = &["c"])]
    Compile(Compile),
}
