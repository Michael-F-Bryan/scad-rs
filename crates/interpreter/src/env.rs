use crate::{Exception, Value};

pub trait Environment {
    fn echo(&mut self, values: &[Value]) -> Result<(), Exception>;
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct Builtin;

impl Environment for Builtin {
    fn echo(&mut self, values: &[Value]) -> Result<(), Exception> {
        for (i, value) in values.iter().enumerate() {
            if i > 0 {
                eprint!(" ");
            }
            eprint!("{value}");
        }
        eprintln!();
        Ok(())
    }
}
