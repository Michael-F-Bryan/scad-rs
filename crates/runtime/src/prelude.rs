use std::{collections::HashMap, sync::Arc};

use crate::{BuiltinFunction, RuntimeError, Value};

/// All functions and constants that are available on startup.
pub fn prelude() -> HashMap<Arc<str>, Value> {
    let mut prelude: HashMap<Arc<str>, Value> = HashMap::new();

    prelude.insert("echo".into(), BuiltinFunction::new(echo).into());
    prelude.insert("norm".into(), BuiltinFunction::new(norm).into());

    prelude
}

fn echo(args: Vec<Value>) -> Result<Value, RuntimeError> {
    for (i, arg) in args.into_iter().enumerate() {
        if i > 0 {
            print!(" ");
        }

        print!("{arg:?}");
    }
    println!();

    Ok(Value::Undef)
}

fn norm(args: Vec<Value>) -> Result<Value, RuntimeError> {
    match args.len() {
        0 => Ok(Value::Undef),
        1 => Ok(args.into_iter().next().unwrap()),
        _ => Ok(Value::List(args)),
    }
}
