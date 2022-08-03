use anyhow::{Context, Error};
use proc_macro2::TokenStream;
use std::{
    io::Write,
    process::{Command, Output, Stdio},
};

/// Use `rustfmt` to pretty-print the tokens.
pub fn pretty_print(tokens: TokenStream) -> Result<String, Error> {
    let tokens = tokens.to_string();

    let mut child = Command::new("rustfmt")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .context("Unable to start `rustfmt`. Is it installed?")?;

    let mut stdin = child.stdin.take().unwrap();
    write!(stdin, "{tokens}")?;
    stdin.flush()?;
    drop(stdin);

    let Output {
        status,
        stdout,
        stderr,
    } = child.wait_with_output()?;
    let stdout = String::from_utf8_lossy(&stdout);
    let stderr = String::from_utf8_lossy(&stderr);

    if !status.success() {
        eprintln!("---- Stdout ----");
        eprintln!("{stdout}");
        eprintln!("---- Stderr ----");
        eprintln!("{stderr}");
        let code = status.code();
        match code {
            Some(code) => anyhow::bail!("The `rustfmt` command failed with return code {code}"),
            None => anyhow::bail!("The `rustfmt` command failed"),
        }
    }

    Ok(stdout.into())
}
