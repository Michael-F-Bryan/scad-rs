#[cfg(test)]
mod tests {
    use scad_codegen::Syntax;

    #[test]
    fn syntax_codegen() {
        let _ = tracing_subscriber::fmt()
            .with_max_level(tracing::metadata::LevelFilter::DEBUG)
            .with_test_writer()
            .try_init();

        let grammar = include_str!("scad.ungrammar").parse().unwrap();
        let syntax = Syntax::from_grammar(&grammar).unwrap();

        let generated = syntax.kind();

        insta::assert_display_snapshot!(scad_codegen::pretty_print(generated).unwrap());
    }
}
