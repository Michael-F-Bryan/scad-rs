use fj::Shape;

#[fj::model]
fn model(filename: String) -> Shape {
    let src = std::fs::read_to_string(&filename).unwrap();
    let tokens = scad_syntax::tokenize(&src);
    let (_pkg, errors) = scad_syntax::parse(tokens);
    assert!(errors.is_empty());

    todo!();
}
