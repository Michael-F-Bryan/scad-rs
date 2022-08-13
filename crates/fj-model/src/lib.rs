use fj::{Group, Shape, Sketch, Sweep};
use scad_interpreter::{Builtin, Geometry};

#[fj::model]
fn model(filename: String) -> Shape {
    let src = std::fs::read_to_string(&filename).unwrap();
    let tokens = scad_syntax::tokenize(&src);
    let (pkg, errors) = scad_syntax::parse(tokens);
    assert!(errors.is_empty());

    let mut interpreter = scad_interpreter::Interpreter::new(pkg, Builtin::default());
    let geometry = interpreter.evaluate().unwrap();

    to_shape(&geometry).unwrap()
}

fn to_shape(geometry: &[Geometry]) -> Option<Shape> {
    let (first, rest) = geometry.split_first()?;
    let a = geometry_to_shape(first);
    match to_shape(rest) {
        Some(b) => Some(Group { a, b }.into()),
        None => Some(a),
    }
}

fn geometry_to_shape(g: &Geometry) -> Shape {
    match *g {
        Geometry::Cube {
            width,
            height,
            depth,
        } => {
            let rectangle = Sketch::from_points(vec![
                [-width / 2.0, -height / 2.0],
                [width / 2.0, -height / 2.0],
                [width / 2.0, height / 2.0],
                [-width / 2.0, height / 2.0],
            ])
            .with_color([100, 255, 0, 200]);
            Sweep::from_path(rectangle.into(), [0.0, 0.0, depth]).into()
        }
    }
}
