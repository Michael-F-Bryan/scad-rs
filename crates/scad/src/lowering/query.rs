use crate::{
    ast,
    hir::{self, Identifiers},
    syntax::Parse,
    Diagnostics,
};

#[salsa::query_group(LoweringStorage)]
pub trait Lowering: Parse {
    fn hir_package(&self) -> (hir::Package, Diagnostics);
}

fn hir_package(db: &dyn Lowering) -> (hir::Package, Diagnostics) {
    let (ast, _) = db.ast();
    let mut pkg = hir::Package::default();
    let diags = Diagnostics::empty();
    let idents = Identifiers::root();

    for stmt in ast.statements() {
        if let Some(assign) = stmt.as_assignment() {
            let ident = assign.identifier().unwrap();
            let name = ident.name();
            let value = assign.expression().unwrap();

            if let Some(n) = value.number() {
                let id = idents.create(&name);
                let c = hir::Constant { value: number(n) };
                let already_defined = pkg.constants.insert(id.clone(), c).is_some();
                assert!(!already_defined);

                pkg.global_namespace.insert(&name, hir::Value::Constant(id));
            }
        } else {
            todo!();
        }
    }

    (pkg, diags)
}

fn number(n: ast::Number) -> hir::Expr {
    let s = n.literal().to_string();

    if let Ok(integer) = s.parse() {
        hir::Expr::IntegerLiteral(integer)
    } else if let Ok(float) = s.parse() {
        hir::Expr::FloatLiteral(float)
    } else {
        todo!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Default)]
    #[salsa::database(crate::syntax::ParseStorage, LoweringStorage)]
    struct Db {
        storage: salsa::Storage<Self>,
    }

    impl salsa::Database for Db {}

    #[test]
    fn single_assignment() {
        let src = "x = 42;";
        let mut db = Db::default();

        db.set_source_code(src.into());
        let (pkg, diags) = db.hir_package();

        assert!(diags.is_empty());
        assert_eq!(pkg.constants.len(), 1);
        let id = pkg.global_namespace.get("x").unwrap().id();
        assert_eq!(&pkg.constants[&id].value, &hir::Expr::IntegerLiteral(42));
    }

    #[test]
    fn multiple_assignments() {
        let src = "x = 42; y = 0.5;";
        let mut db = Db::default();

        db.set_source_code(src.into());
        let (pkg, diags) = db.hir_package();

        assert!(diags.is_empty());
        assert_eq!(pkg.constants.len(), 2);
        let x = pkg.global_namespace.get("x").unwrap().id();
        assert_eq!(pkg.constants[&x].value, 42);
        let y = pkg.global_namespace.get("y").unwrap().id();
        assert_eq!(pkg.constants[&y].value, 0.5);
    }

    #[test]
    fn empty_package_is_just_a_noop() {
        let src = "";
        let mut db = Db::default();

        db.set_source_code(src.into());
        let (
            hir::Package {
                functions,
                modules,
                constants,
                global_namespace,
                syntax,
                script,
            },
            diags,
        ) = db.hir_package();

        assert!(diags.is_empty());
        assert!(constants.is_empty());
        assert!(functions.is_empty());
        assert!(modules.is_empty());
        assert!(global_namespace.is_empty());
        assert!(syntax.is_empty());
        assert_eq!(script.nodes.len(), 1);
        let cfg_node = &script.nodes[0];
        assert_eq!(cfg_node.instructions.len(), 0);
        assert_eq!(cfg_node.exit, hir::Continuation::Return);
    }
}
