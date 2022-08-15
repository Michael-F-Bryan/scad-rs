use rowan::ast::AstNode;
use scad_syntax::ast;

use crate::{hir, lowering::Lowering};

pub(crate) fn inferred_type(db: &dyn Lowering, expr: ast::Expr) -> hir::Type {
    match expr {
        ast::Expr::Atom(ast::Atom::LiteralExpr(
            ast::LiteralExpr::FalseKw(_) | ast::LiteralExpr::TrueKw(_),
        )) => hir::Type::Boolean,
        ast::Expr::Atom(ast::Atom::LiteralExpr(ast::LiteralExpr::Float(_))) => hir::Type::Float,
        ast::Expr::Atom(ast::Atom::LiteralExpr(ast::LiteralExpr::Integer(_))) => hir::Type::Integer,
        ast::Expr::Atom(ast::Atom::LiteralExpr(ast::LiteralExpr::String(_))) => hir::Type::String,
        ast::Expr::Atom(ast::Atom::LiteralExpr(ast::LiteralExpr::UndefKw(_))) => {
            hir::Type::Undefined
        }
        ast::Expr::Atom(ast::Atom::LookupExpr(lookup)) => {
            let path: Vec<_> = lookup.ident_tokens().collect();
            match path.as_slice() {
                [name] => {
                    // we'll need to lookup the item's type
                    let name = name.text();
                    let names_in_scope = db.named_items_in_scope(lookup.syntax().clone());
                    if let Some(definition) = names_in_scope
                        .iter()
                        .find_map(|(n, s)| (name == n).then_some(s))
                         && let Some(body) = definition.body()
                    {
                        db.inferred_type(body)
                    } else {
                        // reference error
                        hir::Type::Error
                    }
                }
                _ => todo!(),
            }
        }
        ast::Expr::Atom(ast::Atom::FunctionCall(_)) => todo!(),
        ast::Expr::Atom(ast::Atom::IndexExpr(_)) => todo!(),
        ast::Expr::ListExpr(_) => todo!(),
        ast::Expr::RangeExpr(_) => todo!(),
        ast::Expr::UnaryExpr(unary) => unary
            .expr()
            .map(|e| db.inferred_type(e))
            .unwrap_or(hir::Type::Error),
        ast::Expr::TernaryExpr(_) => todo!(),
        ast::Expr::ParenExpr(_) => todo!(),
        ast::Expr::ListComprehensionExpr(_) => todo!(),
        ast::Expr::BinExpr(bin) => {
            let [lhs, rhs]: [ast::Expr; 2] = bin.exprs().collect::<Vec<_>>().try_into().unwrap();
            let lhs = db.inferred_type(lhs);
            let rhs = db.inferred_type(rhs);

            if lhs.is_error() || rhs.is_error() {
                hir::Type::Error
            } else if lhs.is_numeric() && rhs.is_numeric() {
                widen_integers(lhs, rhs)
            } else {
                hir::Type::Error
            }
        }
    }
}

fn widen_integers(lhs: hir::Type, rhs: hir::Type) -> hir::Type {
    match (lhs, rhs) {
        (hir::Type::Integer, hir::Type::Integer) => hir::Type::Integer,
        (hir::Type::Float, hir::Type::Integer) | (hir::Type::Integer, hir::Type::Float) => {
            hir::Type::Float
        }
        (hir::Type::Float, hir::Type::Float) => hir::Type::Float,
        (lhs, rhs) if lhs == rhs => lhs,
        _ => hir::Type::Error,
    }
}

#[cfg(test)]
mod tests {
    use crate::{db::Database, parsing::Parsing, Text};

    use super::*;

    #[test]
    fn infer_types() {
        let mut db = Database::default();
        db.set_src(Text::new(
            "
            a = 1;
            b = 2;
            c = a + b;

            // Note: It's okay if we don't know the type for `e` because it's
            // unused.
            function d(e) = c;

            function f(g = 3.14) = g + c;
        ",
        ));
        let (items, _) = db.top_level_items();

        let a = assignment_expr(&items["a"]).unwrap();
        assert_eq!(db.inferred_type(a), hir::Type::Integer);

        let b = assignment_expr(&items["b"]).unwrap();
        assert_eq!(db.inferred_type(b), hir::Type::Integer);

        let c = assignment_expr(&items["c"]).unwrap();
        assert_eq!(db.inferred_type(c), hir::Type::Integer);

        let d_body = items["d"].as_function().unwrap().expr().unwrap();
        assert_eq!(db.inferred_type(d_body), hir::Type::Integer);

        let f = items["f"].as_function().unwrap();
        let f_body = f.expr().unwrap();
        assert_eq!(db.inferred_type(f_body.clone()), hir::Type::Float);
    }

    fn assignment_expr(item: &hir::Item) -> Option<ast::Expr> {
        item.as_constant()?.assignment()?.expr()
    }
}
