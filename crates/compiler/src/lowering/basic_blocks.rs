use im::Vector;
use noisy_float::types::R64;
use rowan::ast::AstNode;
use scad_syntax::{ast, SyntaxKind};

use crate::{hir, lowering::Lowering};

pub(crate) fn basic_blocks(
    db: &dyn Lowering,
    statements: Vector<ast::Statement>,
) -> Vector<hir::BasicBlock> {
    let mut builder = Builder {
        db,
        next_temp: 0,
        basic_blocks: Vector::new(),
        stmt_buffer: Vector::new(),
    };

    for stmt in statements {
        builder.lower_statement(stmt);
    }

    builder.finish()
}

struct Builder<'db> {
    db: &'db dyn Lowering,
    next_temp: usize,
    basic_blocks: Vector<hir::BasicBlock>,
    stmt_buffer: Vector<hir::Statement>,
}

impl<'db> Builder<'db> {
    fn push_stmt(&mut self, stmt: hir::Statement) {
        self.stmt_buffer.push_back(stmt);
    }

    fn finish(self) -> Vector<hir::BasicBlock> {
        let Builder {
            mut basic_blocks,
            stmt_buffer,
            ..
        } = self;
        let bb = hir::BasicBlock {
            statements: stmt_buffer,
        };
        basic_blocks.push_back(bb);

        basic_blocks
    }

    fn lower_statement(&mut self, stmt: ast::Statement) {
        match stmt {
            ast::Statement::Include(_) => todo!("Include basic blocks from this file"),
            ast::Statement::AssignmentStatement(a) => {
                if let Some(stmt) = lower_assignment(self, a) {
                    self.push_stmt(stmt);
                }
            }
            ast::Statement::ModuleInstantiation(m) => {
                if let Some(stmt) = lower_module_instantiation(self, m) {
                    self.push_stmt(stmt);
                }
            }
            ast::Statement::IfStatement(_) => todo!(),
            ast::Statement::Use(_)
            | ast::Statement::NamedFunctionDefinition(_)
            | ast::Statement::NamedModuleDefinition(_) => {}
        }
    }

    fn temp(&mut self) -> hir::Variable {
        let index = self.next_temp;
        self.next_temp += 1;
        hir::Variable::Anonymous(index)
    }
}

fn lower_module_instantiation(
    b: &mut Builder<'_>,
    m: ast::ModuleInstantiation,
) -> Option<hir::Statement> {
    let child: Option<hir::Variable> = m.child().and_then(|c| lower_child(b, c));

    let ident = m.ident_token()?;
    let mut inputs = Vector::new();

    // evaluate the inputs
    for arg in m.arguments_opt()?.arguments() {
        match arg {
            ast::Argument::Expr(e) => {
                let var = lower_expr(b, e)?;
                inputs.push_back(hir::Input::Anonymous(var));
            }
            ast::Argument::Assignment(a) => {
                let name = a.ident_token()?.text().into();
                let value = lower_expr(b, a.expr()?)?;
                inputs.push_back(hir::Input::Named { name, value });
            }
        }
    }

    let def_id =
        b.db.ident_declaration(ident.text().into(), m.syntax().clone())?;
    let definition = match b.db.lookup_declaration(def_id) {
        hir::NameDefinitionSite::Module(def) => def,
        _ => todo!("Emit an error"),
    };

    let params: Vector<_> = definition.parameters_opt()?.parameters().collect();

    if params.len() != inputs.len() {
        todo!("Handle different numbers of parameters");
    }
    if !params.iter().all(|p| matches!(p, ast::Parameter::Ident(_)))
        || !inputs.iter().all(|i| matches!(i, hir::Input::Anonymous(_)))
    {
        todo!("Handle named arguments");
    }

    Some(hir::Statement::ModuleInvocation {
        dest: b.temp(),
        module: def_id,
        inputs,
        child,
    })
}

fn lower_child(_: &mut Builder<'_>, child: ast::Child) -> Option<hir::Variable> {
    match child {
        ast::Child::Semicolon(_) => None,
        ast::Child::BracedChildren(_) => todo!(),
        ast::Child::ModuleInstantiation(_) => todo!(),
    }
}

fn lower_assignment(b: &mut Builder<'_>, a: ast::AssignmentStatement) -> Option<hir::Statement> {
    let a = a.assignment()?;

    let ident = a.ident_token()?;
    let name = ident.text();

    let expr = a.expr()?;
    let value: hir::AssignmentValue = lower_expr(b, expr)?;

    let def_id = b.db.ident_declaration(name.into(), a.syntax().clone())?;

    Some(hir::Statement::Assignment {
        variable_name: hir::Variable::Named(def_id),
        value,
    })
}

fn lower_expr(b: &mut Builder<'_>, expr: ast::Expr) -> Option<hir::AssignmentValue> {
    match expr {
        ast::Expr::Atom(ast::Atom::LiteralExpr(literal)) => lower_literal(literal),
        ast::Expr::Atom(ast::Atom::LookupExpr(lookup)) => lower_variable_lookup(b, lookup),
        ast::Expr::BinExpr(bin) => lower_bin_expr(b, bin),
        _ => todo!(),
    }
}

fn lower_bin_expr(b: &mut Builder<'_>, bin: ast::BinExpr) -> Option<hir::AssignmentValue> {
    let mut exprs = bin.exprs();
    let lhs = lower_expr(b, exprs.next()?)?;
    let rhs = lower_expr(b, exprs.next()?)?;

    let dest = b.temp();

    b.push_stmt(hir::Statement::BinaryOp {
        dest: dest.clone(),
        lhs,
        op: translate_bin_op(bin.bin_op()?),
        rhs,
    });

    Some(dest.into())
}

fn translate_bin_op(op: ast::BinOp) -> hir::BinOp {
    match op {
        ast::BinOp::Plus(_) => hir::BinOp::Plus,
        ast::BinOp::Minus(_) => hir::BinOp::Minus,
        ast::BinOp::Star(_) => hir::BinOp::Star,
        ast::BinOp::Slash(_) => hir::BinOp::Slash,
        ast::BinOp::Percent(_) => hir::BinOp::Percent,
        ast::BinOp::Caret(_) => hir::BinOp::Caret,
        ast::BinOp::GreaterThanEquals(_) => hir::BinOp::GreaterThanEquals,
        ast::BinOp::GreaterThan(_) => hir::BinOp::GreaterThan,
        ast::BinOp::DoubleEquals(_) => hir::BinOp::DoubleEquals,
        ast::BinOp::LessThanEquals(_) => hir::BinOp::LessThanEquals,
        ast::BinOp::LessThan(_) => hir::BinOp::LessThan,
        ast::BinOp::And(_) => hir::BinOp::And,
        ast::BinOp::Or(_) => hir::BinOp::Or,
    }
}

fn lower_literal(literal: ast::LiteralExpr) -> Option<hir::AssignmentValue> {
    match literal {
        ast::LiteralExpr::Integer(i) => {
            let tok = i.first_token()?;
            debug_assert_eq!(tok.kind(), SyntaxKind::INTEGER);

            Some(hir::AssignmentValue::Literal(hir::Literal::Integer(
                tok.text().parse().unwrap(),
            )))
        }
        ast::LiteralExpr::Float(f) => {
            let tok = f.first_token()?;
            debug_assert_eq!(tok.kind(), SyntaxKind::FLOAT);
            let float: f64 = tok.text().parse().unwrap();

            Some(hir::AssignmentValue::Literal(hir::Literal::Float(
                R64::unchecked_new(float),
            )))
        }
        ast::LiteralExpr::TrueKw(_) => {
            Some(hir::AssignmentValue::Literal(hir::Literal::Boolean(true)))
        }
        ast::LiteralExpr::FalseKw(_) => {
            Some(hir::AssignmentValue::Literal(hir::Literal::Boolean(false)))
        }
        _ => todo!(),
    }
}

fn lower_variable_lookup(b: &Builder<'_>, lookup: ast::LookupExpr) -> Option<hir::AssignmentValue> {
    let mut path: Vector<_> = lookup.ident_tokens().collect();
    let first = path.pop_front()?;

    let name = first.text().into();
    let def_id = b.db.ident_declaration(name, lookup.syntax().clone())?;

    let item = hir::AssignmentValue::Variable(hir::Variable::Named(def_id));

    for _segment in path {
        todo!("Handle dotted field access");
    }
    Some(item)
}

#[cfg(test)]
mod tests {
    use crate::{db::Database, parsing::Parsing};

    use super::*;

    #[test]
    fn no_statements() {
        let src = "";
        let mut db = Database::default();
        db.set_src(src.into());
        let (pkg, _) = db.ast();

        let statements = pkg.statements().collect();
        let basic_blocks = db.basic_blocks(statements);

        assert_eq!(basic_blocks.len(), 1);
    }

    #[test]
    fn single_assignment() {
        let src = "x = 5;";
        let mut db = Database::default();
        db.set_src(src.into());
        let (pkg, _) = db.ast();

        let statements: Vector<_> = pkg.statements().collect();
        let scope = statements.last().unwrap().syntax().clone();
        let basic_blocks = db.basic_blocks(statements);

        assert_eq!(basic_blocks.len(), 1);
        let bb = &basic_blocks[0];
        assert_eq!(bb.statements.len(), 1);
        assert_eq!(
            bb.statements[0],
            hir::Statement::Assignment {
                variable_name: hir::Variable::Named(
                    db.ident_declaration("x".into(), scope).unwrap()
                ),
                value: hir::AssignmentValue::Literal(hir::Literal::Integer(5)),
            }
        );
    }

    #[test]
    #[ignore]
    fn assign_x_to_y() {
        let src = "x = 5; y = x;";
        let mut db = Database::default();
        db.set_src(src.into());
        let (pkg, _) = db.ast();

        let statements: Vector<_> = pkg.statements().collect();
        let scope = statements.last().unwrap().syntax().clone();
        let basic_blocks = db.basic_blocks(statements);

        assert_eq!(basic_blocks.len(), 1);
        let bb = &basic_blocks[0];
        assert_eq!(bb.statements.len(), 2);
        assert_eq!(
            bb.statements[0],
            hir::Statement::Assignment {
                variable_name: hir::Variable::Named(
                    db.ident_declaration("y".into(), scope).unwrap()
                ),
                value: hir::AssignmentValue::Literal(hir::Literal::Integer(5)),
            }
        );
        assert_eq!(
            bb.statements[1],
            hir::Statement::Assignment {
                variable_name: hir::Variable::Named(
                    db.ident_declaration("y".into(), pkg.syntax().clone())
                        .unwrap()
                ),
                value: hir::AssignmentValue::Variable(hir::Variable::Named(
                    db.ident_declaration("x".into(), pkg.syntax().clone())
                        .unwrap()
                )),
            }
        );
    }

    #[test]
    fn function_call() {
        let src = "
            module assert(condition) {}
            assert(true);
        ";
        let mut db = Database::default();
        db.set_src(src.into());
        let (pkg, _) = db.ast();

        let statements: Vector<_> = pkg.statements().collect();
        let scope = statements[0].syntax().clone();
        let basic_blocks = db.basic_blocks(statements);

        assert_eq!(basic_blocks.len(), 1);
        let bb = &basic_blocks[0];
        assert_eq!(bb.statements.len(), 1);

        assert_eq!(
            bb.statements[0],
            hir::Statement::ModuleInvocation {
                dest: hir::Variable::Anonymous(0),
                module: db.ident_declaration("assert".into(), scope).unwrap(),
                inputs: [hir::Input::Anonymous(hir::AssignmentValue::Literal(
                    hir::Literal::Boolean(true)
                ))]
                .into_iter()
                .collect(),
                child: None,
            }
        );
    }

    #[test]
    #[ignore = "Not implemented"]
    fn if_statement() {
        let src = "if (true) {}";
        let mut db = Database::default();
        db.set_src(src.into());
        let (pkg, _) = db.ast();

        let statements: Vector<_> = pkg.statements().collect();
        let scope = statements[0].syntax().clone();
        let basic_blocks = db.basic_blocks(statements);

        assert_eq!(basic_blocks.len(), 2);
        let bb = &basic_blocks[0];
        assert_eq!(bb.statements.len(), 1);

        assert_eq!(
            bb.statements[0],
            hir::Statement::ModuleInvocation {
                dest: hir::Variable::Anonymous(0),
                module: db.ident_declaration("assert".into(), scope).unwrap(),
                inputs: [hir::Input::Anonymous(hir::AssignmentValue::Literal(
                    hir::Literal::Boolean(true)
                ))]
                .into_iter()
                .collect(),
                child: None,
            }
        );
    }
}
