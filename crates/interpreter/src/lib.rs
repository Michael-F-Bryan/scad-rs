use std::collections::HashMap;

use rowan::ast::AstNode;
use scad_syntax::ast::{
    Assignment, Atom, BinOp, Expr, LiteralExpr, LookupExpr, Package, Statement,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Interpreter {
    pkg: Package,
    namespace: HashMap<String, Value>,
}

impl Interpreter {
    pub fn new(pkg: Package) -> Self {
        Interpreter {
            pkg,
            namespace: HashMap::new(),
        }
    }

    pub fn evaluate(&mut self) -> Result<(), Exception> {
        for stmt in self.pkg.statements() {
            self.evaluate_stmt(stmt)?;
        }

        Ok(())
    }

    fn evaluate_stmt(&mut self, stmt: Statement) -> Result<(), Exception> {
        match stmt {
            Statement::Include(_) => todo!(),
            Statement::Use(_) => todo!(),
            Statement::AssignmentStatement(a) => self.evaluate_assignment(a.assignment().unwrap()),
            Statement::NamedFunctionDefinition(_) => todo!(),
            Statement::NamedModuleDefinition(_) => todo!(),
            Statement::ModuleInstantiation(_) => todo!(),
        }
    }

    fn evaluate_assignment(&mut self, a: Assignment) -> Result<(), Exception> {
        let ident = a.ident_token().unwrap();
        dbg!(a.syntax().children().collect::<Vec<_>>());

        dbg!(a.syntax().children().find_map(Expr::cast));
        let value = self.evaluate_expr(a.expr().unwrap())?;
        self.namespace.insert(ident.text().to_string(), value);

        Ok(())
    }

    fn evaluate_expr(&mut self, expr: Expr) -> Result<Value, Exception> {
        match expr {
            Expr::Atom(atom) => self.evaluate_atom(atom),
            Expr::ListExpression(_) => todo!(),
            Expr::RangeExpression(_) => todo!(),
            Expr::UnaryExpr(_) => todo!(),
            Expr::TernaryExpr(_) => todo!(),
            Expr::ParenExpr(_) => todo!(),
            Expr::ListComprehensionExpr(_) => todo!(),
            Expr::LetClause(_) => todo!(),
            Expr::BinExpr(b) => {
                let mut exprs = b.exprs();
                let lhs = self.evaluate_expr(exprs.next().unwrap())?;
                let rhs = self.evaluate_expr(exprs.next().unwrap())?;

                match b.bin_op().unwrap() {
                    BinOp::Plus(_) => add(lhs, rhs),
                    BinOp::Minus(_) => todo!(),
                    BinOp::Star(_) => todo!(),
                    BinOp::Slash(_) => todo!(),
                    BinOp::Percent(_) => todo!(),
                    BinOp::Caret(_) => todo!(),
                    BinOp::GreaterThanEquals(_) => todo!(),
                    BinOp::GreaterThan(_) => todo!(),
                    BinOp::Equals(_) => todo!(),
                    BinOp::LessThanEquals(_) => todo!(),
                    BinOp::LessThan(_) => todo!(),
                    BinOp::And(_) => todo!(),
                    BinOp::Or(_) => todo!(),
                }
            }
        }
    }

    fn evaluate_atom(&mut self, atom: Atom) -> Result<Value, Exception> {
        match atom {
            Atom::LiteralExpr(literal) => self.evaluate_literal(literal),
            Atom::LookupExpr(lookup) => self.lookup(lookup),
            Atom::IndexExpr(_) => todo!(),
            Atom::FunctionCall(_) => todo!(),
        }
    }

    fn evaluate_literal(&mut self, literal: LiteralExpr) -> Result<Value, Exception> {
        match literal {
            LiteralExpr::FalseKw(_) => Ok(Value::Boolean(false)),
            LiteralExpr::TrueKw(_) => Ok(Value::Boolean(true)),
            LiteralExpr::UndefKw(_) => Ok(Value::Null),
            LiteralExpr::Float(f) => Ok(Value::Float(
                f.first_token().unwrap().text().parse().unwrap(),
            )),
            LiteralExpr::Integer(i) => Ok(Value::Integer(
                i.first_token().unwrap().text().parse().unwrap(),
            )),
            LiteralExpr::String(s) => {
                Ok(Value::String(s.first_token().unwrap().text().to_string()))
            }
        }
    }

    fn lookup(&self, lookup: LookupExpr) -> Result<Value, Exception> {
        let path: Vec<_> = lookup
            .ident_tokens()
            .map(|t| t.text().to_string())
            .collect();

        match path.as_slice() {
            [name] => Ok(self.namespace.get(name).unwrap().clone()),
            _ => todo!(),
        }
    }
}

fn add(lhs: Value, rhs: Value) -> Result<Value, Exception> {
    match (lhs, rhs) {
        (Value::Integer(a), Value::Integer(b)) => Ok(Value::Integer(a + b)),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
        (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
        _ => todo!(),
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Null,
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub struct Exception {}

#[cfg(test)]
mod tests {
    use scad_syntax::ParseError;

    use super::*;

    #[test]
    fn it_works() {
        let src = "
            a = 1;
            b = 2;
            c = a + b;
        ";
        let tokens = scad_syntax::tokenize(src);
        let (pkg, errors) = scad_syntax::parse(tokens);
        dbg!(&pkg);
        for ParseError { location, msg } in &errors {
            let token = pkg.syntax().covering_element(*location);
            eprintln!("Error: {msg} at\n{token:?}");
        }
        assert!(errors.is_empty(), "{errors:#?}");

        let mut interpreter = Interpreter::new(pkg);

        interpreter.evaluate().unwrap();

        assert_eq!(interpreter.namespace["c"], Value::Integer(3));
    }
}
