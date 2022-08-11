use std::collections::HashMap;

use rowan::ast::AstNode;
use scad_syntax::ast::{
    Argument, Assignment, Atom, BinOp, Expr, LiteralExpr, LookupExpr, ModuleInstantiation, Package,
    Statement, VectorExpr,
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

    pub fn evaluate(&mut self) -> Result<Vec<Geometry>, Exception> {
        let mut geometry = Vec::new();

        for stmt in self.pkg.statements() {
            if let Some(geom) = self.evaluate_stmt(stmt)? {
                geometry.push(geom);
            }
        }

        Ok(geometry)
    }

    fn evaluate_stmt(&mut self, stmt: Statement) -> Result<Option<Geometry>, Exception> {
        match stmt {
            Statement::Include(_) => todo!(),
            Statement::Use(_) => todo!(),
            Statement::AssignmentStatement(a) => {
                self.evaluate_assignment(a.assignment().unwrap())?;
                Ok(None)
            }
            Statement::NamedFunctionDefinition(_) => todo!(),
            Statement::NamedModuleDefinition(_) => todo!(),
            Statement::ModuleInstantiation(m) => {
                let geom = self.evaluate_module_instantiation(m)?;
                Ok(Some(geom))
            }
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
            Expr::VectorExpr(vector) => self.evaluate_vector(vector),
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

    fn evaluate_vector(&mut self, vector: VectorExpr) -> Result<Value, Exception> {
        let mut values = Vec::new();

        for expr in vector.expressions().unwrap().exprs() {
            values.push(self.evaluate_expr(expr)?);
        }

        Ok(Value::Vector(values))
    }

    fn evaluate_module_instantiation(
        &mut self,
        m: ModuleInstantiation,
    ) -> Result<Geometry, Exception> {
        let arguments = m
            .arguments_opt()
            .map(|args| args.arguments().collect::<Vec<_>>())
            .unwrap_or_default();

        let mut args = Vec::new();
        for arg in arguments {
            let expr = match arg {
                Argument::Expr(expr) => self.evaluate_expr(expr)?,
                Argument::Assignment(_) => todo!(),
            };
            args.push(expr);
        }

        match m.ident_token().unwrap().text() {
            "cube" => self.evaluate_cube(args),
            _ => todo!(),
        }
    }

    fn evaluate_cube(&self, args: Vec<Value>) -> Result<Geometry, Exception> {
        assert_eq!(args.len(), 1);
        let vector = match &args[0] {
            Value::Vector(v) => v,
            _ => todo!(),
        };
        let args: Vec<f64> = vector.iter().map(|v| v.as_float().unwrap()).collect();

        match *args.as_slice() {
            [height, width, depth] => Ok(Geometry::Cube {
                width,
                height,
                depth,
            }),
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
    Vector(Vec<Value>),
}

impl Value {
    fn as_float(&self) -> Option<f64> {
        match *self {
            Value::Integer(i) => Some(i as f64),
            Value::Float(f) => Some(f),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Geometry {
    Cube { width: f64, height: f64, depth: f64 },
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
            cube([a, b, c]);
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
