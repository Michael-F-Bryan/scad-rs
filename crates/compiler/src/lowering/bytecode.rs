use rowan::ast::AstNode;
use scad_bytecode::{Chunk, Instruction, Program};
use scad_syntax::{ast, SyntaxKind, SyntaxNode, SyntaxToken};

use crate::{lowering::Lowering, Diagnostics};

pub(crate) fn compile(db: &dyn Lowering) -> (Program, Diagnostics) {
    let mut diags = Diagnostics::empty();

    let src = db.src();
    let (pkg, d) = db.parse(src);
    diags.extend(d);

    let (chunk, d) = db.compile_package(pkg);
    diags.extend(d);

    let program = Program { chunk };
    (program, diags)
}

pub(crate) fn compile_package(_: &dyn Lowering, ast: ast::Package) -> (Chunk, Diagnostics) {
    let diags = Diagnostics::empty();
    let mut chunk = Chunk::empty();

    for stmt in ast.statements() {
        if stmt
            .syntax()
            .descendants()
            .any(|d| d.kind() == SyntaxKind::ERROR)
        {
            todo!("Handle dodgy statements");
        }
        match stmt {
            ast::Statement::AssignmentStatement(a) => compile_assignment(&mut chunk, a),
            ast::Statement::ModuleInstantiation(m) => compile_module_instantiation(&mut chunk, m),
            ast::Statement::NamedFunctionDefinition(_)
            | ast::Statement::NamedModuleDefinition(_)
            | ast::Statement::Include(_)
            | ast::Statement::Use(_)
            | ast::Statement::IfStatement(_)
            | ast::Statement::ForStatement(_) => todo!(),
        }
    }

    // The top-level statements in a package act as a function, so we need to
    // return to the "caller" (i.e. the runtime on startup or during an import).
    // This return statement will be attached to the last line in the file.
    let line_number = match ast.syntax().children().last() {
        Some(last_child) => line_number(&last_child),
        None => 0,
    };
    chunk.push_instruction(Instruction::Undef, line_number);
    chunk.push_instruction(Instruction::Return, line_number);

    (chunk, diags)
}

fn compile_assignment(chunk: &mut Chunk, a: ast::AssignmentStatement) {
    let a = a.assignment().unwrap();
    let line_number = line_number(a.syntax());

    compile_expr(chunk, a.expr().unwrap());

    let constant = chunk.push_constant(a.ident_token().unwrap().text());
    chunk.push_instruction(Instruction::DefineGlobal(constant), line_number);
}

fn compile_expr(chunk: &mut Chunk, expr: ast::Expr) {
    match expr {
        ast::Expr::Atom(ast::Atom::LiteralExpr(lit)) => compile_literal(chunk, lit),
        ast::Expr::Atom(ast::Atom::FunctionCall(call)) => compile_function_call(chunk, call),
        ast::Expr::Atom(ast::Atom::IndexExpr(_)) => todo!(),
        ast::Expr::Atom(ast::Atom::LookupExpr(lookup)) => compile_lookup(chunk, lookup),
        ast::Expr::ListExpr(list) => compile_list(chunk, list),
        ast::Expr::RangeExpr(_) => todo!(),
        ast::Expr::UnaryExpr(_) => todo!(),
        ast::Expr::TernaryExpr(_) => todo!(),
        ast::Expr::ParenExpr(_) => todo!(),
        ast::Expr::ListComprehensionExpr(_) => todo!(),
        ast::Expr::BinExpr(bin) => compile_bin_expr(chunk, bin),
    }
}

fn compile_list(chunk: &mut Chunk, list: ast::ListExpr) {
    // First we add an empty list to the stack
    chunk.push_instruction(Instruction::CreateList, line_number(list.syntax()));

    for expr in list.exprs() {
        // Then execute each expression
        compile_expr(chunk, expr.clone());
        // and append it to the list
        chunk.push_instruction(Instruction::AddToList, line_number(expr.syntax()));
    }
}

fn compile_bin_expr(chunk: &mut Chunk, bin: ast::BinExpr) {
    let mut exprs = bin.exprs();
    let lhs = exprs.next().expect("There should be at least 1 expression");

    // First we evaluate the LHS and push it onto the stack
    compile_expr(chunk, lhs);

    for (op, rhs) in bin.bin_ops().zip(exprs) {
        // then we push the RHS onto the stack
        compile_expr(chunk, rhs);

        // and execute the binary op
        let line_number = line_number(op.syntax());
        match op {
            ast::BinOp::Plus(_) => chunk.push_instruction(Instruction::Add, line_number),
            ast::BinOp::Minus(_) => chunk.push_instruction(Instruction::Sub, line_number),
            ast::BinOp::Star(_) => chunk.push_instruction(Instruction::Mul, line_number),
            ast::BinOp::Slash(_) => chunk.push_instruction(Instruction::Div, line_number),
            ast::BinOp::Percent(_) => todo!(),
            ast::BinOp::Caret(_) => todo!(),
            ast::BinOp::GreaterThanEquals(_) => todo!(),
            ast::BinOp::GreaterThan(_) => todo!(),
            ast::BinOp::DoubleEquals(_) => todo!(),
            ast::BinOp::NotEqual(_) => todo!(),
            ast::BinOp::LessThanEquals(_) => todo!(),
            ast::BinOp::LessThan(_) => todo!(),
            ast::BinOp::And(_) => todo!(),
            ast::BinOp::Or(_) => todo!(),
        }
    }
}

fn compile_lookup(chunk: &mut Chunk, lookup: ast::LookupExpr) {
    let line_number = line_number(lookup.syntax());
    let path: Vec<_> = lookup.ident_tokens().collect();

    let name = match path.as_slice() {
        [name] => name.text(),
        _ => todo!("Handle dotted paths ({})", lookup.syntax().to_string()),
    };

    let constant = chunk.push_constant(name);
    chunk.push_instruction(Instruction::LookupVariable(constant), line_number);
}

fn compile_literal(chunk: &mut Chunk, lit: ast::LiteralExpr) {
    let line_number = line_number(lit.syntax());

    match lit {
        ast::LiteralExpr::TrueKw(_) => chunk.push_instruction(Instruction::True, line_number),
        ast::LiteralExpr::FalseKw(_) => chunk.push_instruction(Instruction::False, line_number),
        ast::LiteralExpr::UndefKw(_) => chunk.push_instruction(Instruction::Undef, line_number),
        ast::LiteralExpr::Integer(node) | ast::LiteralExpr::Float(node) => {
            let number: f64 = node.first_token().unwrap().text().parse().unwrap();
            let constant = chunk.push_constant(number);
            chunk.push_instruction(Instruction::Constant(constant), line_number);
        }
        ast::LiteralExpr::String(node) => {
            let constant = chunk.push_constant(node.first_token().unwrap().text());
            chunk.push_instruction(Instruction::Constant(constant), line_number);
        }
    }
}

fn line_number(_node: &SyntaxNode) -> u16 {
    // TODO: Actually calculate the line number (probably via Salsa so we get
    // caching)
    0
}

fn compile_call(
    chunk: &mut Chunk,
    ident: SyntaxToken,
    args: Option<ast::Arguments>,
    node: &SyntaxNode,
) {
    // When you execute a function/module, first you evaluate its arguments...
    let args: Vec<_> = args.into_iter().flat_map(|a| a.arguments()).collect();
    let arg_count = args.len();

    for arg in args {
        match arg {
            ast::Argument::Expr(expr) => compile_expr(chunk, expr),
            ast::Argument::Assignment(_) => todo!(),
        }
    }

    // ... then you push the function object onto the stack
    let line_number = line_number(node);
    let constant = chunk.push_constant(ident.text());
    chunk.push_instruction(Instruction::LookupVariable(constant), line_number);

    // and finally, we can emit the call
    chunk.push_instruction(
        Instruction::Call(
            arg_count
                .try_into()
                .expect("Function calls don't support more than 255 arguments"),
        ),
        line_number,
    );
}

fn compile_module_instantiation(chunk: &mut Chunk, m: ast::ModuleInstantiation) {
    compile_call(
        chunk,
        m.ident_token().unwrap(),
        m.arguments_opt(),
        m.syntax(),
    );
    chunk.push_instruction(Instruction::SaveGeometry, line_number(m.syntax()));
}

fn compile_function_call(chunk: &mut Chunk, f: ast::FunctionCall) {
    compile_call(
        chunk,
        f.ident_token().unwrap(),
        f.arguments_opt(),
        f.syntax(),
    )
}

#[cfg(test)]
mod tests {
    use scad_bytecode::Disassembler;

    use crate::{db::Database, parsing::Parsing};

    use super::*;

    macro_rules! bytecode_test {
        ($name:ident, $src:literal) => {
            #[test]
            fn $name() {
                let mut db = Database::default();
                let src = $src;
                db.set_src(src.into());
                eprintln!("---- Source ----");
                eprintln!("{src}");

                eprintln!("---- AST ----");
                let (ast, _) = db.parse(src.into());
                eprintln!("{ast:#?}");

                eprintln!("---- Program ----");
                let (program, diags) = db.compile();
                eprintln!("{program:#?}");

                eprintln!("---- Disassembly ----");
                let mut dis = Disassembler::default();
                dis.program(&program);
                let disassembly = dis.finish();
                eprintln!("{disassembly}");

                eprintln!("---- Errors ----");
                eprintln!("{diags:#?}");

                assert!(!diags.has_warnings());

                insta::with_settings! {
                    {
                        description => $src,
                        omit_expression => true,
                    },
                    {
                        insta::assert_display_snapshot!(disassembly);
                    }
                }
            }
        };
    }

    bytecode_test!(
        print_addition_with_variables,
        "x = 42; y = x + 2; print(x);"
    );
    bytecode_test!(
        norm_tests,
        "u=undef; echo(norm([])); echo(norm([1, 2, [1, 3]]));"
    );
}
