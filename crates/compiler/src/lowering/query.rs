use im::{OrdMap, Vector};
use rowan::{ast::AstNode, SyntaxNode};
use scad_syntax::{ast, OpenSCAD};

use crate::{
    diagnostics::DuplicateSymbol,
    hir::{self, Spanned},
    parsing::Parsing,
    Diagnostics, Text,
};

/// Lowering from the [`ast`] representation to [`hir`].
#[salsa::query_group(LoweringStorage)]
pub trait Lowering: Parsing {
    fn top_level_items(&self) -> (OrdMap<Text, hir::Item>, Diagnostics);

    fn function_arguments(
        &self,
        node: ast::NamedFunctionDefinition,
    ) -> (Vector<hir::Argument>, Diagnostics);

    fn module_arguments(
        &self,
        node: ast::NamedModuleDefinition,
    ) -> (Vector<hir::Argument>, Diagnostics);

    /// Get all names that are accessible from a particular node.
    fn named_items_in_scope(
        &self,
        target: SyntaxNode<OpenSCAD>,
    ) -> Vector<(Text, SyntaxNode<OpenSCAD>)>;
}

fn top_level_items(db: &dyn Lowering) -> (OrdMap<Text, hir::Item>, Diagnostics) {
    let (pkg, mut diags) = db.ast();
    let mut items: OrdMap<Text, hir::Item> = OrdMap::new();

    for stmt in pkg.statements() {
        if let Some((name, item)) = item_name(stmt) {
            let span = item.span();

            if let Some(original) = items.insert(name.clone(), item) {
                diags.push(DuplicateSymbol {
                    name,
                    original_definition: original.span(),
                    duplicate_definition: span,
                });
            }
        }
    }

    (items, diags)
}

fn item_name(stmt: ast::Statement) -> Option<(Text, hir::Item)> {
    let item = match stmt {
        ast::Statement::AssignmentStatement(a) => hir::Item::Constant(a),
        ast::Statement::NamedFunctionDefinition(f) => hir::Item::Function(f),
        ast::Statement::NamedModuleDefinition(m) => hir::Item::Module(m),
        ast::Statement::ModuleInstantiation(_) | ast::Statement::IfStatement(_) => return None,
        ast::Statement::Include(_) | ast::Statement::Use(_) => {
            todo!("Add imported functions/modules")
        }
    };

    let name = item.name()?;
    Some((name, item))
}

fn function_arguments(
    _: &dyn Lowering,
    node: ast::NamedFunctionDefinition,
) -> (Vector<hir::Argument>, Diagnostics) {
    extract_parameters(node.parameters_opt())
}

fn module_arguments(
    _: &dyn Lowering,
    node: ast::NamedModuleDefinition,
) -> (Vector<hir::Argument>, Diagnostics) {
    extract_parameters(node.parameters_opt())
}

fn extract_parameters(params: Option<ast::Parameters>) -> (Vector<hir::Argument>, Diagnostics) {
    let params = params.into_iter().flat_map(|p| p.parameters());

    let mut arguments = Vector::new();

    for param in params {
        let name: Text = match &param {
            ast::Parameter::Ident(name) => name.text().into(),
            ast::Parameter::Assignment(ass) => ass.ident_token().unwrap().text().into(),
        };
        arguments.push_back(hir::Argument {
            name,
            span: param.syntax().text_range(),
        });
    }

    (arguments, Diagnostics::empty())
}

fn named_items_in_scope(
    db: &dyn Lowering,
    target: SyntaxNode<OpenSCAD>,
) -> Vector<(Text, SyntaxNode<OpenSCAD>)> {
    let parent = match target.parent() {
        Some(p) => p,
        None => return Vector::new(),
    };

    let mut named_items: Vector<(Text, SyntaxNode<OpenSCAD>)> = Vector::new();

    // First, get any items that were defined before this one
    for child in parent.children() {
        let reached_target = child == target;
        if let Some(item) = hir::Item::cast(child) && let Some(name) = item.name() {
            named_items.push_front((name, item.syntax().clone()));
        }

        if reached_target {
            break;
        }
    }

    // next, make sure any parameters defined by the parent are added
    if let Some(function_like) = FunctionLike::cast(parent.clone()) {
        for param in function_like.parameters() {
            if let Some(name) = param.ident() {
                named_items.push_back((Text::new(name.text()), param.syntax().clone()));
            }
        }
    }

    // Recursively add any nodes defined in the parent scope
    named_items.append(db.named_items_in_scope(parent));

    named_items
}

ast_node_union! {
    #[derive(Debug)]
    enum FunctionLike {
        Function(ast::NamedFunctionDefinition),
        Module(ast::NamedModuleDefinition),
    }
}

impl FunctionLike {
    fn parameters(&self) -> impl Iterator<Item = ast::Parameter> {
        let parameters = match self {
            FunctionLike::Function(f) => f.parameters_opt(),
            FunctionLike::Module(m) => m.parameters_opt(),
        };
        parameters.into_iter().flat_map(|p| p.parameters())
    }
}

#[cfg(test)]
mod tests {
    use crate::db::Database;

    use super::*;

    #[test]
    fn named_items_in_scope() {
        let mut db = Database::default();
        db.set_src(Text::new(
            "
            a = 1;
            b = 2;

            function c(d, e) = f;
        ",
        ));
        let (items, _) = db.top_level_items();

        let c = items["c"].syntax();
        let names = db.named_items_in_scope(c.clone());
        // when `c` is defined, the only variables in scope should be itself,
        // `a` and `b`.
        assert_eq!(names.len(), 3);
        assert_eq!(names[0].0, "c");
        assert_eq!(names[1].0, "b");
        assert_eq!(names[2].0, "a");

        let c = ast::NamedFunctionDefinition::cast(c.clone()).unwrap();
        let f = c.expr().unwrap();

        let names = db.named_items_in_scope(f.syntax().clone());
        // We should see `d` and `e`.
        assert_eq!(names.len(), 5);
        assert_eq!(names[0].0, "d");
        assert_eq!(names[1].0, "e");
        // plus everything from before
        assert_eq!(names[2].0, "c");
        assert_eq!(names[3].0, "b");
        assert_eq!(names[4].0, "a");
    }
}
