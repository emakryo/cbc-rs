use crate::ast;
use crate::entity::GlobalScope;
use crate::error::Error;
use crate::ir;
use crate::types::TypeCell;

impl<'a, 'b> ast::Ast<'a, ast::TypedExpr<'b>, TypeCell<'b>> {
    pub fn transform(self, scope: &GlobalScope) -> Result<ir::IR<'b>, Error> {
        let mut defvars = vec![];
        let mut defuns = vec![];
        let mut funcdecls = vec![];
        for decls in self.declarations {
            match decls {
                ast::Declaration::DefVar(def) => {
                    let mut stmts = vec![];
                    let e = def.init.map(|e| e.transform(&mut stmts));
                    if stmts.len() > 0 {
                        return Err(Error::Semantic("Invalid initial value for global variable".into()));
                    }
                    defvars.push(ast::DefVar {
                        storage: def.storage,
                        type_: def.type_,
                        name: def.name,
                        init: e.map_or(Ok(None), |v| v.map(Some))?,
                    });
                }
                ast::Declaration::Defun(def, block) => {
                    let mut stmts = vec![];
                    block.transform(&mut stmts)?;
                    defuns.push((def, stmts));
                }
                ast::Declaration::FuncDecl(def) => {
                    funcdecls.push(def);
                }
                _ => (),
            }
        }

        Ok(ir::IR {
            defvars,
            defuns,
            funcdecls,
        })
    }
}

fn assign<'a>(stmts: &mut Vec<ir::Statement<'a>>, lhs: ir::Expr<'a>, rhs: ir::Expr<'a>) -> ir::Expr<'a> {
    todo!()
}

// fn new_var(type_: TypeRef) -> ir::Expr {
//     todo!()
// }

impl<'a> ast::TypedExpr<'a> {
    fn transform(self, stmts: &mut Vec<ir::Statement<'a>>) -> Result<ir::Expr<'a>, Error> {
        use ast::BaseExpr::*;
        let e = match *self.inner {
            Assign(lhs, rhs) => {
                let lhs = lhs.transform(stmts)?;
                let rhs = rhs.transform(stmts)?;
                assign(stmts, lhs, rhs)
            }
            _ => todo!(),
        };

        Ok(e)
    }
}

impl<'a> ast::Block<ast::TypedExpr<'a>, TypeCell<'a>> {
    fn transform(self, stmts: &mut Vec<ir::Statement>) -> Result<(), Error> {
        todo!()
    }
}

#[cfg(test)]
mod test{
    use super::*;
    use crate::parser::parse_source;
    use crate::variable_resolver::resolve_variables;
    use crate::type_resolver::resolve_types;
    use crate::dereference_checker::check_dereference;
    use std::path::Path;

    fn test_from_file(file_name: &Path) {
        use std::io::Read;

        dbg!(file_name);
        let mut code = String::new();
        std::fs::File::open(&file_name)
            .unwrap()
            .read_to_string(&mut code)
            .unwrap();
        let mut header_paths = vec!["cbc-1.0/import"];
        if let Some(p) = file_name.parent() {
            header_paths.push(p.to_str().unwrap());
        }
        let mut ast = parse_source(&code, &header_paths).unwrap();
        let scope = resolve_variables(&mut ast);
        if scope.is_err() {
            dbg!(scope).ok();
            return;
        }
        let scope = scope.unwrap();
        let arena = crate::types::TypeArena::new();
        let ast = resolve_types(ast, &arena, &scope);
        if ast.is_err() {
            dbg!(ast).ok();
            return;
        }
        let ast = ast.unwrap();

        let verdict = check_dereference(&ast);
        if verdict.is_err() {
            dbg!(verdict).ok();
            return;
        }

        let ir = ast.transform(&scope);
        if ir.is_err() {
            dbg!(ir).ok();
            return
        }        
    }

    //#[test]
    fn test_from_files() {
        let root = env!("CARGO_MANIFEST_DIR");

        for file_name in glob::glob(&format!("{}/cbc-1.0/test/*.cb", root)).unwrap() {
            test_from_file(&file_name.unwrap());
        }
    }
}