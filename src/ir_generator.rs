use crate::ast;
use crate::entity::GlobalScope;
use crate::error::Error;
use crate::ir;
use crate::types::TypeTable;

impl<'a> ast::Ast<'a> {
    pub fn transform<'b>(
        self,
        types: &TypeTable,
        scope: &'b GlobalScope,
    ) -> Result<ir::IR<'b>, Error> {
        let mut defvars = vec![];
        let mut defuns = vec![];
        let mut funcdecls = vec![];
        for decls in self.declarations {
            match decls {
                ast::Declaration::DefVar(def) => {
                    let e = def.init.as_ref().map(|e| e.transform());
                    defvars.push((def, e.map_or(Ok(None), |v| v.map(Some))?));
                }
                ast::Declaration::Defun(def, block) => {
                    let stmts = block.transform()?;
                    defuns.push((def, stmts));
                }
                ast::Declaration::FuncDecl(def) => {
                    funcdecls.push(def);
                }
                _ => todo!(),
            }
        }

        Ok(ir::IR {
            defvars,
            defuns,
            funcdecls,
            scope,
        })
    }
}

impl ast::Expr {
    fn transform(&self) -> Result<ir::Expr, Error> {
        todo!()
    }
}

impl ast::Block {
    fn transform(&self) -> Result<Vec<ir::Statement>, Error> {
        todo!()
    }
}
