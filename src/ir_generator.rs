use crate::ast;
use crate::entity::GlobalScope;
use crate::ir;
use crate::types::TypeTable;

impl<'a> ast::Ast<'a> {
    pub fn transform(self, types: &TypeTable, scope: &GlobalScope) -> ir::IR {
        let mut defvars = vec![];
        let mut defuns = vec![];
        let mut funcdecls = vec![];
        for decls in self.declarations {
            match decls {
                ast::Declaration::DefVar(def) => {
                    let e = def.init.as_ref().map(|e| e.transform());
                    defvars.push((def, e));
                }
                _ => todo!(),
            }
        }

        ir::IR {
            defvars,
            defuns,
            funcdecls,
        }
    }
}

impl ast::Expr {
    fn transform(&self) -> ir::Expr {
        todo!()
    }
}
