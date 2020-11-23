use crate::ast;
use crate::entity::GlobalScope;
use crate::error::Error;
use crate::ir;
use crate::types::{TypeTable, TypeCell};

impl<'a> ast::Ast<'a, ast::TypedExpr> {
    pub fn transform(self, types: &TypeTable, scope: &GlobalScope) -> Result<ir::IR, Error> {
        let mut defvars = vec![];
        let mut defuns = vec![];
        let mut funcdecls = vec![];
        for decls in self.declarations {
            match decls {
                ast::Declaration::DefVar(def) => {
                    let mut stmts = vec![];
                    let e = def.init.as_ref().map(|e| e.transform(&mut stmts));
                    if stmts.len() > 0 {
                        return Err(Error::Semantic("Invalid initial value for global variable".into()));
                    }
                    defvars.push((def, e.map_or(Ok(None), |v| v.map(Some))?));
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

fn assign(stmts: &mut Vec<ir::Statement>, lhs: ir::Expr, rhs: ir::Expr) {
    todo!()
}

// fn new_var(type_: TypeRef) -> ir::Expr {
//     todo!()
// }

// impl ast::Expr {
//     fn transform(&self, stmts: &mut Vec<ir::Statement>) -> Result<ir::Expr, Error> {
//         match self {
//             Assign(lhs, rhs) => {
//                 let lhs = lhs.transform(stmts)?;
//                 let rhs = rhs.transform(stmts)?;
//                 let tmp = new_var(lhs.type_);

//                 assign(stmts, tmp, lhs);
//                 assign(stmts, rhs, tmp);

//             },
//             _ => todo!()

//         }
//         todo!()
//     }
// }

// impl ast::Block {
//     fn transform(&self, stmts: &mut Vec<ir::Statement>) -> Result<(), Error> {
//         todo!()
//     }
// }
