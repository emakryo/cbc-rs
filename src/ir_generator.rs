use crate::ast::Ast;
use crate::entity::GlobalScope;
use crate::ir::{Expr, Statement, IR};
use crate::types::TypeTable;

impl<'a> Ast<'a> {
    pub fn transform(&self, types: &TypeTable, scope: &GlobalScope) -> IR {
        for decs in &self.declarations {
            todo!();
        }

        todo!()
    }
}
