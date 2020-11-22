use crate::ast::Ast;
use crate::entity::GlobalScope;
use crate::ir::{Expr, Statement};
use crate::types::TypeTable;

impl<'a> Ast<'a> {
    pub fn transform(&self, types: &TypeTable, scope: &GlobalScope) -> Vec<Statement> {
        for decs in &self.declarations {
            todo!();
        }

        todo!()
    }
}
