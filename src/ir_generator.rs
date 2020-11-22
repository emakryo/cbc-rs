use crate::types::TypeTable;
use crate::ast::Source;
use crate::ir::{Statement, Expr};
use crate::variable_resolver::GlobalScope;

impl Source {
    pub fn transform(&self, types: &TypeTable, scope: &GlobalScope) -> Vec<Statement> {
        todo!()
    }
}
