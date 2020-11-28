use crate::ast::{DefVar, Defun};
use crate::entity::Entity;
use crate::error::Error;
use crate::types::TypeCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug)]
pub enum Statement<'a> {
    Assign {
        lhs: Expr<'a>,
        rhs: Expr<'a>,
    },
    CJump {
        cond: Expr<'a>,
        then: Label,
        else_: Label,
    },
    Jump(Label),
    Switch {
        cond: Expr<'a>,
        cases: Vec<(Expr<'a>, Label)>,
        default: Label,
    },
    Label(Label),
    Expr(Expr<'a>),
    Return(Expr<'a>),
}

#[derive(Debug, Clone)]
pub enum BaseExpr<'a> {
    UniOp {
        op: UniOp,
        expr: Box<Expr<'a>>,
    },
    BinOp {
        op: BinOp,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
    Call {
        expr: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
    },
    Addr(Box<Expr<'a>>),
    Mem(Box<Expr<'a>>),
    Var(Rc<Entity<TypeCell<'a>>>),
    Int(isize),
    Str(String),
}

#[derive(Debug, Clone)]
pub struct Expr<'a> {
    pub base: BaseExpr<'a>,
    pub type_: TypeCell<'a>,
}

#[derive(Debug, Clone)]
pub enum UniOp {
    Neg,
    BitNot,
    Not,
    SCast,
    UCast,
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div { signed: bool },
    Mod { signed: bool },
    BitAnd,
    BitOr,
    BitXor,
    BitLShift,
    BitRShift,
    ArihtRShift,
    Eq,
    Neq,
    Gt { signed: bool },
    GtEq { signed: bool },
    Lt { signed: bool },
    LtEq { signed: bool },
}

#[derive(Debug, Clone)]
pub struct Label(String);

#[derive(Debug)]
pub struct LabelTable {
    table: HashMap<String, bool>,
    tmp_count: u64,
}

impl LabelTable {
    pub fn new() -> Self {
        LabelTable {
            table: HashMap::new(),
            tmp_count: 1,
        }
    }

    pub fn add(&mut self, name: String) -> Label {
        if !self.table.contains_key(&name) {
            self.table.insert(name.clone(), false);
        }
        Label(name)
    }

    pub fn define_tmp(&mut self) -> Label {
        let name = format!("tmp{}", self.tmp_count);
        self.tmp_count += 1;
        self.define(name)
    }

    pub fn define(&mut self, name: String) -> Label {
        self.table.insert(name.clone(), true);
        Label(name)
    }

    pub fn check(&self) -> Result<(), Error> {
        for (k, v) in &self.table {
            if !v {
                return Err(Error::Semantic(format!("Unknown label: {}", k)));
            }
        }

        return Ok(());
    }
}

#[derive(Debug)]
pub struct IR<'a> {
    pub defvars: Vec<DefVar<Expr<'a>, TypeCell<'a>>>,
    pub defuns: Vec<(Defun<TypeCell<'a>>, Vec<Statement<'a>>)>,
    pub funcdecls: Vec<Defun<TypeCell<'a>>>,
}
