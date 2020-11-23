use crate::ast::{DefVar, Defun};
use crate::entity::Entity;
use crate::types::TypeRef;

#[derive(Debug)]
pub enum Statement {
    Assign {
        lhs: Expr,
        rhs: Expr,
    },
    CJump {
        cond: Expr,
        then: Label,
        else_: Label,
    },
    Jump(Label),
    Switch {
        cond: Expr,
        cases: Vec<(Expr, Label)>,
        default: Label,
    },
    Label(Label),
    Expr(Expr),
    Return(Expr),
}

#[derive(Debug)]
pub enum Expr {
    UniOp {
        op: UniOp,
        expr: Box<Expr>,
    },
    BinOp {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Call {
        expr: Box<Expr>,
        args: Vec<Expr>,
    },
    Addr(Box<Expr>),
    Mem(Box<Expr>),
    Var(Entity),
    Int(Constant),
    Str(String),
}

#[derive(Debug)]
pub enum UniOp {
    Neg,
    BitNot,
    Not,
    SCast,
    UCast,
}

#[derive(Debug)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Sdiv,
    Udiv,
    Smod,
    Umod,
    BitAnd,
    BitOr,
    BitXor,
    BitLShift,
    BitRShift,
    ArihtRShift,
    Eq,
    Neq,
    SGt,
    SGtEq,
    SLt,
    SLtEq,
    UGt,
    UGtEq,
    ULt,
    ULtEq,
}

#[derive(Debug)]
pub struct Label(String);

#[derive(Debug)]
pub enum Constant {}

#[derive(Debug)]
pub struct IR {
    pub defvars: Vec<(DefVar<Expr, TypeRef>, Option<Expr>)>,
    pub defuns: Vec<(Defun, Vec<Statement>)>,
    pub funcdecls: Vec<Defun>,
}
