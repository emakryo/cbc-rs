use crate::ast::{DefVar, Defun};
use crate::entity::Entity;

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

pub enum UniOp {
    Neg,
    BitNot,
    Not,
    SCast,
    UCast,
}

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

pub struct Label(String);

pub enum Constant {}

pub struct IR {
    pub defvars: Vec<(DefVar, Option<Expr>)>,
    pub defuns: Vec<(Defun, Vec<Statement>)>,
    pub funcdecls: Vec<String>,
}
