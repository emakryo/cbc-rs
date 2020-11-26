use crate::ast::{DefVar, Defun};
use crate::entity::Entity;
use crate::types::TypeCell;
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
pub struct IR<'a> {
    pub defvars: Vec<DefVar<Expr<'a>, TypeCell<'a>>>,
    pub defuns: Vec<(Defun<TypeCell<'a>>, Vec<Statement<'a>>)>,
    pub funcdecls: Vec<Defun<TypeCell<'a>>>,
}
