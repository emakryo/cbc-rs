use crate::entity::{Entity, LocalScope};
use crate::types::{TypeCell, TypeRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Ident(pub String);

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Character(pub char);

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct String_(pub String);

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Integer(pub usize);

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeBase {
    Void,
    Char,
    Short,
    Int,
    Long,
    UChar,
    UShort,
    UInt,
    ULong,
    Struct(Ident),
    Union(Ident),
    TypeName(Ident),
}

pub type TypeMap = HashMap<String, TypeRef>;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum TypeOpt {
    Array(Option<usize>),
    Pointer,
    FuncPointer {
        params: Vec<TypeRef>,
        variable_length: bool,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Storage {
    pub static_: bool,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum UnaryOp {
    Plus,
    Minus,
    Neg,
    Rev,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Primary<E> {
    Integer(Integer),
    Character(Character),
    String(String_),
    Variable(Variable),
    Expr(Box<E>),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Variable {
    name: Ident,
    entity: Option<Rc<Entity>>,
}

impl Variable {
    pub fn new(name: Ident) -> Self {
        Variable { name, entity: None }
    }
    pub fn name(&self) -> String {
        self.name.0.clone()
    }
    pub fn set_entity(&mut self, entity: Rc<Entity>) {
        if self.entity.is_some() {
            panic!("variable already set entity");
        }
        self.entity = Some(entity)
    }
    pub fn get_entity(&self) -> Option<Rc<Entity>> {
        self.entity.as_ref().map(Rc::clone)
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Args<E>(pub Vec<E>);

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum BaseExpr<E, T> {
    Assign(Box<E>, Box<E>),
    AssignOp(Box<E>, AssignOp, Box<E>),
    Ternary(Box<E>, Box<E>, Box<E>),
    BinOp(BinOp, Box<E>, Box<E>),
    Cast(T, Box<E>),
    PreInc(Box<E>),
    PreDec(Box<E>),
    UnaryOp(UnaryOp, Box<E>),
    Deref(Box<E>),
    Addr(Box<E>),
    SizeofT(T),
    SizeofE(Box<E>),
    PostInc(Box<E>),
    PostDec(Box<E>),
    ArrayRef(Box<E>, Box<E>),
    Member(Box<E>, Ident),
    PMember(Box<E>, Ident),
    Call(Box<E>, Args<E>),
    Primary(Primary<E>),
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Expr {
    pub inner: BaseExpr<Expr, TypeRef>,
}

impl Expr {
    pub fn assign(lhs: Expr, rhs: Expr) -> Expr {
        Expr {
            inner: BaseExpr::Assign(Box::new(lhs), Box::new(rhs)),
        }
    }
    pub fn assign_op(lhs: Expr, op: AssignOp, rhs: Expr) -> Expr {
        Expr {
            inner: BaseExpr::AssignOp(Box::new(lhs), op, Box::new(rhs)),
        }
    }
    pub fn ternary(cond: Expr, then: Expr, else_: Expr) -> Expr {
        Expr {
            inner: BaseExpr::Ternary(Box::new(cond), Box::new(then), Box::new(else_)),
        }
    }
    pub fn bin_op(op: BinOp, e1: Expr, e2: Expr) -> Expr {
        Expr {
            inner: BaseExpr::BinOp(op, Box::new(e1), Box::new(e2)),
        }
    }
    pub fn cast(type_: TypeRef, e: Expr) -> Expr {
        Expr {
            inner: BaseExpr::Cast(type_, Box::new(e)),
        }
    }
    pub fn pre_inc(e: Expr) -> Expr {
        Expr {
            inner: BaseExpr::PreInc(Box::new(e)),
        }
    }
    pub fn pre_dec(e: Expr) -> Expr {
        Expr {
            inner: BaseExpr::PreDec(Box::new(e)),
        }
    }
    pub fn op(unary_op: UnaryOp, e: Expr) -> Expr {
        Expr {
            inner: BaseExpr::UnaryOp(unary_op, Box::new(e)),
        }
    }
    pub fn deref(e: Expr) -> Expr {
        Expr {
            inner: BaseExpr::Deref(Box::new(e)),
        }
    }
    pub fn addr(e: Expr) -> Expr {
        Expr {
            inner: BaseExpr::Addr(Box::new(e)),
        }
    }
    pub fn sizeof_type(t: TypeRef) -> Expr {
        Expr {
            inner: BaseExpr::SizeofT(t),
        }
    }
    pub fn sizeof_expr(e: Expr) -> Expr {
        Expr {
            inner: BaseExpr::SizeofE(Box::new(e)),
        }
    }
    pub fn post_inc(e: Expr) -> Expr {
        Expr {
            inner: BaseExpr::PostInc(Box::new(e)),
        }
    }
    pub fn post_dec(e: Expr) -> Expr {
        Expr {
            inner: BaseExpr::PostDec(Box::new(e)),
        }
    }
    pub fn array_ref(array: Expr, index: Box<Expr>) -> Expr {
        Expr {
            inner: BaseExpr::ArrayRef(Box::new(array), index),
        }
    }
    pub fn member(struct_: Expr, field: Ident) -> Expr {
        Expr {
            inner: BaseExpr::Member(Box::new(struct_), field),
        }
    }
    pub fn p_member(struct_: Expr, field: Ident) -> Expr {
        Expr {
            inner: BaseExpr::PMember(Box::new(struct_), field),
        }
    }
    pub fn call(func: Expr, args: Args<Expr>) -> Expr {
        Expr {
            inner: BaseExpr::Call(Box::new(func), args),
        }
    }
    pub fn primary(p: Primary<Expr>) -> Expr {
        Expr {
            inner: BaseExpr::Primary(p),
        }
    }
}

#[derive(Debug)]
pub struct TypedExpr<'a> {
    pub inner: BaseExpr<TypedExpr<'a>, TypeCell<'a>>,
    pub type_: TypeCell<'a>,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum AssignOp {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Lshift,
    Rshift,
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum BinOp {
    LogicalOr,
    LogicalAnd,
    Greater,
    Less,
    GreaterEq,
    LessEq,
    Eq,
    Neq,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    LShift,
    RShift,
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DefVar<E, T> {
    pub storage: Storage,
    pub type_: T,
    pub name: Ident,
    pub init: Option<E>,
}

#[derive(Debug, Clone)]
pub struct Block<E, T> {
    pub vars: Vec<DefVar<E, T>>,
    pub stmts: Vec<Statement<E, T>>,
    pub scope: Option<Rc<RefCell<LocalScope>>>,
}

impl<E: PartialEq, T: PartialEq> PartialEq for Block<E, T> {
    fn eq(&self, other: &Block<E, T>) -> bool {
        self.vars.eq(&other.vars) && self.stmts.eq(&other.stmts)
    }
}

impl<E: Eq, T: Eq> Eq for Block<E, T> {}

impl<E, T> Block<E, T> {
    pub fn new(vars: Vec<DefVar<E, T>>, stmts: Vec<Statement<E, T>>) -> Block<E, T> {
        Block {
            vars,
            stmts,
            scope: None,
        }
    }

    pub fn ref_vars(&self) -> &Vec<DefVar<E, T>> {
        &self.vars
    }

    pub fn mut_vars(&mut self) -> &mut Vec<DefVar<E, T>> {
        &mut self.vars
    }

    pub fn ref_stmts(&self) -> &Vec<Statement<E, T>> {
        &self.stmts
    }

    pub fn mut_stmts(&mut self) -> &mut Vec<Statement<E, T>> {
        &mut self.stmts
    }

    pub fn set_scope(&mut self, scope: Rc<RefCell<LocalScope>>) {
        if self.scope.is_some() {
            panic!("Already scope has set");
        }
        self.scope = Some(scope);
    }

    pub fn get_scope(&self) -> Option<Rc<RefCell<LocalScope>>> {
        self.scope.as_ref().map(Rc::clone)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Statement<E, T> {
    None,
    Label(Ident),
    Expr(E),
    Block(Block<E, T>),
    If(E, Box<Statement<E, T>>, Box<Option<Statement<E, T>>>),
    While(E, Box<Statement<E, T>>),
    DoWhile(E, Box<Statement<E, T>>),
    For(E, E, E, Box<Statement<E, T>>),
    Switch(E, Vec<(Vec<Primary<E>>, Block<E, T>)>),
    Break,
    Continue,
    Goto(Ident),
    Return(Option<E>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Params {
    pub params: Vec<(TypeRef, Ident)>,
    pub variable_length: bool,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Defun {
    pub storage: Storage,
    pub type_: TypeRef,
    pub name: Ident,
    pub params: Params,
}

#[derive(Debug, Eq, PartialEq)]
pub enum Declaration<E, T> {
    DefVar(DefVar<E, T>),
    VarDecl(DefVar<E, T>),
    Defun(Defun, Block<E, T>),
    FuncDecl(Defun),
    DefConst(DefVar<E, T>),
    DefStuct(Ident, Vec<(T, Ident)>),
    DefUnion(Ident, Vec<(T, Ident)>),
    TypeDef(T, Ident),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Import {
    pub libid: String,
}

#[derive(Debug)]
pub struct Ast<'a, E, T> {
    pub source: &'a str,
    pub declarations: Vec<Declaration<E, T>>,
    pub type_alias: TypeMap,
}
