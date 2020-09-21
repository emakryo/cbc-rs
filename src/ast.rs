use crate::variable_resolver::{Entity, LocalScope};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident(pub String);

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.0.clone()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Character(pub char);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct String_(pub String);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Integer(pub usize);

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeOpt {
    Array(Option<usize>),
    Pointer,
    FuncPointer {
        params: Vec<Type>,
        variable_length: bool,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Storage {
    pub static_: bool,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeRef(pub TypeBase, pub Vec<TypeOpt>);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Type(pub TypeRef);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Name(pub Ident);

impl ToString for Name {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Term {
    Unary(Box<Unary>),
    Cast(Type, Box<Term>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Unary {
    PreInc(Box<Unary>),
    PreDec(Box<Unary>),
    Op(UnaryOp, Box<Term>),
    Deref(Box<Term>),
    Addr(Box<Term>),
    SizeofT(Type),
    SizeofE(Box<Unary>),
    PostInc(Box<Unary>),
    PostDec(Box<Unary>),
    ArrayRef(Box<Unary>, Box<Expr>),
    Member(Box<Unary>, Name),
    PMember(Box<Unary>, Name),
    Call(Box<Unary>, Args),
    Primary(Primary),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Neg,
    Rev,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Postfix {
    Inc,
    Dec,
    ArrayRef(Box<Expr>),
    Member(Name),
    PMember(Name),
    Call(Args),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Primary {
    Integer(Integer),
    Character(Character),
    String(String_),
    Variable(Variable),
    Expr(Expr),
}

#[derive(Debug, Eq, PartialEq, Clone)]
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
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Args(pub Vec<Expr>);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Assign(Term, Box<Expr>),
    AssignOp(Term, AssignOp, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Term(Term),
}

#[derive(Debug, Eq, PartialEq, Clone)]
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

#[derive(Debug, Eq, PartialEq, Clone)]
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
pub struct DefVars(pub Storage, pub Type, pub Vec<(Name, Option<Expr>)>);

#[derive(Debug, Clone)]
pub struct Block {
    vars: Vec<DefVars>,
    stmts: Vec<Statement>,
    scope: Option<Rc<RefCell<LocalScope>>>,
}

impl PartialEq for Block {
    fn eq(&self, other: &Block) -> bool {
        self.vars.eq(&other.vars) && self.stmts.eq(&other.stmts)
    }
}

impl Eq for Block {}

impl Block {
    pub fn new(vars: Vec<DefVars>, stmts: Vec<Statement>) -> Block {
        Block {
            vars,
            stmts,
            scope: None,
        }
    }

    pub fn ref_vars(&self) -> &Vec<DefVars> {
        &self.vars
    }

    pub fn mut_stmts(&mut self) -> &mut Vec<Statement> {
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
pub enum Statement {
    None,
    Label(Ident),
    Expr(Expr),
    Block(Block),
    If(Expr, Box<Statement>, Box<Option<Statement>>),
    While(Expr, Box<Statement>),
    DoWhile(Expr, Box<Statement>),
    For(Expr, Expr, Expr, Box<Statement>),
    Switch(Expr, Vec<(Vec<Primary>, Block)>),
    Break,
    Continue,
    Goto(Ident),
    Return(Option<Expr>),
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Params {
    pub params: Vec<(Type, Name)>,
    pub variable_length: bool,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopDef {
    Defun(Storage, TypeRef, Name, Params, Block),
    DefVars(DefVars),
    DefConst(DefVars),
    DefStuct(Name, Vec<(Type, Name)>),
    DefUnion(Name, Vec<(Type, Name)>),
    TypeDef(TypeRef, Ident),
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub struct Import {
    pub lib_id: String,
}

pub type ImportMap = HashMap<Import, Vec<HeaderDecl>>;

#[derive(Debug)]
pub struct Source(pub ImportMap, pub Vec<TopDef>, pub TypeMap);

#[derive(Debug)]
pub enum HeaderDecl {
    FuncDecl(TypeRef, Name, Params),
    VarsDecl(DefVars),
    DefConst(DefVars),
    DefStuct(Name, Vec<(Type, Name)>),
    DefUnion(Name, Vec<(Type, Name)>),
    TypeDef(TypeRef, Ident),
}
