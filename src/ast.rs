use crate::variable_resolver::{Entity, LocalScope};
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
        params: Vec<TypeRef>,
        variable_length: bool,
    },
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Storage {
    pub static_: bool,
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeRef {
    Array {
        base: Box<TypeRef>,
        size: Option<usize>,
    },
    Function {
        base: Box<TypeRef>,
        params: Vec<TypeRef>,
        variable_length: bool,
    },
    Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    Pointer {
        base: Box<TypeRef>,
    },
    Struct(Ident),
    Union(Ident),
    User(Ident),
    Void,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Neg,
    Rev,
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Primary {
    Integer(Integer),
    Character(Character),
    String(String_),
    Variable(Variable),
    Expr(Box<Expr>),
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
    pub fn get_entity(&self) -> Option<Rc<Entity>> {
        self.entity.as_ref().map(Rc::clone)
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Args(pub Vec<Expr>);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Assign(Box<Expr>, Box<Expr>),
    AssignOp(Box<Expr>, AssignOp, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    BinOp(BinOp, Box<Expr>, Box<Expr>),
    Cast(TypeRef, Box<Expr>),
    PreInc(Box<Expr>),
    PreDec(Box<Expr>),
    Op(UnaryOp, Box<Expr>),
    Deref(Box<Expr>),
    Addr(Box<Expr>),
    SizeofT(TypeRef),
    SizeofE(Box<Expr>),
    PostInc(Box<Expr>),
    PostDec(Box<Expr>),
    ArrayRef(Box<Expr>, Box<Expr>),
    Member(Box<Expr>, Ident),
    PMember(Box<Expr>, Ident),
    Call(Box<Expr>, Args),
    Primary(Primary),
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
pub struct DefVars(pub Storage, pub TypeRef, pub Vec<(Ident, Option<Expr>)>);

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

    pub fn mut_vars(&mut self) -> &mut Vec<DefVars> {
        &mut self.vars
    }

    pub fn ref_stmts(&self) -> &Vec<Statement> {
        &self.stmts
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
    pub params: Vec<(TypeRef, Ident)>,
    pub variable_length: bool,
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopDef {
    Defun(Storage, TypeRef, Ident, Params, Block),
    DefVars(DefVars),
    DefConst(DefVars),
    DefStuct(Ident, Vec<(TypeRef, Ident)>),
    DefUnion(Ident, Vec<(TypeRef, Ident)>),
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
    FuncDecl(TypeRef, Ident, Params),
    VarsDecl(DefVars),
    DefConst(DefVars),
    DefStuct(Ident, Vec<(TypeRef, Ident)>),
    DefUnion(Ident, Vec<(TypeRef, Ident)>),
    TypeDef(TypeRef, Ident),
}
