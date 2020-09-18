use std::collections::HashMap;

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
    Plus(Box<Term>),
    Minus(Box<Term>),
    Neg(Box<Term>),
    Rev(Box<Term>),
    Deref(Box<Term>),
    Addr(Box<Term>),
    SizeofT(Type),
    SizeofE(Box<Unary>),
    PostFix(Primary, Vec<PostFix>),
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub enum PostFix {
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
    pub name: Ident,
}

impl Variable {
    pub fn new(name: Ident) -> Self {
        Variable {
            name,
        }
    }
}


#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Args(pub Vec<Expr>);

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Expr {
    Assign(Term, Box<Expr>),
    AssignOp(Term, AssignOp, Box<Expr>),
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
    LogicalOr(Box<Expr>, Box<Expr>),
    LogicalAnd(Box<Expr>, Box<Expr>),
    Greater(Box<Expr>, Box<Expr>),
    Less(Box<Expr>, Box<Expr>),
    GreaterEq(Box<Expr>, Box<Expr>),
    LessEq(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Neq(Box<Expr>, Box<Expr>),
    BitwiseOr(Box<Expr>, Box<Expr>),
    BitwiseXor(Box<Expr>, Box<Expr>),
    BitwiseAnd(Box<Expr>, Box<Expr>),
    LShift(Box<Expr>, Box<Expr>),
    RShift(Box<Expr>, Box<Expr>),
    Plus(Box<Expr>, Box<Expr>),
    Minus(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Mod(Box<Expr>, Box<Expr>),
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
pub struct DefVars(pub Storage, pub Type, pub Vec<(Name, Option<Expr>)>);

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block(pub Vec<DefVars>, pub Vec<Statement>);

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
    Switch(Expr, Vec<(Vec<Primary>, Vec<Statement>)>),
    Break,
    Continue,
    Goto(Ident),
    Return(Option<Expr>),
}

#[derive(Debug, Eq, PartialEq)]
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
