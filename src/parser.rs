use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag, take_until},
    character::complete::{alphanumeric1, char, digit1, hex_digit1, none_of, one_of},
    combinator::{all_consuming, map, map_res, not, opt, peek, value},
    multi::{many0, separated_list, separated_nonempty_list},
    re_find,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};

/// Parse keyword
fn keyword<'a, T: 'a, I: 'a, E: nom::error::ParseError<I>>(
    keyword: T,
) -> impl Fn(I) -> IResult<I, I, E>
where
    I: nom::InputTake + nom::Compare<T> + Clone + nom::InputTakeAtPosition,
    T: nom::InputLength + Clone,
    <I as nom::InputTakeAtPosition>::Item: nom::AsChar,
{
    terminated(tag(keyword), peek(not(alphanumeric1)))
}

fn block_comment(i: &str) -> IResult<&str, ()> {
    map(delimited(tag("/*"), take_until("*/"), tag("*/")), |_| ())(i)
}

fn line_comment(i: &str) -> IResult<&str, ()> {
    map(delimited(tag("//"), take_until("\n"), char('\n')), |_| ())(i)
}

/// Parse whitespaces (space, tab, carriage return, new line)
fn sp(i: &str) -> IResult<&str, ()> {
    let spaces = " \t\r\n";
    map(
        many0(alt((
            map(one_of(spaces), |_| ()),
            block_comment,
            line_comment,
        ))),
        |_| (),
    )(i)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Ident(String);

fn ident(input: &str) -> IResult<&str, Ident> {
    let (input, name) = re_find!(input, r"^[a-zA-Z_][a-zA-Z0-9_]*")?;
    Ok((input, Ident(name.to_string())))
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Integer(usize);

/// parse simple digits
fn integer(i: &str) -> IResult<&str, Integer> {
    map(
        alt((
            map_res(preceded(tag("0x"), hex_digit1), |d| {
                usize::from_str_radix(d, 16)
            }),
            map_res(digit1, |d: &str| d.parse::<usize>()),
        )),
        Integer,
    )(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Character(char);

/// parse simple character
fn character(i: &str) -> IResult<&str, Character> {
    delimited(
        char('\''),
        map(
            alt((none_of(r"\'"), preceded(char('\\'), escaped_char))),
            Character,
        ),
        char('\''),
    )(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct String_(String);

/// parse string
fn string(i: &str) -> IResult<&str, String_> {
    alt((
        map(
            delimited(
                char('"'),
                escaped_transform(none_of("\\\""), '\\', escaped_char),
                char('"'),
            ),
            String_,
        ),
        map(tag("\"\""), |_| String_("".into())),
    ))(i)
}

fn escaped_char(i: &str) -> IResult<&str, char> {
    alt((
        value('\n', tag("n")),
        value('\t', tag("t")),
        value('\'', tag("'")),
        value('"', tag("\"")),
        value('\r', tag("r")),
        value('\\', tag("\\")),
    ))(i)
}

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
}

fn typeref_base(i: &str) -> IResult<&str, TypeBase> {
    // todo typedef-ed types
    alt((
        value(TypeBase::Void, keyword("void")),
        value(TypeBase::Char, keyword("char")),
        value(TypeBase::Short, keyword("short")),
        value(TypeBase::Int, keyword("int")),
        value(TypeBase::Long, keyword("long")),
        value(
            TypeBase::UChar,
            preceded(keyword("unsigned"), preceded(sp, keyword("char"))),
        ),
        value(
            TypeBase::UShort,
            preceded(keyword("unsigned"), preceded(sp, keyword("short"))),
        ),
        value(
            TypeBase::UInt,
            preceded(keyword("unsigned"), preceded(sp, keyword("int"))),
        ),
        value(
            TypeBase::ULong,
            preceded(keyword("unsigned"), preceded(sp, keyword("long"))),
        ),
        map(
            preceded(keyword("struct"), preceded(sp, ident)),
            TypeBase::Struct,
        ),
        map(
            preceded(keyword("union"), preceded(sp, ident)),
            TypeBase::Union,
        ),
    ))(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum TypeOpt {
    Array(Option<usize>),
    Pointer,
    FuncPointer {
        params: Vec<Type>,
        variable_length: bool,
    },
}

fn func_pointer(i: &str) -> IResult<&str, TypeOpt> {
    preceded(
        char('('),
        terminated(
            preceded(
                sp,
                alt((
                    value(
                        TypeOpt::FuncPointer {
                            params: vec![],
                            variable_length: false,
                        },
                        keyword("void"),
                    ),
                    map(
                        tuple((
                            separated_nonempty_list(preceded(sp, char(',')), preceded(sp, type_)),
                            opt(tuple((preceded(sp, char(',')), preceded(sp, tag("..."))))),
                        )),
                        |(v, d)| TypeOpt::FuncPointer {
                            params: v,
                            variable_length: d.is_some(),
                        },
                    ),
                )),
            ),
            preceded(sp, char(')')),
        ),
    )(i)
}

fn typeopt(i: &str) -> IResult<&str, TypeOpt> {
    alt((
        value(TypeOpt::Pointer, char('*')),
        map(
            preceded(
                char('['),
                terminated(opt(preceded(sp, integer)), preceded(sp, char(']'))),
            ),
            |n| TypeOpt::Array(n.map(|x| x.0)),
        ),
        func_pointer,
    ))(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Storage {
    static_: bool,
}

fn storage(i: &str) -> IResult<&str, Storage> {
    map(opt(keyword("static")), |s| Storage {
        static_: s.is_some(),
    })(i)
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct TypeRef(TypeBase, Vec<TypeOpt>);

fn typeref(i: &str) -> IResult<&str, TypeRef> {
    map(
        tuple((typeref_base, many0(preceded(sp, typeopt)))),
        |(b, o)| TypeRef(b, o),
    )(i)
}
#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Type(TypeRef);

fn type_(i: &str) -> IResult<&str, Type> {
    map(typeref, Type)(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Name(Ident);

fn name(i: &str) -> IResult<&str, Name> {
    map(ident, Name)(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Term {
    Unary(Box<Unary>),
    Cast(Type, Box<Term>),
}

fn term(i: &str) -> IResult<&str, Term> {
    alt((
        map(
            tuple((
                delimited(char('('), preceded(sp, type_), preceded(sp, char(')'))),
                preceded(sp, term),
            )),
            |(ty, tm)| Term::Cast(ty, Box::new(tm)),
        ),
        map(unary, |u| Term::Unary(Box::new(u))),
    ))(i)
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

fn unary(i: &str) -> IResult<&str, Unary> {
    alt((
        map(preceded(tag("++"), preceded(sp, unary)), |u| {
            Unary::PreInc(Box::new(u))
        }),
        map(preceded(tag("--"), preceded(sp, unary)), |u| {
            Unary::PreDec(Box::new(u))
        }),
        map(preceded(tag("+"), preceded(sp, term)), |t| {
            Unary::Plus(Box::new(t))
        }),
        map(preceded(tag("-"), preceded(sp, term)), |t| {
            Unary::Minus(Box::new(t))
        }),
        map(preceded(tag("!"), preceded(sp, term)), |t| {
            Unary::Neg(Box::new(t))
        }),
        map(preceded(tag("~"), preceded(sp, term)), |t| {
            Unary::Rev(Box::new(t))
        }),
        map(preceded(char('*'), preceded(sp, term)), |t| {
            Unary::Deref(Box::new(t))
        }),
        map(preceded(char('&'), preceded(sp, term)), |t| {
            Unary::Addr(Box::new(t))
        }),
        map(
            preceded(
                keyword("sizeof"),
                delimited(
                    preceded(sp, char('(')),
                    preceded(sp, type_),
                    preceded(sp, char(')')),
                ),
            ),
            |t| Unary::SizeofT(t),
        ),
        map(preceded(keyword("sizeof"), preceded(sp, unary)), |u| {
            Unary::SizeofE(Box::new(u))
        }),
        map(tuple((primary, many0(postfix))), |(pr, pf)| {
            Unary::PostFix(pr, pf)
        }),
    ))(i)
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

fn postfix(i: &str) -> IResult<&str, PostFix> {
    alt((
        value(PostFix::Inc, tag("++")),
        value(PostFix::Dec, tag("--")),
        map(
            delimited(char('['), preceded(sp, expr), preceded(sp, char(']'))),
            |e| PostFix::ArrayRef(Box::new(e)),
        ),
        map(preceded(char('.'), preceded(sp, name)), |n| {
            PostFix::Member(n)
        }),
        map(preceded(tag("->"), preceded(sp, name)), |n| {
            PostFix::PMember(n)
        }),
        map(
            delimited(char('('), preceded(sp, args), preceded(sp, char(')'))),
            |a| PostFix::Call(a),
        ),
    ))(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Primary {
    Integer(Integer),
    Character(Character),
    String(String_),
    Ident(Ident),
    Expr(Expr),
}

fn primary(i: &str) -> IResult<&str, Primary> {
    alt((
        map(integer, Primary::Integer),
        map(character, Primary::Character),
        map(string, Primary::String),
        map(ident, Primary::Ident),
        map(
            delimited(char('('), preceded(sp, expr), preceded(sp, char(')'))),
            Primary::Expr,
        ),
    ))(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Args(Vec<Expr>);

fn args(i: &str) -> IResult<&str, Args> {
    map(
        opt(separated_nonempty_list(
            preceded(sp, char(',')),
            preceded(sp, expr),
        )),
        |xs| match xs {
            Some(xs) => Args(xs),
            None => Args(vec![]),
        },
    )(i)
}

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
    Lshift,
    Rshift,
}

fn assign_op(i: &str) -> IResult<&str, AssignOp> {
    alt((
        value(AssignOp::Plus, tag("+=")),
        value(AssignOp::Minus, tag("-=")),
        value(AssignOp::Mul, tag("*=")),
        value(AssignOp::Div, tag("/=")),
        value(AssignOp::Mod, tag("*=")),
        value(AssignOp::And, tag("&=")),
        value(AssignOp::Or, tag("|=")),
        value(AssignOp::Lshift, tag("<<=")),
        value(AssignOp::Rshift, tag(">>=")),
    ))(i)
}

fn expr(i: &str) -> IResult<&str, Expr> {
    alt((
        map(
            separated_pair(term, preceded(sp, char('=')), preceded(sp, expr)),
            |(t, e)| Expr::Assign(t, Box::new(e)),
        ),
        map(
            tuple((term, preceded(sp, assign_op), preceded(sp, expr))),
            |(t, a, e)| Expr::AssignOp(t, a, Box::new(e)),
        ),
        expr10,
    ))(i)
}

fn expr10(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            expr9,
            opt(tuple((
                preceded(preceded(sp, char('?')), preceded(sp, expr)),
                preceded(preceded(sp, char(':')), preceded(sp, expr10)),
            ))),
        )),
        |(e1, ternary)| match ternary {
            Some((e2, e3)) => Expr::Ternary(Box::new(e1), Box::new(e2), Box::new(e3)),
            None => e1,
        },
    )(i)
}

fn expr9(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            expr8,
            opt(preceded(preceded(sp, tag("||")), preceded(sp, expr9))),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::LogicalOr(Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr8(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            expr7,
            opt(preceded(preceded(sp, tag("&&")), preceded(sp, expr8))),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::LogicalAnd(Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr7(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            expr6,
            opt(alt((
                tuple((preceded(sp, tag(">")), preceded(sp, expr7))),
                tuple((preceded(sp, tag("<")), preceded(sp, expr7))),
                tuple((preceded(sp, tag(">=")), preceded(sp, expr7))),
                tuple((preceded(sp, tag("<=")), preceded(sp, expr7))),
                tuple((preceded(sp, tag("==")), preceded(sp, expr7))),
                tuple((preceded(sp, tag("!=")), preceded(sp, expr7))),
            ))),
        )),
        |(e1, e2)| match e2 {
            Some(("<", e2)) => Expr::Less(Box::new(e1), Box::new(e2)),
            Some((">", e2)) => Expr::Greater(Box::new(e1), Box::new(e2)),
            Some(("<=", e2)) => Expr::LessEq(Box::new(e1), Box::new(e2)),
            Some((">=", e2)) => Expr::GreaterEq(Box::new(e1), Box::new(e2)),
            Some(("==", e2)) => Expr::Eq(Box::new(e1), Box::new(e2)),
            Some(("!=", e2)) => Expr::Neq(Box::new(e1), Box::new(e2)),
            Some(_) => unimplemented!(),
            None => e1,
        },
    )(i)
}

fn expr6(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            expr5,
            opt(preceded(preceded(sp, char('|')), preceded(sp, expr6))),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::BitwiseOr(Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr5(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            expr4,
            opt(preceded(preceded(sp, char('^')), preceded(sp, expr5))),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::BitwiseXor(Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr4(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            expr3,
            opt(preceded(preceded(sp, char('&')), preceded(sp, expr4))),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::BitwiseAnd(Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr3(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            expr2,
            opt(alt((
                tuple((preceded(sp, tag(">>")), preceded(sp, expr3))),
                tuple((preceded(sp, tag("<<")), preceded(sp, expr3))),
            ))),
        )),
        |(e1, e2)| match e2 {
            Some((">>", e2)) => Expr::RShift(Box::new(e1), Box::new(e2)),
            Some(("<<", e2)) => Expr::LShift(Box::new(e1), Box::new(e2)),
            Some(_) => unimplemented!(),
            None => e1,
        },
    )(i)
}

fn expr2(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            expr1,
            opt(alt((
                tuple((preceded(sp, tag("+")), preceded(sp, expr2))),
                tuple((preceded(sp, tag("-")), preceded(sp, expr2))),
            ))),
        )),
        |(e1, e2)| match e2 {
            Some(("+", e2)) => Expr::Plus(Box::new(e1), Box::new(e2)),
            Some(("-", e2)) => Expr::Minus(Box::new(e1), Box::new(e2)),
            Some(_) => unimplemented!(),
            None => e1,
        },
    )(i)
}

fn expr1(i: &str) -> IResult<&str, Expr> {
    map(
        tuple((
            term,
            opt(alt((
                tuple((preceded(sp, tag("*")), preceded(sp, expr1))),
                tuple((preceded(sp, tag("/")), preceded(sp, expr1))),
                tuple((preceded(sp, tag("%")), preceded(sp, expr1))),
            ))),
        )),
        |(e1, e2)| match e2 {
            Some(("*", e2)) => Expr::Mul(Box::new(Expr::Term(e1)), Box::new(e2)),
            Some(("/", e2)) => Expr::Div(Box::new(Expr::Term(e1)), Box::new(e2)),
            Some(("%", e2)) => Expr::Mod(Box::new(Expr::Term(e1)), Box::new(e2)),
            Some(_) => unimplemented!(),
            None => Expr::Term(e1),
        },
    )(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct DefVars(Storage, Type, Vec<(Name, Option<Expr>)>);

fn defvars(i: &str) -> IResult<&str, DefVars> {
    map(
        tuple((
            storage,
            preceded(sp, type_),
            terminated(
                separated_nonempty_list(
                    preceded(sp, char(',')),
                    tuple((
                        preceded(sp, name),
                        opt(preceded(tuple((sp, char('='), sp)), expr)),
                    )),
                ),
                preceded(sp, char(';')),
            ),
        )),
        |(s, t, vs)| DefVars(s, t, vs),
    )(i)
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Block(Vec<DefVars>, Vec<Statement>);

fn block(i: &str) -> IResult<&str, Block> {
    map(
        delimited(
            char('{'),
            tuple((many0(preceded(sp, defvars)), many0(preceded(sp, statement)))),
            preceded(sp, char('}')),
        ),
        |(d, s)| Block(d, s),
    )(i)
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
    Switch,
    Break,
    Continue,
    Goto(Ident),
    Return(Option<Expr>),
}

fn statement(i: &str) -> IResult<&str, Statement> {
    alt((
        map(char(';'), |_| Statement::None),
        map(terminated(ident, preceded(sp, char(':'))), Statement::Label),
        map(terminated(expr, preceded(sp, char(';'))), Statement::Expr),
        map(block, Statement::Block),
        map(
            tuple((
                preceded(tuple((tag("if"), sp, char('('), sp)), expr),
                preceded(tuple((sp, char(')'), sp)), statement),
                opt(preceded(tuple((sp, tag("else"), sp)), statement)),
            )),
            |(e, b1, b2)| Statement::If(e, Box::new(b1), Box::new(b2)),
        ),
        map(
            tuple((
                preceded(tuple((tag("while"), sp, char('('))), expr),
                preceded(tuple((sp, char(')'), sp)), statement),
            )),
            |(e, b)| Statement::While(e, Box::new(b)),
        ),
        map(
            tuple((
                preceded(tuple((keyword("for"), sp, char('('), sp)), expr),
                preceded(tuple((sp, char(';'), sp)), expr),
                preceded(tuple((sp, char(';'), sp)), expr),
                preceded(tuple((sp, char(')'), sp)), statement),
            )),
            |(b, c, s, x)| Statement::For(b, c, s, Box::new(x)),
        ),
        map(tuple((keyword("break"), sp, char(';'))), |_| {
            Statement::Break
        }),
        map(tuple((keyword("continue"), sp, char(':'))), |_| {
            Statement::Continue
        }),
        map(
            preceded(
                keyword("return"),
                terminated(opt(preceded(sp, expr)), preceded(sp, char(';'))),
            ),
            Statement::Return,
        ),
    ))(i)
}

#[derive(Debug, Eq, PartialEq)]
pub struct Params {
    params: Vec<(Type, Name)>,
    variable_length: bool,
}

fn fixed_params(i: &str) -> IResult<&str, Vec<(Type, Name)>> {
    separated_nonempty_list(
        preceded(sp, char(',')),
        preceded(sp, tuple((type_, preceded(sp, name)))),
    )(i)
}

fn params(i: &str) -> IResult<&str, Params> {
    alt((
        map(keyword("void"), |_| Params {
            params: vec![],
            variable_length: false,
        }),
        map(
            tuple((fixed_params, opt(tuple((sp, char(','), sp, tag("...")))))),
            |(p, v)| Params {
                params: p,
                variable_length: v.is_some(),
            },
        ),
    ))(i)
}

#[derive(Debug, Eq, PartialEq)]
pub enum TopDef {
    Defun(Storage, TypeRef, Name, Params, Block),
    DefVars(DefVars),
    DefConst(DefVars),
    DefStuct(Name, Vec<(Type, Name)>),
    DefUnion,
    TypeDef(TypeRef, Ident),
}

fn defun(i: &str) -> IResult<&str, TopDef> {
    map(
        tuple((
            storage,
            preceded(sp, typeref),
            preceded(sp, name),
            preceded(
                sp,
                delimited(char('('), preceded(sp, params), preceded(sp, char(')'))),
            ),
            preceded(sp, block),
        )),
        |(s, t, n, p, b)| TopDef::Defun(s, t, n, p, b),
    )(i)
}

fn member_list(i: &str) -> IResult<&str, Vec<(Type, Name)>> {
    delimited(
        char('{'),
        many0(tuple((
            preceded(sp, type_),
            preceded(sp, terminated(name, preceded(sp, char(';')))),
        ))),
        preceded(sp, char('}')),
    )(i)
}

fn defconst(i: &str) -> IResult<&str, TopDef> {
    map(preceded(keyword("const"), preceded(sp, defvars)), |d| {
        TopDef::DefConst(d)
    })(i)
}

fn defstruct(i: &str) -> IResult<&str, TopDef> {
    map(
        tuple((
            preceded(keyword("struct"), preceded(sp, name)),
            preceded(sp, terminated(member_list, preceded(sp, char(';')))),
        )),
        |(n, s)| TopDef::DefStuct(n, s),
    )(i)
}

fn defunion(i: &str) -> IResult<&str, TopDef> {
    map(
        tuple((
            preceded(keyword("union"), preceded(sp, name)),
            preceded(sp, terminated(member_list, preceded(sp, char(';')))),
        )),
        |(n, s)| TopDef::DefStuct(n, s),
    )(i)
}

fn typedef(i: &str) -> IResult<&str, TopDef> {
    map(
        tuple((
            preceded(tuple((keyword("typedef"), sp)), typeref),
            preceded(sp, ident),
        )),
        |(t, n)| TopDef::TypeDef(t, n),
    )(i)
}

fn top_def(i: &str) -> IResult<&str, TopDef> {
    alt((
        defun,
        map(defvars, TopDef::DefVars),
        defconst,
        defstruct,
        defunion,
        typedef,
    ))(i)
}

#[derive(Debug, Eq, PartialEq)]
pub struct Import {
    file_id: Vec<Ident>,
}

fn import(i: &str) -> IResult<&str, Import> {
    map(
        delimited(
            tuple((keyword("import"), sp)),
            separated_nonempty_list(char('.'), ident),
            preceded(sp, char(';')),
        ),
        |file_id| Import { file_id },
    )(i)
}

#[derive(Debug)]
pub struct Source(Vec<Import>, Vec<TopDef>);

fn source(i: &str) -> IResult<&str, Source> {
    map(
        all_consuming(terminated(
            tuple((many0(preceded(sp, import)), many0(preceded(sp, top_def)))),
            sp,
        )),
        |(i, d)| Source(i, d),
    )(i)
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::Debug;

    fn consumed<T: Debug>(r: IResult<&str, T>) {
        dbg!(&r);
        assert!(r.map_or(false, |(s, _)| s.len() == 0));
    }
    #[test]
    fn test_ident() {
        assert_eq!(ident("pohe"), Ok(("", Ident("pohe".to_string()))));
        assert_eq!(ident("x10"), Ok(("", Ident("x10".to_string()))));
        assert_eq!(ident("_x10"), Ok(("", Ident("_x10".to_string()))));
        assert!(if let Err(_) = ident("10x") {
            true
        } else {
            false
        });
    }

    #[test]
    fn test_integer() {
        assert_eq!(integer("20"), Ok(("", Integer(20))));
        assert_eq!(integer("0x10"), Ok(("", Integer(16))));
    }

    #[test]
    fn test_character() {
        assert_eq!(character("'x'"), Ok(("", Character('x'))));
        assert_eq!(character(r"'\\'"), Ok(("", Character('\\'))));
    }

    #[test]
    fn test_string() {
        assert_eq!(string(r#""foo\n\\""#), Ok(("", String_("foo\n\\".into()))));
        assert_eq!(string("\"\""), Ok(("", String_("".into()))));
    }

    #[test]
    fn test_typeref_base() {
        assert_eq!(typeref_base("void"), Ok(("", TypeBase::Void)));
        assert_eq!(typeref_base("char"), Ok(("", TypeBase::Char)));
        assert_eq!(typeref_base("short"), Ok(("", TypeBase::Short)));
        assert_eq!(typeref_base("long"), Ok(("", TypeBase::Long)));
        assert_eq!(typeref_base("unsigned char"), Ok(("", TypeBase::UChar)));
        assert_eq!(typeref_base("unsigned short"), Ok(("", TypeBase::UShort)));
        assert_eq!(typeref_base("unsigned long"), Ok(("", TypeBase::ULong)));
        assert_eq!(
            typeref_base("struct bar"),
            Ok(("", TypeBase::Struct(Ident("bar".to_string()))))
        );
        assert_eq!(
            typeref_base("union bar"),
            Ok(("", TypeBase::Union(Ident("bar".to_string()))))
        );

        //dbg!(typeref_base("unsignedlong"));
        assert!(typeref_base("unsignedlong").is_err());
        assert!(typeref_base("   struct foo").is_err());
    }

    #[test]
    fn test_typeopt() {
        assert_eq!(typeopt("*"), Ok(("", TypeOpt::Pointer)));
        assert_eq!(typeopt("[]"), Ok(("", TypeOpt::Array(None))));
        assert_eq!(typeopt("[3]"), Ok(("", TypeOpt::Array(Some(3)))));
        assert_eq!(
            typeopt("(int, unsigned char)"),
            Ok((
                "",
                TypeOpt::FuncPointer {
                    params: vec![
                        Type(TypeRef(TypeBase::Int, vec![])),
                        Type(TypeRef(TypeBase::UChar, vec![])),
                    ],
                    variable_length: false,
                }
            ))
        );
        assert_eq!(
            typeopt("(int, unsigned char, ...)"),
            Ok((
                "",
                TypeOpt::FuncPointer {
                    params: vec![
                        Type(TypeRef(TypeBase::Int, vec![])),
                        Type(TypeRef(TypeBase::UChar, vec![])),
                    ],
                    variable_length: true,
                }
            ))
        );

        assert_eq!(typeopt("[  ]"), Ok(("", TypeOpt::Array(None))));

        assert!(typeopt("[*]").is_err());
    }

    #[test]
    fn test_typeref() {
        assert_eq!(
            typeref("int[3]"),
            Ok(("", TypeRef(TypeBase::Int, vec![TypeOpt::Array(Some(3))])))
        );
        assert_eq!(
            typeref("long (int*)"),
            Ok((
                "",
                TypeRef(
                    TypeBase::Long,
                    vec![TypeOpt::FuncPointer {
                        params: vec![Type(TypeRef(TypeBase::Int, vec![TypeOpt::Pointer]))],
                        variable_length: false,
                    }]
                )
            ))
        );

        assert_eq!(typeref("long int**").unwrap().0, " int**");
    }

    #[test]
    fn test_import() {
        assert_eq!(
            import("import foo.bar;"),
            Ok((
                "",
                Import {
                    file_id: vec![Ident("foo".into()), Ident("bar".into())]
                }
            ))
        );

        assert_eq!(
            import("import foo.bar;   \n"),
            Ok((
                "   \n",
                Import {
                    file_id: vec![Ident("foo".into()), Ident("bar".into())]
                }
            ))
        );

        assert!(import("importfoo.bar;").is_err());
        assert!(import("import foo.bar").is_err());
        assert!(import("  import foo.bar;").is_err());
    }

    #[test]
    fn test_term() {
        assert_eq!(
            term("++x"),
            Ok((
                "",
                Term::Unary(Box::new(Unary::PreInc(Box::new(Unary::PostFix(
                    Primary::Ident(Ident("x".into())),
                    vec![]
                )))))
            ))
        );
        assert_eq!(
            term("x--"),
            Ok((
                "",
                Term::Unary(Box::new(Unary::PostFix(
                    Primary::Ident(Ident("x".into())),
                    vec![PostFix::Dec]
                )))
            ))
        );
        assert_eq!(
            term("(int)sizeof(short)"),
            Ok((
                "",
                Term::Cast(
                    Type(TypeRef(TypeBase::Int, vec![])),
                    Box::new(Term::Unary(Box::new(Unary::SizeofT(Type(TypeRef(
                        TypeBase::Short,
                        vec![]
                    ))))))
                )
            ))
        );

        assert_eq!(
            term("foo.bar->baz"),
            Ok((
                "",
                Term::Unary(Box::new(Unary::PostFix(
                    Primary::Ident(Ident("foo".into())),
                    vec![
                        PostFix::Member(Name(Ident("bar".into()))),
                        PostFix::PMember(Name(Ident("baz".into())))
                    ]
                )))
            ))
        );
    }

    #[test]
    fn test_expr() {
        assert!(dbg!(expr("x=y+z-10")).is_ok());
        assert!(dbg!(expr("a+=10")).is_ok());
        assert!(dbg!(expr("foo?(x+y):\"string\"")).is_ok());
        assert!(dbg!(expr("x*y+z/y-19")).is_ok());
        assert!(dbg!(expr("\"\"")).is_ok());
        assert!(dbg!(expr("foo()")).is_ok());
    }

    #[test]
    fn test_defvars() {
        assert!(dbg!(defvars("int foo = 20;")).is_ok());
    }

    #[test]
    fn test_block() {
        assert!(dbg!(block("{int foo = 20; return foo;}")).is_ok());
    }

    #[test]
    fn test_statement() {
        assert!(dbg!(statement("*x = foo && bar ? (x + y) / z : baz ^ 4324;")).is_ok());
        assert!(dbg!(statement("*x = foo && bar ? (x + y) / z : baz ^ 4324")).is_err());
        assert!(dbg!(statement("for(i=0; i<10; i++) x += 20;")).is_ok());
        assert!(dbg!(statement("{int foo = 20; return foo;}")).is_ok());
        assert!(dbg!(statement("if(x<y) x+=y; else {foo; return 10;}")).is_ok());
        assert!(dbg!(statement("puts(\"\");")).is_ok());
        assert!(dbg!(statement("foo();")).is_ok());
    }

    #[test]
    fn test_defun() {
        consumed(defun(
            r#"
        int main(int argc, char **argv) {
            int i;
            int j = 0;
            printf("%d", 1 + 0);
            printf(";%d", 2 + j);
            i = 2;
            j = 1;
            printf(";%d", i + j);
            printf(";%d", j + g);
            printf(";%d", i + g);
            printf(";%d", f(5));      // 6
            printf(";%d", f(5) + 1);
            
            printf(";%d", f(5) + i);  // 8
            c = 5;
            printf(";%d", f(3) + c);
            i = 9;
            printf(";%d", f(i));
            j = 1;
            printf(";%d", f(i) + j);
            puts("");
            return 0;
        
        }"#,
        ));

        consumed(defun(
            r#"
int
main(int argc, char **argv)
{
    fa();
    puts("");
    return 0;
}"#,
        ))
    }

    #[test]
    fn test_defconst() {
        consumed(defconst("const int RTLD_LAZY = 0x10;"));
    }

    #[test]
    fn test_from_files() {
        use std::io::Read;

        let root = env!("CARGO_MANIFEST_DIR");

        for file_name in glob::glob(&format!("{}/cbc-1.0/test/*.cb", root)).unwrap() {
            let file_name = file_name.unwrap();
            let mut code = String::new();
            std::fs::File::open(&file_name)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();
            let ast = source(&code);
            assert!(dbg!(&ast).is_ok(), "faild: {}", file_name.to_str().unwrap());
        }
    }
}
