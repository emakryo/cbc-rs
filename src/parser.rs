use crate::ast::*;
use crate::error::Error;
use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag, take, take_until},
    character::complete::{alphanumeric1, char, digit1, hex_digit1, none_of, oct_digit0, one_of},
    combinator::{
        all_consuming, map, map_parser, map_res, not, opt, peek, recognize, value, verify,
    },
    multi::{many0, many1, separated_nonempty_list},
    re_find,
    sequence::{delimited, pair, preceded, separated_pair, terminated, tuple},
    IResult,
};
use std::collections::HashMap;
use std::path::Path;

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
    value((), delimited(tag("/*"), take_until("*/"), tag("*/")))(i)
}

fn line_comment(i: &str) -> IResult<&str, ()> {
    value((), delimited(tag("//"), take_until("\n"), char('\n')))(i)
}

/// Parse whitespaces (space, tab, carriage return, new line) and comments
fn sp(i: &str) -> IResult<&str, ()> {
    let spaces = " \t\r\n";
    value(
        (),
        many0(alt((
            value((), one_of(spaces)),
            block_comment,
            line_comment,
        ))),
    )(i)
}

fn ident(input: &str) -> IResult<&str, Ident> {
    let (input, name) = re_find!(input, r"^[a-zA-Z_][a-zA-Z0-9_]*")?;
    Ok((input, Ident(name.to_string())))
}

fn integer(i: &str) -> IResult<&str, Integer> {
    map(
        tuple((
            alt((
                map_res(preceded(tag("0x"), hex_digit1), |d| {
                    usize::from_str_radix(d, 16)
                }),
                map_res(digit1, |d: &str| d.parse::<usize>()),
            )),
            opt(char('U')),
            opt(char('L')),
        )),
        |(n, u, l)| Integer(n),
    )(i)
}

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
        value(String_("".into()), tag("\"\"")),
    ))(i)
}

fn escaped_char(i: &str) -> IResult<&str, char> {
    alt((
        value(std::char::from_u32(0x07).unwrap(), tag("a")),
        value(std::char::from_u32(0x08).unwrap(), tag("b")),
        value(std::char::from_u32(0x1b).unwrap(), tag("e")),
        value(std::char::from_u32(0x0c).unwrap(), tag("f")),
        value('\n', tag("n")),
        value('\r', tag("r")),
        value('\t', tag("t")),
        value(std::char::from_u32(0x0b).unwrap(), tag("v")),
        value('\'', tag("'")),
        value('"', tag("\"")),
        value('\\', tag("\\")),
        map_res(map_parser(take(3usize), oct_digit0), |s| {
            std::char::from_u32(
                u32::from_str_radix(s, 8).map_err(|_| "failed to convert".to_string())?,
            )
            .ok_or("invalid octodicimal".to_string())
        }),
    ))(i)
}

fn typeref_base<'a>(i: &'a str, types: &'_ TypeMap) -> IResult<&'a str, TypeBase> {
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
        map(
            verify(ident, |s| types.contains_key(&s.0)),
            TypeBase::TypeName,
        ),
    ))(i)
}

fn func_pointer<'a>(i: &'a str, types: &'_ TypeMap) -> IResult<&'a str, TypeOpt> {
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
                            separated_nonempty_list(
                                preceded(sp, char(',')),
                                preceded(sp, |i: &'a str| type_(i, types)),
                            ),
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

fn typeopt<'a>(i: &'a str, types: &'_ TypeMap) -> IResult<&'a str, TypeOpt> {
    alt((
        value(TypeOpt::Pointer, char('*')),
        map(
            preceded(
                char('['),
                terminated(opt(preceded(sp, integer)), preceded(sp, char(']'))),
            ),
            |n| TypeOpt::Array(n.map(|x| x.0)),
        ),
        |i: &'a str| func_pointer(i, types),
    ))(i)
}

fn storage(i: &str) -> IResult<&str, Storage> {
    map(opt(keyword("static")), |s| Storage {
        static_: s.is_some(),
    })(i)
}

fn typeref<'a>(i: &'a str, types: &'_ TypeMap) -> IResult<&'a str, TypeRef> {
    map(
        tuple((
            |i: &'a str| typeref_base(i, types),
            many0(preceded(sp, |i: &'a str| typeopt(i, types))),
        )),
        |(b, o)| TypeRef(b, o),
    )(i)
}

fn type_<'a>(i: &'a str, types: &'_ TypeMap) -> IResult<&'a str, Type> {
    map(|i: &str| typeref(i, types), Type)(i)
}

fn name(i: &str) -> IResult<&str, Name> {
    map(ident, Name)(i)
}

fn term<'a>(i: &'a str, types: &'_ TypeMap) -> IResult<&'a str, Term> {
    alt((
        map(
            tuple((
                delimited(
                    char('('),
                    preceded(sp, |i: &'a str| type_(i, types)),
                    preceded(sp, char(')')),
                ),
                preceded(sp, |i: &'a str| term(i, types)),
            )),
            |(ty, tm)| Term::Cast(ty, Box::new(tm)),
        ),
        map(|i| unary(i, types), |u| Term::Unary(Box::new(u))),
    ))(i)
}

fn unary_op<'a>(i: &'a str) -> IResult<&'a str, UnaryOp> {
    alt((
        value(UnaryOp::Plus, tag("+")),
        value(UnaryOp::Minus, tag("-")),
        value(UnaryOp::Neg, tag("!")),
        value(UnaryOp::Rev, tag("~")),
    ))(i)
}

fn unary<'a>(i: &'a str, types: &'_ TypeMap) -> IResult<&'a str, Unary> {
    let unary = |i: &'a str| unary(i, types);
    let term = |i: &'a str| term(i, types);
    alt((
        map(preceded(tag("++"), preceded(sp, unary)), |u| {
            Unary::PreInc(Box::new(u))
        }),
        map(preceded(tag("--"), preceded(sp, unary)), |u| {
            Unary::PreDec(Box::new(u))
        }),
        map(tuple((unary_op, preceded(sp, term))), |(op, t)| {
            Unary::Op(op, Box::new(t))
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
                    preceded(sp, |i| type_(i, types)),
                    preceded(sp, char(')')),
                ),
            ),
            |t| Unary::SizeofT(t),
        ),
        map(preceded(keyword("sizeof"), preceded(sp, unary)), |u| {
            Unary::SizeofE(Box::new(u))
        }),
        map(
            tuple((|i| primary(i, types), many0(|i| postfix(i, types)))),
            |(pr, pfs)| {
                pfs.into_iter().fold(Unary::Primary(pr), |u, pf| match pf {
                    Postfix::Inc => Unary::PostInc(Box::new(u)),
                    Postfix::Dec => Unary::PostDec(Box::new(u)),
                    Postfix::ArrayRef(e) => Unary::ArrayRef(Box::new(u), e),
                    Postfix::Member(n) => Unary::Member(Box::new(u), n),
                    Postfix::PMember(n) => Unary::PMember(Box::new(u), n),
                    Postfix::Call(a) => Unary::Call(Box::new(u), a),
                })
            },
        ),
    ))(i)
}

fn postfix<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Postfix> {
    alt((
        value(Postfix::Inc, tag("++")),
        value(Postfix::Dec, tag("--")),
        map(
            delimited(
                char('['),
                preceded(sp, |i: &'a str| expr(i, types)),
                preceded(sp, char(']')),
            ),
            |e| Postfix::ArrayRef(Box::new(e)),
        ),
        map(preceded(char('.'), preceded(sp, name)), |n| {
            Postfix::Member(n)
        }),
        map(preceded(tag("->"), preceded(sp, name)), |n| {
            Postfix::PMember(n)
        }),
        map(
            delimited(
                char('('),
                preceded(sp, |i: &'a str| args(i, types)),
                preceded(sp, char(')')),
            ),
            |a| Postfix::Call(a),
        ),
    ))(i)
}

fn primary<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Primary> {
    alt((
        map(integer, Primary::Integer),
        map(character, Primary::Character),
        map(string, Primary::String),
        map(ident, |id| Primary::Variable(Variable::new(id))),
        map(
            delimited(
                char('('),
                preceded(sp, |i| expr(i, types)),
                preceded(sp, char(')')),
            ),
            Primary::Expr,
        ),
    ))(i)
}

fn args<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Args> {
    map(
        opt(separated_nonempty_list(
            preceded(sp, char(',')),
            preceded(sp, |i| expr(i, types)),
        )),
        |xs| match xs {
            Some(xs) => Args(xs),
            None => Args(vec![]),
        },
    )(i)
}

fn assign_op(i: &str) -> IResult<&str, AssignOp> {
    alt((
        value(AssignOp::Plus, tag("+=")),
        value(AssignOp::Minus, tag("-=")),
        value(AssignOp::Mul, tag("*=")),
        value(AssignOp::Div, tag("/=")),
        value(AssignOp::Mod, tag("%=")),
        value(AssignOp::And, tag("&=")),
        value(AssignOp::Or, tag("|=")),
        value(AssignOp::Xor, tag("^=")),
        value(AssignOp::Lshift, tag("<<=")),
        value(AssignOp::Rshift, tag(">>=")),
    ))(i)
}

fn expr<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    alt((
        map(
            separated_pair(
                |i| term(i, types),
                preceded(sp, char('=')),
                preceded(sp, |i| expr(i, types)),
            ),
            |(t, e)| Expr::Assign(t, Box::new(e)),
        ),
        map(
            tuple((
                |i| term(i, types),
                preceded(sp, assign_op),
                preceded(sp, |i| expr(i, types)),
            )),
            |(t, a, e)| Expr::AssignOp(t, a, Box::new(e)),
        ),
        |i| expr10(i, types),
    ))(i)
}

fn expr10<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    map(
        tuple((
            |i| expr9(i, types),
            opt(tuple((
                preceded(preceded(sp, char('?')), preceded(sp, |i| expr(i, types))),
                preceded(preceded(sp, char(':')), preceded(sp, |i| expr10(i, types))),
            ))),
        )),
        |(e1, ternary)| match ternary {
            Some((e2, e3)) => Expr::Ternary(Box::new(e1), Box::new(e2), Box::new(e3)),
            None => e1,
        },
    )(i)
}

fn expr9<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    map(
        tuple((
            |i| expr8(i, types),
            opt(preceded(
                preceded(sp, tag("||")),
                preceded(sp, |i| expr9(i, types)),
            )),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::BinOp(BinOp::LogicalOr, Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr8<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    map(
        tuple((
            |i| expr7(i, types),
            opt(preceded(
                preceded(sp, tag("&&")),
                preceded(sp, |i| expr8(i, types)),
            )),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::BinOp(BinOp::LogicalAnd, Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn cond_op<'a>(i: &'a str) -> IResult<&'a str, BinOp> {
    alt((
        value(BinOp::LessEq, tag("<=")),
        value(BinOp::GreaterEq, tag(">=")),
        value(BinOp::Less, tag("<")),
        value(BinOp::Greater, tag(">")),
        value(BinOp::Neq, tag("!=")),
        value(BinOp::Eq, tag("==")),
    ))(i)
}

fn expr7<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    let expr7 = |i| expr7(i, types);
    map(
        tuple((
            |i| expr6(i, types),
            opt(tuple((preceded(sp, cond_op), preceded(sp, expr7)))),
        )),
        |(e1, e2)| match e2 {
            Some((op, e2)) => Expr::BinOp(op, Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr6<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    map(
        tuple((
            |i| expr5(i, types),
            opt(preceded(
                preceded(sp, char('|')),
                preceded(sp, |i| expr6(i, types)),
            )),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::BinOp(BinOp::BitwiseOr, Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr5<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    map(
        tuple((
            |i| expr4(i, types),
            opt(preceded(
                preceded(sp, char('^')),
                preceded(sp, |i| expr5(i, types)),
            )),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::BinOp(BinOp::BitwiseXor, Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr4<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    map(
        tuple((
            |i| expr3(i, types),
            opt(preceded(
                preceded(sp, char('&')),
                preceded(sp, |i| expr4(i, types)),
            )),
        )),
        |(e1, e2)| match e2 {
            Some(e2) => Expr::BinOp(BinOp::BitwiseAnd, Box::new(e1), Box::new(e2)),
            None => e1,
        },
    )(i)
}

fn expr3<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    map(
        tuple((
            |i| expr2(i, types),
            opt(alt((
                tuple((preceded(sp, tag(">>")), preceded(sp, |i| expr3(i, types)))),
                tuple((preceded(sp, tag("<<")), preceded(sp, |i| expr3(i, types)))),
            ))),
        )),
        |(e1, e2)| match e2 {
            Some((">>", e2)) => Expr::BinOp(BinOp::RShift, Box::new(e1), Box::new(e2)),
            Some(("<<", e2)) => Expr::BinOp(BinOp::LShift, Box::new(e1), Box::new(e2)),
            Some(_) => unimplemented!(),
            None => e1,
        },
    )(i)
}

fn expr2<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    let expr2 = |i| expr2(i, types);
    map(
        tuple((
            |i| expr1(i, types),
            opt(alt((
                tuple((preceded(sp, tag("+")), preceded(sp, expr2))),
                tuple((preceded(sp, tag("-")), preceded(sp, expr2))),
            ))),
        )),
        |(e1, e2)| match e2 {
            Some(("+", e2)) => Expr::BinOp(BinOp::Plus, Box::new(e1), Box::new(e2)),
            Some(("-", e2)) => Expr::BinOp(BinOp::Minus, Box::new(e1), Box::new(e2)),
            Some(_) => unimplemented!(),
            None => e1,
        },
    )(i)
}

fn expr1<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Expr> {
    let expr1 = |i| expr1(i, types);
    map(
        tuple((
            |i| term(i, types),
            opt(alt((
                tuple((preceded(sp, tag("*")), preceded(sp, expr1))),
                tuple((preceded(sp, tag("/")), preceded(sp, expr1))),
                tuple((preceded(sp, tag("%")), preceded(sp, expr1))),
            ))),
        )),
        |(e1, e2)| match e2 {
            Some(("*", e2)) => Expr::BinOp(BinOp::Mul, Box::new(Expr::Term(e1)), Box::new(e2)),
            Some(("/", e2)) => Expr::BinOp(BinOp::Div, Box::new(Expr::Term(e1)), Box::new(e2)),
            Some(("%", e2)) => Expr::BinOp(BinOp::Mod, Box::new(Expr::Term(e1)), Box::new(e2)),
            Some(_) => unimplemented!(),
            None => Expr::Term(e1),
        },
    )(i)
}

fn defvars<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, DefVars> {
    map(
        tuple((
            storage,
            preceded(sp, |i| type_(i, types)),
            terminated(
                separated_nonempty_list(
                    preceded(sp, char(',')),
                    tuple((
                        preceded(sp, name),
                        opt(preceded(tuple((sp, char('='), sp)), |i| expr(i, types))),
                    )),
                ),
                preceded(sp, char(';')),
            ),
        )),
        |(s, t, vs)| DefVars(s, t, vs),
    )(i)
}

fn block<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Block> {
    map(
        delimited(
            char('{'),
            tuple((
                many0(preceded(sp, |i| defvars(i, types))),
                many0(preceded(sp, |i| statement(i, types))),
            )),
            preceded(sp, char('}')),
        ),
        |(d, s)| Block::new(d, s),
    )(i)
}

fn statement<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Statement> {
    let expr = |i| expr(i, types);
    let statement = |i| statement(i, types);
    alt((
        value(Statement::Break, tuple((keyword("break"), sp, char(';')))),
        value(
            Statement::Continue,
            tuple((keyword("continue"), sp, char(';'))),
        ),
        map(
            preceded(
                keyword("return"),
                terminated(opt(preceded(sp, expr)), preceded(sp, char(';'))),
            ),
            Statement::Return,
        ),
        value(Statement::None, char(';')),
        map(terminated(ident, preceded(sp, char(':'))), Statement::Label),
        map(terminated(expr, preceded(sp, char(';'))), Statement::Expr),
        map(|i| block(i, types), Statement::Block),
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
                preceded(tuple((tag("do"), sp)), statement),
                delimited(
                    tuple((sp, tag("while"), sp, char('('), sp)),
                    expr,
                    tuple((sp, char(')'))),
                ),
            )),
            |(b, e)| Statement::DoWhile(e, Box::new(b)),
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
        map(
            pair(
                preceded(tuple((keyword("switch"), sp, char('('), sp)), expr),
                delimited(
                    tuple((sp, char(')'), sp, char('{'), sp)),
                    |i| cases(i, types),
                    tuple((sp, char('}'))),
                ),
            ),
            |(e, c)| Statement::Switch(e, c),
        ),
    ))(i)
}

fn cases<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Vec<(Vec<Primary>, Block)>> {
    map(
        pair(
            many0(pair(
                many1(delimited(
                    tuple((keyword("case"), sp)),
                    |i| primary(i, types),
                    tuple((sp, char(':'), sp)),
                )),
                map(many1(terminated(|i| statement(i, types), sp)), |s| {
                    Block::new(vec![], s)
                }),
            )),
            opt(preceded(
                tuple((keyword("default"), sp, char(':'), sp)),
                map(many0(terminated(|i| statement(i, types), sp)), |s| {
                    Block::new(vec![], s)
                }),
            )),
        ),
        |(mut cs, d)| {
            if let Some(d) = d {
                cs.push((vec![], d));
            }
            cs
        },
    )(i)
}

fn fixed_params<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Vec<(Type, Name)>> {
    separated_nonempty_list(
        preceded(sp, char(',')),
        preceded(sp, tuple((|i| type_(i, types), preceded(sp, name)))),
    )(i)
}

fn params<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Params> {
    alt((
        map(
            tuple((
                |i| fixed_params(i, types),
                opt(tuple((sp, char(','), sp, tag("...")))),
            )),
            |(p, v)| Params {
                params: p,
                variable_length: v.is_some(),
            },
        ),
        map(keyword("void"), |_| Params {
            params: vec![],
            variable_length: false,
        }),
    ))(i)
}

fn defun<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, TopDef> {
    map(
        tuple((
            storage,
            preceded(sp, |i| typeref(i, types)),
            preceded(sp, name),
            preceded(
                sp,
                delimited(
                    char('('),
                    preceded(sp, |i| params(i, types)),
                    preceded(sp, char(')')),
                ),
            ),
            preceded(sp, |i| block(i, types)),
        )),
        |(s, t, n, p, b)| TopDef::Defun(s, t, n, p, b),
    )(i)
}

fn member_list<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, Vec<(Type, Name)>> {
    delimited(
        char('{'),
        many0(tuple((
            preceded(sp, |i| type_(i, types)),
            preceded(sp, terminated(name, preceded(sp, char(';')))),
        ))),
        preceded(sp, char('}')),
    )(i)
}

fn defconst<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, DefVars> {
    preceded(keyword("const"), preceded(sp, |i| defvars(i, types)))(i)
}

fn defstruct<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, (Name, Vec<(Type, Name)>)> {
    tuple((
        preceded(keyword("struct"), preceded(sp, name)),
        preceded(
            sp,
            terminated(|i| member_list(i, types), preceded(sp, char(';'))),
        ),
    ))(i)
}

fn defunion<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, (Name, Vec<(Type, Name)>)> {
    tuple((
        preceded(keyword("union"), preceded(sp, name)),
        preceded(
            sp,
            terminated(|i| member_list(i, types), preceded(sp, char(';'))),
        ),
    ))(i)
}

fn typedef<'a>(i: &'a str, types: &mut TypeMap) -> IResult<&'a str, (TypeRef, Ident)> {
    let (i, (t, id)) = tuple((
        preceded(tuple((keyword("typedef"), sp)), |i| typeref(i, &types)),
        preceded(sp, terminated(ident, char(';'))),
    ))(i)?;

    types.insert(id.0.clone(), t.clone());

    Ok((i, (t, id)))
}

fn top_def<'a>(i: &'a str, types: &mut TypeMap) -> IResult<&'a str, TopDef> {
    let res = alt((
        |i| defun(i, types),
        map(|i| defvars(i, types), TopDef::DefVars),
        map(|i| defconst(i, types), TopDef::DefConst),
        map(|i| defstruct(i, types), |(n, t)| TopDef::DefStuct(n, t)),
        map(|i| defunion(i, types), |(n, t)| TopDef::DefUnion(n, t)),
    ))(i);
    if res.is_ok() {
        return res;
    }
    match typedef(i, types) {
        Ok((i, o)) => Ok((i, TopDef::TypeDef(o.0, o.1))),
        Err(e) => Err(e),
    }
}

fn import(i: &str) -> IResult<&str, Import> {
    map(
        delimited(
            tuple((keyword("import"), sp)),
            recognize(separated_nonempty_list(char('.'), ident)),
            preceded(sp, char(';')),
        ),
        |lib_id| Import {
            lib_id: lib_id.to_string(),
        },
    )(i)
}

pub fn parse_source<P: AsRef<Path>>(i: &str, import_paths: &[P]) -> Result<Source, Error> {
    let mut types = TypeMap::new();
    let mut imports = HashMap::new();
    let mut top_defs = vec![];
    let mut i = i;

    loop {
        i = sp(i)?.0;
        match import(i) {
            Ok((j, imp)) => {
                if !imports.contains_key(&imp) {
                    let code = crate::library::load(&imp.lib_id, import_paths)?;
                    let (def, mut ts) = header(&code, import_paths)?;
                    imports.insert(imp, def);
                    for (k, v) in ts.drain() {
                        types.insert(k, v);
                    }
                }
                i = j;
            }
            _ => {
                break;
            }
        }
    }

    loop {
        i = sp(i)?.0;

        match top_def(i, &mut types) {
            Ok((j, d)) => {
                i = j;
                top_defs.push(d);
            }
            _ => {
                all_consuming(sp)(i)?;
                break;
            }
        }
    }

    Ok(Source(imports, top_defs, types))
}

fn func_decl<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, HeaderDecl> {
    map(
        tuple((
            preceded(tuple((keyword("extern"), sp)), |i| typeref(i, types)),
            preceded(sp, name),
            delimited(
                tuple((sp, char('('), sp)),
                |i| params(i, types),
                tuple((sp, char(')'), sp, char(';'))),
            ),
        )),
        |(t, n, p)| HeaderDecl::FuncDecl(t, n, p),
    )(i)
}

fn vars_decl<'a>(i: &'a str, types: &TypeMap) -> IResult<&'a str, HeaderDecl> {
    map(
        preceded(tuple((keyword("extern"), sp)), |i| defvars(i, types)),
        |d| HeaderDecl::VarsDecl(d),
    )(i)
}

fn header_def<'a>(i: &'a str, types: &mut TypeMap) -> IResult<&'a str, HeaderDecl> {
    let res = alt((
        |i| func_decl(i, types),
        |i| vars_decl(i, types),
        map(|i| defconst(i, types), HeaderDecl::DefConst),
        map(|i| defstruct(i, types), |(n, t)| HeaderDecl::DefStuct(n, t)),
        map(|i| defunion(i, types), |(n, t)| HeaderDecl::DefUnion(n, t)),
    ))(i);
    if res.is_ok() {
        return res;
    }

    match typedef(i, types) {
        Ok((i, o)) => Ok((i, HeaderDecl::TypeDef(o.0, o.1))),
        Err(e) => Err(e),
    }
}

fn header<P: AsRef<Path>>(
    i: &str,
    header_paths: &[P],
) -> Result<(Vec<HeaderDecl>, TypeMap), Error> {
    let mut i = i;
    let mut defs = vec![];
    let mut types = TypeMap::new();
    let mut imports = HashMap::new();

    loop {
        i = sp(i)?.0;
        match import(i) {
            Ok((j, imp)) => {
                if !imports.contains_key(&imp) {
                    let code = crate::library::load(&imp.lib_id, header_paths)?;
                    let (def, mut ts) = header(&code, header_paths)?;
                    imports.insert(imp, def);

                    for (k, v) in ts.drain() {
                        types.insert(k, v);
                    }
                }
                i = j;
            }
            _ => {
                break;
            }
        }
    }

    loop {
        i = sp(i)?.0;
        match header_def(i, &mut types) {
            Ok((j, o)) => {
                i = j;
                defs.push(o);
            }
            Err(_) => match all_consuming(sp)(i) {
                Ok((_, _)) => return Ok((defs, types)),
                Err(e) => return Err(e.into()),
            },
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt::Debug;

    fn consumed<T: Debug>(r: IResult<&str, T>) -> T {
        dbg!(&r);
        match r {
            Ok((o, x)) => {
                assert_eq!(o, "");
                x
            }
            Err(e) => {
                panic!("{:?}", e);
            }
        }
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
        consumed(string(r#"";\a\b\e\f\n\r\t\v""#));
        consumed(string(r#"";\101\102\103\141\142\143""#));
    }

    #[test]
    fn test_typeref_base() {
        let typeref_base = |i| typeref_base(i, &HashMap::new());

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
        let typeopt = |i| typeopt(i, &HashMap::new());
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
        let typeref = |i| typeref(i, &HashMap::new());
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
                    lib_id: "foo.bar".into()
                }
            ))
        );

        assert_eq!(
            import("import foo.bar;   \n"),
            Ok((
                "   \n",
                Import {
                    lib_id: "foo.bar".into()
                }
            ))
        );

        assert!(import("importfoo.bar;").is_err());
        assert!(import("import foo.bar").is_err());
        assert!(import("  import foo.bar;").is_err());
    }

    #[test]
    fn test_term() {
        let term = |i| term(i, &HashMap::new());
        assert_eq!(
            term("++x"),
            Ok((
                "",
                Term::Unary(Box::new(Unary::PreInc(Box::new(Unary::Primary(
                    Primary::Variable(Variable::new(Ident("x".into())))
                )))))
            ))
        );
        assert_eq!(
            term("x--"),
            Ok((
                "",
                Term::Unary(Box::new(Unary::PostDec(Box::new(Unary::Primary(
                    Primary::Variable(Variable::new(Ident("x".into())))
                )))))
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
                Term::Unary(Box::new(Unary::PMember(
                    Box::new(Unary::Member(
                        Box::new(Unary::Primary(Primary::Variable(Variable::new(Ident(
                            "foo".into()
                        ))))),
                        Name(Ident("bar".into())),
                    )),
                    Name(Ident("baz".into())),
                )))
            ))
        );
    }

    #[test]
    fn test_expr() {
        let expr = |i| expr(i, &HashMap::new());
        assert!(dbg!(expr("x=y+z-10")).is_ok());
        assert!(dbg!(expr("a+=10")).is_ok());
        assert!(dbg!(expr("foo?(x+y):\"string\"")).is_ok());
        assert!(dbg!(expr("x*y+z/y-19")).is_ok());
        assert!(dbg!(expr("\"\"")).is_ok());
        assert!(dbg!(expr("foo()")).is_ok());
        consumed(expr("x>=y"));
    }

    #[test]
    fn test_defvars() {
        let defvars = |i| defvars(i, &HashMap::new());
        consumed(defvars("int foo = 20;"));
    }

    #[test]
    fn test_block() {
        let block = |i| block(i, &HashMap::new());
        assert!(dbg!(block("{int foo = 20; return foo;}")).is_ok());
        consumed(block(
            r#"{
            printf("");
            printf(";");
            printf(";a");
            printf(";aa;b");
            printf(";\"");
            printf(";\'");
            printf(";\a\b\e\f\n\r\t\v");
            printf(";\101\102\103\141\142\143");
            puts("");
            return 0;
        }"#,
        ));
    }

    #[test]
    fn test_statement() {
        let statement = |i| statement(i, &HashMap::new());
        assert!(dbg!(statement("*x = foo && bar ? (x + y) / z : baz ^ 4324;")).is_ok());
        assert!(dbg!(statement("*x = foo && bar ? (x + y) / z : baz ^ 4324")).is_err());
        assert!(dbg!(statement("for(i=0; i<10; i++) x += 20;")).is_ok());
        assert!(dbg!(statement("{int foo = 20; return foo;}")).is_ok());
        assert!(dbg!(statement("if(x<y) x+=y; else {foo; return 10;}")).is_ok());
        assert!(dbg!(statement("puts(\"\");")).is_ok());
        assert!(dbg!(statement("foo();")).is_ok());
        consumed(statement(
            "switch(args) { case 1: case 2: foo(); default: bar(); }",
        ));
    }

    #[test]
    fn test_cases() {
        let cases = |i| cases(i, &HashMap::new());
        consumed(cases("case 1: case 2: foo(); bar(); default: baz();"));
    }

    #[test]
    fn test_defun() {
        let defun = |i| defun(i, &HashMap::new());
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
        ));

        consumed(defun(r#"static void check(void* f, void* p) {}"#));

        consumed(defun(
            r#"int
        main(int argc, char **argv)
        {
            int i = 3;
        
            printf("%d", i);    // 3
            i += 1;
            printf(";%d", i);   // 4
            i -= 1;
            printf(";%d", i);   // 3
            i *= 4;
            printf(";%d", i);   // 12
            i /= 3;
            printf(";%d", i);   // 4
            i %= 3;
            printf(";%d", i);   // 1
            i &= 7;
            printf(";%d", i);   // 1
            i |= 6;
            printf(";%d", i);   // 7
            i ^= 2;
            printf(";%d", i);   // 5
            i >>= 2;
            printf(";%d", i);   // 1
            i <<= 2;
            printf(";%d", i);   // 4
        
            // pointer diff arithmetic (size=1)
            {
                char *string = "Hello, World!";
                char *p;
        
                p = string;
                p += 1;
                printf(";%c", *p);
        
                p -= 1;
                printf(";%c", *p);
            }
        
            // pointer diff arithmetic (size=4)
            {
                int[4] xs;
                int* p;
        
                xs[0] = 75;
                xs[1] = 76;
                xs[2] = 77;
                xs[3] = 78;
        
                p = xs;
                p += 1;
                printf(";%d", *p);
        
                p -= 1;
                printf(";%d", *p);
            }
        
            // complex LHS
            {
                int x = 0;
                int *p = &x;
        
                *p += 1;
                printf(";%d", *p);
        
                p[0] += 2;
                printf(";%d", *p);
        
                *&*p += 3;
                printf(";%d", *p);
            }
        
            // complex LHS #2
            {
                int[2] a;
        
                a[0] = 77;
                a[1] = 78;
        
                a[0] += 5;
                printf(";%d", a[0]);
        
                *(a + 1) += 3;
                printf(";%d", a[1]);
            }
        
            puts("");
            return 0;
        }"#,
        ));
    }

    #[test]
    fn test_defconst() {
        let defconst = |i| defconst(i, &HashMap::new());
        consumed(defconst("const int RTLD_LAZY = 0x10;"));
    }

    #[test]
    fn test_typedef() {
        let mut types = HashMap::new();
        consumed(typedef("typedef struct X Foo;", &mut types));
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
            let mut header_paths = vec!["cbc-1.0/import"];
            if let Some(p) = file_name.parent() {
                header_paths.push(p.to_str().unwrap());
            }
            let ast = parse_source(&code, &header_paths);
            assert!(dbg!(&ast).is_ok(), "faild: {}", file_name.to_str().unwrap());
        }
    }

    #[test]
    fn test_header_files() {
        use std::io::Read;
        let root = env!("CARGO_MANIFEST_DIR");

        for file_name in glob::glob(&format!("{}/cbc-1.0/import/**/*.hb", root)).unwrap() {
            let file_name = file_name.unwrap();

            let mut code = String::new();
            std::fs::File::open(&file_name)
                .unwrap()
                .read_to_string(&mut code)
                .unwrap();
            let mut header_paths = vec!["cbc-1.0/import"];
            if let Some(p) = file_name.parent() {
                header_paths.push(p.to_str().unwrap());
            }

            let res = header(&code, &header_paths);
            assert!(dbg!(res).is_ok(), "failed: {}", file_name.to_str().unwrap());
        }
    }
}
