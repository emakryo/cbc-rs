use crate::ast::*;
use crate::entity::{GlobalScope, Scope};
use crate::error::Error;
use crate::types::{TypeArena, TypeCell, TypeRef, TypeTable};
use std::collections::HashSet;

pub fn resolve_types<'a, 'b>(
    ast: Ast<'b, Expr, TypeRef>,
    arena: &'a TypeArena<'a>,
    global_scope: &GlobalScope,
) -> Result<Ast<'b, TypedExpr<'a>, TypeCell<'a>>, Error> {
    let mut type_table = TypeTable::new(arena);

    for entity in global_scope.root.borrow().entities.values() {
        type_table.add(entity.get_type().clone())?;
    }

    let mut declarations = vec![];
    for dec in ast.declarations {
        let dec = match dec {
            Declaration::TypeDef(typeref, n) => {
                type_table.add_usertype(n.clone(), typeref.clone())?;
                None
            }
            Declaration::DefStuct(n, members) => {
                type_table.add_struct(n.clone(), members.clone())?;
                None
            }
            Declaration::DefUnion(n, members) => {
                type_table.add_union(n.clone(), members.clone())?;
                None
            }
            Declaration::DefVar(def) => Some(Declaration::DefVar(DefVar {
                storage: def.storage,
                type_: type_table.add(def.type_)?.clone(),
                name: def.name,
                init: def.init.map_or(Ok(None), |e| {
                    e.resolve_types(&mut type_table, global_scope).map(Some)
                })?,
            })),
            Declaration::VarDecl(def) => Some(Declaration::VarDecl(DefVar {
                storage: def.storage,
                type_: type_table.add(def.type_)?.clone(),
                name: def.name,
                init: None,
            })),
            Declaration::DefConst(def) => Some(Declaration::DefConst(DefVar {
                storage: def.storage,
                type_: type_table.add(def.type_)?.clone(),
                name: def.name,
                init: def.init.map_or(Ok(None), |e| {
                    e.resolve_types(&mut type_table, global_scope).map(Some)
                })?,
            })),
            Declaration::Defun(defun, block) => Some(Declaration::Defun(
                defun,
                block.resolve_types(&mut type_table)?,
            )),
            Declaration::FuncDecl(defun) => Some(Declaration::FuncDecl(defun)),
        };
        if let Some(dec) = dec {
            declarations.push(dec);
        }
    }

    let ast = Ast {
        source: ast.source,
        declarations,
        type_alias: ast.type_alias,
    };

    //dbg!(&type_table);

    check_duplication(&type_table)?;
    check_recursive_definition(&type_table)?;

    Ok(ast)
}

fn check_duplication<'a>(table: &TypeTable<'a>) -> Result<(), Error> {
    for t in table.values() {
        let mut names = HashSet::new();
        if let Some(members) = t.members() {
            for (_, n) in members {
                if names.contains(&n) {
                    return Err(Error::Semantic(format!(
                        "Duplicated field: {}",
                        n.to_string()
                    )));
                }
                names.insert(n.clone());
            }
        }
    }

    Ok(())
}

fn check_recursive_definition<'a>(type_table: &TypeTable<'a>) -> Result<(), Error> {
    type TypeSet<'a> = HashSet<TypeCell<'a>>;
    let mut mark = TypeSet::new();
    let mut done = TypeSet::new();

    fn rec<'a, 'b>(
        type_table: &TypeTable<'a>,
        mark: &mut TypeSet<'a>,
        done: &mut TypeSet<'a>,
        v: TypeCell<'a>,
    ) -> Result<(), Error> {
        if done.contains(&v) {
            return Ok(());
        }

        if mark.contains(&v) {
            return Err(Error::Semantic(format!(
                "Cyclic type definition found: {:?}",
                v
            )));
        }

        mark.insert(v.clone());

        if let Some(members) = v.members() {
            for (t, _) in members {
                rec(type_table, mark, done, t.clone())?;
            }
        } else if let Ok(base) = v.array_base() {
            rec(type_table, mark, done, base.clone())?;
        }

        done.insert(v);

        Ok(())
    }

    for v in type_table.values() {
        rec(type_table, &mut mark, &mut done, v)?;
    }

    Ok(())
}

impl Block<Expr, TypeRef> {
    fn resolve_types<'a>(
        self,
        type_table: &mut TypeTable<'a>,
    ) -> Result<Block<TypedExpr<'a>, TypeCell<'a>>, Error> {
        use std::ops::Deref;
        let scope = self.scope.as_ref().unwrap().borrow();

        let vars: Result<Vec<_>, Error> = self
            .vars
            .into_iter()
            .map(|var| {
                // type_table.add(def.type_.clone())?;
                Ok(DefVar {
                    storage: var.storage,
                    type_: type_table.add(var.type_)?.clone(),
                    name: var.name,
                    init: var
                        .init
                        .map(|e| e.resolve_types(type_table, scope.deref()))
                        .map_or(Ok(None), |e| e.map(Some))?,
                })
            })
            .collect();

        let stmts: Result<Vec<_>, _> = self
            .stmts
            .into_iter()
            .map(|stmt| stmt.resolve_types(type_table, scope.deref()))
            .collect();

        drop(scope);

        Ok(Block {
            vars: vars?,
            stmts: stmts?,
            scope: self.scope,
        })
    }
}

impl Statement<Expr, TypeRef> {
    fn resolve_types<'a, S: Scope>(
        self,
        type_table: &mut TypeTable<'a>,
        scope: &S,
    ) -> Result<Statement<TypedExpr<'a>, TypeCell<'a>>, Error> {
        let stmt = match self {
            Statement::Expr(e) => {
                Statement::Expr(e.resolve_types(type_table, scope)?)
            }
            Statement::Block(b) => {
                Statement::Block(b.resolve_types(type_table)?)
            }
            Statement::If(cond, then, else_) => {
                let cond = cond.resolve_types(type_table, scope)?;
                let then = then.resolve_types(type_table, scope)?;
                let else_ = else_.map(|s| s.resolve_types(type_table, scope)).map_or(Ok(None), |s| s.map(Some))?;
                Statement::If(cond, Box::new(then), Box::new(else_))
            }
            Statement::While(cond, body) => {
                let cond = cond.resolve_types(type_table, scope)?;
                let body = body.resolve_types(type_table, scope)?;
                Statement::While(cond, Box::new(body))
            }
            Statement::DoWhile(cond, body) => {
                let cond = cond.resolve_types(type_table, scope)?;
                let body = body.resolve_types(type_table, scope)?;
                Statement::DoWhile(cond, Box::new(body))
            }
            Statement::For(init, cond, step, body) => {
                let init = init.resolve_types(type_table, scope)?;
                let cond = cond.resolve_types(type_table, scope)?;
                let step = step.resolve_types(type_table, scope)?;
                let body = body.resolve_types(type_table, scope)?;
                Statement::For(init, cond, step, Box::new(body))
            }
            Statement::Switch(e, branches) => {
                let e = e.resolve_types(type_table, scope)?;
                let branches = branches.into_iter().map(|(vals, body)| {
                    Ok((vals.into_iter().map(|v| {
                        Ok(v.resolve_types(type_table, scope)?.0)
                    }).collect::<Result<Vec<_>, Error>>()?,
                    body.resolve_types(type_table)?))
                }).collect::<Result<Vec<_>, Error>>()?;

                Statement::Switch(e, branches)
            }
            Statement::Return(e) => {
                Statement::Return(e.map_or(Ok(None), |e| e.resolve_types(type_table, scope).map(Some))?)
            }
            Statement::Break => Statement::Break,
            Statement::Continue => Statement::Continue,
            Statement::Label(l) => Statement::Label(l),
            Statement::Goto(l) => Statement::Goto(l),
            Statement::None => Statement::None,
        };

        Ok(stmt)
    }
}

fn cast_to<'a>(expr: TypedExpr<'a>, type_: &TypeCell<'a>) -> Result<TypedExpr<'a>, Error> {
    Ok(TypedExpr {
        inner: BaseExpr::Cast(type_.clone(), Box::new(expr)),
        type_: type_.clone(),
    })
}

fn cast<'a>(e1: TypedExpr<'a>, e2: TypedExpr<'a>, type_table: &TypeTable<'a>) -> Result<(TypedExpr<'a>, TypedExpr<'a>), Error> {
    if e1.type_ == e2.type_ {
        Ok((e1, e2))
    } else {
        let t1 = &e1.type_;
        let t2 = &e2.type_;
        if t1.is_numeric() || t2.is_numeric() {
            return Err(Error::Semantic("Failed to implicit conversion".into()));
        }

        let t = type_table.long();
        Ok((cast_to(e1, t)?, cast_to(e2, t)?))
    }
}

impl Expr {
    pub fn resolve_types<'a, S: Scope>(
        self,
        type_table: &mut TypeTable<'a>,
        scope: &S,
    ) -> Result<TypedExpr<'a>, Error> {
        let (t, e) = match self.inner {
            BaseExpr::Assign(e1, e2) => {
                let e1 = e1.resolve_types(type_table, scope)?;
                let t = e1.type_.clone();
                let e2 = e2.resolve_types(type_table, scope)?;
                let e2 = cast_to(e2, &t)?;
                (t, BaseExpr::Assign(Box::new(e1), Box::new(e2)))
            }
            BaseExpr::AssignOp(e1, op, e2) => {
                let e1 = e1.resolve_types(type_table, scope)?;
                let t = e1.type_.clone();
                let e2 = e2.resolve_types(type_table, scope)?;
                let e2 = cast_to(e2, &t)?;
                (t, BaseExpr::AssignOp(Box::new(e1), op, Box::new(e2)))
            }
            BaseExpr::BinOp(op, e1, e2) => {
                let e1 = e1.resolve_types(type_table, scope)?;
                let e2 = e2.resolve_types(type_table, scope)?;
                let (e1, e2) = cast(e1, e2, type_table)?;
                (
                    e1.type_.clone(),
                    BaseExpr::BinOp(op, Box::new(e1), Box::new(e2)),
                )
            }
            BaseExpr::Ternary(cond, e1, e2) => {
                let cond = cond.resolve_types(type_table, scope)?;
                let e1 = e1.resolve_types(type_table, scope)?;
                let e2 = e2.resolve_types(type_table, scope)?;
                let (e1, e2) = cast(e1, e2, type_table)?;
                (
                    e1.type_.clone(),
                    BaseExpr::Ternary(Box::new(cond), Box::new(e1), Box::new(e2)),
                )
            }
            BaseExpr::Cast(t, e) => {
                let e = e.resolve_types(type_table, scope)?;
                let t = type_table.get(&t).unwrap().clone();
                (t.clone(), BaseExpr::Cast(t, Box::new(e)))
            }
            BaseExpr::PreInc(e) => {
                let e = e.resolve_types(type_table, scope)?;
                (e.type_.clone(), BaseExpr::PreInc(Box::new(e)))
            }
            BaseExpr::PreDec(e) => {
                let e = e.resolve_types(type_table, scope)?;
                (e.type_.clone(), BaseExpr::PreDec(Box::new(e)))
            }
            BaseExpr::UnaryOp(op, e) => {
                let e = e.resolve_types(type_table, scope)?;
                (e.type_.clone(), BaseExpr::UnaryOp(op, Box::new(e)))
            }
            BaseExpr::Deref(e) => {
                let e = e.resolve_types(type_table, scope)?;
                let t = &e.type_;
                (t.deref()?, BaseExpr::Deref(Box::new(e)))
            }
            BaseExpr::PostInc(e) => {
                let e = e.resolve_types(type_table, scope)?;
                (e.type_.clone(), BaseExpr::PostInc(Box::new(e)))
            }
            BaseExpr::PostDec(e) => {
                let e = e.resolve_types(type_table, scope)?;
                (e.type_.clone(), BaseExpr::PostDec(Box::new(e)))
            }
            BaseExpr::Member(e, name) => {
                let e = e.resolve_types(type_table, scope)?;
                (
                    e.type_.get_field(&name)?,
                    BaseExpr::Member(Box::new(e), name),
                )
            }
            BaseExpr::PMember(e, name) => {
                let e = e.resolve_types(type_table, scope)?;
                (
                    e.type_.deref()?.get_field(&name)?,
                    BaseExpr::PMember(Box::new(e), name),
                )
            }
            BaseExpr::Addr(e) => {
                let e = e.resolve_types(type_table, scope)?;
                (type_table.addr(&e.type_).clone(), BaseExpr::Addr(Box::new(e)))
            }
            BaseExpr::SizeofT(t) => {
                let t = type_table.add(t)?.clone();
                (type_table.long().clone(), BaseExpr::SizeofT(t))
            }
            BaseExpr::SizeofE(e) => {
                let e = e.resolve_types(type_table, scope)?;
                (type_table.long().clone(), BaseExpr::SizeofE(Box::new(e)))
            }
            BaseExpr::ArrayRef(a, idx) => {
                let a = a.resolve_types(type_table, scope)?;
                let idx = idx.resolve_types(type_table, scope)?;
                (
                    a.type_.array_base()?,
                    BaseExpr::ArrayRef(Box::new(a), Box::new(idx)),
                )
            }
            BaseExpr::Call(f, args) => {
                let f = f.resolve_types(type_table, scope)?;
                let args = args
                    .0
                    .into_iter()
                    .map(|a| a.resolve_types(type_table, scope))
                    .collect::<Result<_, Error>>()?;

                (
                    f.type_.return_type()?,
                    BaseExpr::Call(Box::new(f), Args(args)),
                )
            }
            BaseExpr::Primary(p) => {
                let (p, t) = p.resolve_types(type_table, scope)?;
                (t, BaseExpr::Primary(p))
            }
        };

        Ok(TypedExpr { inner: e, type_: t })
    }
}

impl Primary<Expr> {
    pub fn resolve_types<'a, S: Scope>(
        self,
        type_table: &mut TypeTable<'a>,
        scope: &S,
    ) -> Result<(Primary<TypedExpr<'a>>, TypeCell<'a>), Error> {
        Ok(match self {
            Primary::Expr(e) => {
                let e = e.resolve_types(type_table, scope)?;
                let t = e.type_.clone();
                (Primary::Expr(Box::new(e)), t)
            }
            Primary::Character(c) => (Primary::Character(c), type_table.char().clone()),
            Primary::Integer(i) => (Primary::Integer(i), type_table.long().clone()),
            Primary::String(s) => (Primary::String(s), type_table.string().clone()),
            Primary::Variable(v) => {
                let t = scope.get_entity(&v.name()).unwrap();
                (
                    Primary::Variable(v),
                    type_table.get(t.get_type()).unwrap().clone(),
                )
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_source;
    use crate::variable_resolver::resolve_variables;

    fn test_from_file(file_name: &std::path::Path) {
        use std::io::Read;
        dbg!(&file_name);
        let mut code = String::new();
        std::fs::File::open(&file_name)
            .unwrap()
            .read_to_string(&mut code)
            .unwrap();
        let mut header_paths = vec!["cbc-1.0/import"];
        if let Some(p) = file_name.parent() {
            header_paths.push(p.to_str().unwrap());
        }
        let mut ast = parse_source(&code, &header_paths).unwrap();
        let scope = resolve_variables(&mut ast);
        if scope.is_err() {
            dbg!(scope).ok();
            return;
        }

        let arena = TypeArena::new();
        let table = resolve_types(ast, &arena, &scope.unwrap());

        if table.is_err() {
            dbg!(table).ok();
            return;
        }
    }

    #[test]
    fn test_from_files() {
        let root = env!("CARGO_MANIFEST_DIR");

        for file_name in glob::glob(&format!("{}/cbc-1.0/test/*.cb", root)).unwrap() {
            let file_name = file_name.unwrap();
            test_from_file(&file_name);
        }
    }
}
