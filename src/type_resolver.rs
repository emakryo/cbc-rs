use crate::ast::*;
use crate::error::Error;
use crate::types::{TypeArena, TypeCell, TypeTable};
use std::collections::HashSet;

pub fn resolve_types<'a, 'b>(
    ast: &'b Ast,
    arena: &'a TypeArena<'a>,
) -> Result<TypeTable<'a, 'b>, Error> {
    let mut type_table = TypeTable::new(arena);

    for def in &ast.declarations {
        match def {
            Declaration::TypeDef(typeref, n) => {
                type_table.add_usertype(n.clone(), typeref.clone())?;
            }
            Declaration::DefStuct(n, members) => {
                type_table.add_struct(n.clone(), members.clone())?;
            }
            Declaration::DefUnion(n, members) => {
                type_table.add_union(n.clone(), members.clone())?;
            }
            Declaration::DefVar(def) | Declaration::VarDecl(def) | Declaration::DefConst(def) => {
                type_table.add(def.type_.clone())?;
                if let Some(e) = &def.init {
                    e.resolve_types(&mut type_table)?;
                }
            }
            Declaration::Defun(_, typeref, _, params, block) => {
                type_table.add(typeref.clone())?;
                for (t, _) in &params.params {
                    type_table.add(t.clone())?;
                }
                block.resolve_types(&mut type_table)?;
            }
            Declaration::FuncDecl(typeref, ..) => {
                type_table.add(typeref.clone())?;
            }
        }
    }

    //dbg!(&type_table);

    check_duplication(&type_table)?;
    check_recursive_definition(&type_table)?;

    Ok(type_table)
}

fn check_duplication<'a, 'b>(table: &TypeTable<'a, 'b>) -> Result<(), Error> {
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

fn check_recursive_definition<'a, 'b>(type_table: &TypeTable<'a, 'b>) -> Result<(), Error> {
    type TypeSet<'a> = HashSet<TypeCell<'a>>;
    let mut mark = TypeSet::new();
    let mut done = TypeSet::new();

    fn rec<'a, 'b>(
        type_table: &TypeTable<'a, 'b>,
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
        } else if let Some(base) = v.array_base() {
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

impl Block {
    pub fn resolve_types<'a, 'b>(
        &'b self,
        type_table: &mut TypeTable<'a, 'b>,
    ) -> Result<(), Error> {
        for def in self.ref_vars() {
            type_table.add(def.type_.clone())?;
            if let Some(e) = &def.init {
                e.resolve_types(type_table)?;
            }
        }

        for stmt in self.ref_stmts() {
            stmt.resolve_types(type_table)?;
        }
        Ok(())
    }
}

impl Statement {
    pub fn resolve_types<'a, 'b>(
        &'b self,
        type_table: &mut TypeTable<'a, 'b>,
    ) -> Result<(), Error> {
        match self {
            Statement::Expr(e) => {
                e.resolve_types(type_table)?;
            }
            Statement::Block(b) => {
                b.resolve_types(type_table)?;
            }
            Statement::If(e, st, sf) => {
                e.resolve_types(type_table)?;
                st.resolve_types(type_table)?;
                if let Some(s) = sf.as_ref() {
                    s.resolve_types(type_table)?;
                }
            }
            Statement::While(e, s) | Statement::DoWhile(e, s) => {
                e.resolve_types(type_table)?;
                s.resolve_types(type_table)?;
            }
            Statement::For(init, cond, step, body) => {
                init.resolve_types(type_table)?;
                cond.resolve_types(type_table)?;
                step.resolve_types(type_table)?;
                body.resolve_types(type_table)?;
            }
            Statement::Switch(e, branch) => {
                e.resolve_types(type_table)?;
                for (vals, body) in branch {
                    for v in vals {
                        v.resolve_types(type_table)?;
                    }

                    body.resolve_types(type_table)?;
                }
            }
            Statement::Return(e) => {
                if let Some(e) = e {
                    e.resolve_types(type_table)?;
                }
            }
            _ => (),
        };
        Ok(())
    }
}

impl Expr {
    pub fn resolve_types<'a, 'b, 'c>(
        &'c self,
        type_table: &'b mut TypeTable<'a, 'c>,
    ) -> Result<TypeCell<'a>, Error> {
        let t = match self {
            Expr::Assign(e1, e2) | Expr::AssignOp(e1, _, e2) | Expr::BinOp(_, e1, e2) => {
                e1.resolve_types(type_table)?;
                e2.resolve_types(type_table)?
            }
            Expr::Ternary(cond, e1, e2) => {
                cond.resolve_types(type_table)?;
                e1.resolve_types(type_table)?;
                e2.resolve_types(type_table)?
            }
            Expr::Cast(t, e) => {
                e.resolve_types(type_table)?;
                type_table.add(t.clone())?.clone()
            }
            Expr::PreInc(e)
            | Expr::PreDec(e)
            | Expr::Op(_, e)
            | Expr::Deref(e)
            | Expr::PostInc(e)
            | Expr::PostDec(e)
            | Expr::Member(e, _)
            | Expr::PMember(e, _) => e.resolve_types(type_table)?,
            Expr::Addr(e) => e.resolve_types(type_table)?,
            Expr::SizeofT(t) => type_table.add(t.clone())?.clone(),
            Expr::SizeofE(e) => e.resolve_types(type_table)?,
            Expr::ArrayRef(e1, e2) => {
                e1.resolve_types(type_table)?;
                e2.resolve_types(type_table)?
            }
            Expr::Call(e, args) => {
                for a in &args.0 {
                    a.resolve_types(type_table)?;
                }
                e.resolve_types(type_table)?
            }
            Expr::Primary(p) => p.resolve_types(type_table)?,
        };

        type_table.add_expr(self, t.clone());
        Ok(t)
    }
}

impl Primary {
    pub fn resolve_types<'a, 'b, 'c>(
        &'c self,
        type_table: &'b mut TypeTable<'a, 'c>,
    ) -> Result<TypeCell<'a>, Error> {
        match self {
            Primary::Variable(v) => Ok(type_table
                .add(v.get_entity().unwrap().get_type().clone())?
                .clone()),
            Primary::Expr(e) => e.resolve_types(type_table),
            Primary::Integer(_) => Ok(type_table.long().clone()),
            Primary::Character(_) => Ok(type_table.char().clone()),
            Primary::String(_) => Ok(type_table.string().clone()),
        }
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
        let table = resolve_types(&mut ast, &arena);

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
