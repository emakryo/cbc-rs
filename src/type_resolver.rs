use crate::ast::*;
use crate::error::Error;
use crate::types::{TypeArena, TypeCell, TypeTable};
use std::collections::HashSet;

pub fn resolve_types<'a>(
    ast: &mut Source,
    arena: &'a TypeArena<'a>,
) -> Result<TypeTable<'a>, Error> {
    let mut type_table = TypeTable::new(arena);

    for (_, decls) in &ast.0 {
        for decl in decls {
            match decl {
                HeaderDecl::TypeDef(typeref, n) => {
                    type_table.add_usertype(n.clone(), typeref.clone())?;
                }
                HeaderDecl::DefStuct(_, _) => todo!(),
                HeaderDecl::DefUnion(_, _) => todo!(),
                HeaderDecl::FuncDecl(typeref, ..) => {
                    type_table.add(typeref.clone())?;
                },
                HeaderDecl::VarsDecl(def) | HeaderDecl::DefConst(def) => {
                    type_table.add(def.1.clone())?;
                    for (_, e) in &def.2 {
                        if let Some(e) = e {
                            e.resolve_types(&mut type_table)?;
                        }
                    }
                }
            }
        }
    }

    for def in &ast.1 {
        match def {
            TopDef::TypeDef(typeref, n) => {
                type_table.add_usertype(n.clone(), typeref.clone())?;
            }
            TopDef::DefStuct(n, members) => {
                type_table.add_struct(n.clone(), members.clone())?;
            }
            TopDef::DefUnion(n, members) => {
                type_table.add_union(n.clone(), members.clone())?;
            }
            TopDef::DefVars(def) | TopDef::DefConst(def) => {
                type_table.add(def.1.clone())?;
                for (_, e) in &def.2 {
                    if let Some(e) = e {
                        e.resolve_types(&mut type_table)?;
                    }
                }
            }
            TopDef::Defun(_, typeref, _, params, block) => {
                type_table.add(typeref.clone())?;
                for (t, _) in &params.params {
                    type_table.add(t.clone())?;
                }
                block.resolve_types(&mut type_table)?;
            }
        }
    }

    //dbg!(&type_table);

    check_duplication(&type_table)?;
    check_recursive_definition(&type_table)?;

    Ok(type_table)
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

    fn rec<'a>(
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
    pub fn resolve_types<'a>(&self, type_table: &mut TypeTable<'a>) -> Result<(), Error> {
        for defs in self.ref_vars() {
            type_table.add(defs.1.clone())?;
            for (_, e) in &defs.2 {
                if let Some(e) = e {
                    e.resolve_types(type_table)?;
                }
            }
        }

        for stmt in self.ref_stmts() {
            stmt.resolve_types(type_table)?;
        }
        Ok(())
    }
}

impl Statement {
    pub fn resolve_types<'a>(&self, type_table: &mut TypeTable<'a>) -> Result<(), Error> {
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
                        v.resolve_types( type_table)?;
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
    pub fn resolve_types<'a, 'b>(&self, type_table: &'b mut TypeTable<'a>) -> Result<&'b TypeCell<'a>, Error> {
        match self {
            Expr::Assign(e1, e2) | Expr::AssignOp(e1, _, e2) | Expr::BinOp(_, e1, e2) => {
                e1.resolve_types(type_table)?;
                e2.resolve_types(type_table)
            }
            Expr::Ternary(cond, e1, e2) => {
                cond.resolve_types(type_table)?;
                e1.resolve_types(type_table)?;
                e2.resolve_types(type_table)
            }
            Expr::Cast(t, e) => {
                e.resolve_types(type_table)?;
                type_table.add(t.clone())
            }
            Expr::PreInc(e) | Expr::PreDec(e) | Expr::Op(_, e) | Expr::Deref(e) | Expr::PostInc(e) | Expr::PostDec(e) |
                Expr::Member(e, _) | Expr::PMember(e, _) => {
                e.resolve_types(type_table)
            }
            Expr::Addr(e) => {
                e.resolve_types(type_table)
            }
            Expr::SizeofT(t) => type_table.add(t.clone()),
            Expr::SizeofE(e) => e.resolve_types(type_table),
            Expr::ArrayRef(e1, e2) => {
                e1.resolve_types(type_table)?;
                e2.resolve_types(type_table)
            }
            Expr::Call(e, args) => {
                for a in &args.0 {
                    a.resolve_types(type_table)?;
                }
                e.resolve_types(type_table)
            }
            Expr::Primary(p) => p.resolve_types(type_table),
        }
    }
}

impl Primary {
    pub fn resolve_types<'a, 'b>(&self, type_table: &'b mut TypeTable<'a>) -> Result<&'b TypeCell<'a>, Error> {
        match self {
            Primary::Variable(v) => {
                type_table.add(v.get_entity().unwrap().get_type().clone())
            }
            Primary::Expr(e) => {
                e.resolve_types(type_table)
            }
            Primary::Integer(_) => Ok(type_table.long()),
            Primary::Character(_) => Ok(type_table.char()),
            Primary::String(_) => Ok(type_table.string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_source;
    use crate::variable_resolver::resolve_variables;

    #[test]
    fn test_from_files() {
        use std::io::Read;

        let root = env!("CARGO_MANIFEST_DIR");

        for file_name in glob::glob(&format!("{}/cbc-1.0/test/*.cb", root)).unwrap() {
            let file_name = file_name.unwrap();
            // if !file_name.to_str().unwrap().contains("while2.cb") {
            //     continue;
            // }
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
                continue;
            }

            let arena = TypeArena::new();
            let table = resolve_types(&mut ast, &arena);

            if table.is_err() {
                dbg!(table).ok();
                continue;
            }
        }
    }
}
