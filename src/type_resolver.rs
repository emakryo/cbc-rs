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
                    type_table.add_usertype(&arena, n.clone(), typeref.clone())?;
                }
                HeaderDecl::DefStuct(_, _) => todo!(),
                HeaderDecl::DefUnion(_, _) => todo!(),
                _ => (),
            }
        }
    }

    for def in &ast.1 {
        match def {
            TopDef::TypeDef(typeref, n) => {
                type_table.add_usertype(arena, n.clone(), typeref.clone())?;
            }
            TopDef::DefStuct(n, members) => {
                type_table.add_struct(arena, n.clone(), members.clone())?;
            }
            TopDef::DefUnion(n, members) => {
                type_table.add_union(arena, n.clone(), members.clone())?;
            }
            _ => (),
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
            rec(type_table, mark, done, base)?;
        }

        done.insert(v);

        Ok(())
    }

    for v in type_table.values() {
        rec(type_table, &mut mark, &mut done, v)?;
    }

    Ok(())
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
