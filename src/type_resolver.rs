use crate::ast::*;
use crate::error::Error;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Type {
    Void,
    Array {
        base: Rc<Type>,
        size: Option<usize>,
    },
    Integer {
        size: usize,
        signed: bool,
    },
    Pointer {
        base: Rc<Type>,
    },
    Function {
        base: Rc<Type>,
        params: Vec<Rc<Type>>,
        variable_length: bool,
    },
    Struct {
        name: Ident,
        members: Vec<(TypeRef, Ident)>,
    },
    Union {
        name: Ident,
        members: Vec<(TypeRef, Ident)>,
    },
    User(String, Rc<Type>),
}

impl Type {
    fn is_void(&self) -> bool {
        *self == Type::Void
    }
}

#[derive(Debug)]
pub struct TypeTable(HashMap<TypeRef, Rc<Type>>);

impl TypeTable {
    fn new() -> Self {
        let mut table = HashMap::new();

        table.insert(
            TypeRef::Char,
            Rc::new(Type::Integer {
                size: 1,
                signed: false,
            }),
        );
        table.insert(
            TypeRef::UChar,
            Rc::new(Type::Integer {
                size: 1,
                signed: false,
            }),
        );
        table.insert(
            TypeRef::Short,
            Rc::new(Type::Integer {
                size: 4,
                signed: true,
            }),
        );
        table.insert(
            TypeRef::UShort,
            Rc::new(Type::Integer {
                size: 4,
                signed: false,
            }),
        );
        table.insert(
            TypeRef::Int,
            Rc::new(Type::Integer {
                size: 4,
                signed: true,
            }),
        );
        table.insert(
            TypeRef::UInt,
            Rc::new(Type::Integer {
                size: 4,
                signed: false,
            }),
        );
        table.insert(
            TypeRef::Long,
            Rc::new(Type::Integer {
                size: 8,
                signed: true,
            }),
        );
        table.insert(
            TypeRef::ULong,
            Rc::new(Type::Integer {
                size: 8,
                signed: false,
            }),
        );
        table.insert(TypeRef::Void, Rc::new(Type::Void));

        TypeTable(table)
    }

    fn get(&self, k: &TypeRef) -> Option<Rc<Type>> {
        match k {
            TypeRef::Array { base, size } => Some(Rc::new(Type::Array {
                base: self.get(base.as_ref())?,
                size: *size,
            })),
            TypeRef::Pointer { base } => Some(Rc::new(Type::Pointer {
                base: self.get(base.as_ref())?,
            })),
            TypeRef::Function {
                base,
                params,
                variable_length,
            } => Some(Rc::new(Type::Function {
                base: self.get(base.as_ref())?,
                params: params
                    .iter()
                    .map(|p| self.get(p))
                    .collect::<Option<Vec<_>>>()?,
                variable_length: *variable_length,
            })),
            k => self.0.get(k).map(Rc::clone),
        }
    }

    fn insert(&mut self, k: TypeRef, v: Rc<Type>) {
        self.0.insert(k, v);
    }

    fn add_usertype(&mut self, name: Ident, typeref: TypeRef) -> Result<(), Error> {
        if let Some(t) = self.get(&typeref) {
            let user_type = TypeRef::User(name);
            if self.0.contains_key(&user_type) {
                return Err(Error::Semantic(format!(
                    "Duplicated type definition: {:?}",
                    user_type
                )));
            }
            self.insert(user_type, t.clone());
        } else {
            return Err(Error::Semantic(format!("Undefined type: {:?}", typeref)));
        }
        Ok(())
    }

    fn add_struct(&mut self, name: Ident, members: Vec<(TypeRef, Ident)>) -> Result<(), Error> {
        let typeref = TypeRef::Struct(name.clone());
        if self.0.contains_key(&typeref) {
            return Err(Error::Semantic(format!(
                "Duplicated struct definition: {:?}",
                typeref
            )));
        }

        self.insert(
            typeref,
            Rc::new(Type::Struct {
                name: name,
                members,
            }),
        );
        Ok(())
    }

    fn add_union(&mut self, name: Ident, members: Vec<(TypeRef, Ident)>) -> Result<(), Error> {
        let typeref = TypeRef::Union(name.clone());
        if self.0.contains_key(&typeref) {
            return Err(Error::Semantic(format!(
                "Duplicated union definition: {:?}",
                typeref
            )));
        }

        self.insert(
            typeref,
            Rc::new(Type::Union {
                name: name,
                members,
            }),
        );
        Ok(())
    }
}

pub fn resolve_types(ast: &mut Source) -> Result<TypeTable, Error> {
    let mut type_table = TypeTable::new();

    for (_, decls) in &ast.0 {
        for decl in decls {
            match decl {
                HeaderDecl::TypeDef(typeref, n) => {
                    type_table.add_usertype(n.clone(), typeref.clone())?;
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
                type_table.add_usertype(n.clone(), typeref.clone())?;
            }
            TopDef::DefStuct(n, members) => {
                type_table.add_struct(n.clone(), members.clone())?;
            }
            TopDef::DefUnion(n, members) => {
                type_table.add_union(n.clone(), members.clone())?;
            }
            _ => (),
        }
    }

    check_void(&type_table)?;
    check_duplication(&type_table)?;
    check_recursive_definition(&type_table)?;

    Ok(type_table)
}

pub fn check_void(type_table: &TypeTable) -> Result<(), Error> {
    fn check(t: &Rc<Type>, type_table: &TypeTable) -> Result<(), Error> {
        let err = Err(Error::Semantic(format!("Invalid void in type")));
        match t.as_ref() {
            Type::Array { base, size: _ } => {
                if base.as_ref().is_void() {
                    return err;
                }
            }
            Type::Struct { members, name: _ } | Type::Union { members, name: _ } => {
                for (r, _) in members {
                    if let Some(t) = type_table.get(r) {
                        if t.as_ref().is_void() {
                            return err;
                        }
                    } else {
                        return Err(Error::Semantic(format!("Unknown type: {:?}", r)));
                    }
                }
            }
            _ => (),
        }
        Ok(())
    }

    for t in type_table.0.values() {
        check(t, type_table)?;
    }

    Ok(())
}

pub fn check_duplication(type_table: &TypeTable) -> Result<(), Error> {
    for t in type_table.0.values() {
        match t.as_ref() {
            Type::Struct { members, name: _ } | Type::Union { members, name: _ } => {
                let mut names = HashSet::new();
                for (_, n) in members {
                    if names.contains(n) {
                        return Err(Error::Semantic(format!(
                            "Duplicated field: {}",
                            n.to_string()
                        )));
                    }
                    names.insert(n.clone());
                }
            }
            _ => (),
        }
    }

    Ok(())
}

pub fn check_recursive_definition(type_table: &TypeTable) -> Result<(), Error> {
    type TypeSet = HashSet<Rc<Type>>;
    let mut mark = TypeSet::new();
    let mut done = TypeSet::new();

    fn rec(
        type_table: &TypeTable,
        mark: &mut TypeSet,
        done: &mut TypeSet,
        v: Rc<Type>,
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

        mark.insert(Rc::clone(&v));

        match v.as_ref() {
            Type::Struct { name: _, members } | Type::Union { name: _, members } => {
                for (typeref, _) in members {
                    if let Some(t) = type_table.get(typeref) {
                        rec(type_table, mark, done, t)?;
                    } else {
                        return Err(Error::Semantic(format!("Undefined type: {:?}", typeref)));
                    }
                }
            }
            Type::User(_, t) => {
                rec(type_table, mark, done, Rc::clone(t))?;
            }
            Type::Array { base, size: _ } => {
                rec(type_table, mark, done, Rc::clone(base))?;
            }
            _ => (),
        }

        done.insert(Rc::clone(&v));

        Ok(())
    }

    for v in type_table.0.values() {
        rec(type_table, &mut mark, &mut done, Rc::clone(v))?;
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

            let table = resolve_types(&mut ast);

            if table.is_err() {
                dbg!(&table);
            }
        }
    }
}
