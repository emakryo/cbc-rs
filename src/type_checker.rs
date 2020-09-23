use crate::ast::*;
use crate::error::Error;
use crate::type_resolver::{Type, TypeTable};

pub fn check_type(ast: &mut Source, type_table: &TypeTable) -> Result<(), Error> {
    for def in &mut ast.1 {
        match def {
            TopDef::Defun(_, _, _, _, block) => {
                block.check_type(type_table)?;
            }
            TopDef::DefVars(vars) | TopDef::DefConst(vars) => {
                vars.check_type(type_table)?;
            }
            _ => (),
        }
    }

    Ok(())
}

fn implicit_conversion(lhs: &TypeRef, rhs: &TypeRef) -> TypeRef {
    todo!()
}

impl Block {
    fn check_type(&mut self, type_table: &TypeTable) -> Result<(), Error> {
        for vars in self.mut_vars() {
            vars.check_type(type_table)?;
        }

        for stmt in self.mut_stmts() {
            stmt.check_type(type_table)?;
        }

        Ok(())
    }
}

impl DefVars {
    fn check_type(&mut self, type_table: &TypeTable) -> Result<(), Error> {
        for (_, init) in &mut self.2 {
            if let Some(e) = init.as_mut() {
                let t = e.check_type(type_table)?;
                if &t != &self.1 {
                    e.cast_to(t)?;
                }
            }
        }
        Ok(())
    }
}

impl Statement {
    fn check_type(&mut self, type_table: &TypeTable) -> Result<(), Error> {
        match self {
            Statement::Expr(e) => {
                e.check_type(type_table)?;
            }
            s => todo!("{:?}", s),
        }

        Ok(())
    }
}

impl Expr {
    fn check_type(&mut self, type_table: &TypeTable) -> Result<TypeRef, Error> {
        match self {
            Expr::Primary(p) => p.check_type(type_table),
            Expr::Call(e, args) => {
                let t = type_table
                    .get(&e.check_type(type_table)?)
                    .ok_or(Error::Semantic("Type not found".into()))?;

                if let Type::Function {
                    base,
                    params,
                    variable_length,
                } = t
                    .pointer_base()
                    .ok_or(Error::Semantic(
                        "Function call to non function pointer".into(),
                    ))?
                    .as_ref()
                {
                    if *variable_length {
                        if params.len() > args.0.len() {
                            return Err(Error::Semantic("Fewer arguments are given to variable length function".into()));
                        }
                    } else {
                        if params.len() != args.0.len() {
                            return Err(Error::Semantic("Number of argument does not match".into()));
                        }
                    }

                    for (t, e) in params.iter().zip(args.0.iter_mut()) {
                        let tref = e.check_type(type_table)?;
                        let u = type_table
                            .get(&tref)
                            .ok_or(Error::Semantic(format!("Type not found: {:?}", tref)))?;
                        if *t != u {
                            e.cast_to(t.to_typeref())?;
                        }
                    }
                    Ok(base.to_typeref())
                } else {
                    Err(Error::Semantic(
                        "Function call to non function pointer".into(),
                    ))
                }
            }
            e => todo!("{:?}", e),
        }
    }

    fn cast_to(&mut self, ty: TypeRef) -> Result<(), Error> {
        let dummy = Expr::Primary(Primary::Integer(Integer(0)));
        let expr = std::mem::replace(self, dummy);
        *self = Expr::Cast(ty, Box::new(expr));

        Ok(())
    }
}

impl Primary {
    fn check_type(&mut self, type_table: &TypeTable) -> Result<TypeRef, Error> {
        match self {
            Primary::Integer(_) => Ok(TypeRef::Long),
            Primary::Character(_) => Ok(TypeRef::Char),
            Primary::String(_) => Ok(TypeRef::Array {
                base: Box::new(TypeRef::Char),
                size: None,
            }),
            Primary::Variable(v) => Ok(v.get_entity().unwrap().get_type().clone()),
            Primary::Expr(e) => e.check_type(type_table),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dereference_checker::check_dereference;
    use crate::parser::parse_source;
    use crate::type_resolver::resolve_types;
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
                continue;
            }

            let table = table.unwrap();
            let deref = check_dereference(&ast, &table);
            if deref.is_err() {
                dbg!(&deref);
                continue;
            }

            let type_check = check_type(&mut ast, &table);
            if type_check.is_err() {
                dbg!(&type_check);
            }
        }
    }
}
