use crate::ast::*;
use crate::error::Error;
use crate::types::{TypeCell, TypeTable};

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

fn implicit_conversion<'a>(lhs: &TypeCell<'a>, rhs: &TypeCell<'a>) -> Result<TypeCell<'a>, Error> {
    // TODO implement
    if lhs == rhs {
        Ok(lhs.clone())
    } else {
        Err(Error::Semantic("Type does not match".into()))
    }
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
                if &t != type_table.get(&self.1).unwrap() {
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
            Statement::Return(e) => {
                if let Some(e) = e {
                    e.check_type(type_table)?;
                }
            }
            s => todo!("{:?}", s),
        }

        Ok(())
    }
}

impl Expr {
    fn check_type<'a, 'b>(&mut self, type_table: &TypeTable<'a, 'b>) -> Result<TypeCell<'a>, Error> {
        match self {
            Expr::Primary(p) => p.check_type(type_table),
            Expr::Call(e, args) => {
                let t = e.get_type(type_table)?;
                if !t.is_func_pointer() {
                    return Err(Error::Semantic("Function call to non-function value".into()));
                }

                let params = t.params().unwrap();

                if params.len() != args.0.len() {
                    return Err(Error::Semantic("Number of parameter does not match.".into()));
                }

                for (p, a) in params.iter().zip(args.0.iter_mut()) {
                    let t = a.get_type(type_table)?;
                    if &t != p {
                        let t = implicit_conversion(p, &t)?;
                        a.cast_to(t)?;
                    }
                }

                // if let Type::Function {
                //     base,
                //     params,
                //     variable_length,
                // } = t
                //     .pointer_base()
                //     .ok_or(Error::Semantic(
                //         "Function call to non function pointer".into(),
                //     ))?
                //     .as_ref()
                // {
                //     if *variable_length {
                //         if params.len() > args.0.len() {
                //             return Err(Error::Semantic("Fewer arguments are given to variable length function".into()));
                //         }
                //     } else {
                //         if params.len() != args.0.len() {
                //             return Err(Error::Semantic("Number of argument does not match".into()));
                //         }
                //     }

                //     for (t, e) in params.iter().zip(args.0.iter_mut()) {
                //         let u = e.get_type(type_table)?;
                //         if *t != u {
                //             e.cast_to(t.to_typeref())?;
                //         }
                //     }
                //     Ok(base.to_typeref())
                // } else {
                //     Err(Error::Semantic(
                //         "Function call to non function pointer".into(),
                //     ))
                // }
                todo!()
            }
            Expr::Assign(d, e) | Expr::AssignOp(d, _, e) => {
                let td = d.check_type(type_table)?;
                let te = e.check_type(type_table)?;

                // if te != td {
                //     e.cast_to(td.to_typeref())?;
                // }
                // Ok(td.to_typeref())
                todo!()
            }
            Expr::BinOp(_, e1, e2) => {
                let t1 = e1.get_type(type_table)?;
                let t2 = e2.get_type(type_table)?;

                // if t1 != t2 {
                //     let tt = implicit_conversion(Rc::clone(&t1), Rc::clone(&t2))?;
                //     if tt != t1 {
                //         e1.cast_to(tt.to_typeref())?;
                //     }
                //     if tt != t2 {
                //         e2.cast_to(tt.to_typeref())?;
                //     }
                //     Ok(tt.to_typeref())
                // } else {
                //     Ok(t1.to_typeref())
                // }
                todo!()
            }
            Expr::Member(e, f) => {
                let t = e.get_type(type_table)?;
                t.get_field(f)
            }
            Expr::Addr(e) => {
                // Ok(TypeRef::Pointer { base: Box::new(type_table.get(k)) })
                todo!()
            }
            e => todo!("{:?}", e),
        }
    }

    fn cast_to<'a>(&mut self, ty: TypeCell<'a>) -> Result<(), Error> {
        let dummy = Expr::Primary(Primary::Integer(Integer(0)));
        let expr = std::mem::replace(self, dummy);
        *self = Expr::Cast(ty.to_typeref(), Box::new(expr));

        Ok(())
    }
}

impl Primary {
    fn check_type<'a, 'b>(&mut self, type_table: &TypeTable<'a, 'b>) -> Result<TypeCell<'a>, Error> {
        match self {
            Primary::Expr(e) => e.check_type(type_table),
            p => p.get_type(type_table)
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

    fn test_from_file(file_name: std::path::Path) {
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
        let arena = crate::types::TypeArena::new();
        let mut ast = parse_source(&code, &header_paths).unwrap();
        let scope = resolve_variables(&mut ast);
        if scope.is_err() {
            dbg!(scope).ok();
            return;
        }

        let table = resolve_types(&mut ast, &arena);
        if table.is_err() {
            dbg!(&table);
            return;
        }

        let table = table.unwrap();
        let deref = check_dereference(&ast, &table);
        if deref.is_err() {
            dbg!(&deref);
            return;
        }

        let type_check = check_type(&mut ast, &table);
        if type_check.is_err() {
            dbg!(&type_check);
        }
    }

    #[test]
    fn test_from_files() {
        use std::io::Read;

        let root = env!("CARGO_MANIFEST_DIR");

        for file_name in glob::glob(&format!("{}/cbc-1.0/test/*.cb", root)).unwrap() {
            let file_name = file_name.unwrap();
        }
    }
}
