use crate::ast::*;
use crate::error::Error;
use crate::type_resolver::{Type, TypeTable};
use std::rc::Rc;

pub fn check_dereference(ast: &Source, type_table: &TypeTable) -> Result<(), Error> {
    for defs in &ast.1 {
        match defs {
            TopDef::DefVars(DefVars(_, _, defs)) | TopDef::DefConst(DefVars(_, _, defs)) => {
                for (_, expr) in defs {
                    if let Some(expr) = expr.as_ref() {
                        expr.check(&type_table)?;
                        if !expr.is_constant() {
                            return Err(Error::Semantic(format!(
                                "Global initializer is not constant: {:?}",
                                expr
                            )));
                        }
                    }
                }
            }
            TopDef::Defun(_, _, _, _, b) => {
                b.check(type_table)?;
            }
            TopDef::DefStuct(_, _) | TopDef::DefUnion(_, _) | TopDef::TypeDef(_, _) => (),
        }
    }
    Ok(())
}

impl Statement {
    fn check(&self, type_table: &TypeTable) -> Result<(), Error> {
        match self {
            Statement::Expr(e) => e.check(type_table),
            Statement::Block(b) => b.check(type_table),
            Statement::If(e, t, f) => {
                e.check(type_table)?;
                t.check(type_table)?;
                if let Some(f) = f.as_ref() {
                    f.check(type_table)?;
                }
                Ok(())
            }
            Statement::Return(e) => {
                if let Some(e) = e {
                    e.check(type_table)?;
                }
                Ok(())
            }
            Statement::While(e, s) | Statement::DoWhile(e, s) => {
                e.check(type_table)?;
                s.check(type_table)
            }
            Statement::For(i, c, s, b) => {
                i.check(type_table)?;
                c.check(type_table)?;
                s.check(type_table)?;
                b.check(type_table)
            }
            Statement::Switch(e, bs) => {
                e.check(type_table)?;
                for (_, b) in bs {
                    b.check(type_table)?;
                }
                Ok(())
            }
            Statement::Break | Statement::Continue | Statement::None | Statement::Label(_) => {
                Ok(())
            }
            s => todo!("{:?}", s),
        }
    }
}

impl Block {
    fn check(&self, type_table: &TypeTable) -> Result<(), Error> {
        for vars in self.ref_vars() {
            for (_, expr) in &vars.2 {
                if let Some(expr) = expr {
                    expr.check(type_table)?;
                }
            }
        }

        for stmt in self.ref_stmts() {
            stmt.check(type_table)?;
        }
        Ok(())
    }
}

impl Expr {
    fn check(&self, type_table: &TypeTable) -> Result<(), Error> {
        match self {
            Expr::Assign(t, e) | Expr::AssignOp(t, _, e) => {
                if !t.is_assignable(type_table)? {
                    return Err(Error::Semantic(format!("Not assignable: {:?}", t)));
                }
                e.check(type_table)?;
            }
            Expr::BinOp(_, e1, e2) => {
                e1.check(type_table)?;
                e2.check(type_table)?;
            }
            Expr::Term(t) => {
                t.check(type_table)?;
            }
            Expr::Ternary(c, t, f) => {
                c.check(type_table)?;
                t.check(type_table)?;
                f.check(type_table)?;
            }
        }
        Ok(())
    }

    fn is_constant(&self) -> bool {
        match self {
            Expr::Term(t) => t.is_constant(),
            _ => false,
        }
    }

    fn get_type(&self, type_table: &TypeTable) -> Result<Rc<Type>, Error> {
        match self {
            Expr::BinOp(_, e, _) => e.get_type(type_table),
            Expr::Term(t) => t.get_type(type_table),
            e => todo!("{:?}", e),
        }
    }
}

impl Term {
    fn check(&self, type_table: &TypeTable) -> Result<(), Error> {
        match self {
            Term::Unary(u) => u.check(type_table)?,
            Term::Cast(_, t) => t.check(type_table)?,
        }

        Ok(())
    }

    fn is_constant(&self) -> bool {
        match self {
            Term::Unary(u) => u.is_constant(),
            _ => false,
        }
    }

    fn is_assignable(&self, type_table: &TypeTable) -> Result<bool, Error> {
        match self {
            Term::Unary(u) => u.is_assignable(type_table),
            Term::Cast(_, t) => t.is_assignable(type_table),
        }
    }

    fn get_type(&self, type_table: &TypeTable) -> Result<Rc<Type>, Error> {
        match self {
            Term::Unary(u) => u.get_type(type_table),
            Term::Cast(t, _) => {
                if let Some(t) = type_table.get(t) {
                    Ok(t)
                } else {
                    Err(Error::Semantic(format!("Invalid type: {:?}", t)))
                }
            }
        }
    }
}

impl Unary {
    fn check(&self, type_table: &TypeTable) -> Result<(), Error> {
        match self {
            Unary::PreInc(u) | Unary::PreDec(u) | Unary::PostInc(u) | Unary::PostDec(u) => {
                u.check(type_table)?;
                if !u.is_assignable(type_table)? {
                    return Err(Error::Semantic(format!("Not assignable: {:?}", u)));
                }
                Ok(())
            }
            Unary::Call(u, a) => {
                u.check(type_table)?;
                for e in &a.0 {
                    e.check(type_table)?;
                }
                let t = u.get_type(type_table)?;
                if t.is_func_pointer() {
                    Ok(())
                } else {
                    Err(Error::Semantic(format!(
                        "Function call to non-function variable: {:?}",
                        self
                    )))
                }
            }
            Unary::Member(u, name) => {
                let t = u.get_type(type_table)?;
                t.get_field_type(name, type_table)?;
                Ok(())
            }
            Unary::PMember(u, name) => {
                let t = u.get_type(type_table)?;
                let t = if let Type::Pointer { base } = t.as_ref() {
                    base
                } else {
                    return Err(Error::Semantic(format!(
                        "Arrow operator for non-pointer value: {:?}",
                        self
                    )));
                };
                t.get_field_type(name, type_table)?;
                Ok(())
            }
            Unary::Addr(u) => u.check(type_table),
            Unary::Primary(_) => Ok(()),
            Unary::Op(_, t) => t.check(type_table),
            Unary::ArrayRef(u, e) => {
                u.check(type_table)?;
                e.check(type_table)?;

                let t = u.get_type(type_table)?;
                if t.is_pointer() || t.is_array() {
                    Ok(())
                } else {
                    Err(Error::Semantic(format!(
                        "Index access to non-array type: {:?}",
                        self
                    )))
                }
            }
            Unary::Deref(t) => t.check(type_table),
            Unary::SizeofE(u) => u.check(type_table),
            Unary::SizeofT(_) => Ok(()),
        }
    }

    fn is_constant(&self) -> bool {
        match self {
            Unary::Primary(p) => p.is_constant(),
            _ => false,
        }
    }

    fn is_assignable(&self, type_table: &TypeTable) -> Result<bool, Error> {
        let t = self.get_type(type_table)?;
        Ok(match t.as_ref() {
            Type::Array { .. } | Type::Function { .. } => false,
            _ => true,
        })
    }

    fn get_type(&self, type_table: &TypeTable) -> Result<Rc<Type>, Error> {
        match self {
            Unary::Primary(p) => p.get_type(type_table),
            Unary::ArrayRef(u, _) => {
                let t = u.get_type(type_table)?;
                if let Type::Array { base, .. } = t.as_ref() {
                    return Ok(Rc::clone(base));
                }
                Err(Error::Semantic(format!("Invalid index access: {:?}", self)))
            }
            Unary::Deref(u) => {
                let t = u.get_type(type_table)?;
                if let Type::Pointer { base } = t.as_ref() {
                    return Ok(Rc::clone(base));
                }

                Err(Error::Semantic(format!("Invalid dereference: {:?}", self)))
            }
            Unary::Member(u, name) => {
                let t = u.get_type(type_table)?;
                t.get_field_type(name, type_table)
            }
            Unary::PMember(u, name) => {
                let t = u.get_type(type_table)?;
                if let Type::Pointer { base } = t.as_ref() {
                    base.get_field_type(name, type_table)
                } else {
                    Err(Error::Semantic(format!(
                        "Arrow access to non-pointer value: {:?}",
                        self
                    )))
                }
            }
            Unary::PostInc(u) | Unary::PostDec(u) => u.as_ref().get_type(type_table),
            Unary::Call(u, _) => {
                let t = u.get_type(type_table)?;
                if let Type::Pointer { base } = t.as_ref() {
                    if let Type::Function { base, .. } = base.as_ref() {
                        return Ok(Rc::clone(base));
                    }
                }
                Err(Error::Semantic(format!(
                    "Function call to non-function variable: {:?}",
                    self
                )))
            }
            u => todo!("{:?}", u),
        }
    }
}

impl Primary {
    fn is_constant(&self) -> bool {
        match self {
            Primary::Integer(_) | Primary::Character(_) | Primary::String(_) => true,
            Primary::Variable(_) => false,
            Primary::Expr(e) => e.is_constant(),
        }
    }

    fn get_type(&self, type_table: &TypeTable) -> Result<Rc<Type>, Error> {
        match self {
            Primary::Variable(v) => {
                let entity = if let Some(e) = v.get_entity() {
                    e
                } else {
                    return Err(Error::Semantic(format!("Unresolved entity: {:?}", self)));
                };
                let t = entity.get_type();
                Ok(type_table
                    .get(t)
                    .expect(&format!("Type must be resolved: {:?}", t)))
            }
            Primary::Integer(_) => Ok(Type::long()),
            Primary::Character(_) => Ok(Type::char()),
            Primary::String(_) => Ok(Type::string()),
            Primary::Expr(e) => e.get_type(type_table),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_source;
    use crate::type_resolver::resolve_types;
    use crate::variable_resolver::resolve_variables;

    #[test]
    fn test_from_files() {
        use std::io::Read;

        let root = env!("CARGO_MANIFEST_DIR");

        for file_name in glob::glob(&format!("{}/cbc-1.0/test/*.cb", root)).unwrap() {
            let file_name = file_name.unwrap();
            //let file_name: std::path::PathBuf = "/home/user/std_compiler/cbc-rs/cbc-1.0/test/usertype.cb".into();
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
                dbg!(table).ok();
                continue;
            }
            let table = table.unwrap();

            let verdict = check_dereference(&ast, &table);
            if verdict.is_err() {
                dbg!(verdict).ok();
            }
            //break;
        }
    }
}
