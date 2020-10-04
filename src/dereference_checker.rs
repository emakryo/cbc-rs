use crate::ast::*;
use crate::error::Error;
use crate::types::{TypeCell, TypeTable};

pub fn check_dereference<'a>(ast: &Source, type_table: &'a TypeTable<'a>) -> Result<(), Error> {
    for defs in &ast.1 {
        match defs {
            TopDef::DefVars(DefVars(_, _, defs)) | TopDef::DefConst(DefVars(_, _, defs)) => {
                for (_, expr) in defs {
                    if let Some(expr) = expr.as_ref() {
                        expr.check_deref(&type_table)?;
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
                b.check_deref(type_table)?;
            }
            TopDef::DefStuct(_, _) | TopDef::DefUnion(_, _) | TopDef::TypeDef(_, _) => (),
        }
    }
    Ok(())
}

impl Statement {
    fn check_deref<'a>(&self, type_table: &'a TypeTable<'a>) -> Result<(), Error> {
        match self {
            Statement::Expr(e) => e.check_deref(type_table),
            Statement::Block(b) => b.check_deref(type_table),
            Statement::If(e, t, f) => {
                e.check_deref(type_table)?;
                t.check_deref(type_table)?;
                if let Some(f) = f.as_ref() {
                    f.check_deref(type_table)?;
                }
                Ok(())
            }
            Statement::Return(e) => {
                if let Some(e) = e {
                    e.check_deref(type_table)?;
                }
                Ok(())
            }
            Statement::While(e, s) | Statement::DoWhile(e, s) => {
                e.check_deref(type_table)?;
                s.check_deref(type_table)
            }
            Statement::For(i, c, s, b) => {
                i.check_deref(type_table)?;
                c.check_deref(type_table)?;
                s.check_deref(type_table)?;
                b.check_deref(type_table)
            }
            Statement::Switch(e, bs) => {
                e.check_deref(type_table)?;
                for (_, b) in bs {
                    b.check_deref(type_table)?;
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
    fn check_deref<'a>(&self, type_table: &'a TypeTable<'a>) -> Result<(), Error> {
        for vars in self.ref_vars() {
            for (_, expr) in &vars.2 {
                if let Some(expr) = expr {
                    expr.check_deref(type_table)?;
                }
            }
        }

        for stmt in self.ref_stmts() {
            stmt.check_deref(type_table)?;
        }
        Ok(())
    }
}

impl Expr {
    fn check_deref<'a>(&self, type_table: &'a TypeTable<'a>) -> Result<(), Error> {
        match self {
            Expr::Assign(t, e) | Expr::AssignOp(t, _, e) => {
                if !t.is_assignable(type_table)? {
                    return Err(Error::Semantic(format!("Not assignable: {:?}", t)));
                }
                e.check_deref(type_table)?;
            }
            Expr::BinOp(_, e1, e2) => {
                e1.check_deref(type_table)?;
                e2.check_deref(type_table)?;
            }
            Expr::Ternary(c, t, f) => {
                c.check_deref(type_table)?;
                t.check_deref(type_table)?;
                f.check_deref(type_table)?;
            }
            Expr::Cast(_, t) => t.check_deref(type_table)?,
            Expr::PreInc(e) | Expr::PreDec(e) | Expr::PostInc(e) | Expr::PostDec(e) => {
                e.check_deref(type_table)?;
                if !e.is_assignable(type_table)? {
                    return Err(Error::Semantic(format!("Not assignable: {:?}", e)));
                }
            }
            Expr::Call(e, args) => {
                e.check_deref(type_table)?;
                for a in &args.0 {
                    a.check_deref(type_table)?;
                }
                let t = e.get_type(type_table)?;
                if !t.is_func_pointer() {
                    return Err(Error::Semantic(format!(
                        "Function call to non-function variable: {:?}",
                        self
                    )));
                }
            }
            Expr::Member(e, name) => {
                let t = e.get_type(type_table)?;
                t.get_field(name)?;
            }
            Expr::PMember(e, name) => {
                e.get_type(type_table)?
                    .pointer_base()
                    .ok_or(Error::Semantic(format!(
                        "Arrow operator for non-pointer value: {:?}",
                        self
                    )))?
                    .get_field(name)?;
            }
            Expr::Addr(e) => e.check_deref(type_table)?,
            Expr::Primary(_) => (),
            Expr::Op(_, t) => t.check_deref(type_table)?,
            Expr::ArrayRef(e, i) => {
                e.check_deref(type_table)?;
                i.check_deref(type_table)?;

                let t = e.get_type(type_table)?;
                if !t.is_pointer() && !t.is_array() {
                    return Err(Error::Semantic(format!(
                        "Index access to non-array type: {:?}",
                        self
                    )));
                }
            }
            Expr::Deref(e) => e.check_deref(type_table)?,
            Expr::SizeofE(e) => e.check_deref(type_table)?,
            Expr::SizeofT(_) => (),
        }
        Ok(())
    }

    fn is_constant(&self) -> bool {
        match self {
            Expr::Primary(p) => p.is_constant(),
            _ => false,
        }
    }

    fn is_assignable<'a>(&self, type_table: &'a TypeTable<'a>) -> Result<bool, Error> {
        let t = self.get_type(type_table)?;
        Ok(!t.is_array() && !t.is_function())
    }

    pub fn get_type<'a>(&self, type_table: &TypeTable<'a>) -> Result<TypeCell<'a>, Error> {
        match self {
            Expr::BinOp(_, e, _) => e.get_type(type_table),
            Expr::Cast(t, _) => {
                if let Some(t) = type_table.get(t) {
                    Ok(t.clone())
                } else {
                    Err(Error::Semantic(format!("Invalid type: {:?}", t)))
                }
            }
            Expr::Primary(p) => p.get_type(type_table),
            Expr::ArrayRef(e, _) => {
                let t = e.get_type(type_table)?;
                if let Some(base) = t.array_base() {
                    return Ok(base);
                }
                Err(Error::Semantic(format!("Invalid index access: {:?}", self)))
            }
            Expr::Deref(e) => {
                let t = e.get_type(type_table)?;
                if let Some(base) = t.pointer_base() {
                    return Ok(base);
                }

                Err(Error::Semantic(format!("Invalid dereference: {:?}", self)))
            }
            Expr::Member(e, name) => {
                let t = e.get_type(type_table)?;
                t.get_field(name)
            }
            Expr::PMember(e, name) => {
                let t = e.get_type(type_table)?;
                if let Some(base) = t.pointer_base() {
                    base.get_field(name)
                } else {
                    Err(Error::Semantic(format!(
                        "Arrow access to non-pointer value: {:?}",
                        self
                    )))
                }
            }
            Expr::PostInc(e) | Expr::PostDec(e) => e.as_ref().get_type(type_table),
            Expr::Call(e, _) => {
                let t = e.get_type(type_table)?;
                if let Some(base) = t.pointer_base() {
                    if let Some(base) = base.return_type() {
                        return Ok(base);
                    }
                }
                Err(Error::Semantic(format!(
                    "Function call to non-function variable: {:?}",
                    self
                )))
            }
            e => todo!("{:?}", e),
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

    pub fn get_type<'a>(&self, type_table: &TypeTable<'a>) -> Result<TypeCell<'a>, Error> {
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
                    .expect(&format!("Type must be resolved: {:?}", t))
                    .clone())
            }
            Primary::Integer(_) => Ok(type_table.long()),
            Primary::Character(_) => Ok(type_table.char()),
            Primary::String(_) => Ok(type_table.string()),
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
            let arena = crate::types::TypeArena::new();
            let table = resolve_types(&mut ast, &arena);
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
