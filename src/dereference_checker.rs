use crate::ast::*;
use crate::error::Error;
use crate::types::TypeCell;

pub fn check_dereference<'a, 'b, 'c>(
    ast: &Ast<'c, TypedExpr<'a>, TypeCell<'a>>,
) -> Result<(), Error> {
    for defs in &ast.declarations {
        match defs {
            Declaration::DefVar(DefVar { init, .. })
            | Declaration::DefConst(DefVar { init, .. }) => {
                if let Some(expr) = init.as_ref() {
                    expr.check_deref()?;
                    if !expr.is_constant() {
                        return Err(Error::Semantic(format!(
                            "Global initializer is not constant: {:?}",
                            expr
                        )));
                    }
                }
            }
            Declaration::Defun(_, b) => {
                b.check_deref()?;
            }
            Declaration::FuncDecl(_)
            | Declaration::VarDecl(_)
            | Declaration::DefStuct(_, _)
            | Declaration::DefUnion(_, _)
            | Declaration::TypeDef(_, _) => (),
        }
    }
    Ok(())
}

impl<'a> Statement<TypedExpr<'a>, TypeCell<'a>> {
    fn check_deref<'b>(&self) -> Result<(), Error> {
        match self {
            Statement::Expr(e) => e.check_deref(),
            Statement::Block(b) => b.check_deref(),
            Statement::If(e, t, f) => {
                e.check_deref()?;
                t.check_deref()?;
                if let Some(f) = f.as_ref() {
                    f.check_deref()?;
                }
                Ok(())
            }
            Statement::Return(e) => {
                if let Some(e) = e {
                    e.check_deref()?;
                }
                Ok(())
            }
            Statement::While(e, s) | Statement::DoWhile(e, s) => {
                e.check_deref()?;
                s.check_deref()
            }
            Statement::For(i, c, s, b) => {
                i.check_deref()?;
                c.check_deref()?;
                s.check_deref()?;
                b.check_deref()
            }
            Statement::Switch(e, bs) => {
                e.check_deref()?;
                for (_, b) in bs {
                    b.check_deref()?;
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

impl<'a> Block<TypedExpr<'a>, TypeCell<'a>> {
    fn check_deref<'b>(&self) -> Result<(), Error> {
        for DefVar { init, .. } in self.ref_vars() {
            if let Some(expr) = init {
                expr.check_deref()?;
            }
        }

        for stmt in self.ref_stmts() {
            stmt.check_deref()?;
        }
        Ok(())
    }
}

impl<'a> TypedExpr<'a> {
    fn check_deref<'b>(&self) -> Result<(), Error> {
        match self.inner.as_ref() {
            BaseExpr::Assign(t, e) | BaseExpr::AssignOp(_, t, e) => {
                if !t.is_assignable()? {
                    return Err(Error::Semantic(format!("Not assignable: {:?}", t)));
                }
                e.check_deref()?;
            }
            BaseExpr::BinOp(_, e1, e2) => {
                e1.check_deref()?;
                e2.check_deref()?;
            }
            BaseExpr::Ternary(c, t, f) => {
                c.check_deref()?;
                t.check_deref()?;
                f.check_deref()?;
            }
            BaseExpr::Cast(_, t) => t.check_deref()?,
            BaseExpr::PreInc(e)
            | BaseExpr::PreDec(e)
            | BaseExpr::PostInc(e)
            | BaseExpr::PostDec(e) => {
                e.check_deref()?;
                if !e.is_assignable()? {
                    return Err(Error::Semantic(format!("Not assignable: {:?}", e)));
                }
            }
            BaseExpr::Call(e, args) => {
                e.check_deref()?;
                for a in &args.0 {
                    a.check_deref()?;
                }
                let t = &e.type_;
                if !t.is_func_pointer() {
                    return Err(Error::Semantic(format!(
                        "Function call to non-function variable: {:?}",
                        self
                    )));
                }
            }
            BaseExpr::Member(e, name) => {
                let t = &e.type_;
                t.get_field(&name)?;
            }
            BaseExpr::PMember(e, name) => {
                e.type_
                    .pointer_base()
                    .ok_or(Error::Semantic(format!(
                        "Arrow operator for non-pointer value: {:?}",
                        self
                    )))?
                    .get_field(&name)?;
            }
            BaseExpr::Addr(e) => e.check_deref()?,
            BaseExpr::Primary(_) => (),
            BaseExpr::UnaryOp(_, t) => t.check_deref()?,
            BaseExpr::ArrayRef(e, i) => {
                e.check_deref()?;
                i.check_deref()?;

                let t = &e.type_;
                if !t.is_pointer() && !t.is_array() {
                    return Err(Error::Semantic(format!(
                        "Index access to non-array type: {:?}",
                        self
                    )));
                }
            }
            BaseExpr::Deref(e) => e.check_deref()?,
            BaseExpr::SizeofE(e) => e.check_deref()?,
            BaseExpr::SizeofT(_) => (),
        }
        Ok(())
    }

    fn is_constant(&self) -> bool {
        match self.inner.as_ref() {
            BaseExpr::Primary(p) => p.is_constant(),
            _ => false,
        }
    }

    fn is_assignable<'b>(&self) -> Result<bool, Error> {
        let t = &self.type_;
        Ok(!t.is_array() && !t.is_function())
    }
}

impl<'a> Primary<TypedExpr<'a>, TypeCell<'a>> {
    fn is_constant(&self) -> bool {
        match self {
            Primary::Integer(_) | Primary::Character(_) | Primary::String(_) => true,
            Primary::Variable(_) => false,
            Primary::Expr(e) => e.is_constant(),
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
            let ast = resolve_types(ast, &arena, scope.unwrap());
            if ast.is_err() {
                dbg!(ast).ok();
                continue;
            }
            let (ast, _scope) = ast.unwrap();

            let verdict = check_dereference(&ast);
            if verdict.is_err() {
                dbg!(verdict).ok();
            }
            //break;
        }
    }
}
