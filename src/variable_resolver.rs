use crate::ast::*;
use crate::entity::{new_scope, GlobalScope, LocalScope};
use crate::error::Error;
use std::cell::RefCell;
use std::rc::Rc;

pub fn resolve_variables(ast: &mut Ast) -> Result<GlobalScope, Error> {
    let mut global = GlobalScope::new();

    for def in &mut ast.declarations {
        match def {
            Declaration::Defun(defun, block) => {
                let scope = global.add_function(
                    defun.name.to_string(),
                    defun.type_.clone(),
                    defun.params.clone(),
                )?;
                block.set_scope(scope);
            }
            Declaration::DefVar(def) | Declaration::VarDecl(def) | Declaration::DefConst(def) => {
                global.add_variable(def.name.to_string(), def.type_.clone())?;
            }
            Declaration::FuncDecl(t, name, p) => {
                global.add_function(name.to_string(), t.clone(), p.clone())?;
            }
            Declaration::DefStuct(_, _)
            | Declaration::DefUnion(_, _)
            | Declaration::TypeDef(_, _) => (),
        }
    }

    for def in &mut ast.declarations {
        match def {
            Declaration::Defun(_, block) => {
                block.resolve_variables(block.get_scope().expect("No scope set"))?;
            }
            Declaration::DefVar(def) => {
                if let Some(expr) = &mut def.init {
                    expr.resolve_variables(Rc::clone(&global.root))?;
                }
            }
            Declaration::DefConst(def) => {
                if let Some(expr) = &mut def.init {
                    expr.resolve_variables(Rc::clone(&global.root))?;
                } else {
                    return Err(Error::Semantic(format!(
                        "const declaration has no initial value: {}",
                        def.name.to_string()
                    )));
                }
            }
            _ => (),
        }
    }

    Ok(global)
}

impl Block {
    fn resolve_variables(&mut self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        for var in self.mut_vars() {
            scope
                .borrow_mut()
                .add_variable(var.name.to_string(), var.type_.clone())?;

            if let Some(init) = &mut var.init {
                init.resolve_variables(Rc::clone(&scope))?;
            }
        }

        for statement in self.mut_stmts() {
            statement.resolve_variables(Rc::clone(&scope))?;
        }

        Ok(())
    }
}

impl Statement {
    fn resolve_variables(&mut self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        macro_rules! resolve {
            ($e:expr) => {
                $e.resolve_variables(Rc::clone(&scope))?;
            };
        }
        match self {
            Statement::Expr(e) => resolve!(e),
            Statement::Block(b) => {
                b.resolve_variables(new_scope(scope))?;
            }
            Statement::If(e, t, f) => {
                resolve!(e);
                resolve!(t);
                if let Some(f) = f.as_mut() {
                    resolve!(f);
                }
            }
            Statement::While(e, s) => {
                resolve!(e);
                resolve!(s);
            }
            Statement::DoWhile(e, s) => {
                resolve!(e);
                resolve!(s);
            }
            Statement::For(i, c, s, b) => {
                resolve!(i);
                resolve!(c);
                resolve!(s);
                resolve!(b);
            }
            Statement::Return(e) => {
                if let Some(e) = e.as_mut() {
                    resolve!(e);
                }
            }
            Statement::Switch(e, bs) => {
                resolve!(e);
                for (_, b) in &mut bs.into_iter() {
                    resolve!(b);
                }
            }
            Statement::None => (),
            Statement::Break => (),
            Statement::Continue => (),
            Statement::Label(_) => (),
            e => todo!("{:?}", e),
        }
        Ok(())
    }
}

impl Expr {
    fn resolve_variables(&mut self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        macro_rules! resolve {
            ($e:expr) => {
                $e.resolve_variables(Rc::clone(&scope))?;
            };
        }

        match self {
            Expr::Assign(t, e) => {
                resolve!(t);
                resolve!(e);
            }
            Expr::AssignOp(t, _, e) => {
                resolve!(t);
                resolve!(e);
            }
            Expr::BinOp(_, e1, e2) => {
                resolve!(e1.as_mut());
                resolve!(e2.as_mut());
            }
            Expr::Ternary(c, e1, e2) => {
                resolve!(c.as_mut());
                resolve!(e1.as_mut());
                resolve!(e2.as_mut());
            }
            Expr::Cast(_, t) => resolve!(t.as_mut()),
            Expr::Call(e, a) => {
                resolve!(e.as_mut());
                resolve!(a);
            }
            Expr::Primary(p) => resolve!(p),
            Expr::Member(e, _) => resolve!(e.as_mut()),
            Expr::PMember(e, _) => resolve!(e.as_mut()),
            Expr::Addr(e) => resolve!(e.as_mut()),
            Expr::PostInc(e) => resolve!(e.as_mut()),
            Expr::PostDec(e) => resolve!(e.as_mut()),
            Expr::Op(_, e) => resolve!(e.as_mut()),
            Expr::ArrayRef(e, i) => {
                resolve!(e.as_mut());
                resolve!(i.as_mut());
            }
            Expr::Deref(e) => resolve!(e.as_mut()),
            Expr::PreInc(e) => resolve!(e.as_mut()),
            Expr::PreDec(e) => resolve!(e.as_mut()),
            Expr::SizeofE(e) => resolve!(e.as_mut()),
            Expr::SizeofT(_) => (),
        }
        Ok(())
    }
}

impl Primary {
    fn resolve_variables(&mut self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        match self {
            Primary::Variable(v) => {
                let name = v.name();
                let entity = scope.borrow().get_entity(&name);
                if let Some(entity) = entity {
                    v.set_entity(entity);
                } else {
                    return Err(Error::Semantic(format!("Undefined variable: {}", name)));
                }
            }
            Primary::Integer(_) | Primary::Character(_) | Primary::String(_) => (),
            Primary::Expr(e) => e.resolve_variables(Rc::clone(&scope))?,
        }
        Ok(())
    }
}

impl Args {
    fn resolve_variables(&mut self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        (&mut self.0)
            .into_iter()
            .map(|e| e.resolve_variables(Rc::clone(&scope)))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::parse_source;

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
            let mut ast = parse_source(&code, &header_paths);
            let scope = resolve_variables(&mut ast.as_mut().unwrap());
            if scope.is_err() {
                dbg!(scope).ok();
            }
            assert!(ast.is_ok(), "faild: {}", file_name.to_str().unwrap());
        }
    }
}
