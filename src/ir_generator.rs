use crate::ast;
use crate::entity::{Entity, GlobalScope, Scope};
use crate::error::Error;
use crate::ir;
use crate::types::TypeCell;
use std::rc::Rc;

impl<'a, 'b> ast::Ast<'a, ast::TypedExpr<'b>, TypeCell<'b>> {
    pub fn transform<'c>(self, scope: &GlobalScope<TypeCell<'a>>) -> Result<ir::IR<'b>, Error> {
        let mut defvars = vec![];
        let mut defuns = vec![];
        let mut funcdecls = vec![];
        let mut labels = ir::LabelTable::new();
        for decls in self.declarations {
            match decls {
                ast::Declaration::DefVar(def) => {
                    let mut stmts = vec![];
                    let e = def.init.map(|e| e.transform(&mut stmts));
                    if stmts.len() > 0 {
                        return Err(Error::Semantic(
                            "Invalid initial value for global variable".into(),
                        ));
                    }
                    defvars.push(ast::DefVar {
                        storage: def.storage,
                        type_: def.type_,
                        name: def.name,
                        init: e.map_or(Ok(None), |v| v.map(Some))?,
                    });
                }
                ast::Declaration::Defun(def, block) => {
                    let mut stmts = vec![];
                    block.transform(&mut stmts, &mut labels)?;
                    defuns.push((def, stmts));
                }
                ast::Declaration::FuncDecl(def) => {
                    funcdecls.push(def);
                }
                _ => (),
            }
        }

        Ok(ir::IR {
            defvars,
            defuns,
            funcdecls,
        })
    }
}

fn assign<'a>(stmts: &mut Vec<ir::Statement<'a>>, lhs: ir::Expr<'a>, rhs: ir::Expr<'a>) {
    stmts.push(ir::Statement::Assign { lhs, rhs });
}

fn tmp_var<'a>(type_: &TypeCell<'a>) -> ir::Expr<'a> {
    ir::Expr {
        base: ir::BaseExpr::Var(Rc::new(Entity::Variable {
            name: "tmp".into(),
            type_: type_.clone(),
        })),
        type_: type_.clone(),
    }
}

impl<'a> ast::Block<ast::TypedExpr<'a>, TypeCell<'a>> {
    fn transform(
        self,
        stmts: &mut Vec<ir::Statement<'a>>,
        labels: &mut ir::LabelTable,
    ) -> Result<(), Error> {
        for var in self.vars {
            if let Some(init) = var.init {
                let ent = self
                    .scope
                    .clone()
                    .unwrap()
                    .borrow()
                    .get_entity(&var.name.0)
                    .unwrap();
                let lhs = ir::Expr {
                    base: ir::BaseExpr::Var(ent),
                    type_: var.type_,
                };
                if var.storage.static_ {
                    todo!()
                } else {
                    let rhs = init.transform(stmts)?;
                    assign(stmts, lhs, rhs);
                }
            }
        }

        for stmt in self.stmts {
            stmt.transform(stmts, labels)?;
        }

        Ok(())
    }
}

impl<'a> ast::Statement<ast::TypedExpr<'a>, TypeCell<'a>> {
    fn transform(
        self,
        stmts: &mut Vec<ir::Statement<'a>>,
        labels: &mut ir::LabelTable,
    ) -> Result<(), Error> {
        match self {
            ast::Statement::None => (),
            ast::Statement::Label(label) => stmts.push(ir::Statement::Label(labels.add(label.0))),
            ast::Statement::Expr(expr) => {
                let e = expr.transform(stmts)?;
                stmts.push(ir::Statement::Expr(e));
            }
            ast::Statement::Block(block) => {
                block.transform(stmts, labels)?;
            }
            ast::Statement::If(cond, then, else_) => {
                let then_label = labels.define_tmp();
                let else_label = labels.define_tmp();
                let end_label = labels.define_tmp();
                let cond = cond.transform(stmts)?;
                if let Some(else_) = else_ {
                    stmts.push(ir::Statement::CJump {
                        cond,
                        then: then_label.clone(),
                        else_: else_label.clone(),
                    });
                    stmts.push(ir::Statement::Label(then_label));
                    then.transform(stmts, labels)?;
                    stmts.push(ir::Statement::Jump(end_label.clone()));
                    stmts.push(ir::Statement::Label(else_label));
                    else_.transform(stmts, labels)?;
                    stmts.push(ir::Statement::Label(end_label));
                } else {
                    stmts.push(ir::Statement::CJump {
                        cond,
                        then: then_label.clone(),
                        else_: end_label.clone(),
                    });
                    stmts.push(ir::Statement::Label(then_label));
                    then.transform(stmts, labels)?;
                    stmts.push(ir::Statement::Label(end_label));
                }
            }
            _ => todo!(),
        }

        Ok(())
    }
}

impl ast::BinOp {
    fn transform<'a>(self, type_: &TypeCell<'a>) -> Result<ir::BinOp, Error> {
        let signed = type_.is_signed().unwrap();
        let op = match self {
            ast::BinOp::Greater => ir::BinOp::Gt { signed },
            ast::BinOp::GreaterEq => ir::BinOp::GtEq { signed },
            ast::BinOp::Less => ir::BinOp::Lt { signed },
            ast::BinOp::LessEq => ir::BinOp::LtEq { signed },
            ast::BinOp::Eq => ir::BinOp::Eq,
            ast::BinOp::Neq => ir::BinOp::Neq,
            ast::BinOp::BitwiseOr => ir::BinOp::BitOr,
            ast::BinOp::BitwiseXor => ir::BinOp::BitXor,
            ast::BinOp::BitwiseAnd => ir::BinOp::BitAnd,
            ast::BinOp::LShift => ir::BinOp::BitLShift,
            ast::BinOp::RShift => {
                if signed {
                    ir::BinOp::ArihtRShift
                } else {
                    ir::BinOp::BitRShift
                }
            }
            ast::BinOp::Plus => ir::BinOp::Add,
            ast::BinOp::Minus => ir::BinOp::Sub,
            ast::BinOp::Mul => ir::BinOp::Mul,
            ast::BinOp::Div => ir::BinOp::Div { signed },
            ast::BinOp::Mod => ir::BinOp::Mod { signed },
            _ => panic!(),
        };

        Ok(op)
    }
}

impl<'a> ast::TypedExpr<'a> {
    fn transform(self, stmts: &mut Vec<ir::Statement<'a>>) -> Result<ir::Expr<'a>, Error> {
        let e = match *self.inner {
            ast::BaseExpr::Assign(lhs, rhs) => {
                let rhs = rhs.transform(stmts)?;
                let tmp = tmp_var(&rhs.type_);

                assign(stmts, tmp.clone(), rhs);
                let lhs = lhs.transform(stmts)?;
                assign(stmts, lhs, tmp.clone());
                tmp
            }
            ast::BaseExpr::Primary(p) => p.transform(stmts, &self.type_)?,
            ast::BaseExpr::Call(func, args) => {
                let mut ir_args = vec![];
                for a in args.0 {
                    ir_args.push(a.transform(stmts)?);
                }
                let ret_type = func.type_.clone();
                let tmp = tmp_var(&ret_type);
                let call = ir::Expr {
                    base: ir::BaseExpr::Call {
                        expr: Box::new(func.transform(stmts)?),
                        args: ir_args,
                    },
                    type_: ret_type,
                };

                assign(stmts, tmp.clone(), call);
                tmp
            }
            ast::BaseExpr::BinOp(op, e1, e2) => {
                let e1 = e1.transform(stmts)?;
                let e2 = e2.transform(stmts)?;
                let op = op.transform(&self.type_)?;
                if &e1.type_ != &e2.type_ {
                    return Err(Error::Semantic("Type mismatch".into()));
                }

                ir::Expr {
                    base: ir::BaseExpr::BinOp {
                        op,
                        left: Box::new(e1.clone()),
                        right: Box::new(e2),
                    },
                    type_: e1.type_,
                }
            }
            e => {
                dbg!(e);
                todo!();
            }
        };

        Ok(e)
    }
}

impl<'a> ast::Primary<ast::TypedExpr<'a>, TypeCell<'a>> {
    fn transform(
        self,
        stmts: &mut Vec<ir::Statement<'a>>,
        type_: &TypeCell<'a>,
    ) -> Result<ir::Expr<'a>, Error> {
        let e = match self {
            ast::Primary::Integer(ast::Integer(n)) => ir::Expr {
                base: ir::BaseExpr::Int(n),
                type_: type_.clone(),
            },
            ast::Primary::String(x) => ir::Expr {
                base: ir::BaseExpr::Str(x.0),
                type_: type_.clone(),
            },
            ast::Primary::Variable(x) => ir::Expr {
                base: ir::BaseExpr::Var(x.get_entity().unwrap()),
                type_: type_.clone(),
            },
            p => {
                dbg!(p);
                todo!()
            }
        };

        Ok(e)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::dereference_checker::check_dereference;
    use crate::parser::parse_source;
    use crate::type_resolver::resolve_types;
    use crate::variable_resolver::resolve_variables;
    use std::path::Path;

    fn test_from_file(file_name: &Path) {
        use std::io::Read;

        dbg!(file_name);
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
        let scope = scope.unwrap();
        let arena = crate::types::TypeArena::new();
        let ast = resolve_types(ast, &arena, scope);
        if ast.is_err() {
            dbg!(ast).ok();
            return;
        }
        let (ast, scope) = ast.unwrap();

        let verdict = check_dereference(&ast);
        if verdict.is_err() {
            dbg!(verdict).ok();
            return;
        }

        let ir = ast.transform(&scope);
        dbg!(&ir);
        if ir.is_err() {
            dbg!(ir).ok();
            return;
        }
    }

    #[test]
    fn test_from_files() {
        let root = env!("CARGO_MANIFEST_DIR");

        for file_name in glob::glob(&format!("{}/cbc-1.0/test/*.cb", root)).unwrap() {
            test_from_file(&file_name.unwrap());
        }
    }
}
