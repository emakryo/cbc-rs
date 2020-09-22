use crate::ast::*;
use crate::error::Error;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Eq, PartialEq, Clone)]
pub enum Entity {
    Variable { name: String, type_: TypeRef },
    Function { name: String, type_: TypeRef },
}

impl Entity {
    fn var(name: String, type_: TypeRef) -> Self {
        Entity::Variable { name, type_ }
    }

    fn func(name: String, type_: TypeRef) -> Self {
        Entity::Function { name, type_ }
    }

    pub fn get_type(&self) -> &TypeRef {
        match self {
            Entity::Variable { name: _, type_ } | Entity::Function { name: _, type_ } => type_,
        }
    }
}

#[derive(Debug)]
pub struct GlobalScope {
    root: Rc<RefCell<LocalScope>>,
}

impl GlobalScope {
    fn new() -> Self {
        GlobalScope {
            root: Rc::new(RefCell::new(LocalScope::root())),
        }
    }

    fn add_variable(&mut self, name: String, type_: TypeRef) -> Result<(), Error> {
        self.root.borrow_mut().add_variable(name, type_)
    }

    fn add_function(
        &mut self,
        name: String,
        return_type: TypeRef,
        params: Params,
    ) -> Result<Rc<RefCell<LocalScope>>, Error> {
        let type_ = TypeRef::Pointer {
            base: Box::new(TypeRef::Function {
                base: Box::new(return_type),
                params: params.params.iter().map(|(t, _)| t.clone()).collect(),
                variable_length: params.variable_length,
            }),
        };

        self.root
            .borrow_mut()
            .add_entity(name.clone(), Entity::func(name, type_))?;

        let child = Rc::new(RefCell::new(LocalScope {
            entities: HashMap::new(),
            children: Vec::new(),
            parent: Some(Rc::clone(&self.root)),
        }));

        self.root.borrow_mut().children.push(Rc::clone(&child));

        for (type_, var) in params.params {
            child.borrow_mut().add_variable(var.to_string(), type_)?;
        }

        Ok(child)
    }
}

pub struct LocalScope {
    entities: HashMap<String, Rc<Entity>>,
    children: Vec<Rc<RefCell<LocalScope>>>,
    parent: Option<Rc<RefCell<LocalScope>>>,
}

impl std::fmt::Debug for LocalScope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LocalScope")
            .field("variables", &self.entities)
            .field("children", &self.children)
            .field("parent", &self.parent.as_ref().map(|r| r.as_ptr()))
            .finish()
    }
}

impl LocalScope {
    fn root() -> Self {
        let entities = HashMap::new();
        LocalScope {
            entities,
            children: Vec::new(),
            parent: None,
        }
    }

    fn add_entity(&mut self, name: String, entity: Entity) -> Result<(), Error> {
        if self.entities.contains_key(&name) {
            Err(Error::Semantic(format!(
                "Duplicated variable declaration: {}",
                &name
            )))
        } else {
            self.entities.insert(name.clone(), Rc::new(entity));
            Ok(())
        }
    }

    fn add_variable(&mut self, name: String, type_: TypeRef) -> Result<(), Error> {
        self.add_entity(name.clone(), Entity::var(name, type_))
    }

    fn get_entity(&self, name: &str) -> Option<Rc<Entity>> {
        if let Some(var) = self.entities.get(name) {
            Some(Rc::clone(var))
        } else {
            if let Some(par) = &self.parent {
                par.borrow().get_entity(name)
            } else {
                None
            }
        }
    }
}

fn new_scope(parent: Rc<RefCell<LocalScope>>) -> Rc<RefCell<LocalScope>> {
    let child = Rc::new(RefCell::new(LocalScope {
        entities: HashMap::new(),
        children: Vec::new(),
        parent: Some(Rc::clone(&parent)),
    }));

    parent.borrow_mut().children.push(Rc::clone(&child));
    child
}

pub fn resolve_variables(ast: &mut Source) -> Result<GlobalScope, Error> {
    let mut global = GlobalScope::new();

    for decls in ast.0.values() {
        for decl in decls {
            match decl {
                HeaderDecl::VarsDecl(vs) | HeaderDecl::DefConst(vs) => {
                    for (name, _) in &vs.2 {
                        global.add_variable(name.to_string(), vs.1.clone())?;
                    }
                }
                HeaderDecl::FuncDecl(t, name, p) => {
                    global.add_function(name.to_string(), t.clone(), p.clone())?;
                }
                HeaderDecl::TypeDef(_, _) => (),
                d => todo!("{:?}", d),
            }
        }
    }

    for def in &mut ast.1 {
        match def {
            TopDef::Defun(_, t, name, param, block) => {
                let scope = global.add_function(name.to_string(), t.clone(), param.clone())?;
                block.set_scope(scope);
            }
            TopDef::DefVars(defs) => {
                for (name, _) in &mut defs.2 {
                    global.add_variable(name.to_string(), defs.1.clone())?;
                }
            }
            TopDef::DefStuct(_, _) | TopDef::DefUnion(_, _) | TopDef::TypeDef(_, _) => (),
            TopDef::DefConst(defs) => {
                for (name, _) in &mut defs.2 {
                    global.add_variable(name.to_string(), defs.1.clone())?;
                }
            }
        }
    }

    for def in &mut ast.1 {
        match def {
            TopDef::Defun(_, _, _, _, block) => {
                block.resolve_variables(block.get_scope().expect("No scope set"))?;
            }
            TopDef::DefVars(defs) => {
                for (_, expr) in &mut defs.2 {
                    if let Some(expr) = expr {
                        expr.resolve_variables(Rc::clone(&global.root))?;
                    }
                }
            }
            TopDef::DefConst(defs) => {
                for (name, expr) in &mut defs.2 {
                    if let Some(expr) = expr {
                        expr.resolve_variables(Rc::clone(&global.root))?;
                    } else {
                        return Err(Error::Semantic(format!(
                            "const declaration has no initial value: {}",
                            name.to_string()
                        )));
                    }
                }
            }
            _ => (),
        }
    }

    Ok(global)
}

impl Block {
    fn resolve_variables(&mut self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        for vars in self.mut_vars() {
            for (var, init) in &mut vars.2 {
                scope
                    .borrow_mut()
                    .add_variable(var.to_string(), vars.1.clone())?;

                if let Some(init) = init.as_mut() {
                    init.resolve_variables(Rc::clone(&scope))?;
                }
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
            Expr::Term(t) => resolve!(t),
            Expr::Ternary(c, e1, e2) => {
                resolve!(c.as_mut());
                resolve!(e1.as_mut());
                resolve!(e2.as_mut());
            }
        }
        Ok(())
    }
}

impl Term {
    fn resolve_variables(&mut self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        macro_rules! resolve {
            ($e:expr) => {
                $e.resolve_variables(Rc::clone(&scope))?;
            };
        }
        match self {
            Term::Unary(u) => resolve!(u.as_mut()),
            Term::Cast(_, t) => resolve!(t.as_mut()),
        }
        Ok(())
    }
}

impl Unary {
    fn resolve_variables(&mut self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        macro_rules! resolve {
            ($e:expr) => {
                $e.resolve_variables(Rc::clone(&scope))?;
            };
        }

        match self {
            Unary::Call(e, a) => {
                resolve!(e.as_mut());
                resolve!(a);
            }
            Unary::Primary(p) => resolve!(p),
            Unary::Member(e, _) => resolve!(e.as_mut()),
            Unary::PMember(e, _) => resolve!(e.as_mut()),
            Unary::Addr(e) => resolve!(e.as_mut()),
            Unary::PostInc(e) => resolve!(e.as_mut()),
            Unary::PostDec(e) => resolve!(e.as_mut()),
            Unary::Op(_, e) => resolve!(e.as_mut()),
            Unary::ArrayRef(e, i) => {
                resolve!(e.as_mut());
                resolve!(i.as_mut());
            }
            Unary::Deref(e) => resolve!(e.as_mut()),
            Unary::PreInc(e) => resolve!(e.as_mut()),
            Unary::PreDec(e) => resolve!(e.as_mut()),
            Unary::SizeofE(e) => resolve!(e.as_mut()),
            Unary::SizeofT(_) => (),
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
