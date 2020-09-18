use crate::ast::*;
use crate::error::Error;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Entity;

#[derive(Debug)]
pub struct DefinedVariable{
    name: String,
}

impl DefinedVariable {
    fn new(name: String) -> Self {
        DefinedVariable { name }
    }
}

#[derive(Debug)]
pub struct Function {
    name: String,
}

impl Function {
    fn new(name: String) -> Self {
        Function { name }
    }
}

#[derive(Debug)]
pub struct GlobalScope {
    types: HashMap<String, Type>,
    functions: HashMap<String, Function>,
    root: Rc<RefCell<LocalScope>>,
}

impl GlobalScope {
    fn new() -> Self {
        GlobalScope {
            types: HashMap::new(),
            functions: HashMap::new(),
            root: Rc::new(RefCell::new(LocalScope::root())),
        }
    }

    fn add_variable(&mut self, var: DefinedVariable) -> Result<(), Error> {
        self.root.borrow_mut().add_variable(var)
    }

    fn add_function(&mut self, func: Function) -> Result<Rc<RefCell<LocalScope>>, Error> {
        if self.functions.contains_key(&func.name) {
            Err(Error::Semantic(format!("Duplicated function declaration: {}", &func.name)))
        } else {
            self.functions.insert(func.name.clone(), func);
            let child = Rc::new(RefCell::new(LocalScope {
                variables: HashMap::new(),
                children: Vec::new(),
                parent: Some(Rc::clone(&self.root))
            }));

            self.root.borrow_mut().children.push(Rc::clone(&child));
            Ok(child)
        }
    }
}

#[derive(Debug)]
pub struct LocalScope {
    variables: HashMap<String, DefinedVariable>,
    children: Vec<Rc<RefCell<LocalScope>>>,
    parent: Option<Rc<RefCell<LocalScope>>>,
}

impl LocalScope {
    fn root() -> Self {
        LocalScope {
            variables: HashMap:: new(),
            children: Vec::new(),
            parent: None,
        }
    }

    fn add_variable(&mut self, var: DefinedVariable) -> Result<(), Error> {
        if self.variables.contains_key(&var.name) {
            Err(Error::Semantic(format!("Duplicated variable declaration: {}", &var.name)))
        } else {
            self.variables.insert(var.name.clone(), var);
            Ok(())
        }
    }
}

fn new_scope(parent: Rc<RefCell<LocalScope>>) -> Rc<RefCell<LocalScope>> {
    let child = Rc::new(RefCell::new(LocalScope {
        variables: HashMap::new(),
        children: Vec::new(),
        parent: Some(Rc::clone(&parent))
    }));

    parent.borrow_mut().children.push(Rc::clone(&child));
    child
}

pub fn resolve_variables(ast: &mut Source) -> Result<GlobalScope, Error> {
    let mut global = GlobalScope::new();

    for decls in ast.0.values() {
        for decl in decls {
            match decl {
                HeaderDecl::VarsDecl(vs) => {
                    for (name, _) in &vs.2 {
                        global.add_variable(DefinedVariable::new(name.to_string()))?;
                    }
                },
                HeaderDecl::FuncDecl(_, name, p) => {
                    let scope = global.add_function(Function::new(name.to_string()))?;

                    for (_, var) in &p.params {
                        scope.borrow_mut().add_variable(DefinedVariable::new(var.to_string()))?;
                    }
                }
                _ => ()
            }
        }
    }

    for def in &ast.1 {
        match def {
            TopDef::Defun(_, _, name, param, block) => {
                let scope = global.add_function(Function::new(name.to_string()))?;
                for (_, var) in &param.params {
                    scope.borrow_mut().add_variable(DefinedVariable::new(var.to_string()))?;
                }

                block.resolve_variables(scope)?;
            },
            _ => ()
        }
    }

    Ok(global)
}

impl Block {
    fn resolve_variables(&self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        for vars in &self.0 {
            for (var, _) in &vars.2 {
                scope.borrow_mut().add_variable(DefinedVariable::new(var.to_string()))?;
            }
        }

        for statement in &self.1{
            statement.resolve_variables(Rc::clone(&scope))?;
        }

        Ok(())
    }
}

impl Statement {
    fn resolve_variables(&self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        match self {
            Statement::Expr(e) => e.resolve_variables(Rc::clone(&scope))?,
            Statement::Block(b) => {
                let child = new_scope(Rc::clone(&scope));
                b.resolve_variables(child)?
            }
            Statement::If(e, t, f) => {
                e.resolve_variables(Rc::clone(&scope))?;
                t.resolve_variables(Rc::clone(&scope))?;
                if let Some(f) = f.as_ref() {
                    f.resolve_variables(Rc::clone(&scope))?;
                }
            }
            Statement::While(e, s) => {
                e.resolve_variables(Rc::clone(&scope))?;
                s.resolve_variables(Rc::clone(&scope))?;
            }
            Statement::DoWhile(e, s) => {
                e.resolve_variables(Rc::clone(&scope))?;
                s.resolve_variables(Rc::clone(&scope))?;

            }
            Statement::For(i, c, s, b) => {
                i.resolve_variables(Rc::clone(&scope))?;
                c.resolve_variables(Rc::clone(&scope))?;
                s.resolve_variables(Rc::clone(&scope))?;
                b.resolve_variables(Rc::clone(&scope))?;
            }
            Statement::Return(e) => {
                if let Some(e) = e.as_ref() {
                    e.resolve_variables(Rc::clone(&scope))?;
                }
            }
            Statement::None => (),
            Statement::Break => (),
            Statement::Continue => (),
            _ => todo!(),
        }
        Ok(())
    }
}

impl Expr {
    fn resolve_variables(&self, scope: Rc<RefCell<LocalScope>>) -> Result<(), Error> {
        todo!()
    }
}