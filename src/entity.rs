use crate::ast::Defun;
use crate::error::Error;
use crate::types::TypeRef;
use std::collections::HashMap;
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Entity<T> {
    Variable { name: String, type_: T },
    Function { name: String, type_: T },
}

impl<T> Entity<T> {
    pub fn var(name: String, type_: T) -> Self {
        Entity::Variable { name, type_ }
    }

    pub fn func(name: String, type_: T) -> Self {
        Entity::Function { name, type_ }
    }

    pub fn get_type(&self) -> &T {
        match self {
            Entity::Variable { name: _, type_ } | Entity::Function { name: _, type_ } => type_,
        }
    }
}

#[derive(Debug)]
pub struct GlobalScope<T> {
    pub root: Rc<RefCell<LocalScope<T>>>,
}

impl GlobalScope<TypeRef> {
    pub fn new() -> Self {
        GlobalScope {
            root: Rc::new(RefCell::new(LocalScope::root())),
        }
    }

    pub fn add_variable(&mut self, name: String, type_: TypeRef) -> Result<(), Error> {
        self.root.borrow_mut().add_variable(name, type_)
    }

    pub fn add_function(
        &mut self,
        defun: &Defun<TypeRef>,
    ) -> Result<Rc<RefCell<LocalScope<TypeRef>>>, Error> {
        let type_ = TypeRef::Pointer {
            base: Box::new(TypeRef::Function {
                base: Box::new(defun.type_.clone()),
                params: defun.params.iter().map(|(t, _)| t.clone()).collect(),
                variable_length: defun.variable_length,
            }),
        };
        let name = defun.name.0.clone();

        self.root
            .borrow_mut()
            .add_entity(name.clone(), Entity::func(name, type_))?;

        let child = Rc::new(RefCell::new(LocalScope {
            entities: HashMap::new(),
            children: Vec::new(),
            parent: Some(Rc::clone(&self.root)),
        }));

        self.root.borrow_mut().children.push(Rc::clone(&child));

        for (type_, var) in &defun.params {
            child
                .borrow_mut()
                .add_variable(var.to_string(), type_.clone())?;
        }

        Ok(child)
    }
}

pub struct LocalScope<T> {
    pub entities: HashMap<String, Rc<Entity<T>>>,
    children: Vec<Rc<RefCell<LocalScope<T>>>>,
    parent: Option<Rc<RefCell<LocalScope<T>>>>,
}

impl<T: std::fmt::Debug> std::fmt::Debug for LocalScope<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LocalScope")
            .field("variables", &self.entities)
            .field("children", &self.children)
            .field("parent", &self.parent.as_ref().map(|r| r.as_ptr()))
            .finish()
    }
}

impl LocalScope<TypeRef> {
    fn root() -> Self {
        let entities = HashMap::new();
        LocalScope {
            entities,
            children: Vec::new(),
            parent: None,
        }
    }

    fn add_entity(&mut self, name: String, entity: Entity<TypeRef>) -> Result<(), Error> {
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

    pub fn add_variable(&mut self, name: String, type_: TypeRef) -> Result<(), Error> {
        self.add_entity(name.clone(), Entity::var(name, type_))
    }
}

pub fn new_scope(parent: Rc<RefCell<LocalScope<TypeRef>>>) -> Rc<RefCell<LocalScope<TypeRef>>> {
    let child = Rc::new(RefCell::new(LocalScope {
        entities: HashMap::new(),
        children: Vec::new(),
        parent: Some(Rc::clone(&parent)),
    }));

    parent.borrow_mut().children.push(Rc::clone(&child));
    child
}

pub trait Scope {
    type Type;
    fn get_entity(&self, name: &str) -> Option<Rc<Entity<Self::Type>>>;
}

impl<T> Scope for LocalScope<T> {
    type Type = T;
    fn get_entity(&self, name: &str) -> Option<Rc<Entity<T>>> {
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

impl<T> Scope for GlobalScope<T> {
    type Type = T;
    fn get_entity(&self, name: &str) -> Option<Rc<Entity<T>>> {
        let scope = self.root.borrow();
        scope.get_entity(name)
    }
}
