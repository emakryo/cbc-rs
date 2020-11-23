use crate::ast::Params;
use crate::error::Error;
use crate::types::TypeRef;
use std::collections::HashMap;
use std::{cell::RefCell, rc::Rc};

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum Entity {
    Variable { name: String, type_: TypeRef },
    Function { name: String, type_: TypeRef },
}

impl Entity {
    pub fn var(name: String, type_: TypeRef) -> Self {
        Entity::Variable { name, type_ }
    }

    pub fn func(name: String, type_: TypeRef) -> Self {
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
    pub root: Rc<RefCell<LocalScope>>,
}

impl GlobalScope {
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

    pub fn add_variable(&mut self, name: String, type_: TypeRef) -> Result<(), Error> {
        self.add_entity(name.clone(), Entity::var(name, type_))
    }
}

pub fn new_scope(parent: Rc<RefCell<LocalScope>>) -> Rc<RefCell<LocalScope>> {
    let child = Rc::new(RefCell::new(LocalScope {
        entities: HashMap::new(),
        children: Vec::new(),
        parent: Some(Rc::clone(&parent)),
    }));

    parent.borrow_mut().children.push(Rc::clone(&child));
    child
}

pub trait Scope {
    fn get_entity(&self, name: &str) -> Option<Rc<Entity>>;
}

impl Scope for LocalScope {
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

impl Scope for GlobalScope {
    fn get_entity(&self, name: &str) -> Option<Rc<Entity>> {
        let scope = self.root.borrow();
        scope.get_entity(name)
    }
}
