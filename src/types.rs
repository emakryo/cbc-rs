use crate::ast::Ident;
use crate::error::Error;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use typed_arena::Arena;

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum TypeRef {
    Array {
        base: Box<TypeRef>,
        size: Option<usize>,
    },
    Function {
        base: Box<TypeRef>,
        params: Vec<TypeRef>,
        variable_length: bool,
    },
    Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    Pointer {
        base: Box<TypeRef>,
    },
    Struct(Ident),
    Union(Ident),
    User(Ident),
    Void,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type<'a> {
    Void,
    Array {
        base: TypeCell<'a>,
        size: Option<usize>,
    },
    Integer {
        size: usize,
        signed: bool,
    },
    Pointer {
        base: TypeCell<'a>,
    },
    Function {
        base: TypeCell<'a>,
        params: Vec<TypeCell<'a>>,
        variable_length: bool,
    },
    Struct {
        name: Ident,
        members: Vec<(TypeCell<'a>, Ident)>,
    },
    Union {
        name: Ident,
        members: Vec<(TypeCell<'a>, Ident)>,
    },
    Undefined,
}

impl<'a> Type<'a> {
    fn is_void(&self) -> bool {
        *self == Type::Void
    }

    fn is_pointer(&self) -> bool {
        self.pointer_base().is_some()
    }

    fn is_array(&self) -> bool {
        match self {
            Type::Array { .. } => true,
            _ => false,
        }
    }

    fn is_function(&self) -> bool {
        match self {
            Type::Function { .. } => true,
            _ => false,
        }
    }

    fn is_func_pointer(&self) -> bool {
        self.return_type().is_some()
    }

    fn pointer_base(&self) -> Option<TypeCell<'a>> {
        match self {
            Type::Pointer { base } => Some(base.clone()),
            _ => None,
        }
    }

    fn array_base(&self) -> Option<TypeCell<'a>> {
        match self {
            Type::Array { base, .. } => Some(base.clone()),
            _ => None,
        }
    }

    fn return_type(&self) -> Option<TypeCell<'a>> {
        let t = self.pointer_base()?;
        let t = t.borrow();

        match &*t {
            Type::Function { base, .. } => Some(base.clone()),
            _ => None,
        }
    }

    fn members(&self) -> Option<Vec<(TypeCell<'a>, Ident)>> {
        match self {
            Type::Struct { members, .. } | Type::Union { members, .. } => Some(members.clone()),
            _ => None,
        }
    }

    fn long() -> Self {
        Type::Integer {
            size: 8,
            signed: true,
        }
    }

    fn ulong() -> Self {
        Type::Integer {
            size: 8,
            signed: false,
        }
    }

    fn int() -> Self {
        Type::Integer {
            size: 4,
            signed: true,
        }
    }

    fn uint() -> Self {
        Type::Integer {
            size: 4,
            signed: false,
        }
    }

    fn short() -> Self {
        Type::Integer {
            size: 2,
            signed: true,
        }
    }

    fn ushort() -> Self {
        Type::Integer {
            size: 2,
            signed: false,
        }
    }

    fn char() -> Self {
        Type::Integer {
            size: 1,
            signed: true,
        }
    }

    fn uchar() -> Self {
        Type::Integer {
            size: 1,
            signed: false,
        }
    }

    fn get_field(&self, name: &Ident) -> Result<&TypeCell<'a>, Error> {
        match self {
            Type::Struct { members, .. } | Type::Union { members, .. } => {
                if let Some((t, _)) = members.iter().find(|(_, n)| n == name) {
                    Ok(t)
                } else {
                    Err(Error::Semantic(format!("No field: {}", name.to_string())))
                }
            }
            _ => Err(Error::Semantic(format!(
                "Cannot access field of non-composit type: {:?}",
                self
            ))),
        }
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct TypeCell<'a>(Cell<&'a RefCell<Type<'a>>>);

impl<'a> std::hash::Hash for TypeCell<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.get() as *const _ as usize).hash(state)
    }
}

impl<'a> std::fmt::Debug for TypeCell<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        f.debug_tuple("TypeCell")
            .field(&(self.0.get() as *const _))
            //.field(&self.borrow())
            .finish()
    }
}

impl<'a> TypeCell<'a> {
    fn borrow(&self) -> Ref<Type<'a>> {
        self.0.get().borrow()
    }

    fn borrow_mut(&self) -> RefMut<Type<'a>> {
        self.0.get().borrow_mut()
    }

    pub fn is_pointer(&self) -> bool {
        self.borrow().is_pointer()
    }

    pub fn is_array(&self) -> bool {
        self.borrow().is_array()
    }

    pub fn is_function(&self) -> bool {
        self.borrow().is_function()
    }

    pub fn is_func_pointer(&self) -> bool {
        self.borrow().is_func_pointer()
    }

    pub fn get_field(&self, name: &Ident) -> Result<TypeCell<'a>, Error> {
        Ok(self.borrow().clone().get_field(name)?.clone())
    }

    pub fn pointer_base(&self) -> Option<TypeCell<'a>> {
        self.borrow().clone().pointer_base()
    }

    pub fn array_base(&self) -> Option<TypeCell<'a>> {
        self.borrow().clone().array_base()
    }

    pub fn return_type(&self) -> Option<TypeCell<'a>> {
        self.borrow().clone().return_type()
    }

    pub fn members(&self) -> Option<Vec<(TypeCell<'a>, Ident)>> {
        self.borrow().members()
    }
}

#[derive(Debug)]
pub struct TypeTable<'a>(HashMap<TypeRef, TypeCell<'a>>);
pub type TypeArena<'a> = Arena<RefCell<Type<'a>>>;

impl<'a> TypeTable<'a> {
    fn empty() -> Self {
        TypeTable(HashMap::new())
    }

    pub fn new(arena: &'a TypeArena<'a>) -> Self {
        let mut table = Self::empty();

        table.insert(arena, TypeRef::Char, Type::char()).unwrap();
        table.insert(arena, TypeRef::UChar, Type::uchar()).unwrap();
        table.insert(arena, TypeRef::Short, Type::short()).unwrap();
        table
            .insert(arena, TypeRef::UShort, Type::ushort())
            .unwrap();
        table.insert(arena, TypeRef::Int, Type::int()).unwrap();
        table.insert(arena, TypeRef::UInt, Type::uint()).unwrap();
        table.insert(arena, TypeRef::Long, Type::long()).unwrap();
        table.insert(arena, TypeRef::ULong, Type::ulong()).unwrap();
        table.insert(arena, TypeRef::Void, Type::Void).unwrap();

        table
    }

    pub fn get<'b>(&'b self, type_ref: &TypeRef) -> Option<&'b TypeCell<'a>> {
        self.0.get(type_ref)
    }

    fn insert<'b>(
        &'b mut self,
        arena: &'a TypeArena<'a>,
        k: TypeRef,
        v: Type<'a>,
    ) -> Result<(), Error> {
        let v = arena.alloc(RefCell::new(v));
        self.0.insert(k, TypeCell(Cell::new(v)));
        Ok(())
    }

    fn add(&mut self, arena: &'a TypeArena<'a>, tref: TypeRef) -> Result<(), Error> {
        if self.0.contains_key(&tref) {
            return Ok(());
        }

        let t = match &tref {
            TypeRef::Array { base, size } => {
                self.add(arena, *base.clone())?;
                let base = self.get(base.as_ref()).unwrap();
                if base.borrow().is_void() {
                    return Err(Error::Semantic(
                        "Array type cannot have void elements.".into(),
                    ));
                }
                Type::Array {
                    base: base.clone(),
                    size: *size,
                }
            }
            TypeRef::Function {
                base,
                params,
                variable_length,
            } => {
                self.add(arena, *base.clone())?;
                let base = self.get(base.as_ref()).unwrap().clone();
                let mut param_types = vec![];

                for param in params {
                    self.add(arena, param.clone())?;

                    param_types.push(self.get(param).unwrap().clone());
                }
                Type::Function {
                    base: base.clone(),
                    params: param_types,
                    variable_length: *variable_length,
                }
            }
            TypeRef::Pointer { base } => {
                self.add(arena, *base.clone())?;
                let base = self.get(base.as_ref()).unwrap();
                Type::Pointer { base: base.clone() }
            }
            TypeRef::Struct(_) | TypeRef::Union(_) | TypeRef::User(_) => Type::Undefined,
            t => {
                dbg!(t);
                todo!()
            }
        };

        self.insert(arena, tref, t)?;
        Ok(())
    }

    pub fn add_usertype(
        &mut self,
        arena: &'a TypeArena<'a>,
        name: Ident,
        typeref: TypeRef,
    ) -> Result<(), Error> {
        let user_type = TypeRef::User(name);
        self.add(arena, user_type.clone())?;

        self.add(arena, typeref.clone())?;
        let t = self.get(&typeref).unwrap();
        self.get(&user_type).unwrap().0.set(&t.0.get());

        Ok(())
    }

    pub fn add_struct(
        &mut self,
        arena: &'a TypeArena<'a>,
        name: Ident,
        members: Vec<(TypeRef, Ident)>,
    ) -> Result<(), Error> {
        let mut member_types = vec![];

        for (r, n) in members {
            self.add(arena, r.clone())?;
            let t = self.get(&r).unwrap();
            if t.borrow().is_void() {
                return Err(Error::Semantic(
                    "Array type cannot have void elements.".into(),
                ));
            }
            member_types.push((t.clone(), n));
        }

        let r = TypeRef::Struct(name.clone());
        let t = Type::Struct {
            name,
            members: member_types,
        };

        if let Some(cell) = self.get(&r) {
            *cell.borrow_mut() = t
        } else {
            self.insert(arena, r.clone(), t)?;
        }

        // let t = self.get(&r).unwrap();
        // dbg!((&t, t.borrow()));

        Ok(())
    }

    pub fn add_union(
        &mut self,
        arena: &'a TypeArena<'a>,
        name: Ident,
        members: Vec<(TypeRef, Ident)>,
    ) -> Result<(), Error> {
        let mut member_types = vec![];

        for (r, n) in members {
            self.add(arena, r.clone())?;
            let t = self.get(&r).unwrap();
            if t.borrow().is_void() {
                return Err(Error::Semantic(
                    "Array type cannot have void elements.".into(),
                ));
            }
            member_types.push((t.clone(), n));
        }

        let r = TypeRef::Union(name.clone());
        let t = Type::Union {
            name,
            members: member_types,
        };

        if let Some(cell) = self.get(&r) {
            *cell.borrow_mut() = t
        } else {
            self.insert(arena, r.clone(), t)?;
        }

        // let t = self.get(&r).unwrap();
        // dbg!((&t, t.borrow()));

        Ok(())
    }

    pub fn long(&self) -> TypeCell<'a> {
        self.get(&TypeRef::Long).unwrap().clone()
    }

    pub fn char(&self) -> TypeCell<'a> {
        self.get(&TypeRef::Char).unwrap().clone()
    }

    pub fn string(&self) -> TypeCell<'a> {
        self.get(&TypeRef::Pointer {
            base: Box::new(TypeRef::Char),
        })
        .unwrap()
        .clone()
    }

    pub fn values<'b>(&'b self) -> Box<dyn Iterator<Item = TypeCell<'a>> + 'b> {
        Box::new(self.0.values().cloned())
    }
}
