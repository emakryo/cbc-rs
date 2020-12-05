use crate::ast::Ident;
use crate::error::Error;
use std::cell::{Cell, Ref, RefCell, RefMut};
use std::collections::HashMap;
use std::hash::Hash;
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

#[derive(Clone, Eq, PartialEq)]
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

impl<'a> std::fmt::Debug for Type<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Type::Void => f.write_str("Void")?,
            Type::Array { base, size } => {
                write!(f, "{:?}[", base)?;
                if let Some(s) = size {
                    f.write_str(&s.to_string())?;
                }
                write!(f, "]")?;
            }
            Type::Integer { size, signed } => {
                f.write_str(if *signed { "i" } else { "u" })?;
                write!(f, "{}", 8 * size)?;
            }
            Type::Pointer { base } => {
                write!(f, "*{:?}", base)?;
            }
            Type::Function {
                base,
                params,
                variable_length,
            } => {
                let mut g = f.debug_tuple(&format!("{:?}", base));
                for t in params {
                    g.field(t);
                }
                if *variable_length {
                    g.field(&"...");
                }
                g.finish()?;
            }
            Type::Struct { name, members } => {
                let mut g = f.debug_struct(&format!("struct {:?}", &name.0));
                for (t, n) in members {
                    g.field(&n.0, &format!("{}", t));
                }
                g.finish()?;
            }
            Type::Union { name, members } => {
                let mut g = f.debug_struct(&format!("union {:?}", &name.0));
                for (t, n) in members {
                    g.field(&n.0, &format!("{}", t));
                }
                g.finish()?;
            }
            Type::Undefined => {
                f.write_str("undefined")?;
            }
        }

        Ok(())
    }
}

impl<'a> Type<'a> {
    fn is_numeric(&self) -> bool {
        matches!(self, Type::Integer{..})
    }
    fn is_signed(&self) -> Option<bool> {
        match self {
            Type::Integer { signed, .. } => Some(*signed),
            _ => None,
        }
    }
    fn is_void(&self) -> bool {
        *self == Type::Void
    }
    fn is_pointer(&self) -> bool {
        self.pointer_base().is_some()
    }
    fn is_array(&self) -> bool {
        matches!(self, Type::Array { .. })
    }
    fn is_function(&self) -> bool {
        matches!(self, Type::Function { .. })
    }
    fn is_func_pointer(&self) -> bool {
        self.return_type().is_some()
    }
    fn is_undefined(&self) -> bool {
        matches!(self, Type::Undefined)
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
    fn params(&self) -> Option<Vec<TypeCell<'a>>> {
        let t = self.pointer_base()?;
        let t = t.borrow();

        match &*t {
            Type::Function { params, .. } => Some(params.iter().cloned().collect()),
            _ => None,
        }
    }
    fn members(&self) -> Option<Vec<(TypeCell<'a>, Ident)>> {
        match self {
            Type::Struct { members, .. } | Type::Union { members, .. } => Some(members.clone()),
            _ => None,
        }
    }
    fn deref(&self) -> Result<&TypeCell<'a>, Error> {
        match self {
            Type::Pointer { base } => Ok(base),
            _ => Err(Error::Semantic("Dereference of non-pointer type".into())),
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

impl<'a> std::fmt::Display for TypeCell<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("TypeCell")
            .field(&(self.0.get() as *const _))
            .finish()
    }
}

impl<'a> std::fmt::Debug for TypeCell<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self.borrow())?;
        // write!(f, "({:?})", &(self.0.get() as *const _))
        Ok(())
    }
}

impl<'a> TypeCell<'a> {
    fn borrow(&self) -> Ref<Type<'a>> {
        self.0.get().borrow()
    }
    fn borrow_mut(&self) -> RefMut<Type<'a>> {
        self.0.get().borrow_mut()
    }
    pub fn is_numeric(&self) -> bool {
        self.borrow().is_numeric()
    }
    pub fn is_signed(&self) -> Option<bool> {
        self.borrow().is_signed()
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
        Ok(self.borrow().get_field(name)?.clone())
    }
    pub fn pointer_base(&self) -> Option<TypeCell<'a>> {
        self.borrow().pointer_base()
    }
    pub fn array_base(&self) -> Result<TypeCell<'a>, Error> {
        self.borrow()
            .array_base()
            .ok_or(Error::Semantic("Invalid index access to array".into()))
    }
    pub fn return_type(&self) -> Result<TypeCell<'a>, Error> {
        self.borrow()
            .return_type()
            .ok_or(Error::Semantic("Function call to non-function type".into()))
    }
    pub fn params(&self) -> Option<Vec<TypeCell<'a>>> {
        self.borrow().params()
    }
    pub fn members(&self) -> Option<Vec<(TypeCell<'a>, Ident)>> {
        self.borrow().members()
    }
    pub fn deref(&self) -> Result<TypeCell<'a>, Error> {
        self.borrow().deref().map(|t| t.clone())
    }
}

pub type TypeArena<'a> = Arena<RefCell<Type<'a>>>;
pub struct TypeTable<'a> {
    rmap: HashMap<TypeRef, TypeCell<'a>>,
    // emap: HashMap<&'b Expr, TypeCell<'a>>,
    arena: &'a TypeArena<'a>,
}

impl<'a> std::fmt::Debug for TypeTable<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.rmap.fmt(f)
    }
}

impl<'a> TypeTable<'a> {
    fn empty(arena: &'a TypeArena<'a>) -> Self {
        TypeTable {
            rmap: HashMap::new(),
            arena,
        }
    }

    pub fn new(arena: &'a TypeArena<'a>) -> Self {
        let mut table = Self::empty(arena);

        table.insert(TypeRef::Char, Type::char()).unwrap();
        table.insert(TypeRef::UChar, Type::uchar()).unwrap();
        table.insert(TypeRef::Short, Type::short()).unwrap();
        table.insert(TypeRef::UShort, Type::ushort()).unwrap();
        table.insert(TypeRef::Int, Type::int()).unwrap();
        table.insert(TypeRef::UInt, Type::uint()).unwrap();
        table.insert(TypeRef::Long, Type::long()).unwrap();
        table.insert(TypeRef::ULong, Type::ulong()).unwrap();
        table.insert(TypeRef::Void, Type::Void).unwrap();

        table
    }

    pub fn get<'c>(&'c self, type_ref: &TypeRef) -> Option<&'c TypeCell<'a>> {
        self.rmap.get(type_ref)
    }

    fn insert(&mut self, k: TypeRef, v: Type<'a>) -> Result<&TypeCell<'a>, Error> {
        let v = self.arena.alloc(RefCell::new(v));
        self.rmap.insert(k.clone(), TypeCell(Cell::new(v)));
        Ok(self.get(&k).unwrap())
    }

    pub fn add(&mut self, tref: TypeRef) -> Result<&TypeCell<'a>, Error> {
        if self.rmap.contains_key(&tref) {
            return Ok(self.rmap.get(&tref).unwrap());
        }

        let t = match &tref {
            TypeRef::Array { base, size } => {
                let base = self.add(*base.clone())?;
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
                let base = self.add(*base.clone())?.clone();
                let mut param_types = vec![];

                for param in params {
                    let param_t = self.add(param.clone())?.clone();

                    param_types.push(param_t);
                }
                Type::Function {
                    base,
                    params: param_types,
                    variable_length: *variable_length,
                }
            }
            TypeRef::Pointer { base } => {
                let base = self.add(*base.clone())?;
                Type::Pointer { base: base.clone() }
            }
            TypeRef::Struct(_) | TypeRef::Union(_) | TypeRef::User(_) => Type::Undefined,
            t => {
                dbg!(t);
                todo!()
            }
        };

        self.insert(tref, t)
    }

    pub fn add_usertype(&mut self, name: Ident, typeref: TypeRef) -> Result<(), Error> {
        let user_type = TypeRef::User(name);
        self.add(user_type.clone())?;

        let t = self.add(typeref.clone())?.0.get();
        self.get(&user_type).unwrap().0.set(t);

        Ok(())
    }

    pub fn add_struct(&mut self, name: Ident, members: Vec<(TypeRef, Ident)>) -> Result<(), Error> {
        let mut member_types = vec![];

        for (r, n) in members {
            let t = self.add(r)?;
            if t.borrow().is_void() {
                return Err(Error::Semantic("Struct type cannot have void field".into()));
            }
            member_types.push((t.clone(), n));
        }

        let r = TypeRef::Struct(name.clone());
        let t = Type::Struct {
            name,
            members: member_types,
        };

        if let Some(cell) = self.get(&r) {
            if !cell.borrow().is_undefined() {
                return Err(Error::Semantic("Duplicated struct name".into()));
            }
            *cell.borrow_mut() = t
        } else {
            self.insert(r.clone(), t)?;
        }

        // let t = self.get(&r).unwrap();
        // dbg!((&t, t.borrow()));

        Ok(())
    }

    pub fn add_union(&mut self, name: Ident, members: Vec<(TypeRef, Ident)>) -> Result<(), Error> {
        let mut member_types = vec![];

        for (r, n) in members {
            let t = self.add(r)?;
            if t.borrow().is_void() {
                return Err(Error::Semantic("Union type cannot have void field".into()));
            }
            member_types.push((t.clone(), n));
        }

        let r = TypeRef::Union(name.clone());
        let t = Type::Union {
            name,
            members: member_types,
        };

        if let Some(cell) = self.get(&r) {
            if !cell.borrow().is_undefined() {
                return Err(Error::Semantic("Duplicated union name".into()));
            }
            *cell.borrow_mut() = t
        } else {
            self.insert(r.clone(), t)?;
        }

        Ok(())
    }

    pub fn long(&self) -> &TypeCell<'a> {
        self.get(&TypeRef::Long).unwrap()
    }

    pub fn char(&self) -> &TypeCell<'a> {
        self.get(&TypeRef::Char).unwrap()
    }

    pub fn string(&self) -> &TypeCell<'a> {
        self.get(&TypeRef::Pointer {
            base: Box::new(TypeRef::Char),
        })
        .unwrap()
    }

    pub fn addr(&self, base: &TypeCell<'a>) -> TypeCell<'a> {
        let t = Type::Pointer { base: base.clone() };
        TypeCell(Cell::new(self.arena.alloc(RefCell::new(t))))
    }

    pub fn values<'c>(&'c self) -> Box<dyn Iterator<Item = TypeCell<'a>> + 'c> {
        Box::new(self.rmap.values().cloned())
    }
}
