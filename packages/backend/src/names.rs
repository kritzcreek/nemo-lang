use std::{collections::HashMap, fmt};

use crate::ir::Spanned;
use text_size::TextRange;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Id {
    pub it: String,
    pub at: TextRange,
}

impl Spanned for Id {
    fn at(&self) -> &TextRange {
        &self.at
    }
}

// Should we have spanned names as well?
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Name {
    Global(u32),
    Local(u32),
    Func(u32),
    Type(u32),
    TypeVar(u32),
    Field(u32),
    Gen(u32),
}

impl Name {
    pub fn display<'a>(&'a self, name_map: &'a NameMap) -> NameDisplay<'a> {
        NameDisplay {
            name: self,
            name_map,
        }
    }
}

pub struct NameDisplay<'a> {
    name: &'a Name,
    name_map: &'a NameMap,
}

impl fmt::Display for NameDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name_map.get(self.name).unwrap().it)
    }
}

pub type NameMap = HashMap<Name, Id>;

#[derive(Debug, Clone, Default)]
pub struct NameSupply {
    local: u32,
    global: u32,
    func: u32,
    typ: u32,
    typ_var: u32,
    field: u32,
    pub name_map: HashMap<Name, Id>,
}

impl NameSupply {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn local_idx(&mut self, id: Id) -> Name {
        self.local += 1;
        let name = Name::Local(self.local);
        self.name_map.insert(name, id);
        name
    }

    pub fn global_idx(&mut self, id: Id) -> Name {
        self.global += 1;
        let name = Name::Global(self.global);
        self.name_map.insert(name, id);
        name
    }

    pub fn func_idx(&mut self, id: Id) -> Name {
        self.func += 1;
        let name = Name::Func(self.func);
        self.name_map.insert(name, id);
        name
    }

    pub fn type_idx(&mut self, id: Id) -> Name {
        self.typ += 1;
        let name = Name::Type(self.typ);
        self.name_map.insert(name, id);
        name
    }

    pub fn type_var(&mut self, id: Id) -> Name {
        self.typ_var += 1;
        let name = Name::TypeVar(self.typ_var);
        self.name_map.insert(name, id);
        name
    }

    pub fn field_idx(&mut self, id: Id) -> Name {
        self.field += 1;
        let name = Name::Field(self.field);
        self.name_map.insert(name, id);
        name
    }

    pub fn lookup(&self, name: Name) -> Option<&Id> {
        self.name_map.get(&name)
    }
}
