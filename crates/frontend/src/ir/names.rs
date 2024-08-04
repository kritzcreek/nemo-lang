use std::{collections::HashMap, fmt, num::NonZeroU16, u16};

use text_size::TextRange;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Id {
    pub it: String,
    pub at: TextRange,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum NameTag {
    Local,
    Global,
    Func,
    Type,
    TypeVar,
    Field,
    Gen,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct ModuleId(NonZeroU16);

impl ModuleId {
    pub fn new(id: u16) -> Self {
        Self(NonZeroU16::new(id).expect("ModuleId must be non-zero"))
    }

    pub const PRIM: Self = ModuleId(unsafe { NonZeroU16::new_unchecked(1) });
    pub const CODEGEN: Self = ModuleId(unsafe { NonZeroU16::new_unchecked(2) });
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Name {
    pub tag: NameTag,
    pub module: ModuleId,
    pub idx: u32,
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
    supply: u32,
    pub name_map: HashMap<Name, Id>,
}

impl NameSupply {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn local_idx(&mut self, module: ModuleId, id: Id) -> Name {
        self.supply += 1;
        let name = Name {
            tag: NameTag::Local,
            module,
            idx: self.supply,
        };
        self.name_map.insert(name, id);
        name
    }

    pub fn global_idx(&mut self, module: ModuleId, id: Id) -> Name {
        self.supply += 1;
        let name = Name {
            tag: NameTag::Global,
            module,
            idx: self.supply,
        };
        self.name_map.insert(name, id);
        name
    }

    pub fn func_idx(&mut self, module: ModuleId, id: Id) -> Name {
        self.supply += 1;
        let name = Name {
            tag: NameTag::Func,
            module,
            idx: self.supply,
        };
        self.name_map.insert(name, id);
        name
    }

    pub fn type_idx(&mut self, module: ModuleId, id: Id) -> Name {
        self.supply += 1;
        let name = Name {
            tag: NameTag::Type,
            module,
            idx: self.supply,
        };
        self.name_map.insert(name, id);
        name
    }

    pub fn type_var(&mut self, module: ModuleId, id: Id) -> Name {
        self.supply += 1;
        let name = Name {
            tag: NameTag::TypeVar,
            module,
            idx: self.supply,
        };
        self.name_map.insert(name, id);
        name
    }

    pub fn field_idx(&mut self, module: ModuleId, id: Id) -> Name {
        self.supply += 1;
        let name = Name {
            tag: NameTag::Field,
            module,
            idx: self.supply,
        };
        self.name_map.insert(name, id);
        name
    }

    pub fn gen_idx(&mut self, module: ModuleId) -> Name {
        self.supply += 1;
        let name = Name {
            tag: NameTag::Gen,
            module,
            idx: self.supply,
        };
        self.name_map.insert(
            name,
            Id {
                it: format!("gen{}", self.supply),
                at: TextRange::default(),
            },
        );
        name
    }

    pub fn lookup(&self, name: Name) -> Option<&Id> {
        self.name_map.get(&name)
    }
}
