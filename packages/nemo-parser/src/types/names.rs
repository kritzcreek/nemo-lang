use std::{collections::HashMap, fmt};

use rowan::TextRange;

use crate::syntax::SyntaxToken;

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub struct Id {
    pub it: String,
    pub at: TextRange,
}

impl From<&SyntaxToken> for Id {
    fn from(value: &SyntaxToken) -> Self {
        Id {
            it: value.text().to_string(),
            at: value.text_range(),
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct NameSupply {
    local: u32,
    global: u32,
    func: u32,
    typ: u32,
    field: u32,
    name_map: HashMap<Name, Id>,
}

impl NameSupply {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn local_idx(&mut self, local: &SyntaxToken) -> Name {
        self.local += 1;
        let name = Name::Local(self.local);
        self.name_map.insert(name, local.into());
        name
    }

    pub fn global_idx(&mut self, global: &SyntaxToken) -> Name {
        self.global += 1;
        let name = Name::Global(self.global);
        self.name_map.insert(name, global.into());
        name
    }

    pub fn func_idx(&mut self, func: &SyntaxToken) -> Name {
        self.func += 1;
        let name = Name::Func(self.func);
        self.name_map.insert(name, func.into());
        name
    }

    pub fn type_idx(&mut self, typ: &SyntaxToken) -> Name {
        self.typ += 1;
        let name = Name::Type(self.typ);
        self.name_map.insert(name, typ.into());
        name
    }

    pub fn field_idx(&mut self, field: &SyntaxToken) -> Name {
        self.field += 1;
        let name = Name::Field(self.field);
        self.name_map.insert(name, field.into());
        name
    }

    pub fn lookup(&self, name: Name) -> Option<&Id> {
        self.name_map.get(&name)
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub enum Name {
    Global(u32),
    Local(u32),
    Func(u32),
    Type(u32),
    Field(u32),
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Name::Global(x) => write!(f, "$g:{x}"),
            Name::Local(x) => write!(f, "$l:{x}"),
            Name::Func(x) => write!(f, "$fn:{x}"),
            Name::Type(x) => write!(f, "$t:{x}"),
            Name::Field(x) => write!(f, "$f:{x}"),
        }
    }
}
