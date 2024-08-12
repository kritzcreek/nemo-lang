use std::{
    cell::{Cell, RefCell},
    fmt,
    num::NonZeroU16,
};
use string_interner::{DefaultStringInterner, DefaultSymbol};

use text_size::TextRange;

use crate::types::Interface;

// Indexed by ModuleId
#[derive(Debug)]
pub struct Ctx {
    // Indexed by ModuleId
    interfaces: Vec<Interface>,
    name_supplies: Vec<MutableNameSupply>,
}

impl Ctx {
    pub fn new(module_count: u16) -> Ctx {
        // TODO: Should zero initialize all of these
        let mut name_supplies =
            Vec::with_capacity((module_count + ModuleId::FIRST_NON_RESERVED.get() - 1) as usize);
        for _ in 0..module_count {
            name_supplies.push(MutableNameSupply::new())
        }
        Ctx { name_supplies }
    }
    pub fn set_name_supply(&mut self, module: ModuleId, supply: MutableNameSupply) {
        self.name_supplies[(module.0.get() - 1) as usize] = supply
    }
    pub fn get_name_supply(&self, module: ModuleId) -> &MutableNameSupply {
        &self.name_supplies[(module.0.get() - 1) as usize]
    }
    pub fn resolve(&self, name: Name) -> (String, TextRange) {
        let supply = self.get_name_supply(name.module);
        let id = supply.lookup(name);
        (supply.lookup_symbol(id.it), id.at)
    }
    pub fn display_name(&self, name: Name) -> String {
        self.resolve(name).0
    }
    pub fn fmt_name(&self, f: &mut fmt::Formatter<'_>, name: Name) -> fmt::Result {
        write!(f, "{}", self.display_name(name))
    }
}

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
        debug_assert!(id > 2, "module ids 1 and 2 are reserved");
        Self(NonZeroU16::new(id).expect("ModuleId must be non-zero"))
    }

    pub const PRIM: Self = ModuleId(unsafe { NonZeroU16::new_unchecked(1) });
    pub const CODEGEN: Self = ModuleId(unsafe { NonZeroU16::new_unchecked(2) });
    const FIRST_NON_RESERVED: NonZeroU16 = unsafe { NonZeroU16::new_unchecked(3) };
}

impl Into<u16> for ModuleId {
    fn into(self) -> u16 {
        self.0.get()
    }
}

pub struct ModuleIdGen(NonZeroU16);
impl ModuleIdGen {
    pub fn new() -> Self {
        Self(ModuleId::FIRST_NON_RESERVED)
    }
    pub fn next(&mut self) -> ModuleId {
        let tmp = ModuleId(self.0);
        self.0 = self.0.checked_add(1).unwrap();
        tmp
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Copy)]
pub struct Name {
    pub tag: NameTag,
    pub module: ModuleId,
    pub idx: u32,
}

pub type Symbol = DefaultSymbol;

#[derive(Debug, Copy, Clone)]
pub struct CompactId {
    pub it: Symbol,
    pub at: TextRange,
}

#[derive(Debug)]
pub struct MutableNameSupply {
    supply: Cell<u32>,
    name_map: RefCell<Vec<CompactId>>,
    strings: RefCell<DefaultStringInterner>,
}

impl Default for MutableNameSupply {
    fn default() -> Self {
        Self::new()
    }
}

impl MutableNameSupply {
    pub fn new() -> Self {
        Self {
            supply: Cell::new(0),
            name_map: RefCell::new(Vec::new()),
            strings: RefCell::new(DefaultStringInterner::default()),
        }
    }
    pub fn new_name(
        &self,
        tag: NameTag,
        module: ModuleId,
        it: &str,
        at: TextRange,
    ) -> (Name, Symbol) {
        let idx = self.supply.get();
        self.supply.set(idx + 1);
        let it = self.strings.borrow_mut().get_or_intern(it);
        self.name_map.borrow_mut().push(CompactId { it, at });
        (Name { tag, module, idx }, it)
    }
    pub fn local_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Local, module, it, at)
    }
    pub fn global_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Global, module, it, at)
    }
    pub fn func_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Func, module, it, at)
    }
    pub fn type_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Type, module, it, at)
    }
    pub fn type_var_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::TypeVar, module, it, at)
    }
    pub fn field_idx(&self, module: ModuleId, it: &str, at: TextRange) -> (Name, Symbol) {
        self.new_name(NameTag::Field, module, it, at)
    }
    pub fn gen_idx(&self, module: ModuleId, it: &str) -> (Name, Symbol) {
        self.new_name(NameTag::Gen, module, it, TextRange::default())
    }
    pub fn get_or_intern(&self, s: &str) -> Symbol {
        self.strings.borrow_mut().get_or_intern(s)
    }
    pub fn lookup(&self, name: Name) -> CompactId {
        *self.name_map.borrow().get(name.idx as usize).unwrap()
    }
    pub fn lookup_symbol(&self, sym: Symbol) -> String {
        self.strings.borrow().resolve(sym).unwrap().to_string()
    }
}
