use crate::ir;
use crate::ir::Name;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Visibility {
    Public,
    Private,
}

/// Contains all information needed to type check another module that uses
/// this module
#[derive(Debug, Default)]
pub struct Interface {
    pub structs: HashMap<Name, (ir::Struct, Visibility)>,
    pub variants: HashMap<Name, (ir::Variant, Visibility)>,
    pub functions: HashMap<Name, (ir::Func, Visibility)>,
}
