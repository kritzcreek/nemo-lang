use std::collections::HashMap;
use crate::ir;
use crate::ir::Name;


#[derive(Debug, Clone, Copy, Ord, PartialOrd, Eq, PartialEq, Hash)]
pub enum Visibility {
    Public,
    Private
}

/// Contains all information needed to type check another module that uses
/// this module
#[derive(Debug)]
pub struct Interface<'a> {
    pub structs: HashMap<Name, (&'a ir::Struct, Visibility)>,
    pub variants: HashMap<Name, (&'a ir::Variant, Visibility)>,
    pub functions: HashMap<Name, (&'a ir::Func, Visibility)>,
}