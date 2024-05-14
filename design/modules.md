# Modules

Being able to group related functionality into a "module" is a common need.
As Nemo is trying to be a simple language we want to keep the module system
simple, mostly as a means of namespacing, rather than trying to use it as a
means of abstraction (first class modules etc.)

## Requirements

- Needs to provide a way of controlling encapsulation/visibility
- Should enable separate compilation (tricky as I'm also considering monomorphization). Doesn't need to be implemented, but possible to add later on
- Unambiguous name resolution for a given module without requiring access to its dependencies (improves IDE tooling). Impossible given that we do type directed resolution of struct fields?
- Types have their own namespaces (Nested?)
