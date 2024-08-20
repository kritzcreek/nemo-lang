# nemo-lang

nemo is a small procedural programming language that compiles to Wasm GC

## Playground

Check out the [Nemo playground] for example programs.
It runs entirely in your browser, so you can play around and experiment however you like.

## Motivation

I "know" how to do a lot of things in compilers theoretically, conceptually, or in isolation.
This project helps me verify I can actually put them together and understand them well enough to put into practice.

I think Wasm GC is a fantastic opportunity.
It could make creating new high-level interesting languages fun, and accessible.
Part of making that a reality is building tooling, examples, and teaching materials.
I hope Nemo can help in that effort.

Depending on how the language evolves it might become part of my compilers course at TH Cologne.

## Language Features

Most features can be directly mapped to Wasm GC constructs. I'm just listing them here:

- Function imports
- Top-level functions
- Globals
- Primitive types: i32, f32, bool, unit
- Composite types: Structs (nominal), Arrays (structural)
- if-expressions
- while
- Early returns
- Infix operators for most numeric instructions
- Built-in functions/intrinsics for all other numeric functions

Features we implement "on-top"

- First order parametric polymorphism (Generics) via monomorphization
- Variants & Pattern matching
- First class functions (closures)
- Lazy initialization for non-const globals
- Type directed operator resolution (+ can mean both i32.add and f32.add)
- Type directed field resolution for struct access
- Nested set targets for composite types: `set p.particles[2].vx = 10.0`
- Block scoping

## Working on the project

The compiler is written in Rust, so at the minimum you'll need a [Rust toolchain].

While developing the compiler a couple of other tools are useful/necessary to have:
- just: The command runner used for tasks in the [justfile]
- wasm-bindgen: Required to build the playground
- wasm-tools: Wasm pretty printer/parser tools
- wasm-opt: The wasm-opt tool from Emscripten packaged up as a Rust package

The easiest way I've found to install them is using [cargo-binstall]:
```sh
cargo binstall just wasm-bindgen-cli wasm-tools wasm-opt watchexec-cli
```

Additionally you'll need [Deno] to run the tests.

[Nemo playground]: https://kritzcreek.github.io/nemo-lang
[Rust toolchain]: https://rustup.rs
[cargo-binstall]: https://github.com/cargo-bins/cargo-binstall
[justfile]: ./justfile
[Deno]: https://deno.com/
