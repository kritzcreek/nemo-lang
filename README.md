# nemo-lang

nemo is a small procedural programming language that compiles to Wasm GC

## Playground

Check out the [Nemo playground] for example programs. It runs entirely in your browser, so you can play around and experiment however you like.

## Motivation

I "know" how to do a lot of things in compilers theoretically, conceptually, or in isolation. This project helps me verify I can actually put them together and understand them well enough to put into practice.

I think Wasm GC is a fantastic opportunity. It could make creating new high-level interesting languages fun, and accessible. Part of making that a reality is building tooling, examples, and teaching materials. I hope Nemo can help in that effort.

Depending on how the language evolves it might become part of my compilers course at TH Cologne.

## Language Features

Most features can be directly mapped to Wasm GC constructs. I'm just listing them here:

- Function imports
- Top-level functions
- Globals
- Primitive types: i32, f32, bool, unit
- Composite types: Structs (nominal), Arrays (structural)
- Typed function references (no closures)
- if-expressions
- while
- Infix operators for most numeric instructions
- Built-in functions/intrinsics for all other numeric functions

Features we implement "on-top"

- Variants & Pattern matching
- Lazy initialization for non-const globals
- Type directed operator resolution (+ can mean both i32.add and f32.add)
- Type directed field resolution for struct access
- Nested set targets for composite types: `set p.particles[2].vx = 10.0`
- Block scoping

[Nemo playground]: https://kritzcreek.github.io/nemo-lang
