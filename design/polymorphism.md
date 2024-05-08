# Polymorphism

We want parametric polymorphism for sure, ad-hoc might come in the form of
Go-like "structural" interfaces later.
Only allow first-order polymorphism, no let-generalization of function references.

Keep it simple syntax wise:

```
fn pick|a, b|(first : a, second : a, b : bool) -> a {
  if b {
    let f : a = first;
    f
  } else {
    second
  }
}

fn main() -> i32 {
  // Require explicit instantiation at first, make optional later
  pick|i32|(1, 2, true)
}

variant List(a) {
  struct Nil {},
  struct Cons {
    head: a,
    tail: List(a)
  }
}
```

Implement in the backend via monomorphization. Emit on demand during codegen for now. Later on we might want to generate multiple IR definitions to allow type specific optimizations.

First implement polymorphic functions, then move on to polymorphic data types.
