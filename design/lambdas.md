# Anonymous functions (Lambdas)

Lambdas are nameless functions that can appear in expression position. They
capture variables from their environment. Value types are copied when captured
and trying to `set` them results in a type error.

They are defined using a `\` followed by a parameter list:

```
\(x : i32) -> i32 { x + 1 }
```

When a lambda appears in checking position its argument and result types
can be ommitted and will be inferred.

```
let x : i32 = (\(y) { y + 1 })(2);
```

Lambdas are always monomorphic.
