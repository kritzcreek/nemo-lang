# String implementation for Nemo

Idea: Use a rope data structure to enable high-level immutable string operations (maybe a mutable builder subset?)

Requirements:

- Uses Wasm GC structs and arrays
- UTF-8 throughout, no byte level access from public API
- UCS indexing
- Iterators
- Slicing/Substrings

First Sketch:

```
Rope {
  // Can I be smart and say i31 <- means Leaf, and (ref concat)?
  tag: u8
  size(ucs): u24?
  // Potential optimization for later?
  // Char {
  //   byte: u32
  // }
  Leaf {
    bytes: Array(u8)
  }
  Concat {
    size(ucs): u32
    left: Rope
    right: Rope
  }
  SubString {
    root: Rope,
    start: u16,
    end: u16,
  }
}
```

An iterator would need to remember size(ucs) until Leaf currently iterating + byte offset inside that Leaf. That way we can omit storing byte size on all Rope nodes.
