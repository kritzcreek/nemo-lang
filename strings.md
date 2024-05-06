# String implementation for Nemo

Do flat strings first. (array i8), assuming/ensuring well formed unicode.
Strings are immutable.
Iterators/Slice (same thing?):

```
// If funcref is part of iter, we can use same interface for reverse iterator
struct StringIter {
  bytes: Option<[i8]> <- reference to string (optional so we can drop once exhausted)
  offset: i32         <- byte offset
  next: fn (self) -> Option<i32>
}

struct Slice {
  start: StringIter,
  length: Option<i32> <- remaining codepoint count. None means until end of string
}
```

## Future

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

```
(type $rope (sub (struct (field $size i32))))
(type $leaf (sub $rope
  (struct
    (field $size i32)
    (field $bytes (ref $bytes)))))
(type $concat (sub $rope
  (struct
    (field $size i32)
    (field $left (ref $rope))
    (field $right (ref $rope)))))

(func $leaf (param $bytes (ref $bytes)) (result (ref $rope))
  local.get $bytes
  call $utf8_len
  local.get $bytes
  struct.new $leaf)

(func $concat (param $l (ref $rope)) (param $r (ref $rope)) (result (ref $rope))
  local.get $l
  call $size
  local.get $r
  call $size
  i32.add

  ;; TODO Balancing
  local.get $l
  local.get $r
  struct.new $concat)

(func $size (param (ref $rope)) (result i32)
  local.get 0
  struct.get $rope $size)
```
