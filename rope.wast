(module
  (type $rope (sub (struct (field $size i32))))
  (type $bytes (array i8))
  (type $leaf (sub $rope
    (struct
      (field $size i32)
      (field $bytes (ref $bytes)))))
  (type $concat (sub $rope
    (struct
      (field $size i32)
      (field $left (ref $rope))
      (field $right (ref $rope)))))

  (func $mk_bytes (result (ref $bytes))
    i32.const 26
    i32.const 27
    i32.const 28
    i32.const 29
    array.new_fixed $bytes 4)

  (func $size (param (ref $rope)) (result i32)
    local.get 0
    struct.get $rope $size)

  (func $concat (param $l (ref $rope)) (param $r (ref $rope)) (result (ref $rope))
    local.get $l
    call $size
    local.get $r
    call $size
    i32.add

    local.get $l
    local.get $r
    struct.new $concat)

  (func $main (result i32)
    i32.const 4
    call $mk_bytes
    struct.new $leaf
    call $size)

  (export "main" (func $main))
)
