(module
  (type $bytes (array i8))
  (elem declare func $next_char)

  (rec 
    (type $next_fn (func (param (ref $iter)) (result i32)))

    (type $iter (sub
      (struct
        (field $string (ref $bytes))
        (field $offset (mut i32))
        (field $next (ref $next_fn)))))

    (type $slice (sub $iter
      (struct
        (field $string (ref $bytes))
        (field $offset (mut i32))
        (field $next (ref $next_fn))
        (field $end (mut i32))))))

  (data $violin "\f0\9d\84\9e\f0\9d\84\9e")
  (func $mk_bytes (result (ref $bytes))
    i32.const 0
    i32.const 8
    array.new_data $bytes $violin)

  ;; Assumes well formed utf-8
  (func $utf8_len (param $bytes (ref $bytes)) (result i32)
    (local $result i32)
    (local $i i32)
    (local $bytes_len i32)
    local.get $bytes
    array.len
    local.set $bytes_len

    block $done
      local.get $i
      local.get $bytes_len
      i32.lt_u
      i32.eqz
      br_if $done
      loop $continue
        local.get $bytes
        local.get $i
        array.get_u $bytes

        i32.const 0xC0 ;; 0b11000000
        i32.and
        i32.const 0x80
        i32.ne

        local.get $result
        i32.add
        local.set $result

        ;; i++ and loop condition
        local.get $i
        i32.const 1
        i32.add
        local.tee $i
        local.get $bytes_len
        i32.lt_u
        i32.eqz
        br_if $done
        br $continue
      end
    end

    local.get $result)

  ;; Assumes well formed utf8. Returns the codepoint as the first value
  ;; on the stack and the number of bytes consumed as the second
  (func $codepoint_at_byte (param $offset i32) (param $bytes (ref $bytes)) (result i32 i32)
    (local $first_byte i32)
    (local $extra_bytes i32)
    local.get $bytes
    local.get $offset
    array.get_u $bytes
    local.tee $first_byte

    ;; ASCII case
    i32.const 0x80
    i32.and
    i32.eqz
    if
      local.get $first_byte
      i32.const 1
      return
    end

    ;; Count leading 1s
    local.get $first_byte
    i32.const -1
    i32.xor
    local.set $extra_bytes
    (block $three
      (block $two
        (block $one
          local.get $extra_bytes
          i32.const 1
          i32.sub
          br_table 0 1 2
        )
        ;; 1
        local.get $first_byte
        i32.const 63
        i32.and

        i32.const 6
        i32.shl

        local.get $bytes
        local.get $offset
        i32.const 1
        i32.add
        array.get_u $bytes

        i32.const 63
        i32.and

        i32.add

        i32.const 2
        return
      )
      ;; 2
      local.get $first_byte
      i32.const 15
      i32.and
      i32.const 6
      i32.shl

      local.get $bytes
      local.get $offset
      i32.const 1
      i32.add
      array.get_u $bytes

      i32.const 63
      i32.and

      i32.add
      i32.const 6
      i32.shl

      local.get $bytes
      local.get $offset
      i32.const 2
      i32.add
      array.get_u $bytes

      i32.const 63
      i32.and

      i32.add

      i32.const 3
      return
    )
    ;; 3
    local.get $first_byte
    i32.const 7
    i32.and
    i32.const 6
    i32.shl

    local.get $bytes
    local.get $offset
    i32.const 1
    i32.add
    array.get_u $bytes

    i32.const 63
    i32.and

    i32.add
    i32.const 6
    i32.shl

    local.get $bytes
    local.get $offset
    i32.const 2
    i32.add
    array.get_u $bytes

    i32.const 63
    i32.and

    i32.add
    i32.const 6
    i32.shl

    local.get $bytes
    local.get $offset
    i32.const 3
    i32.add
    array.get_u $bytes

    i32.const 63
    i32.and

    i32.add

    i32.const 4
  )

  (func $next_char (type $next_fn) (param $iter (ref $iter)) (result i32)
    (local $offset i32)
    (call $codepoint_at_byte
      (struct.get $iter $offset (local.get $iter))
      (struct.get $iter $string (local.get $iter)))
    local.set $offset
    local.get $iter
    local.get $offset
    (struct.get $iter $offset (local.get $iter))
    i32.add
    struct.set $iter $offset)

  (func $mk_iter (param $string (ref $bytes)) (result (ref $iter))
    local.get $string
    i32.const 0
    ref.func $next_char
    struct.new $iter)

  (func $main (result i32 i32)
    (local $iter (ref $iter))
    call $mk_bytes
    call $mk_iter
    local.set $iter

    ;; iter.next(iter)
    local.get $iter
    local.get $iter
    struct.get $iter $next
    call_ref $next_fn

    local.get $iter
    local.get $iter
    struct.get $iter $next
    call_ref $next_fn)

  (export "main" (func $main))
)
