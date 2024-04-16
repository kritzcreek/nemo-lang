use nemo_parser::parser::parse_prog;

use insta::{assert_snapshot, glob};

/// Copied from the Rust compiler: https://github.com/rust-lang/rust/pull/62948/
/// Replaces `\r\n` with `\n` in-place in `src`.
///
/// Returns error if there's a lone `\r` in the string
fn normalize_newlines(src: &mut String) {
    if !src.as_bytes().contains(&b'\r') {
        return;
    }

    // We replace `\r\n` with `\n` in-place, which doesn't break utf-8 encoding.
    // While we *can* call `as_mut_vec` and do surgery on the live string
    // directly, let's rather steal the contents of `src`. This makes the code
    // safe even if a panic occurs.

    let mut buf = std::mem::replace(src, String::new()).into_bytes();
    let mut gap_len = 0;
    let mut tail = buf.as_mut_slice();
    loop {
        let idx = match find_crlf(&tail[gap_len..]) {
            None => tail.len(),
            Some(idx) => idx + gap_len,
        };
        tail.copy_within(gap_len..idx, 0);
        tail = &mut tail[idx - gap_len..];
        if tail.len() == gap_len {
            break;
        }
        gap_len += 1;
    }

    // Account for removed `\r`.
    // After `set_len`, `buf` is guaranteed to contain utf-8 again.
    let new_len = buf.len() - gap_len;
    unsafe {
        buf.set_len(new_len);
        *src = String::from_utf8_unchecked(buf);
    }

    fn find_crlf(src: &[u8]) -> Option<usize> {
        let mut search_idx = 0;
        while let Some(idx) = find_cr(&src[search_idx..]) {
            if src[search_idx..].get(idx + 1) != Some(&b'\n') {
                search_idx += idx + 1;
                continue;
            }
            return Some(search_idx + idx);
        }
        None
    }

    fn find_cr(src: &[u8]) -> Option<usize> {
        src.iter().position(|&b| b == b'\r')
    }
}

#[test]
fn parse_toplevel() {
    use std::fs;

    glob!("passing/*.nemo", |path| {
        let input = {
            let mut input = fs::read_to_string(path).unwrap();
            normalize_newlines(&mut input);
            input
        };
        for inp in input.split("---\n") {
            let parse = parse_prog(inp);
            let errors = parse.errors();
            let error_output = if errors.is_empty() {
                "".to_string()
            } else {
                format!("=== ERRORS ===\n\n{:#?}", errors)
            };
            let output = format!("{}\n---\n{}\n{}", inp, parse.debug_tree(), error_output);
            assert_snapshot!(output);
        }
    });
}

#[test]
fn parse_example() {
    use std::fs;
    let input = {
        let mut input = fs::read_to_string("../../example.nemo").unwrap();
        normalize_newlines(&mut input);
        input
    };
    let parse = parse_prog(&input);
    let errors = parse.errors();
    let error_output = if errors.is_empty() {
        "".to_string()
    } else {
        format!("=== ERRORS ===\n\n{:#?}", errors)
    };
    let output = format!("{}\n{}", parse.debug_tree(), error_output);
    assert_snapshot!(output);
}
