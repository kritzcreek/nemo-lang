use frontend::run_frontend;
use frontend::{parser::parse_prog, CheckError};
use insta::{assert_snapshot, glob};
use std::fmt::Write;
use std::fs;
use std::path::Path;
use std::str;

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

    let mut buf = std::mem::take(src).into_bytes();
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

fn snapshot_type_errors(path: &Path, source: &str) -> String {
    let result = run_frontend(source);
    if result
        .errors()
        .any(|e| matches!(e, CheckError::ParseError(_)))
    {
        panic!(
            "{} was expected to fail with a type error, but had a parse error instead",
            path.display()
        )
    }
    if !result.has_errors() {
        panic!("{} was expected to fail, but didn't", path.display())
    }

    let mut err_buf = String::new();
    for error in result.errors() {
        write!(
            &mut err_buf,
            "{}",
            error.display(source, &result.ctx, false)
        )
        .unwrap();
    }
    err_buf
}

#[test]
fn test_type_errors() {
    glob!("type_errors", "*.nemo", |path| {
        let input = {
            let mut input = fs::read_to_string(path).unwrap();
            normalize_newlines(&mut input);
            input
        };
        let output = {
            let mut output = snapshot_type_errors(path, &input);
            normalize_newlines(&mut output);
            output
        };
        assert_snapshot!(output)
    });
}

fn snapshot_parse_tree(input: &str) -> String {
    let parse = parse_prog(input);
    let tree = parse.debug_tree();
    let (_, errors) = parse.take();
    let errors = errors
        .into_iter()
        .map(|e| format!("{}", e.display(input, false)))
        .collect::<Vec<_>>()
        .join("\n");
    format!("{}\n\n{}", tree, errors)
}

#[test]
fn test_parsing() {
    glob!("parsing/*.nemo", |path| {
        let input = {
            let mut input = fs::read_to_string(path).unwrap();
            normalize_newlines(&mut input);
            input
        };
        let output = {
            let mut output = snapshot_parse_tree(&input);
            normalize_newlines(&mut output);
            output
        };
        assert_snapshot!(output);
    });
}
