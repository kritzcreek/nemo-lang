#![deny(warnings)]

use std::{
    io::Write,
    process::{Command, Stdio},
};

use serde_derive::{Deserialize, Serialize};
use tokio;

use warp::Filter;

#[derive(Deserialize, Serialize)]
struct Code {
    code: String,
}

#[derive(Deserialize, Serialize)]
struct Wasm {
    renamed: String,
    // TODO: encode via base64
    compiled: Vec<u8>,
}

#[tokio::main]
async fn main() {
    pretty_env_logger::init();

    println!("Open your browser at http://127.0.0.1:3030");

    // POST /compile  {"code": "fn main(): i32 = { 10 + 23 }"}
    let compile = warp::post()
        .and(warp::path("compile"))
        .and(warp::body::json())
        .map(|code: Code| {
            let mut child = Command::new("cargo")
                .args(vec!["run", "--bin", "nemo-backend"])
                .stdin(Stdio::piped())
                .stdout(Stdio::piped())
                .stderr(Stdio::inherit())
                .spawn()
                .expect("Failed to start process");
            let mut stdin = child.stdin.take().unwrap();
            stdin.write_all(code.code.as_bytes()).unwrap();
            drop(stdin);

            let output = child.wait_with_output().expect("Failed to read stdout");
            let wasm = Wasm {
                renamed: "but y tho?".to_string(),
                compiled: output.stdout.to_vec(),
            };
            warp::reply::json(&wasm)
        });

    let files = warp::fs::dir("packages/playground/static");

    warp::serve(compile.or(files))
        .run(([127, 0, 0, 1], 3030))
        .await
}
