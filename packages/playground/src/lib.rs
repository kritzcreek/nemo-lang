use nemo_parser::{compile_program, render_errors};
use serde_derive::{Deserialize, Serialize};

use warp::{
    hyper::StatusCode,
    reply::{Json, WithStatus},
    Filter,
};

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

pub fn run_playground() {
    let rt = tokio::runtime::Runtime::new().unwrap();
    rt.block_on(async { async_run().await })
}

fn run_pipeline(code: Code) -> WithStatus<Json> {
    match compile_program(&code.code) {
        (name_map, Err(errs)) => {
            let json = warp::reply::json(&render_errors(&errs, &code.code, &name_map));
            warp::reply::with_status(json, StatusCode::BAD_REQUEST)
        }
        (_, Ok(compiled)) => {
            let wasm = Wasm {
                renamed: code.code,
                compiled,
            };
            let json = warp::reply::json(&wasm);
            warp::reply::with_status(json, StatusCode::OK)
        }
    }
}

async fn async_run() {
    pretty_env_logger::init();

    println!("Open your browser at http://127.0.0.1:3030");

    // POST /compile  {"code": "fn main(): i32 = { 10 + 23 }"}
    let compile = warp::post()
        .and(warp::path("compile"))
        .and(warp::body::json())
        .map(run_pipeline);

    let files = warp::fs::dir("packages/playground/static");

    warp::serve(compile.or(files))
        .run(([127, 0, 0, 1], 3030))
        .await
}
