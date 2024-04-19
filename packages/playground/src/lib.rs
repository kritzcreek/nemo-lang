use nemo_backend::codegen::codegen;
use nemo_parser::{check_program, compile_program};
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
        None => {
            let json = warp::reply::json(&"Error compiling program");
            warp::reply::with_status(json, StatusCode::BAD_REQUEST)
        }
        Some(compiled) => {
            let wasm = Wasm {
                renamed: code.code,
                compiled,
            };
            let json = warp::reply::json(&wasm);
            warp::reply::with_status(json, StatusCode::OK)
        }
    }
    // match check_program(&code.code) {
    //     Err(err) => {
    //         let err_msg = format!("{}", err);
    //         eprintln!("{err_msg}");
    //         let json = warp::reply::json(&err_msg);
    //         warp::reply::with_status(json, StatusCode::BAD_REQUEST)
    //     }
    //     Ok(checked) => {
    //         let printed = print_program(&checked);
    //         let (program, name_map) = lower(checked);
    //         let compiled = codegen(program, name_map);
    //         let wasm = Wasm {
    //             renamed: printed,
    //             compiled,
    //         };
    //         let json = warp::reply::json(&wasm);
    //         warp::reply::with_status(json, StatusCode::OK)
    //     }
    // }
}

async fn async_run() {
    pretty_env_logger::init();

    println!("Open your browser at http://127.0.0.1:3030");

    // POST /compile  {"code": "fn main(): i32 = { 10 + 23 }"}
    let compile = warp::post()
        .and(warp::path("compile"))
        .and(warp::body::json())
        .map(|c| run_pipeline(c));

    let files = warp::fs::dir("packages/playground/static");

    warp::serve(compile.or(files))
        .run(([127, 0, 0, 1], 3030))
        .await
}
