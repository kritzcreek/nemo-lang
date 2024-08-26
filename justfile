set windows-shell := ["pwsh.exe", "-NoLogo", "-Command"]

install:
    cargo install --path crates/cli

ci:
    just gen
    git update-index -q --really-refresh
    git diff-index --quiet HEAD crates/frontend/src/syntax/nodes.rs
    cargo test --all
    cargo clippy --all-targets --all-features
    cargo fmt --all --check
    just build-playground

gen:
    cargo xtask-gen-ast
    cargo fmt -- crates/frontend/src/syntax/nodes.rs

build-wasm-lib:
    cd crates/wasm-lib && cargo rustc  --crate-type cdylib --target wasm32-unknown-unknown --release
    wasm-bindgen target/wasm32-unknown-unknown/release/wasm_lib.wasm --out-dir playground/wasm-lib/ --target web
    wasm-opt -Os playground/wasm-lib/wasm_lib_bg.wasm -o playground/wasm-lib/wasm_lib_bg.wasm

run-wasm FILE:
    mkdir -p build
    cargo run --bin nemo compile std/* {{ FILE }} --output build/{{ without_extension(file_name(FILE)) }}.wasm
    wasm-opt --enable-reference-types --enable-gc --enable-bulk-memory -O3 build/{{ without_extension(file_name(FILE)) }}.wasm -o build/{{ without_extension(file_name(FILE)) }}_opt.wasm
    wasm-tools print build/{{ without_extension(file_name(FILE)) }}.wasm -o build/{{ without_extension(file_name(FILE)) }}.wast
    wasm-tools print build/{{ without_extension(file_name(FILE)) }}_opt.wasm -o build/{{ without_extension(file_name(FILE)) }}_opt.wast
    deno run --allow-read dev/wasm-runner.ts build/{{ without_extension(file_name(FILE)) }}.wasm

dev FILE:
    watchexec --quiet --no-vcs-ignore -e nemo,rs,mjs just run-wasm {{ FILE }}

playground: build-wasm-lib
    cd playground && npm i && npm run dev

build-playground: build-wasm-lib
    cd playground && npm i && npm run build

update-gh-pages: build-playground
    rm -r gh-pages/*
    cp -r playground/dist/* gh-pages/

@run-wast FILE:
    wasm-tools parse {{ FILE }}.wast -o build/{{ FILE }}.wasm
    wasm-tools validate -f gc build/{{ FILE }}.wasm
    node dev/run-wasm.mjs build/{{ FILE }}.wasm

dev-wast FILE:
    watchexec --quiet -e wast just run-wast {{ FILE }}
