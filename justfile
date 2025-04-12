set windows-shell := ["pwsh.exe", "-NoLogo", "-Command"]

default:
    just --list

install:
    cargo install --path crates/cli

test:
    cargo nextest run --all
    cargo clippy --all-targets --all-features

test-accept:
    cargo insta test --test-runner=nextest --accept

ci:
    just gen
    git update-index -q --really-refresh
    git diff-index --quiet HEAD crates/frontend/src/syntax/nodes.rs
    just test
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
    cargo run --bin nemo run std/* {{ FILE }}

dev FILE:
    watchexec --quiet --no-vcs-ignore -e nemo,rs,mjs just run-wasm {{ FILE }}

playground: build-wasm-lib
    cd playground && npm i && npm run dev

build-playground: build-wasm-lib
    cd playground && npm i && npm run build

update-gh-pages: build-playground
    rm -r gh-pages/*
    cp -r playground/dist/* gh-pages/

install-tools:
    cargo binstall just wasm-bindgen-cli wasm-tools wasm-opt watchexec-cli cargo-nextest --secure
