build-cli:
    cargo build --release --bin nemo

build-wasm-lib:
    cd packages/wasm-lib && cargo rustc  --crate-type cdylib --target wasm32-unknown-unknown --release
    wasm-bindgen target/wasm32-unknown-unknown/release/wasm_lib.wasm --out-dir playground/wasm-lib/ --target web
    wasm-opt -Os playground/wasm-lib/wasm_lib_bg.wasm -o playground/wasm-lib/wasm_lib_bg.wasm

@run-wasm FILE:
    mkdir -p build
    ./target/release/nemo compile {{ FILE }} --output build/{{ without_extension(FILE) }}.wasm
    wasm-opt --enable-reference-types --enable-gc -O3 build/{{ without_extension(FILE) }}.wasm -o build/{{ without_extension(FILE) }}_opt.wasm
    wasm-tools print build/{{ without_extension(FILE) }}.wasm -o build/{{ without_extension(FILE) }}.wast
    wasm-tools print build/{{ without_extension(FILE) }}_opt.wasm -o build/{{ without_extension(FILE) }}_opt.wast
    node dev/run-wasm.mjs build/{{ without_extension(FILE) }}.wasm

dev FILE: build-cli
    watchexec --quiet -e nemo just run-wasm {{ FILE }}

playground: build-wasm-lib
    cd playground && npm i && npm run dev

update-gh-pages: build-wasm-lib
    cd playground && npm i && npm run build
    rm -r gh-pages/*
    cp -r playground/dist/* gh-pages/

@run-wast FILE:
    wasm-tools parse {{ FILE }}.wast -o build/{{ FILE }}.wasm
    wasm-tools validate -f gc build/{{ FILE }}.wasm
    node dev/run-wasm.mjs build/{{ FILE }}.wasm

dev-wast FILE:
    watchexec --quiet -e wast just run-wast {{ FILE }}
