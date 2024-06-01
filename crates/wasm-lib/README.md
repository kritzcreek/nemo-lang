Build with

```
cargo rustc  --crate-type cdylib --target wasm32-unknown-unknown --release
wasm-bindgen ../../target/wasm32-unknown-unknown/release/wasm_lib.wasm --out-dir ../../playground/wasm-lib/ --target web
```
