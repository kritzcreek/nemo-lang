build-cli:
  cargo build --release --bin nemo

@run-wasm FILE:
  mkdir -p build
  ./target/release/nemo compile {{FILE}} --output build/{{ without_extension(FILE) }}.wasm
  wasm-opt -O3 build/{{ without_extension(FILE) }}.wasm -o build/{{ without_extension(FILE) }}_opt.wasm
  wasm-tools print build/{{ without_extension(FILE) }}.wasm -o build/{{ without_extension(FILE) }}.wast
  wasm-tools print build/{{ without_extension(FILE) }}_opt.wasm -o build/{{ without_extension(FILE) }}_opt.wast
  node dev/run-wasm.mjs build/{{ without_extension(FILE) }}.wasm

dev FILE: build-cli
  watchexec --quiet -e nemo just run-wasm {{FILE}}