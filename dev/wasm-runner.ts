if (Deno.args.length !== 1) {
  console.error("Please provide a file path as an argument");
  Deno.exit(1);
}

const encoder = new TextEncoder();
const imports = {
  env: {
    log: (arg: number) => console.log(arg),
    print_char: (cp: number) =>
      Deno.stdout.writeSync(encoder.encode(String.fromCodePoint(cp))),
    random: () => Math.random(),
  },
};

const filePath = Deno.args[0];
// TODO catch?
const wasmCode = await Deno.readFile(filePath);

const wasmModule = new WebAssembly.Module(wasmCode);
const wasmInstance = new WebAssembly.Instance(wasmModule, imports);
const main = (wasmInstance.exports["main"] ??
  wasmInstance.exports["main::main"]) as CallableFunction;
console.log(main());
