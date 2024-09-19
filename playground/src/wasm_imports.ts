let consoleBuffer = "";

export function getConsoleBuffer(): string {
  return consoleBuffer;
}

export function clearConsoleBuffer(): void {
  consoleBuffer = "";
}

let cachedImports: WebAssembly.Imports | undefined = undefined;

function setupWasmImports(): WebAssembly.Imports {
  const canvas = document.getElementById("output-canvas")! as HTMLCanvasElement;
  const ctx = canvas.getContext("2d")!;
  ctx.lineWidth = 3.0;

  function clear_canvas() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
  }

  function begin_path() {
    ctx.beginPath();
  }

  function move_to(x: number, y: number) {
    ctx.moveTo(x, y);
  }

  function line_to(x: number, y: number) {
    ctx.lineTo(x, y);
  }

  function close_path() {
    ctx.closePath();
  }

  function stroke() {
    ctx.stroke();
  }

  function fill() {
    ctx.fill();
  }

  function set_stroke_color(r: number, g: number, b: number) {
    ctx.strokeStyle = `rgb(${r}, ${g}, ${b})`;
  }

  function set_fill_color(r: number, g: number, b: number) {
    ctx.fillStyle = `rgb(${r}, ${g}, ${b})`;
  }

  function arc(x: number, y: number, a: number, b: number, c: number) {
    ctx.arc(x, y, a, b, c);
  }

  function fill_rect(x: number, y: number, dx: number, dy: number) {
    ctx.fillRect(x, y, dx, dy)
  }

  function save() {
    ctx.save()
  }

  function restore() {
    ctx.restore()
  }

  function translate(x: number, y: number) {
    ctx.translate(x, y)
  }

  function rotate(x: number) {
    ctx.rotate(x)
  }

  function log(x: any) {
    consoleBuffer += x + "\n";
    console.log(x);
  }

  function random(): number {
    return Math.random();
  }

  function print_char(cp: number) {
    const s = String.fromCodePoint(cp);
    consoleBuffer += s;
    console.log(s);
  }

  const imports = {
    env: {
      log,
      print_char,
      random,
      clear_canvas,
      begin_path,
      move_to,
      line_to,
      arc,
      fill_rect,
      close_path,
      set_fill_color,
      set_stroke_color,
      stroke,
      fill,
      save,
      restore,
      translate,
      rotate,
    },
  };
  return imports;
}

export function getWasmImports(): WebAssembly.Imports {
  if (cachedImports === undefined) {
    cachedImports = setupWasmImports();
  }
  return cachedImports;
}
