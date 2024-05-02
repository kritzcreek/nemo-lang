let consoleBuffer = "";

export function getConsoleBuffer(): string {
  return consoleBuffer
}

export function clearConsoleBuffer(): void {
  consoleBuffer = ""
}

export function setupWasmImports(): WebAssembly.Imports {
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

  function set_stroke_color(r: number, g: number, b: number) {
    ctx.strokeStyle = `rgb(${r}, ${g}, ${b})`;
  }

  function arc(x: number, y: number, a: number, b: number, c: number) {
    ctx.arc(x, y, a, b, c);
  }

  function log(x: any) {
    consoleBuffer += x + "\n";
    console.log(x);
  }

  function random(): number {
    return Math.random();
  }

  const imports = {
    env: {
      log,
      random,
      clear_canvas,
      begin_path,
      move_to,
      line_to,
      arc,
      close_path,
      set_stroke_color,
      stroke,
    },
  };
  return imports;
}
