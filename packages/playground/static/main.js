function mainProgram() {
  return localStorage.getItem("mainProgram") ?? "";
}

function canvasProgram() {
  return localStorage.getItem("canvasProgram") ?? "";
}

const canvas = document.getElementById("canvas");
/**
 * @type CanvasRenderingContext2D
 */
const ctx = canvas.getContext("2d");
ctx.lineWidth = 3.0

function clear_canvas() {
  ctx.clearRect(0, 0, canvas.width, canvas.height)
}

function begin_path(_x) {
  ctx.beginPath()
}

function move_to(x, y) {
  ctx.moveTo(x, y)
}

function line_to(x, y) {
  ctx.lineTo(x, y)
}

function close_path(_x) {
  ctx.closePath()
}

function stroke(_x) {
  ctx.stroke()
}

function set_stroke_color(r, g, b) {
  ctx.strokeStyle = (`rgb(${r}, ${g}, ${b})`)
}

let tick = () => { }
let autoRecompile = true
let useCanvas = true

function setInfo(text) {
  document.getElementById("info-box").innerText = text
}

function prependInfo(text) {
  const old = document.getElementById("info-box").innerText
  document.getElementById("info-box").innerText = text + "\n" + old
}

function appendInfo(text) {
  document.getElementById("info-box").innerText += "\n" + text
}

const editor = ace.edit("editor");

function setEditorContent(text) {
  editor.session.setValue(text);
}

function getEditorContent() {
  return editor.getValue()
}

function stopRender() {
  tick = undefined
}

document.getElementById("stopBtn").onclick = function (e) {
  e.preventDefault()
  e.stopPropagation()

  console.log("Stopping")
  stopRender()
}

document.getElementById("startBtn").onclick = function (e) {
  e.preventDefault()
  e.stopPropagation()

  console.log("Starting")
  restartRender()
}

document.getElementById("toggleAuto").onclick = function (e) {
  e.preventDefault()
  e.stopPropagation()

  autoRecompile = !autoRecompile
  localStorage.setItem("autoRecompile", autoRecompile)
  document.getElementById("toggleAuto").innerText = autoRecompile ? "Recompile: On" : "Recompile: Off";
}

document.getElementById("toggleCanvas").onclick = function (e) {
  e.preventDefault()
  e.stopPropagation()

  useCanvas = !useCanvas
  localStorage.setItem("useCanvas", useCanvas)

  if (useCanvas) {
    setEditorContent(canvasProgram())
    document.getElementById("toggleCanvas").innerText = "Use canvas: On"
  } else {
    clear_canvas()
    stopRender()
    setEditorContent(mainProgram())
    document.getElementById("toggleCanvas").innerText = "Use canvas: Off"
  }
}

async function runCompiler(code) {
  const response = await fetch("compile", {
    method: "POST",
    headers: {
      "Content-Type": "application/json"
    },
    body: JSON.stringify({ code })
  });
  const parsed = await response.json();
  return { renamed: parsed.renamed, compiled: new Uint8Array(parsed.compiled) }
}

async function instantiateWasm(compiledWasm, imports) {
  try {
    const inst = await WebAssembly.instantiate(compiledWasm, imports)
    return inst.instance
  } catch (err) {
    appendInfo("Failed to instantiate wasm with: " + err)
  }
}

let previousTimeStamp;
function render(timeStamp) {
  const elapsed = timeStamp - (previousTimeStamp ?? timeStamp);
  previousTimeStamp = timeStamp;
  if (tick) {
    tick(elapsed)
    requestAnimationFrame(render)
  }
}

async function restartRender() {
  previousTimeStamp = undefined
  const { renamed, compiled } = await runCompiler(getEditorContent())
  if (renamed) {
    appendInfo(renamed)
  }
  if (compiled) {
    const imports = {
      env: {
        clear_canvas,
        begin_path,
        move_to,
        line_to,
        arc: (x, y, a, b, c) => ctx.arc(x, y, a, b, c),
        close_path,
        set_stroke_color,
        stroke
      }
    }
    instantiateWasm(compiled, imports).then(inst => {
      clear_canvas()
      tick = (elapsed) => inst.exports.tick(elapsed)
    }).then(() => {
      requestAnimationFrame(render)
    })
  }
}

async function runStaticWasm() {
  const { renamed, compiled } = await runCompiler(getEditorContent())
  if (renamed) {
    appendInfo(renamed)
  }
  if (compiled) {
    instantiateWasm(compiled, { env: { log: x => { console.log(x); return x } } }).then(inst => {
      const result = inst.exports.main();
      prependInfo("Run result: " + result)
    }).catch(err => {
      prependInfo("Failed to run wasm: " + err.toString())
    })
  }
}

async function watchEditor() {
  editor.session.on('change', () => {
    if (useCanvas) {
      localStorage.setItem("canvasProgram", getEditorContent())
      if (autoRecompile) {
        restartRender()
      }
    } else {
      localStorage.setItem("mainProgram", getEditorContent())
      if (autoRecompile) {
        runStaticWasm()
      }
    }
  });
}

function loadSettings() {
  autoRecompile = JSON.parse(localStorage.getItem("autoRecompile") ?? "true")
  useCanvas = JSON.parse(localStorage.getItem("useCanvas" ?? "true"))
}

loadSettings()
setEditorContent(useCanvas ? canvasProgram() : mainProgram())

if (useCanvas) {
  restartRender()
} else {
  runStaticWasm()
}

watchEditor()
