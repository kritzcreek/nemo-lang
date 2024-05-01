# nemo-lang

nemo is a small procedural programming language that compiles to Wasm GC

Its main purpose is to serve as a teaching tool in my compilers course.

# Example Program

Here's a small program that when given a few canvas functions as imports draws three colored cubes bouncing up and down

```
import clear : () -> i32 from clear_canvas
import begin_path : (f32) -> f32 from begin_path
import move_to : (f32, f32) -> f32 from move_to
import line_to : (f32, f32) -> f32 from line_to
import close_path : (f32) -> f32 from close_path
import set_stroke_color : (f32, f32, f32) -> f32 from set_stroke_color
import stroke : (f32) -> f32 from stroke

struct Color { r : f32, g : f32, b : f32 }

struct Cube {
  x : f32,
  y : f32,
  vx : f32,
  vy : f32,
  size : f32,
  color : Color
}

global red = Color { r = 180.0, g = 0.0, b = 0.0 }
global green = Color { r = 0.0, g = 180.0, b = 0.0 }
global blue = Color { r = 0.0, g = 0.0, b = 180.0 }

global cubes = [
  Cube { x = 100.0, y = 0.0, vx = 0.0, vy = 14.0, size = 10.0, color = red },
  Cube { x = 250.0, y = 0.0, vx = 0.0, vy = 13.0, size = 15.0, color = green },
  Cube { x = 400.0, y = 0.0, vx = 0.0, vy = 12.0, size = 20.0, color = blue }
]

fn clamp(min : f32, val : f32, max : f32) -> f32 {
  f32_min(max, f32_max(min, val))
}

fn draw_cube(cube : Cube) -> f32 {
  let x = cube.x;
  let y = cube.y;
  let size = cube.size;
  begin_path(0.0);
  move_to(x, y);
  line_to(x , y + cube.size);
  line_to(x + cube.size, y + cube.size);
  line_to(x + cube.size, y);
  set_stroke_color(cube.color.r, cube.color.g, cube.color.b);
  close_path(0.0);
  stroke(0.0)
}

fn tick_cube(cube : Cube, elapsed_time_ms : f32) {
  let elapsed_factor = elapsed_time_ms / 48.0;

  set cube.x = cube.x + cube.vx * elapsed_factor;
  set cube.y = cube.y + cube.vy * elapsed_factor;

  if cube.x < 0.0 || cube.x > 500.0 {
    set cube.x = clamp(0.0, cube.x, 500.0);
    set cube.vx = f32_neg(cube.vx)
  } else {};

  if cube.y < 0.0 || cube.y > 500.0 {
    set cube.y = clamp(0.0, cube.y, 500.0);
    set cube.vy = f32_neg(cube.vy)
  } else {}
}

fn tick(elapsed_time_ms : f32) {
  clear();

  let idx = 0;
  while idx < @array_len(cubes) {
    tick_cube(cubes[idx], elapsed_time_ms);
    draw_cube(cubes[idx]);

    set idx = idx + 1
  }
}
```

# Features

Most features can be directly mapped to Wasm GC constructs. I'm just listing them here:

- Function imports
- Top-level functions
- Globals
- Primitive types: i32, f32, bool, unit
- Composite types: Structs (nominal), Arrays (structural)
- Typed function references (no closures)
- if-expressions
- while
- Infix operators for most numeric instructions
- Built-in functions/intrinsics for all other numeric functions

Features we implement "on-top"

- Lazy initialization for non-const globals
- Type directed operator resolution (+ can mean both i32.add and f32.add)
- Type directed field resolution for struct access
- Nested set targets for composite types: `set p.particles[2].vx = 10.0`
- Block scoping

# Compiler Structure/Pipeline Overview (Slightly out-of-date)

![CompilerOverview](https://github.com/kritzcreek/nemo-lang/assets/6189397/db0ef74c-c7c3-410f-9e71-4b4c28ed0451)
