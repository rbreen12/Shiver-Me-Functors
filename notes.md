# Design notes

## How to run

Install the dependencies once (needs OCaml and opam):

```sh
opam install dune ounit2 raylib
```

Run these from the repo root. The game loads `levels/level1.txt` by a relative
path, so it has to be started from there.

| Command                                  | What it does                          |
| ---------------------------------------- | ------------------------------------- |
| `dune build`                             | build everything                      |
| `dune exec bin/main.exe`                 | run the game                          |
| `dune test`                              | run the OUnit tests                   |
| `dune exec demo/fps_demo.exe`            | open the FPS test window              |
| `dune exec demo/fps_demo.exe -- --bench 4` | benchmark both draw modes for 4 s each |

`Window` is still a stub (every function is `failwith "TODO"`), so
`bin/main.exe` builds but stops with a `TODO` failure as soon as it starts.
It will run once `Window` is implemented on top of raylib. Until then the demo
is the only thing that opens a window.

## Proposed: initial interfaces (pending team agreement)

- `Level` instead of `Map`, because a local `Map` module would shadow
  `Stdlib.Map`.
- `Raycaster` (pure) is separate from `Renderer` (drawing), so ray math can be
  tested and reused for hitscan and line of sight.
- Only `Window` touches the graphics library. If the library is too slow, only
  that module changes. The library itself is not chosen yet.
- `Renderer.draw` takes a `Game.t`, so its signature stays the same when enemies
  and items are added.
- Coordinates: one cell is 1.0 x 1.0, x grows right, y grows down. Angle 0 faces
  +x and increases clockwise on screen.

## Level file format

| Char            | Meaning                                  |
| --------------- | ---------------------------------------- |
| `#`             | stone wall                               |
| `%`             | wood wall                                |
| `.`             | floor                                    |
| `>` `<` `^` `v` | player spawn, facing right/left/up/down  |

Exits, enemies, and pickups are left out until after the first playable version.

## Graphics library and FPS test

- `raylib` (opam, 2.2.2), because unlike `Graphics` it reports held keys, so
  moving and turning together works.
- `demo/fps_demo.ml` opens a 640x480 window with vsync and the frame cap off. It
  measures two modes: 640 one-pixel wall strips per frame, like
  `Renderer.draw`, and a full 640x480 pixel upload per frame, which textured
  walls would need.
- After `opam install raylib`, `dune exec demo/fps_demo.exe -- --bench 4` runs
  each mode for 4 seconds and prints the averages. Without `--bench`, Tab
  switches modes and the keys being held are shown at the top.

| Machine                          | Strips fps | Upload fps |
| -------------------------------- | ---------- | ---------- |
| Parth (i7-1255U, Iris Xe, Linux) | 1678       | 778        |
| Ryan  (Mac M3 Pro)               | 3954       | 1579       |
| Weston  (i7, 8 Core, Windows)    | 501        | 764        |
