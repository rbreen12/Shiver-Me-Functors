# Design notes

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
| Ryan                             |            |            |
| Weston                           |            |            |
