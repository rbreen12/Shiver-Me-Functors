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
