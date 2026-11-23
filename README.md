# 3110-Group-Project
Team Project 65; "Shiver Me Functors"

# Names
Weston Clark - wrc65

Ryan Breen - rmb382

Parth Mittal - pm653

# Setup
We use raylib for graphics. Install it once into your opam switch:

```
opam install raylib
```

On Linux it also needs the X11 and OpenGL dev libraries. On Ubuntu:
`sudo apt install libgl1-mesa-dev libxcursor-dev libxi-dev libxinerama-dev libxrandr-dev`

Then `dune build` and `dune test`. `dune exec bin/main.exe` runs the game: WASD
to move and strafe, arrow keys to turn, Esc to quit.
