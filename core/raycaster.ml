type side = Vertical | Horizontal
type hit = { distance : float; wall : Level.wall; side : side; offset : float }

(** [delta d] is the distance a ray travels between two consecutive grid lines
    on one axis, where [d] is the ray's direction component on that axis. For
    example, [delta 0.5] is [2.0]. It is [infinity] if [d = 0.], since the ray
    never crosses a grid line on that axis. Requires: [d] is a component of a
    unit-length direction vector, so [-1. <= d <= 1.]. *)
let delta d = if d = 0. then infinity else Float.abs (1. /. d)

(** [first_grid_cross p d] is the distance a ray travels from coordinate [p] to
    the first grid line it crosses on one axis, where [d] is the ray's direction
    component on that axis. For example, [first_grid_cross 1.5 1.] is [0.5]. It
    is [infinity] if [d = 0.], and [0.] if [p] is exactly on a grid line and
    [d < 0.]. Requires: [p] is finite and [d] is a component of a unit-length
    direction vector. *)
let first_grid_cross p d =
  if d = 0. then infinity
  else if d < 0. then (p -. Float.floor p) *. delta d
  else (Float.floor p +. 1. -. p) *. delta d

(** [frac v] is the fractional part of [v], in [\[0, 1)]. For example,
    [frac 2.75] is [0.75] and [frac (-0.25)] is [0.75]. Requires: [v] is finite.
*)
let frac v = v -. Float.floor v

let cast level ~(origin : Vec.t) ~angle =
  let dx = cos angle in
  let dy = sin angle in
  let step_x = if dx < 0. then -1 else 1 in
  let step_y = if dy < 0. then -1 else 1 in
  let delta_x = delta dx in
  let delta_y = delta dy in

  (* [hit wall side distance] is the hit for this ray when it meets a face of
     [wall], of kind [side], after traveling [distance] from [origin]. The
     offset is measured along the face: by y for a [Vertical] face, by x for a
     [Horizontal] one. *)
  let hit wall side distance =
    let hit_x = origin.x +. (distance *. dx) in
    let hit_y = origin.y +. (distance *. dy) in
    let offset =
      match side with Vertical -> frac hit_y | Horizontal -> frac hit_x
    in
    { distance; wall; side; offset }
  in
  (* [travel col row side_x side_y] is the first wall this ray hits after it is in
     cell ([col], [row]), where [side_x] and [side_y] are the distances along
     the ray from [origin] to the next vertical and horizontal grid lines. Each
     step crosses whichever line is closer. Terminates because every step moves
     one cell, and [Level.tile_at] is a wall outside the level. *)
  let rec travel col row side_x side_y =
    if side_x < side_y then
      let col = col + step_x in
      match Level.tile_at level ~row ~col with
      | Wall w -> hit w Vertical side_x
      | Floor -> travel col row (side_x +. delta_x) side_y
    else
      let row = row + step_y in
      match Level.tile_at level ~row ~col with
      | Wall w -> hit w Horizontal side_y
      | Floor -> travel col row side_x (side_y +. delta_y)
  in
  travel
    (int_of_float (Float.floor origin.x))
    (int_of_float (Float.floor origin.y))
    (first_grid_cross origin.x dx)
    (first_grid_cross origin.y dy)

let cast_screen level player ~fov ~columns =
  let origin = Player.pos player in
  let facing = Player.angle player in
  let half = tan (fov /. 2.) in
  Array.init columns (fun i ->
      (* [camera_x] runs from -1 at the left edge to 1 at the right edge, through
         the center of column [i]. *)
      let camera_x =
        (2. *. (float_of_int i +. 0.5) /. float_of_int columns) -. 1.
      in
      let ray = facing +. atan (camera_x *. half) in
      let h = cast level ~origin ~angle:ray in
      (* Perpendicular distance: removes fisheye. *)
      { h with distance = h.distance *. cos (ray -. facing) })
