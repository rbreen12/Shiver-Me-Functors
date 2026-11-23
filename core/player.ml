type t = { pos : Vec.t; angle : float }
(** AF: the player stands at [pos] facing [angle] radians. RI: [angle] is in
    [\[0, 2pi)]. *)

(** Movement speed, in cells per second. *)
let move_speed = 3.0

(** Turning speed, in radians per second. *)
let turn_speed = Float.pi

(** Half the side length of the player's square hitbox, in cells. Must be less
    than [0.5] so the hitbox overlaps at most a 2 x 2 block of cells, which is
    why checking its four corners is enough. *)
let radius = 0.2

(** Longest distance moved in one collision substep. At most [radius], so a
    substep can never carry the hitbox through a whole wall cell. *)
let max_substep = 0.1

(** Gap kept between the hitbox and a wall it is pushed against, so the floating
    point result never lands exactly on the wall's edge. *)
let epsilon = 1e-6

let two_pi = 2. *. Float.pi

(** [normalize a] is [a] wrapped into [\[0, 2pi)]. *)
let normalize a =
  let a = Float.rem a two_pi in
  let a = if a < 0. then a +. two_pi else a in
  (* Adding [two_pi] to a tiny negative number can round up to [two_pi]. *)
  if a >= two_pi then 0. else a

let make ~pos ~angle = { pos; angle = normalize angle }
let spawn lvl = make ~pos:(Level.spawn_pos lvl) ~angle:(Level.spawn_angle lvl)
let pos p = p.pos
let angle p = p.angle

(** [collides lvl c] is whether a hitbox centered at [c] overlaps a wall. *)
let collides lvl (c : Vec.t) =
  List.exists
    (fun (dx, dy) -> Level.is_solid lvl { x = c.x +. dx; y = c.y +. dy })
    [
      (-.radius, -.radius);
      (radius, -.radius);
      (-.radius, radius);
      (radius, radius);
    ]

(** [axis d] is [1.] when [d] is positive or zero and [-1.] when negative, so
    the clamp below knows which side of the wall the player approached from. *)
let axis d = if d < 0. then -1. else 1.

(** [clamp_to_wall c d] is the center coordinate that leaves the hitbox just
    short of the grid line it crossed while moving [d] to reach [c]. *)
let clamp_to_wall c d =
  if axis d > 0. then Float.floor (c +. radius) -. radius -. epsilon
  else Float.ceil (c -. radius) +. radius +. epsilon

(** [move_axis lvl p d ~set] moves [p] by [d] along one axis, where [set p v] is
    [p] with that axis's coordinate replaced by [v] and [get] reads it. If the
    move would overlap a wall, the player stops against the wall instead.
    Requires: [p] does not collide and [|d| <= max_substep]. *)
let move_axis lvl (p : Vec.t) d ~get ~set =
  if d = 0. then p
  else
    let target = set p (get p +. d) in
    if not (collides lvl target) then target
    else
      let clamped = set p (clamp_to_wall (get target) d) in
      (* Only accept the clamp if it moved in the direction of travel and is
         clear of walls; otherwise stay put. *)
      let progressed = (get clamped -. get p) *. axis d >= 0. in
      if progressed && not (collides lvl clamped) then clamped else p

(** [slide lvl p (dx, dy)] moves [p] by ([dx], [dy]) in substeps, resolving x
    and y separately so the player slides along walls instead of sticking. *)
let slide lvl (p : Vec.t) (dx, dy) =
  let dist = Float.sqrt ((dx *. dx) +. (dy *. dy)) in
  let steps = max 1 (int_of_float (Float.ceil (dist /. max_substep))) in
  let sx = dx /. float_of_int steps and sy = dy /. float_of_int steps in
  let get_x (v : Vec.t) = v.x and set_x (v : Vec.t) x = { v with x } in
  let get_y (v : Vec.t) = v.y and set_y (v : Vec.t) y = { v with y } in
  let rec go p n =
    if n = 0 then p
    else
      let p = move_axis lvl p sx ~get:get_x ~set:set_x in
      let p = move_axis lvl p sy ~get:get_y ~set:set_y in
      go p (n - 1)
  in
  go p steps

(** [sign a b] is [1.] if only [a] holds, [-1.] if only [b] holds, and [0.] if
    both or neither do, so opposite keys cancel. *)
let sign a b = (if a then 1. else 0.) -. if b then 1. else 0.

let step lvl (input : Input.t) ~dt p =
  if dt <= 0. then p
  else
    (* Turn first, so the move uses the new facing. Turning right increases
       the angle (clockwise on screen, since y grows down). *)
    let turn = sign input.turn_right input.turn_left in
    let angle = normalize (p.angle +. (turn *. turn_speed *. dt)) in
    let fwd = sign input.forward input.backward in
    let right = sign input.strafe_right input.strafe_left in
    (* Facing (cos a, sin a); the player's right is (-sin a, cos a). *)
    let mx = (fwd *. cos angle) -. (right *. sin angle) in
    let my = (fwd *. sin angle) +. (right *. cos angle) in
    let len = Float.sqrt ((mx *. mx) +. (my *. my)) in
    if len = 0. then { p with angle }
    else
      (* Normalize so moving diagonally is no faster than moving straight. *)
      let dist = move_speed *. dt in
      let delta = (mx /. len *. dist, my /. len *. dist) in
      { pos = slide lvl p.pos delta; angle }
