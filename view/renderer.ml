open Smf_core

type strip = { top : int; height : int; color : Window.color }

(** Field of view: 60 degrees. *)
let fov = Float.pi /. 3.

let ceiling_color = { Window.r = 70; g = 70; b = 80 }
let floor_color = { Window.r = 100; g = 85; b = 70 }

(** [base_color w] is the unshaded color of wall type [w]. *)
let base_color = function
  | Level.Stone -> { Window.r = 160; g = 160; b = 170 }
  | Level.Wood -> { Window.r = 170; g = 110; b = 50 }

(** [change_shade f c] is [c] with each channel multiplied by [f], rounded down.
    Requires: [0. <= f <= 1.]. *)
let change_shade f (c : Window.color) =
  let s x = int_of_float (float_of_int x *. f) in
  { Window.r = s c.r; g = s c.g; b = s c.b }

(** [darken_horizontal s] is the brightness factor for side [s]: [0.7] for
    [Horizontal] and [1.0] for [Vertical], so corners are visible. *)
let darken_horizontal = function
  | Raycaster.Vertical -> 1.0
  | Raycaster.Horizontal -> 0.7

(** [distance_factor d] is in [(0, 1\]]: [1.] at distance [0.], falling as [d]
    grows. Requires: [d >= 0.]. *)
let distance_factor d = 1. /. (1. +. (0.15 *. d))

let strip (hit : Raycaster.hit) ~screen_height =
  let screen = float_of_int screen_height in
  (* Clamp before converting, so a tiny distance can't overflow the int. *)
  let full = Float.min screen (screen /. hit.distance) in
  let height = int_of_float (Float.round full) in
  let top = (screen_height - height) / 2 in
  let shade = darken_horizontal hit.side *. distance_factor hit.distance in
  { top; height; color = change_shade shade (base_color hit.wall) }

let draw game =
  let w = Window.width and h = Window.height in
  Window.fill_rect ~x:0 ~y:0 ~w ~h:(h / 2) ceiling_color;
  Window.fill_rect ~x:0 ~y:(h / 2) ~w ~h:(h - (h / 2)) floor_color;
  let hits =
    Raycaster.cast_screen (Game.level game) (Game.player game) ~fov ~columns:w
  in
  Array.iteri
    (fun x hit ->
      let s = strip hit ~screen_height:h in
      Window.fill_rect ~x ~y:s.top ~w:1 ~h:s.height s.color)
    hits
