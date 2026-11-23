open OUnit2
open Smf_core
open Smf_view

(* ---------- Helpers ---------- *)

(** [close a b] is whether floats [a] and [b] are within [1e-9], since exact
    float equality is fragile. *)
let close a b = Float.abs (a -. b) < 1e-9

let str_f = string_of_float
let str_i = string_of_int

(* ---------- Raycaster ---------- *)

(** [level_of rows] parses [rows] as a level file. Fails the test if the level
    is malformed. *)
let level_of rows =
  match Level.parse (String.concat "\n" rows) with
  | Ok l -> l
  | Error e -> failwith (Level.error_to_string e)

(** [room ()] is a small test level. A function, not a value, so that if
    [Level.parse] is broken only the raycaster tests fail, not the whole test
    executable.

    {v
      #####   row 0
      #>..#   row 1
      #...#   row 2
      #..%#   row 3   (% is wood at col 3)
      #####   row 4
    v} *)
let room () = level_of [ "#####"; "#>..#"; "#...#"; "#..%#"; "#####" ]

(** [cast_from x y angle] casts a ray in [room ()] from [(x, y)]. *)
let cast_from x y angle = Raycaster.cast (room ()) ~origin:{ Vec.x; y } ~angle

let side_str = function
  | Raycaster.Vertical -> "Vertical"
  | Raycaster.Horizontal -> "Horizontal"

let wall_str = function Level.Stone -> "Stone" | Level.Wood -> "Wood"

(** [check_hit name ~x ~y ~angle ~distance ~side] is a test that a ray from
    [(x, y)] at [angle] hits a wall [distance] away on a face of kind [side]. *)
let check_hit name ~x ~y ~angle ~distance ~side =
  name >:: fun _ ->
  let h = cast_from x y angle in
  assert_equal ~cmp:close ~printer:str_f distance h.distance;
  assert_equal ~printer:side_str side h.side

let pi = Float.pi

let raycaster_tests =
  "raycaster"
  >::: [
         (* Axis-aligned rays from the center of cell (1, 1). The east wall is
            the line x = 4, the south wall is y = 4, and the west and north
            walls are x = 1 and y = 1. *)
         check_hit "east hits x = 4" ~x:1.5 ~y:1.5 ~angle:0. ~distance:2.5
           ~side:Raycaster.Vertical;
         check_hit "west hits x = 1" ~x:1.5 ~y:1.5 ~angle:pi ~distance:0.5
           ~side:Raycaster.Vertical;
         check_hit "down (+y) hits y = 4" ~x:1.5 ~y:1.5 ~angle:(pi /. 2.)
           ~distance:2.5 ~side:Raycaster.Horizontal;
         check_hit "up (-y) hits y = 1" ~x:1.5 ~y:1.5
           ~angle:(3. *. pi /. 2.)
           ~distance:0.5 ~side:Raycaster.Horizontal;
         (* Diagonal at 30 degrees from (1.5, 1.2): reaches x = 4 after
            2.5 / cos 30 deg, before reaching any horizontal wall. *)
         check_hit "diagonal at 30 degrees" ~x:1.5 ~y:1.2 ~angle:(pi /. 6.)
           ~distance:(2.5 /. cos (pi /. 6.))
           ~side:Raycaster.Vertical;
         ( "offset is where the ray lands on the face" >:: fun _ ->
           (* Heading east from y = 1.2, the ray hits x = 4 at y = 1.2. *)
           let h = cast_from 1.5 1.2 0. in
           assert_equal ~cmp:close ~printer:str_f 0.2 h.offset );
         ( "offset on a horizontal face uses x" >:: fun _ ->
           (* Heading down from x = 2.3, the ray hits y = 4 at x = 2.3. *)
           let h = cast_from 2.3 1.5 (pi /. 2.) in
           assert_equal ~cmp:close ~printer:str_f 0.3 h.offset );
         ( "wall type is reported" >:: fun _ ->
           (* Heading east along row 3 hits the wood block at col 3. *)
           let h = cast_from 1.5 3.5 0. in
           assert_equal ~printer:wall_str Level.Wood h.wall;
           assert_equal ~cmp:close ~printer:str_f 1.5 h.distance );
         ( "flat wall: equal perpendicular distance in every column" >:: fun _ ->
           (* The player faces east at a flat wall 2.5 away (the line x = 4).
              Edge rays travel farther in a straight line, but after the
              fisheye correction every column should report 2.5. The room is
              tall enough that even the edge rays of a 60 degree view reach
              the east wall, not the top or bottom walls. *)
           let lvl =
             level_of
               [ "#####"; "#...#"; "#...#"; "#>..#"; "#...#"; "#...#"; "#####" ]
           in
           let player = Player.make ~pos:{ Vec.x = 1.5; y = 3.5 } ~angle:0. in
           let hits =
             Raycaster.cast_screen lvl player ~fov:(pi /. 3.) ~columns:64
           in
           assert_equal ~printer:str_i 64 (Array.length hits);
           Array.iter
             (fun (h : Raycaster.hit) ->
               assert_equal ~cmp:close ~printer:str_f 2.5 h.distance;
               assert_equal ~printer:side_str Raycaster.Vertical h.side)
             hits );
       ]

(* ---------- Renderer ---------- *)

(** [hit d] is a raycaster hit at distance [d], on a [Vertical] [Stone] face
    unless [wall] or [side] says otherwise. *)
let hit ?(wall = Level.Stone) ?(side = Raycaster.Vertical) distance =
  { Raycaster.distance; wall; side; offset = 0. }

(** [strip d] is the renderer's strip for [hit d] on a screen [screen_height]
    pixels tall, 480 by default. *)
let strip ?wall ?side ?(screen_height = 480) d =
  Renderer.strip (hit ?wall ?side d) ~screen_height

let height s = s.Renderer.height
let top s = s.Renderer.top
let red s = s.Renderer.color.Window.r

(** [on_screen s] is whether strip [s] lies within a 480-pixel screen. *)
let on_screen s = top s >= 0 && height s >= 0 && top s + height s <= 480

(** [check_strip name d ~height:h ~top:t] is a test that a wall at distance [d]
    gives a strip [h] pixels tall starting at row [t]. *)
let check_strip ?screen_height name d ~height:h ~top:t =
  name >:: fun _ ->
  let s = strip ?screen_height d in
  assert_equal ~printer:str_i h (height s);
  assert_equal ~printer:str_i t (top s)

let renderer_tests =
  "renderer"
  >::: [
         (* Height is 480 / distance, centered: top = (480 - height) / 2. *)
         check_strip "distance 1 fills the screen" 1. ~height:480 ~top:0;
         check_strip "distance 2 is half height" 2. ~height:240 ~top:120;
         check_strip "distance 4 is quarter height" 4. ~height:120 ~top:180;
         check_strip "distance 0.5 is clamped" 0.5 ~height:480 ~top:0;
         check_strip "distance 0 is clamped" 0. ~height:480 ~top:0;
         check_strip "odd screen height still centers" ~screen_height:481 2.
           ~height:241 ~top:120;
         ( "tiny distance stays on screen" >:: fun _ ->
           assert_bool "on screen" (on_screen (strip 1e-9)) );
         ( "very far wall stays on screen" >:: fun _ ->
           assert_bool "on screen" (on_screen (strip 1000.)) );
         ( "horizontal faces are darker than vertical" >:: fun _ ->
           assert_bool "darker"
             (red (strip ~side:Raycaster.Horizontal 3.)
             < red (strip ~side:Raycaster.Vertical 3.)) );
         ( "stone and wood: same height, different color" >:: fun _ ->
           let s = strip ~wall:Level.Stone 3.
           and w = strip ~wall:Level.Wood 3. in
           assert_equal ~printer:str_i (height s) (height w);
           assert_bool "different color" (s.Renderer.color <> w.Renderer.color)
         );
         ( "farther walls are darker" >:: fun _ ->
           assert_bool "darker" (red (strip 6.) < red (strip 3.)) );
       ]

let tests =
  "smf"
  >::: [
         ( "idle input" >:: fun _ ->
           assert_bool "idle" (not Smf_core.Input.none.forward) );
         raycaster_tests;
         renderer_tests;
       ]

let () = run_test_tt_main tests
