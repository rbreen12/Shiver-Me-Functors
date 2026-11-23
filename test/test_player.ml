open OUnit2
open Smf_core

(* The tests avoid depending on Player's internal speeds and hitbox size, since
   those are tuning values that may change. They check directions, ratios, and
   that the player never enters a wall. *)

(** A 7 x 6 room with a one-cell pillar at row 3, column 3. Floor cells have x
    in (1, 6) and y in (1, 5). *)
let room =
  match
    Level.parse "#######\n#>....#\n#.....#\n#..#..#\n#.....#\n#######\n"
  with
  | Ok l -> l
  | Error e -> failwith (Level.error_to_string e)

let pi = Float.pi
let near ?(eps = 1e-9) a b = Float.abs (a -. b) < eps
let float_eq = assert_equal ~printer:string_of_float ~cmp:(near ~eps:1e-6)

let input ?(forward = false) ?(backward = false) ?(strafe_left = false)
    ?(strafe_right = false) ?(turn_left = false) ?(turn_right = false) () =
  { Input.forward; backward; strafe_left; strafe_right; turn_left; turn_right }

let at x y angle = Player.make ~pos:{ x; y } ~angle

(** [moved p q] is the distance between the positions of [p] and [q]. *)
let moved p q =
  let a = Player.pos p and b = Player.pos q in
  Float.hypot (b.x -. a.x) (b.y -. a.y)

let pos_printer p =
  let v = Player.pos p in
  Printf.sprintf "(%g, %g) angle %g" v.x v.y (Player.angle p)

(** [assert_not_in_wall p] fails if [p]'s center is inside a wall or outside the
    room's floor area. *)
let assert_not_in_wall p =
  let v = Player.pos p in
  assert_bool
    ("inside a wall at " ^ pos_printer p)
    (not (Level.is_solid room v));
  assert_bool
    ("left the room at " ^ pos_printer p)
    (v.x > 1. && v.x < 6. && v.y > 1. && v.y < 5.)

let construction_tests =
  [
    ( "spawn uses level position and angle" >:: fun _ ->
      let p = Player.spawn room in
      float_eq 1.5 (Player.pos p).x;
      float_eq 1.5 (Player.pos p).y;
      float_eq 0. (Player.angle p) );
    ( "make keeps an angle in range" >:: fun _ ->
      float_eq 1. (Player.angle (at 2.5 2.5 1.)) );
    ( "make wraps a negative angle" >:: fun _ ->
      float_eq (3. *. pi /. 2.) (Player.angle (at 2.5 2.5 (-.pi /. 2.))) );
    ( "make wraps 2pi to 0" >:: fun _ ->
      float_eq 0. (Player.angle (at 2.5 2.5 (2. *. pi))) );
    ( "make wraps a large angle" >:: fun _ ->
      float_eq pi (Player.angle (at 2.5 2.5 (5. *. pi))) );
  ]

let still_tests =
  let p = at 2.5 2.5 1. in
  let same name q =
    name >:: fun _ ->
    assert_equal p q ~printer:pos_printer ~cmp:(fun a b ->
        moved a b = 0. && Player.angle a = Player.angle b)
  in
  [
    same "no input" (Player.step room Input.none ~dt:0.1 p);
    same "zero dt"
      (Player.step room (input ~forward:true ~turn_left:true ()) ~dt:0. p);
    same "forward and backward cancel"
      (Player.step room (input ~forward:true ~backward:true ()) ~dt:0.1 p);
    same "strafes cancel"
      (Player.step room
         (input ~strafe_left:true ~strafe_right:true ())
         ~dt:0.1 p);
    same "turns cancel"
      (Player.step room (input ~turn_left:true ~turn_right:true ()) ~dt:0.1 p);
  ]

let turn_tests =
  [
    ( "turn right increases the angle" >:: fun _ ->
      let a =
        Player.angle
          (Player.step room (input ~turn_right:true ()) ~dt:0.1 (at 2.5 2.5 1.))
      in
      assert_bool "increased" (a > 1. && a < 1. +. pi) );
    ( "turn left decreases the angle" >:: fun _ ->
      let a =
        Player.angle
          (Player.step room (input ~turn_left:true ()) ~dt:0.1 (at 2.5 2.5 1.))
      in
      assert_bool "decreased" (a < 1. && a > 0.) );
    ( "turn left from 0 wraps below 2pi" >:: fun _ ->
      let a =
        Player.angle
          (Player.step room (input ~turn_left:true ()) ~dt:0.1 (at 2.5 2.5 0.))
      in
      assert_bool "wrapped" (a > pi && a < 2. *. pi) );
    ( "turning does not move" >:: fun _ ->
      let p = at 2.5 2.5 1. in
      float_eq 0.
        (moved p (Player.step room (input ~turn_right:true ()) ~dt:0.1 p)) );
    ( "angle stays in range over many turns" >:: fun _ ->
      let rec go p n =
        if n > 0 then (
          let a = Player.angle p in
          assert_bool (Printf.sprintf "angle %g" a) (a >= 0. && a < 2. *. pi);
          go (Player.step room (input ~turn_left:true ()) ~dt:0.05 p) (n - 1))
      in
      go (at 2.5 2.5 0.) 1000 );
  ]

(** [direction name keys angle ~dx ~dy] is a test that holding [keys] while
    facing [angle] from an open spot moves the player in direction ([dx], [dy])
    (a unit vector). *)
let direction name keys angle ~dx ~dy =
  name >:: fun _ ->
  let p = at 2.5 1.5 angle in
  let q = Player.step room keys ~dt:0.05 p in
  let d = moved p q in
  assert_bool "moved" (d > 0.);
  let a = Player.pos p and b = Player.pos q in
  float_eq dx ((b.x -. a.x) /. d);
  float_eq dy ((b.y -. a.y) /. d)

let move_tests =
  [
    direction "forward facing right" (input ~forward:true ()) 0. ~dx:1. ~dy:0.;
    direction "forward facing down" (input ~forward:true ()) (pi /. 2.) ~dx:0.
      ~dy:1.;
    direction "backward facing right" (input ~backward:true ()) 0. ~dx:(-1.)
      ~dy:0.;
    direction "strafe right facing right goes down"
      (input ~strafe_right:true ())
      0. ~dx:0. ~dy:1.;
    direction "strafe left facing right goes up"
      (input ~strafe_left:true ())
      0. ~dx:0. ~dy:(-1.);
    direction "strafe left facing down goes right"
      (input ~strafe_left:true ())
      (pi /. 2.) ~dx:1. ~dy:0.;
    direction "forward and strafe right go diagonally"
      (input ~forward:true ~strafe_right:true ())
      0.
      ~dx:(1. /. Float.sqrt 2.)
      ~dy:(1. /. Float.sqrt 2.);
    ( "distance is proportional to dt" >:: fun _ ->
      let p = at 1.5 2.5 0. in
      let d1 = moved p (Player.step room (input ~forward:true ()) ~dt:0.05 p) in
      let d2 = moved p (Player.step room (input ~forward:true ()) ~dt:0.1 p) in
      float_eq (2. *. d1) d2 );
    ( "diagonal is no faster than straight" >:: fun _ ->
      let p = at 2.5 1.5 0. in
      let straight =
        moved p (Player.step room (input ~forward:true ()) ~dt:0.05 p)
      in
      let diag =
        moved p
          (Player.step room
             (input ~forward:true ~strafe_right:true ())
             ~dt:0.05 p)
      in
      float_eq straight diag );
  ]

let collision_tests =
  [
    ( "stops at a wall" >:: fun _ ->
      let q =
        Player.step room (input ~forward:true ()) ~dt:1. (at 4.5 2.5 0.)
      in
      let v = Player.pos q in
      assert_bool ("moved toward wall: " ^ pos_printer q) (v.x > 5.);
      assert_bool ("stopped before wall: " ^ pos_printer q) (v.x < 6.);
      float_eq 2.5 v.y );
    ( "slides along a wall" >:: fun _ ->
      (* Facing down-right into the right wall: x is blocked, y keeps going. *)
      let p = at 5.5 1.5 (pi /. 4.) in
      let q = Player.step room (input ~forward:true ()) ~dt:0.3 p in
      let v = Player.pos q in
      assert_bool ("x blocked: " ^ pos_printer q) (v.x < 6.);
      assert_bool ("y slid: " ^ pos_printer q) (v.y > 1.5 +. 0.5) );
    ( "large dt does not pass through the pillar" >:: fun _ ->
      let q =
        Player.step room (input ~forward:true ()) ~dt:100. (at 1.5 3.5 0.)
      in
      assert_bool
        ("stopped before pillar: " ^ pos_printer q)
        ((Player.pos q).x < 3.);
      assert_bool
        ("reached the pillar: " ^ pos_printer q)
        ((Player.pos q).x > 2.5) );
    ( "large dt does not leave the room" >:: fun _ ->
      assert_not_in_wall
        (Player.step room (input ~forward:true ()) ~dt:100. (at 2.5 2.5 1.)) );
    ( "pushing into a corner" >:: fun _ ->
      let rec go p n =
        if n > 0 then (
          assert_not_in_wall p;
          go (Player.step room (input ~forward:true ()) ~dt:0.05 p) (n - 1))
      in
      go (at 4.5 4.5 (pi /. 4.)) 100 );
    ( "never ends inside a wall" >:: fun _ ->
      (* Walk in many directions, at several frame lengths, with strafing and
         turning mixed in, from several starting spots. *)
      let starts =
        [ (1.5, 1.5); (2.5, 3.5); (4.5, 2.5); (5.5, 4.5); (3.5, 2.5) ]
      in
      let keys =
        [
          input ~forward:true ();
          input ~forward:true ~strafe_left:true ~turn_right:true ();
          input ~backward:true ~strafe_right:true ();
        ]
      in
      List.iter
        (fun (x, y) ->
          for i = 0 to 15 do
            let angle = float_of_int i *. pi /. 8. in
            List.iter
              (fun dt ->
                List.iter
                  (fun k ->
                    let rec go p n =
                      if n > 0 then (
                        assert_not_in_wall p;
                        go (Player.step room k ~dt p) (n - 1))
                    in
                    go (at x y angle) 40)
                  keys)
              [ 0.016; 0.05; 0.3; 2. ]
          done)
        starts );
  ]

let () =
  run_test_tt_main
    ("player"
    >::: construction_tests @ still_tests @ turn_tests @ move_tests
         @ collision_tests)
