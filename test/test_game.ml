open OUnit2
open Smf_core

(** A 5 x 4 room with the player spawning at (1.5, 1.5) facing right. *)
let room =
  match Level.parse "#####\n#>..#\n#...#\n#####\n" with
  | Ok l -> l
  | Error e -> failwith (Level.error_to_string e)

let forward = { Input.none with forward = true }

let player_printer p =
  let v = Player.pos p in
  Printf.sprintf "(%g, %g) angle %g" v.x v.y (Player.angle p)

(** [assert_same_player expected actual] checks the two players have exactly the
    same position and angle. *)
let assert_same_player expected actual =
  assert_equal expected actual ~printer:player_printer ~cmp:(fun a b ->
      Player.pos a = Player.pos b && Player.angle a = Player.angle b)

let tests =
  [
    ( "init spawns the player from the level" >:: fun _ ->
      let g = Game.init room in
      assert_same_player (Player.spawn room) (Game.player g) );
    ( "init keeps the level" >:: fun _ ->
      assert_bool "same level" (Game.level (Game.init room) == room) );
    ( "step with no input leaves the player unchanged" >:: fun _ ->
      let g = Game.init room in
      assert_same_player (Game.player g)
        (Game.player (Game.step Input.none ~dt:0.1 g)) );
    ( "step advances the player like Player.step" >:: fun _ ->
      let g = Game.init room in
      assert_same_player
        (Player.step room forward ~dt:0.1 (Game.player g))
        (Game.player (Game.step forward ~dt:0.1 g)) );
    ( "step actually moves the player" >:: fun _ ->
      let g = Game.init room in
      let before = Player.pos (Game.player g) in
      let after = Player.pos (Game.player (Game.step forward ~dt:0.1 g)) in
      assert_bool "moved right" (after.x > before.x) );
    ( "step keeps the level" >:: fun _ ->
      let g = Game.init room in
      assert_bool "same level" (Game.level (Game.step forward ~dt:0.1 g) == room)
    );
  ]

let () = run_test_tt_main ("game" >::: tests)
