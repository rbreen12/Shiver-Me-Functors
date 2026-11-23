open OUnit2
open Smf_core

(** [lvl text] is the level parsed from [text]. Fails the test if parsing fails.
*)
let lvl text =
  match Level.parse text with
  | Ok l -> l
  | Error e -> assert_failure ("unexpected error: " ^ Level.error_to_string e)

(** [parse_error name text expected] is a test named [name] that parsing [text]
    fails with exactly [expected]. *)
let parse_error name text expected =
  name >:: fun _ ->
  match Level.parse text with
  | Ok _ -> assert_failure "expected an error, but parsing succeeded"
  | Error e ->
      assert_equal expected e ~printer:Level.error_to_string ~msg:"wrong error"

let small = "#####\n#>..#\n#.%.#\n#####\n"

let tile_printer = function
  | Level.Floor -> "Floor"
  | Level.Wall Level.Stone -> "Wall Stone"
  | Level.Wall Level.Wood -> "Wall Wood"

let float_eq =
  assert_equal ~printer:string_of_float ~cmp:(fun a b ->
      Float.abs (a -. b) < 1e-9)

let tile_test name l ~row ~col expected =
  name >:: fun _ ->
  assert_equal expected (Level.tile_at l ~row ~col) ~printer:tile_printer

let small_lvl = lvl small

let accessor_tests =
  [
    ( "dimensions" >:: fun _ ->
      assert_equal 5 (Level.width small_lvl) ~printer:string_of_int;
      assert_equal 4 (Level.height small_lvl) ~printer:string_of_int );
    tile_test "stone corner" small_lvl ~row:0 ~col:0 (Wall Stone);
    tile_test "floor" small_lvl ~row:1 ~col:2 Floor;
    tile_test "wood" small_lvl ~row:2 ~col:2 (Wall Wood);
    tile_test "spawn cell is floor" small_lvl ~row:1 ~col:1 Floor;
    tile_test "out of bounds above" small_lvl ~row:(-1) ~col:2 (Wall Stone);
    tile_test "out of bounds left" small_lvl ~row:1 ~col:(-1) (Wall Stone);
    tile_test "out of bounds below" small_lvl ~row:4 ~col:1 (Wall Stone);
    tile_test "out of bounds right" small_lvl ~row:1 ~col:5 (Wall Stone);
    ( "spawn position is cell center" >:: fun _ ->
      let p = Level.spawn_pos small_lvl in
      float_eq 1.5 p.x;
      float_eq 1.5 p.y );
    ( "is_solid floor" >:: fun _ ->
      assert_bool "floor" (not (Level.is_solid small_lvl { x = 2.5; y = 1.5 }))
    );
    ( "is_solid wall" >:: fun _ ->
      assert_bool "wood" (Level.is_solid small_lvl { x = 2.5; y = 2.5 }) );
    ( "is_solid uses the cell containing the point" >:: fun _ ->
      (* x = 2.0 is the left edge of column 2 (wood), not column 1. *)
      assert_bool "edge" (Level.is_solid small_lvl { x = 2.0; y = 2.5 });
      assert_bool "just left"
        (not (Level.is_solid small_lvl { x = 1.99; y = 2.5 })) );
    ( "is_solid outside the level" >:: fun _ ->
      assert_bool "negative" (Level.is_solid small_lvl { x = -0.5; y = 1.5 });
      assert_bool "far" (Level.is_solid small_lvl { x = 100.; y = 100. }) );
  ]

let angle_test name spawn expected =
  name >:: fun _ ->
  float_eq expected (Level.spawn_angle (lvl ("###\n#" ^ spawn ^ "#\n###")))

let spawn_angle_tests =
  [
    angle_test "> faces right" ">" 0.;
    angle_test "v faces down" "v" (Float.pi /. 2.);
    angle_test "< faces left" "<" Float.pi;
    angle_test "^ faces up" "^" (3. *. Float.pi /. 2.);
  ]

let format_tests =
  [
    ( "no trailing newline" >:: fun _ ->
      assert_equal 4 (Level.height (lvl "#####\n#>..#\n#.%.#\n#####")) );
    ( "extra trailing blank lines" >:: fun _ ->
      assert_equal 4 (Level.height (lvl (small ^ "\n\n"))) );
    ( "CRLF line endings" >:: fun _ ->
      let l = lvl "###\r\n#>#\r\n###\r\n" in
      assert_equal 3 (Level.width l) ~printer:string_of_int );
    ( "level1.txt loads" >:: fun _ ->
      match Level.load "../levels/level1.txt" with
      | Ok l ->
          assert_equal 10 (Level.width l) ~printer:string_of_int;
          assert_equal 6 (Level.height l) ~printer:string_of_int
      | Error e -> assert_failure (Level.error_to_string e) );
  ]

let error_tests =
  [
    parse_error "empty string" "" Empty;
    parse_error "only newlines" "\n\n\n" Empty;
    parse_error "short row" "#####\n#>.#\n#####"
      (Not_rectangular { row = 1; expected = 5; actual = 4 });
    parse_error "long row" "###\n#>##\n###"
      (Not_rectangular { row = 1; expected = 3; actual = 4 });
    parse_error "blank line in middle" "###\n\n#>#\n###"
      (Not_rectangular { row = 1; expected = 3; actual = 0 });
    parse_error "unknown char" "####\n#>x#\n####"
      (Unknown_char { row = 1; col = 2; char = 'x' });
    parse_error "space is unknown" "####\n#> #\n####"
      (Unknown_char { row = 1; col = 2; char = ' ' });
    parse_error "first unknown char reported" "#?##\n#>x#\n####"
      (Unknown_char { row = 0; col = 1; char = '?' });
    parse_error "no spawn" "###\n#.#\n###" No_spawn;
    parse_error "two spawns" "####\n#><#\n####"
      (Extra_spawn { row = 1; col = 2 });
    parse_error "hole in top wall" "#.#\n#>#\n###"
      (Not_enclosed { row = 0; col = 1 });
    parse_error "hole in left wall" "###\n.>#\n###"
      (Not_enclosed { row = 1; col = 0 });
    parse_error "spawn on border" "#>#\n#.#\n###"
      (Not_enclosed { row = 0; col = 1 });
    parse_error "rectangle checked before chars" "###\n#x##\n###"
      (Not_rectangular { row = 1; expected = 3; actual = 4 });
    parse_error "chars checked before spawn" "###\n#x#\n###"
      (Unknown_char { row = 1; col = 1; char = 'x' });
    parse_error "spawn checked before enclosure" "#.#\n#.#\n###" No_spawn;
    ( "missing file" >:: fun _ ->
      match Level.load "no_such_level.txt" with
      | Error (Cannot_read _) -> ()
      | Error e -> assert_failure ("wrong error: " ^ Level.error_to_string e)
      | Ok _ -> assert_failure "expected Cannot_read" );
  ]

let message_tests =
  [
    ( "messages are 1-based" >:: fun _ ->
      assert_equal
        "Unknown character 'x' at line 2, column 3 (expected one of # % . > < \
         ^ v)"
        (Level.error_to_string (Unknown_char { row = 1; col = 2; char = 'x' }))
        ~printer:Fun.id );
    ( "every error has a message" >:: fun _ ->
      List.iter
        (fun e -> assert_bool "nonempty" (Level.error_to_string e <> ""))
        [
          Level.Cannot_read "x";
          Empty;
          Not_rectangular { row = 0; expected = 1; actual = 2 };
          Unknown_char { row = 0; col = 0; char = 'x' };
          No_spawn;
          Extra_spawn { row = 0; col = 0 };
          Not_enclosed { row = 0; col = 0 };
        ] );
  ]

let () =
  run_test_tt_main
    ("level"
    >::: accessor_tests @ spawn_angle_tests @ format_tests @ error_tests
         @ message_tests)
