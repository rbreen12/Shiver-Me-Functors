open OUnit2

let tests =
  "smf"
  >::: [
         ( "idle input" >:: fun _ ->
           assert_bool "idle" (not Smf_core.Input.none.forward) );
       ]

let () = run_test_tt_main tests
