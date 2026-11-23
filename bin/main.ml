open Smf_core
open Smf_view

let rec loop game =
  if not (Window.should_close ()) then (
    let input = Window.poll_input () in
    let game = Game.step input ~dt:(min 0.05 (Window.frame_time ())) game in
    Window.draw_frame (fun () -> Renderer.draw game);
    loop game)

let () =
  match Level.load "levels/level1.txt" with
  | Error e ->
      prerr_endline (Level.error_to_string e);
      exit 1
  | Ok level ->
      Window.init ~title:"Shiver Me Functors";
      loop (Game.init level);
      Window.close ()
