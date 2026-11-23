type color = { r : int; g : int; b : int }

let width = 640
let height = 480

let init ~title =
  Raylib.init_window width height title;
  (* The FPS test ran well over 60 fps, so capping here keeps the game from
     using a whole CPU core for frames nobody sees. *)
  Raylib.set_target_fps 60

let close () = Raylib.close_window ()
let should_close () = Raylib.window_should_close ()

let poll_input () =
  let down = Raylib.is_key_down in
  {
    Smf_core.Input.forward = down Raylib.Key.W;
    backward = down Raylib.Key.S;
    strafe_left = down Raylib.Key.A;
    strafe_right = down Raylib.Key.D;
    turn_left = down Raylib.Key.Left;
    turn_right = down Raylib.Key.Right;
  }

let frame_time () = Raylib.get_frame_time ()

let draw_frame f =
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.black;
  f ();
  Raylib.end_drawing ()

let fill_rect ~x ~y ~w ~h c =
  Raylib.draw_rectangle x y w h (Raylib.Color.create c.r c.g c.b 255)
