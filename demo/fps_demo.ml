open Raylib

let width = 640
let height = 480

type mode = Strips | Upload

type stats = {
  mutable frames : int;
  mutable seconds : float;
  mutable fill : float;
  mutable upload : float;
}

let new_stats () = { frames = 0; seconds = 0.; fill = 0.; upload = 0. }

let keys =
  [
    (Key.W, "W");
    (Key.A, "A");
    (Key.S, "S");
    (Key.D, "D");
    (Key.Left, "Left");
    (Key.Right, "Right");
    (Key.Up, "Up");
    (Key.Down, "Down");
  ]

(** [held ()] is the names of the keys in [keys] that are down this frame. *)
let held () =
  List.filter_map
    (fun (k, name) -> if is_key_down k then Some name else None)
    keys

(** [draw_strips phase] draws a ceiling, a floor, and one 1-pixel-wide wall
    strip per column, the same draw calls the renderer makes. Strip heights and
    shades depend on [phase]. *)
let draw_strips phase =
  draw_rectangle 0 0 width (height / 2) (Color.create 70 70 80 255);
  draw_rectangle 0 (height / 2) width
    (height - (height / 2))
    (Color.create 100 85 70 255);
  for x = 0 to width - 1 do
    let t = (float_of_int x *. 0.02) +. phase in
    let h = int_of_float (240. +. (200. *. sin t)) in
    let shade = 100 + int_of_float (80. *. cos t) in
    draw_rectangle x
      ((height - h) / 2)
      1 h
      (Color.create shade shade (shade + 10) 255)
  done

let pixels = Bigarray.(Array1.create int32 c_layout (width * height))

(** [fill_pixels phase] writes every pixel of [pixels], as a textured renderer
    would each frame. Each pixel is RGBA bytes read as a little-endian int32. *)
let fill_pixels phase =
  let p = int_of_float (phase *. 60.) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let v = (x + y + p) land 255 in
      Bigarray.Array1.unsafe_set pixels
        ((y * width) + x)
        (Int32.logor 0xFF000000l
           (Int32.of_int ((v lsl 16) lor (v lsl 8) lor v)))
    done
  done

(** [timed f] runs [f ()] and is how many seconds it took. *)
let timed f =
  let start = get_time () in
  f ();
  get_time () -. start

(** [report name s] prints the averages in [s] for the mode called [name], if
    that mode ran. *)
let report name s =
  if s.frames > 0 then (
    let n = float_of_int s.frames in
    Printf.printf "%-7s %6d frames  %7.1f fps" name s.frames (n /. s.seconds);
    if s.fill > 0. then
      Printf.printf "  fill %.2f ms  upload %.2f ms"
        (1000. *. s.fill /. n)
        (1000. *. s.upload /. n);
    print_newline ())

let () =
  (* [--bench SECS] runs each mode for SECS seconds and exits, so every
     machine measures the same thing. Otherwise Tab switches modes and Esc
     quits. *)
  let bench =
    match Sys.argv with
    | [| _; "--bench"; secs |] -> Some (float_of_string secs)
    | _ -> None
  in
  (* No vsync hint and no target FPS, so nothing caps the frame rate. *)
  init_window width height "Shiver Me Functors: FPS test";
  set_target_fps 0;
  let texture =
    load_texture_from_image (gen_image_color width height Color.black)
  in
  let pixel_ptr = Ctypes.(to_voidp (bigarray_start array1 pixels)) in
  let strips = new_stats () and upload = new_stats () in
  let rec loop mode phase elapsed =
    let switch =
      match bench with
      | Some secs -> elapsed >= secs
      | None -> is_key_pressed Key.Tab
    in
    let finished =
      window_should_close () || (switch && mode = Upload && bench <> None)
    in
    if not finished then (
      let mode, elapsed =
        if switch then ((if mode = Strips then Upload else Strips), 0.)
        else (mode, elapsed)
      in
      let dt = get_frame_time () in
      let keys = held () in
      let phase = if keys = [] && bench = None then phase else phase +. dt in
      let s = if mode = Strips then strips else upload in
      begin_drawing ();
      (match mode with
      | Strips -> draw_strips phase
      | Upload ->
          s.fill <- s.fill +. timed (fun () -> fill_pixels phase);
          s.upload <-
            s.upload +. timed (fun () -> update_texture texture pixel_ptr);
          draw_texture texture 0 0 Color.white);
      draw_rectangle 0 0 width 70 (Color.create 0 0 0 180);
      draw_text
        (Printf.sprintf "%s  %d fps  (Tab: switch mode)"
           (if mode = Strips then "640 strips" else "pixel upload")
           (get_fps ()))
        10 10 20 Color.white;
      draw_text ("Held: " ^ String.concat " " keys) 10 40 20 Color.yellow;
      end_drawing ();
      (* The first frame of a mode includes setup time, so skip it. *)
      if elapsed > 0. then (
        s.frames <- s.frames + 1;
        s.seconds <- s.seconds +. dt);
      loop mode phase (elapsed +. dt))
  in
  loop Strips 0. 0.;
  unload_texture texture;
  close_window ();
  report "strips" strips;
  report "upload" upload
