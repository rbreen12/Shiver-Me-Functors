type wall = Stone | Wood
type tile = Floor | Wall of wall

type t = {
  tiles : tile array array;
      (** [tiles.(row).(col)]. Never mutated after [parse] builds it. *)
  spawn_row : int;
  spawn_col : int;
  spawn_angle : float;
}
(** AF: [tiles] is the grid read top to bottom, left to right. The player starts
    in the center of cell ([spawn_row], [spawn_col]) facing [spawn_angle]. RI:
    [tiles] is nonempty and rectangular, every border cell is a [Wall], the
    spawn cell is [Floor] and in bounds, and [spawn_angle] is in [\[0, 2pi)]. *)

type error =
  | Cannot_read of string
  | Empty
  | Not_rectangular of { row : int; expected : int; actual : int }
  | Unknown_char of { row : int; col : int; char : char }
  | No_spawn
  | Extra_spawn of { row : int; col : int }
  | Not_enclosed of { row : int; col : int }

(** What one character of a level file means. *)
type cell = Tile of tile | Spawn of float

(** [cell_of_char c] is what [c] means in a level file, or [None] if [c] is not
    part of the format. Spawn angles follow {!Player.angle}: 0 faces right and
    angles increase clockwise on screen, since y grows down. *)
let cell_of_char = function
  | '#' -> Some (Tile (Wall Stone))
  | '%' -> Some (Tile (Wall Wood))
  | '.' -> Some (Tile Floor)
  | '>' -> Some (Spawn 0.)
  | 'v' -> Some (Spawn (Float.pi /. 2.))
  | '<' -> Some (Spawn Float.pi)
  | '^' -> Some (Spawn (3. *. Float.pi /. 2.))
  | _ -> None

(** [lines text] is the rows of [text]. It accepts LF or CRLF line endings, and
    drops trailing blank lines so a final newline is optional. *)
let lines text =
  let strip_cr s =
    let n = String.length s in
    if n > 0 && s.[n - 1] = '\r' then String.sub s 0 (n - 1) else s
  in
  String.split_on_char '\n' text
  |> List.map strip_cr |> List.rev
  |> List.drop_while (fun s -> s = "")
  |> List.rev

(** [check_rectangular rows] is [Error] for the first row whose length differs
    from row 0's. Requires: [rows] is nonempty. *)
let check_rectangular rows =
  let expected = String.length rows.(0) in
  let rec go row =
    if row >= Array.length rows then Ok ()
    else
      let actual = String.length rows.(row) in
      if actual <> expected then
        Error (Not_rectangular { row; expected; actual })
      else go (row + 1)
  in
  go 0

(** [find_cells rows] decodes every character, or is [Error] for the first
    unknown one in reading order. *)
let find_cells rows =
  let exception Unknown of int * int * char in
  try
    Ok
      (Array.mapi
         (fun row line ->
           Array.init (String.length line) (fun col ->
               let c = line.[col] in
               match cell_of_char c with
               | Some cell -> cell
               | None -> raise (Unknown (row, col, c))))
         rows)
  with Unknown (row, col, char) -> Error (Unknown_char { row; col; char })

(** [find_spawn cells] is the position and angle of the only spawn, or [Error]
    if there is none or more than one (reporting the second in reading order).
*)
let find_spawn cells =
  let spawns = ref [] in
  Array.iteri
    (fun row line ->
      Array.iteri
        (fun col -> function
          | Spawn a -> spawns := (row, col, a) :: !spawns | Tile _ -> ())
        line)
    cells;
  match List.rev !spawns with
  | [] -> Error No_spawn
  | [ s ] -> Ok s
  | _ :: (row, col, _) :: _ -> Error (Extra_spawn { row; col })

(** [to_tiles cells] is [cells] with the spawn replaced by [Floor]. *)
let to_tiles cells =
  Array.map (Array.map (function Tile t -> t | Spawn _ -> Floor)) cells

(** [check_enclosed tiles] is [Error] for the first border cell, in reading
    order, that is not a wall. *)
let check_enclosed tiles =
  let height = Array.length tiles in
  let width = Array.length tiles.(0) in
  let on_border row col =
    row = 0 || row = height - 1 || col = 0 || col = width - 1
  in
  let exception Open of int * int in
  try
    Array.iteri
      (fun row line ->
        Array.iteri
          (fun col tile ->
            match tile with
            | Floor when on_border row col -> raise (Open (row, col))
            | _ -> ())
          line)
      tiles;
    Ok ()
  with Open (row, col) -> Error (Not_enclosed { row; col })

(* Checks run in this order and stop at the first problem: empty, not
   rectangular, unknown character, spawn count, not enclosed. Each check can
   then rely on the ones before it, e.g. [check_enclosed] can assume a
   nonempty rectangle. *)
let parse text =
  let ( let* ) = Result.bind in
  let rows = Array.of_list (lines text) in
  let* () = if Array.length rows = 0 then Error Empty else Ok () in
  let* () = check_rectangular rows in
  (* A level of empty lines passed the rectangle check but has no cells. *)
  let* () = if String.length rows.(0) = 0 then Error Empty else Ok () in
  let* cells = find_cells rows in
  let* spawn_row, spawn_col, spawn_angle = find_spawn cells in
  let tiles = to_tiles cells in
  let* () = check_enclosed tiles in
  Ok { tiles; spawn_row; spawn_col; spawn_angle }

let load path =
  match In_channel.with_open_bin path In_channel.input_all with
  | text -> parse text
  | exception Sys_error msg -> Error (Cannot_read msg)

(* Messages use 1-based line and column numbers to match text editors, even
   though [error] stores 0-based ones. *)
let error_to_string = function
  | Cannot_read msg -> "Cannot read level file: " ^ msg
  | Empty -> "Level is empty"
  | Not_rectangular { row; expected; actual } ->
      Printf.sprintf
        "Line %d has %d characters, but line 1 has %d; every line must be the \
         same length"
        (row + 1) actual expected
  | Unknown_char { row; col; char } ->
      Printf.sprintf
        "Unknown character %C at line %d, column %d (expected one of # %% . > \
         < ^ v)"
        char (row + 1) (col + 1)
  | No_spawn -> "Level has no player spawn (one of > < ^ v)"
  | Extra_spawn { row; col } ->
      Printf.sprintf
        "Second player spawn at line %d, column %d; a level needs exactly one"
        (row + 1) (col + 1)
  | Not_enclosed { row; col } ->
      Printf.sprintf
        "Border cell at line %d, column %d is not a wall; the level must be \
         enclosed by walls"
        (row + 1) (col + 1)

let height lvl = Array.length lvl.tiles
let width lvl = Array.length lvl.tiles.(0)

let tile_at lvl ~row ~col =
  if row < 0 || row >= height lvl || col < 0 || col >= width lvl then Wall Stone
  else lvl.tiles.(row).(col)

let is_solid lvl (p : Vec.t) =
  let row = int_of_float (Float.floor p.y) in
  let col = int_of_float (Float.floor p.x) in
  match tile_at lvl ~row ~col with Wall _ -> true | Floor -> false

let spawn_pos lvl =
  {
    Vec.x = float_of_int lvl.spawn_col +. 0.5;
    y = float_of_int lvl.spawn_row +. 0.5;
  }

let spawn_angle lvl = lvl.spawn_angle
