(** A parsed, validated level. *)

type wall = Stone | Wood
type tile = Floor | Wall of wall

type t
(** Invariant: rectangular, nonempty, every border cell is a wall, and contains
    exactly one spawn. *)

(** Rows and columns are 0-indexed from the top left. *)
type error =
  | Cannot_read of string
  | Empty
  | Not_rectangular of { row : int; expected : int; actual : int }
  | Unknown_char of { row : int; col : int; char : char }
  | No_spawn
  | Extra_spawn of { row : int; col : int }
  | Not_enclosed of { row : int; col : int }

val parse : string -> (t, error) result
(** [parse text] parses the contents of a level file. *)

val load : string -> (t, error) result
(** [load path] reads and parses the file at [path]. *)

val error_to_string : error -> string
val width : t -> int
val height : t -> int

val tile_at : t -> row:int -> col:int -> tile
(** Total: out of bounds is [Wall Stone], so callers never bounds check. *)

val is_solid : t -> Vec.t -> bool
(** [is_solid lvl p] is whether the cell containing [p] is a wall. *)

val spawn_pos : t -> Vec.t
(** Center of the spawn cell. *)

val spawn_angle : t -> float
(** Radians, using the convention in {!Player.angle}. *)
