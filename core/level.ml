type wall = Stone | Wood
type tile = Floor | Wall of wall
type t = unit

type error =
  | Cannot_read of string
  | Empty
  | Not_rectangular of { row : int; expected : int; actual : int }
  | Unknown_char of { row : int; col : int; char : char }
  | No_spawn
  | Extra_spawn of { row : int; col : int }
  | Not_enclosed of { row : int; col : int }

let parse _ = failwith "TODO"
let load _ = failwith "TODO"
let error_to_string _ = failwith "TODO"
let width _ = failwith "TODO"
let height _ = failwith "TODO"
let tile_at _ ~row:_ ~col:_ = failwith "TODO"
let is_solid _ _ = failwith "TODO"
let spawn_pos _ = failwith "TODO"
let spawn_angle _ = failwith "TODO"
