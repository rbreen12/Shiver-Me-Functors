(** Pure ray/wall intersection. Draws nothing. *)

type side =
  | Vertical  (** crossed a line of constant x *)
  | Horizontal  (** crossed a line of constant y *)

type hit = {
  distance : float;
  wall : Level.wall;
  side : side;
  offset : float;  (** position along the wall face, from 0 to 1; for textures *)
}

val cast : Level.t -> origin:Vec.t -> angle:float -> hit
(** [cast lvl ~origin ~angle] is the first wall hit, with [distance] measured
    straight from [origin]. Requires: [origin] is in a floor cell. *)

val cast_screen : Level.t -> Player.t -> fov:float -> columns:int -> hit array
(** One hit per screen column, left to right. Here [distance] is perpendicular
    to the view plane, so there is no fisheye. *)
