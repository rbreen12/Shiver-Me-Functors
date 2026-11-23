(** The player's position, facing, and movement. *)

type t

val make : pos:Vec.t -> angle:float -> t
val spawn : Level.t -> t
val pos : t -> Vec.t

val angle : t -> float
(** Radians in [0, 2pi). 0 faces +x (right); increasing turns clockwise on
    screen, since y grows down. *)

val step : Level.t -> Input.t -> dt:float -> t -> t
(** [step lvl input ~dt p] is [p] after [dt] seconds of holding [input]. Slides
    along walls and never ends inside one. *)
