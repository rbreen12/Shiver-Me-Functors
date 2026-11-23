(** The whole game state and the pure step function. *)

type t

(** Enemies can be added later **)
val init : Level.t -> t
val level : t -> Level.t
val player : t -> Player.t

val step : Input.t -> dt:float -> t -> t
(** [step input ~dt g] is the state [dt] seconds after [g]. *)
