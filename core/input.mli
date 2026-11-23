(** What the player is holding this frame. Plain data, no graphics library. *)

type t = {
  forward : bool;
  backward : bool;
  strafe_left : bool;
  strafe_right : bool;
  turn_left : bool;
  turn_right : bool;
}

val none : t
(** [none] has nothing held. *)
