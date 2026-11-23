(** Draws a game state using [Window]. *)

val draw : Smf_core.Game.t -> unit
(** Draws ceiling, floor, and walls. Only call inside [Window.draw_frame]. *)
