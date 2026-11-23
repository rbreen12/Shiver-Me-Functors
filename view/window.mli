(** The only module that touches the graphics library. *)

type color = { r : int; g : int; b : int }

val width : int
val height : int
val init : title:string -> unit
val close : unit -> unit
val should_close : unit -> bool
val poll_input : unit -> Smf_core.Input.t

val frame_time : unit -> float
(** Seconds taken by the previous frame. *)

val draw_frame : (unit -> unit) -> unit
(** [draw_frame f] clears the screen, runs the drawing in [f], and shows it. *)

val fill_rect : x:int -> y:int -> w:int -> h:int -> color -> unit
(** (x, y) is the top left. Only call inside [draw_frame]. *)
