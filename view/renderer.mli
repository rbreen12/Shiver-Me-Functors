(** Draws a game state using [Window]. *)

type strip = { top : int; height : int; color : Window.color }
(** One column's wall strip, covering screen rows [top] through
    [top + height - 1]. *)

val strip : Smf_core.Raycaster.hit -> screen_height:int -> strip
(** [strip hit ~screen_height] is the wall strip for a column that shows [hit].
    Its height is proportional to [1 /. hit.distance], clamped so the strip
    stays on screen, and it is centered vertically. Its color depends on the
    wall type, is darker for [Horizontal] sides than [Vertical] ones, and gets
    darker with distance. Requires: [hit.distance >= 0.] and
    [screen_height >= 0]. *)

val draw : Smf_core.Game.t -> unit
(** Draws ceiling, floor, and walls. Only call inside [Window.draw_frame]. *)
