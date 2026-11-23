type t = { level : Level.t; player : Player.t }

let init level = { level; player = Player.spawn level }
let level g = g.level
let player g = g.player

(* The level never changes, so stepping the game is just stepping the player
   inside it. Enemies and items will be stepped here once they exist. *)
let step input ~dt g =
  { g with player = Player.step g.level input ~dt g.player }
