type t = { level : Level.t; player : Player.t }

let init level = { level; player = Player.spawn level }
let level g = g.level
let player g = g.player
let step _ ~dt:_ _ = failwith "TODO"
