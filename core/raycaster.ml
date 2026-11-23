type side = Vertical | Horizontal
type hit = { distance : float; wall : Level.wall; side : side; offset : float }

let cast _ ~origin:_ ~angle:_ = failwith "TODO"
let cast_screen _ _ ~fov:_ ~columns:_ = failwith "TODO"
