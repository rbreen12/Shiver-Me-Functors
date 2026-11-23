type t = {
  forward : bool;
  backward : bool;
  strafe_left : bool;
  strafe_right : bool;
  turn_left : bool;
  turn_right : bool;
}

let none =
  {
    forward = false;
    backward = false;
    strafe_left = false;
    strafe_right = false;
    turn_left = false;
    turn_right = false;
  }
