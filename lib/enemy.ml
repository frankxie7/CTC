type m = {
  dmg : int;
  defend : int;
  effect : string;
}

type t = {
  hp : int;
  moves : m list;
}

let basicA = { dmg = 6; defend = 0; effect = "None" }
let basicD = { dmg = 0; defend = 10; effect = "None" }
let enemy1 = { hp = 20; moves = [ basicD; basicA ] }
