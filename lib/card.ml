type t = {
  cost : int;
  dmg : int;
  defend : int;
  effect : string;
}

let basicA = { cost = 1; dmg = 6; defend = 0; effect = "None" }
let basicD = { cost = 1; dmg = 0; defend = 6; effect = "None" }
let basicAD = { cost = 2; dmg = 10; defend = 5; effect = "None" }
let get_cost t = t.cost
let get_dmg t = t.dmg
let get_defend t = t.defend
let get_effect t = t.effect
