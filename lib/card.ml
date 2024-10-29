type t = {
  name : string;
  cost : int;
  dmg : int;
  defend : int;
  effect : string;
}

let print_card card =
  print_endline
    (card.name ^ " | cost: " ^ string_of_int card.cost ^ " | dmg : "
   ^ string_of_int card.dmg ^ " | defend : " ^ string_of_int card.defend
   ^ " | effect : " ^ card.effect)

let basicA = { name = "Attack"; cost = 1; dmg = 6; defend = 0; effect = "None" }
let basicD = { name = "Defend"; cost = 1; dmg = 0; defend = 6; effect = "None" }

let basicAD =
  { name = "Attack & Defend"; cost = 2; dmg = 10; defend = 5; effect = "None" }

let get_cost t = t.cost
let get_dmg t = t.dmg
let get_defend t = t.defend
let get_effect t = t.effect
