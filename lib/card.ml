type t = {
  name : string;
  cost : int;
  dmg : int;
  defend : int;
  effect : string;
}

let card_to_string card =
  card.name ^ " | cost: " ^ string_of_int card.cost ^ " | dmg : "
  ^ string_of_int card.dmg ^ " | defend : " ^ string_of_int card.defend
  ^ " | effect : " ^ card.effect

let print_card card = print_endline (card_to_string card)
let spit = { name = "spit"; cost = 1; dmg = 10; defend = 0; effect = "Weaken" }
let throw = { name = "throw"; cost = 1; dmg = 20; defend = 0; effect = "Bleed" }

let defend =
  { name = "defend"; cost = 1; dmg = 0; defend = 10; effect = "None" }

let stomp = { name = "stomp"; cost = 2; dmg = 10; defend = 0; effect = "Stun" }

let tackle =
  { name = "tackle"; cost = 1; dmg = 10; defend = 0; effect = "None" }

let get_name t = t.name
let get_cost t = t.cost
let get_dmg t = t.dmg
let get_defend t = t.defend
let get_effect t = t.effect
