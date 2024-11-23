open Deck
open Card

type t = {
  hp : int;
  energy : int;
  status : string;
}

let create_camel h e s : t = { hp = h; energy = e; status = s }
let get_hp t = t.hp
let get_energy t = t.energy
let get_status t = t.status
