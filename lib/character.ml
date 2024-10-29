open Deck
open Card

type t = {
  hp : int;
  energy : int;
  status : string;
}

let create_camel h e s : t = { hp = h; energy = e; status = s }
