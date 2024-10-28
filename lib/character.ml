open Deck
open Card

type t = {
  hp : int;
  energy : int;
  status : string;
}

let camel1 : t = { hp = 80; energy = 3; status = "None" }

let camel1A =
  [
    Card.basicA;
    Card.basicA;
    Card.basicA;
    Card.basicA;
    Card.basicD;
    Card.basicD;
    Card.basicD;
    Card.basicD;
    Card.basicAD;
  ]

let camel1_deck = Deck.empty |> List.fold_right Deck.push camel1A
