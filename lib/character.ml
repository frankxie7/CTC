open Deck
open Card

type t = {
  hp : int;
  energy : int;
  status : string;
}

let create_camel h e s : t = { hp = h; energy = e; status = s }

(* let camel1 : t = { hp = 80; energy = 3; status = "None" }

   let camel1A_deck = [ Card.basicA; Card.basicA; Card.basicA; Card.basicA;
   Card.basicD; Card.basicD; Card.basicD; Card.basicD; Card.basicAD; ]

   let camel1A_hand = [ Card.basicA; Card.basicA; Card.basicD; Card.basicD;
   Card.basicAD ]

   let camel1_deck = Deck.empty |> List.fold_right Deck.push camel1A_deck

   (* TODO - randomization function of Deck *) let camel1_hand = Deck.empty |>
   List.fold_right Deck.push camel1A_hand

   (**[draw_one] takes in a hand and deck and appends the first element of deck
   onto the top of the hand. It returns a tuple of the updated hand and updated
   deck*) let draw_one (hand : 'a Deck.t) (deck : 'a Deck.t) : 'a Deck.t * 'a
   Deck.t = let new_hand = Deck.push (Deck.peek deck) hand in (new_hand,
   Deck.pop deck)

   (**[play_card] takes in a hand and returns a tuple of the top card and the
   updated hand.*) let play_card (hand : 'a Deck.t) : 'a * 'a Deck.t = let
   top_card = Deck.peek hand in (top_card, Deck.pop hand) *)
