open OUnit2
open Game

let camel1A_deck =
  [
    Lib.Card.spit;
    Lib.Card.tackle;
    Lib.Card.tackle;
    Lib.Card.tackle;
    Lib.Card.defend;
    Lib.Card.defend;
    Lib.Card.stomp;
    Lib.Card.tackle;
    Lib.Card.throw;
    Lib.Card.throw;
  ]

let draw_one hand deck =
  let shuffled_deck = Lib.Deck.shuffle deck in
  if Lib.Deck.is_empty shuffled_deck then (
    print_endline "No more cards to draw!";
    (hand, shuffled_deck))
  else
    let new_hand = Lib.Deck.push (Lib.Deck.peek shuffled_deck) hand in
    (new_hand, Lib.Deck.pop shuffled_deck)

let test_get_name () =
  let card = Lib.Card.spit in
  assert_equal "spit" (Lib.Card.get_name card)

let test_get_cost () =
  let card = Lib.Card.stomp in
  assert_equal 2 (Lib.Card.get_cost card)

let test_get_dmg () =
  let card = Lib.Card.throw in
  assert_equal 20 (Lib.Card.get_dmg card)

let test_get_defend () =
  let card = Lib.Card.defend in
  assert_equal 10 (Lib.Card.get_defend card)

let test_get_effect () =
  let card = Lib.Card.tackle in
  assert_equal "None" (Lib.Card.get_effect card)

let test_print_card () =
  let expected_output =
    "spit | cost: 1 | dmg : 10 | defend : 0 | effect : Weaken"
  in
  assert_equal expected_output (Lib.Card.card_to_string Lib.Card.spit)

let card_tests =
  "card tests"
  >::: [
         ("test_get_name" >:: fun _ -> test_get_name ());
         ("test_get_cost" >:: fun _ -> test_get_cost ());
         ("test_get_dmg" >:: fun _ -> test_get_dmg ());
         ("test_get_defend" >:: fun _ -> test_get_defend ());
         ("test_get_effect" >:: fun _ -> test_get_effect ());
         ("test_print_card" >:: fun _ -> test_print_card ());
       ]

let test_empty_deck () = assert_equal true (Lib.Deck.is_empty Lib.Deck.empty)

let test_push_and_peek () =
  let card = Lib.Card.spit in
  let deck = Lib.Deck.push card Lib.Deck.empty in
  assert_equal card (Lib.Deck.peek deck)

let test_pop () =
  let card = Lib.Card.spit in
  let deck = Lib.Deck.push card Lib.Deck.empty in
  let deck_after_pop = Lib.Deck.pop deck in
  assert_equal [] (Lib.Deck.to_list deck_after_pop)

let test_size () =
  let deck =
    List.fold_left
      (fun acc card -> Lib.Deck.push card acc)
      Lib.Deck.empty
      [ Lib.Card.spit; Lib.Card.throw; Lib.Card.tackle ]
  in
  assert_equal 3 (Lib.Deck.size deck)

let test_draw () =
  let deck =
    List.fold_left
      (fun acc card -> Lib.Deck.push card acc)
      Lib.Deck.empty
      [ Lib.Card.spit; Lib.Card.throw; Lib.Card.tackle; Lib.Card.defend ]
  in
  let hand, deck_after_draw = Lib.Deck.draw 2 deck Lib.Deck.empty in
  assert_equal 2 (Lib.Deck.size hand);
  assert_equal 2 (Lib.Deck.size deck_after_draw)

let test_draw_empty_deck () =
  let hand, deck = Lib.Deck.draw 5 Lib.Deck.empty Lib.Deck.empty in
  assert_equal 0 (Lib.Deck.size hand);
  assert_equal 0 (Lib.Deck.size deck)

let test_shuffle () =
  let deck =
    List.fold_left
      (fun acc card -> Lib.Deck.push card acc)
      Lib.Deck.empty
      [ Lib.Card.spit; Lib.Card.throw; Lib.Card.tackle ]
  in
  let shuffled_deck = Lib.Deck.shuffle deck in
  assert_equal 3 (Lib.Deck.size shuffled_deck);
  assert_bool "Shuffling doesn't change deck size"
    (Lib.Deck.size shuffled_deck = 3)

let test_get () =
  let deck =
    List.fold_left
      (fun acc card -> Lib.Deck.push card acc)
      Lib.Deck.empty
      [ Lib.Card.spit; Lib.Card.throw; Lib.Card.tackle ]
  in
  assert_equal Lib.Card.tackle (Lib.Deck.get 1 deck);
  assert_equal Lib.Card.throw (Lib.Deck.get 2 deck);
  assert_equal Lib.Card.spit (Lib.Deck.get 3 deck);
  assert_raises Lib.Deck.Empty (fun () -> Lib.Deck.get 4 deck)

let test_remove () =
  let deck =
    List.fold_left
      (fun acc card -> Lib.Deck.push card acc)
      Lib.Deck.empty
      [ Lib.Card.spit; Lib.Card.throw; Lib.Card.tackle ]
  in
  let new_deck = Lib.Deck.remove 1 deck in
  assert_equal 2 (Lib.Deck.size new_deck)

let test_deck_to_strings () =
  let deck = [ Lib.Card.spit; Lib.Card.throw ] in
  let expected_output =
    [
      "1 -> spit | cost: 1 | dmg : 10 | defend : 0 | effect : Weaken";
      "2 -> throw | cost: 1 | dmg : 20 | defend : 0 | effect : Bleed";
    ]
  in
  assert_equal expected_output (Lib.Deck.deck_to_string deck)

let deck_tests =
  "deck tests"
  >::: [
         ("test_empty_deck" >:: fun _ -> test_empty_deck ());
         ("test_push_and_peek" >:: fun _ -> test_push_and_peek ());
         ("test_pop" >:: fun _ -> test_pop ());
         ("test_size" >:: fun _ -> test_size ());
         ("test_draw" >:: fun _ -> test_draw ());
         ("test_draw_empty_deck" >:: fun _ -> test_draw_empty_deck ());
         ("test_shuffle" >:: fun _ -> test_shuffle ());
         ("test_get" >:: fun _ -> test_get ());
         ("test_remove" >:: fun _ -> test_remove ());
         ("test_deck_to_strings" >:: fun _ -> test_deck_to_strings ());
       ]

let () = run_test_tt_main ("test suite" >::: [ deck_tests; card_tests ])
