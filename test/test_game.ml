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
    print_endline "No more cards to\n   draw!";
    (hand, shuffled_deck))
  else
    let new_hand = Lib.Deck.push (Lib.Deck.peek shuffled_deck) hand in
    (new_hand, Lib.Deck.pop shuffled_deck)

let test_init_camel () =
  let camel = Lib.Camel.init_camel () in
  assert_equal 100 (Lib.Camel.get_hp camel);
  assert_equal 3 (Lib.Camel.get_energy camel);
  assert_equal 0 (Lib.Camel.get_def camel);
  assert_equal [] (Lib.Camel.get_status camel);
  assert_equal "idle" (Lib.Camel.get_animation camel)

let test_update_hp () =
  let camel = Lib.Camel.init_camel () in
  Lib.Camel.update_hp camel 10;
  assert_equal 90 (Lib.Camel.get_hp camel)

let test_update_defense () =
  let camel = Lib.Camel.init_camel () in
  Lib.Camel.update_def camel 5;
  assert_equal 5 (Lib.Camel.get_def camel)

let test_update_energy () =
  let camel = Lib.Camel.init_camel () in
  Lib.Camel.update_energy camel 1;
  assert_equal 2 (Lib.Camel.get_energy camel)

let test_update_status () =
  let camel = Lib.Camel.init_camel () in
  Lib.Camel.update_status camel "Bleed";
  assert_equal [ ("Bleed", 3) ] (Lib.Camel.get_status camel);
  Lib.Camel.update_status camel "Stun";
  assert_equal [ ("Stun", 2); ("Bleed", 3) ] (Lib.Camel.get_status camel);
  Lib.Camel.update_status camel "Bleed";
  assert_equal [ ("Bleed", 3); ("Stun", 2) ] (Lib.Camel.get_status camel)

let test_update_animation () =
  let camel = Lib.Camel.init_camel () in
  Lib.Camel.update_animation camel "attack";
  assert_equal "attack" (Lib.Camel.get_animation camel)

let test_degrade_status () =
  let camel = Lib.Camel.init_camel () in
  Lib.Camel.update_status camel "Bleed";
  Lib.Camel.update_status camel "Stun";
  Lib.Camel.degrade_status camel;
  assert_equal [ ("Stun", 1); ("Bleed", 2) ] (Lib.Camel.get_status camel);
  Lib.Camel.degrade_status camel;
  assert_equal [ ("Bleed", 1) ] (Lib.Camel.get_status camel);
  Lib.Camel.degrade_status camel;
  assert_equal [] (Lib.Camel.get_status camel)

let test_repeated_update_status () =
  let camel = Lib.Camel.init_camel () in
  Lib.Camel.update_status camel "Bleed";
  assert_equal [ ("Bleed", 3) ] (Lib.Camel.get_status camel);
  Lib.Camel.update_status camel "Stun";
  assert_equal [ ("Stun", 2); ("Bleed", 3) ] (Lib.Camel.get_status camel);
  Lib.Camel.update_status camel "Bleed";
  assert_equal [ ("Bleed", 3); ("Stun", 2) ] (Lib.Camel.get_status camel);
  Lib.Camel.update_status camel "Stun";
  assert_equal [ ("Stun", 2); ("Bleed", 3) ] (Lib.Camel.get_status camel);
  Lib.Camel.degrade_status camel;
  assert_equal [ ("Stun", 1); ("Bleed", 2) ] (Lib.Camel.get_status camel);
  Lib.Camel.update_status camel "Bleed";
  assert_equal [ ("Bleed", 3); ("Stun", 1) ] (Lib.Camel.get_status camel);
  Lib.Camel.update_status camel "Stun";
  assert_equal [ ("Stun", 2); ("Bleed", 3) ] (Lib.Camel.get_status camel)

let camel_tests =
  "camel tests"
  >::: [
         ("test_init_camel" >:: fun _ -> test_init_camel ());
         ("test_update_hp" >:: fun _ -> test_update_hp ());
         ("test_update_defense" >:: fun _ -> test_update_defense ());
         ("test_update_energy" >:: fun _ -> test_update_energy ());
         ("test_update_status" >:: fun _ -> test_update_status ());
         ("test_update_animation" >:: fun _ -> test_update_animation ());
         ("test_degrade_status" >:: fun _ -> test_degrade_status ());
         ( "test_repeated_update_status" >:: fun _ ->
           test_repeated_update_status () );
       ]

let test_create_move () =
  let move = Lib.Enemy.create_move "strike" 10 5 "Stun" in
  assert_equal "strike" (Lib.Enemy.get_name move);
  assert_equal 10 (Lib.Enemy.get_dmg move);
  assert_equal 5 move.defend;
  assert_equal "Stun" move.effect

let test_init_snake () =
  let snake = Lib.Enemy.init_snake () in
  assert_equal 50 (Lib.Enemy.get_hp snake);
  assert_equal "idle" (Lib.Enemy.get_animation snake);
  let moves = Lib.Enemy.get_moves snake in
  assert_equal 2 (Array.length moves);
  let move = moves.(0) in
  assert_equal "bite" (Lib.Enemy.get_name move);
  assert_equal 7 (Lib.Enemy.get_dmg move)

let test_enemy_update_hp () =
  let enemy = Lib.Enemy.init_snake () in
  Lib.Enemy.update_hp enemy 3;
  assert_equal 47 (Lib.Enemy.get_hp enemy)

let test_update_animation () =
  let enemy = Lib.Enemy.init_snake () in
  Lib.Enemy.update_animation enemy "attack";
  assert_equal "attack" (Lib.Enemy.get_animation enemy)

let test_create_enemy () =
  let moves = [| Lib.Enemy.create_move "smash" 15 0 "burn" |] in
  let enemy = Lib.Enemy.create_enemy 20 moves in
  assert_equal 20 (Lib.Enemy.get_hp enemy);
  assert_equal "idle" (Lib.Enemy.get_animation enemy);
  assert_equal 1 (Array.length (Lib.Enemy.get_moves enemy));
  let move = (Lib.Enemy.get_moves enemy).(0) in
  assert_equal "smash" (Lib.Enemy.get_name move);
  assert_equal 15 (Lib.Enemy.get_dmg move)

let test_get_effect () =
  let moves = [| Lib.Enemy.create_move "smash" 15 0 "burn" |] in
  let enemy = Lib.Enemy.create_enemy 20 moves in
  let move = (Lib.Enemy.get_moves enemy).(0) in
  assert_equal "burn" (Lib.Enemy.get_effect move)

let test_enemy_degrade_status () =
  let enemy = Lib.Enemy.init_bear () in
  Lib.Enemy.update_status enemy "Bleed";
  Lib.Enemy.update_status enemy "Stun";
  Lib.Enemy.update_status enemy "Weaken";
  Lib.Enemy.degrade_status enemy;
  assert_equal [ ("Weaken", 2); ("Bleed", 2) ] (Lib.Enemy.get_status enemy);
  Lib.Enemy.degrade_status enemy;
  assert_equal [ ("Weaken", 1); ("Bleed", 1) ] (Lib.Enemy.get_status enemy);
  Lib.Enemy.degrade_status enemy;
  assert_equal [] (Lib.Enemy.get_status enemy)

let test_enemy_repeated_update_status () =
  let enemy = Lib.Enemy.init_man () in
  Lib.Enemy.update_status enemy "Stun";
  assert_equal [ ("Stun", 1) ] (Lib.Enemy.get_status enemy);
  Lib.Enemy.update_status enemy "Bleed";
  assert_equal [ ("Bleed", 3); ("Stun", 1) ] (Lib.Enemy.get_status enemy);
  Lib.Enemy.update_status enemy "Weaken";
  assert_equal
    [ ("Weaken", 3); ("Bleed", 3); ("Stun", 1) ]
    (Lib.Enemy.get_status enemy);
  Lib.Enemy.update_status enemy "Stun";
  assert_equal
    [ ("Stun", 1); ("Weaken", 3); ("Bleed", 3) ]
    (Lib.Enemy.get_status enemy);
  Lib.Enemy.update_status enemy "Bleed";
  assert_equal
    [ ("Bleed", 3); ("Stun", 1); ("Weaken", 3) ]
    (Lib.Enemy.get_status enemy);
  Lib.Enemy.update_status enemy "Weaken";
  assert_equal
    [ ("Weaken", 3); ("Bleed", 3); ("Stun", 1) ]
    (Lib.Enemy.get_status enemy);
  Lib.Enemy.degrade_status enemy;
  assert_equal [ ("Weaken", 2); ("Bleed", 2) ] (Lib.Enemy.get_status enemy);
  Lib.Enemy.update_status enemy "Weaken";
  assert_equal [ ("Weaken", 3); ("Bleed", 2) ] (Lib.Enemy.get_status enemy);
  Lib.Enemy.update_status enemy "Bleed";
  assert_equal [ ("Bleed", 3); ("Weaken", 3) ] (Lib.Enemy.get_status enemy);
  Lib.Enemy.update_status enemy "Stun";
  assert_equal
    [ ("Stun", 1); ("Bleed", 3); ("Weaken", 3) ]
    (Lib.Enemy.get_status enemy)

let enemy_tests =
  "enemy tests"
  >::: [
         ("test_create_move" >:: fun _ -> test_create_move ());
         ("test_init_snake" >:: fun _ -> test_init_snake ());
         ("test_update_hp" >:: fun _ -> test_enemy_update_hp ());
         ("test_update_animation" >:: fun _ -> test_update_animation ());
         ("test_create_enemy" >:: fun _ -> test_create_enemy ());
         ("test_get_effect" >:: fun _ -> test_get_effect ());
         ("test_enemy_degrade_status" >:: fun _ -> test_enemy_degrade_status ());
         ( "test_enemy_repeated_update_status" >:: fun _ ->
           test_enemy_repeated_update_status () );
       ]

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
  assert_bool "Shuffling doesn't\n   change deck size"
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
  assert_equal Lib.Card.spit (Lib.Deck.get 3 deck)

let test_remove () =
  let deck =
    List.fold_left
      (fun acc card -> Lib.Deck.push card acc)
      Lib.Deck.empty
      [ Lib.Card.spit; Lib.Card.throw; Lib.Card.tackle ]
  in
  let new_deck = Lib.Deck.remove 1 deck in
  assert_equal 2 (Lib.Deck.size new_deck)

let test_peek_empty_deck () =
  assert_raises Lib.Deck.Empty (fun () -> Lib.Deck.peek Lib.Deck.empty)

let test_pop_empty_deck () =
  assert_raises Lib.Deck.Empty (fun () -> Lib.Deck.pop Lib.Deck.empty)

let test_get_empty_deck () =
  assert_raises Lib.Deck.Empty (fun () -> Lib.Deck.get 1 Lib.Deck.empty)

let test_remove_empty_deck () =
  assert_raises Lib.Deck.Empty (fun () -> Lib.Deck.remove 1 Lib.Deck.empty)

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
         ("test_peek_empty_deck" >:: fun _ -> test_peek_empty_deck ());
         ("test_pop_empty_deck" >:: fun _ -> test_pop_empty_deck ());
         ("test_get_empty_deck" >:: fun _ -> test_get_empty_deck ());
         ("test_remove_empty_deck" >:: fun _ -> test_remove_empty_deck ());
       ]

let test_get_name () =
  let camel_name =
    Lib.Animations.get_name Lib.Animations.camel_animation_table "spit"
  in
  assert_equal "spit" camel_name;
  assert_raises (Failure "Error: Get_name function: Animation not found")
    (fun () ->
      Lib.Animations.get_name Lib.Animations.camel_animation_table "nonexistent")

let test_get_col () =
  let camel_col =
    Lib.Animations.get_col Lib.Animations.camel_animation_table "spit"
  in
  assert_equal 2 camel_col;
  assert_raises (Failure "Error: Get_col function: Animation not found")
    (fun () ->
      Lib.Animations.get_col Lib.Animations.camel_animation_table "nonexistent")

let test_get_frame_num () =
  let camel_frame_num =
    Lib.Animations.get_frame_num Lib.Animations.camel_animation_table "spit"
  in
  assert_equal 17 camel_frame_num;
  assert_raises (Failure "Error: Frame_num function: Animation not found")
    (fun () ->
      Lib.Animations.get_frame_num Lib.Animations.camel_animation_table
        "nonexistent")

let animation_tests =
  "animation tests"
  >::: [
         ("test_get_name" >:: fun _ -> test_get_name ());
         ("test_get_col" >:: fun _ -> test_get_col ());
         ("test_get_frame_num" >:: fun _ -> test_get_frame_num ());
       ]

let test_deck_after_draw () =
  let deck =
    List.fold_left
      (fun acc card -> Lib.Deck.push card acc)
      Lib.Deck.empty
      [ Lib.Card.spit; Lib.Card.throw; Lib.Card.tackle; Lib.Card.defend ]
  in
  let hand, deck_after_draw = Lib.Deck.draw 2 deck Lib.Deck.empty in
  assert_equal 2 (Lib.Deck.size hand);
  assert_equal 2 (Lib.Deck.size deck_after_draw);
  assert_equal 2 (Lib.Deck.size deck_after_draw)

let test_deck_size_after_multiple_draws () =
  let deck =
    List.fold_left
      (fun acc card -> Lib.Deck.push card acc)
      Lib.Deck.empty
      [ Lib.Card.spit; Lib.Card.throw; Lib.Card.tackle; Lib.Card.defend ]
  in
  let hand, deck_after_draw1 = Lib.Deck.draw 2 deck Lib.Deck.empty in
  let hand2, deck_after_draw2 =
    Lib.Deck.draw 2 deck_after_draw1 Lib.Deck.empty
  in
  assert_equal 2 (Lib.Deck.size hand2);
  assert_equal 0 (Lib.Deck.size deck_after_draw2);
  assert_equal 0 (Lib.Deck.size deck_after_draw2)

let test_empty_deck_draw () =
  let hand, deck = Lib.Deck.draw 1 Lib.Deck.empty Lib.Deck.empty in
  assert_equal 0 (Lib.Deck.size hand);
  assert_equal 0 (Lib.Deck.size deck)

let test_shuffle_randomness () =
  let deck =
    List.fold_left
      (fun acc card -> Lib.Deck.push card acc)
      Lib.Deck.empty
      [ Lib.Card.spit; Lib.Card.throw; Lib.Card.tackle; Lib.Card.defend ]
  in
  let shuffled_deck1 = Lib.Deck.shuffle deck in
  let shuffled_deck2 = Lib.Deck.shuffle deck in
  assert_bool "Shuffled decks should not be identical"
    (shuffled_deck1 != shuffled_deck2)

let interaction_tests =
  "interaction tests"
  >::: [
         ("test_deck_after_draw" >:: fun _ -> test_deck_after_draw ());
         ( "test_deck_size_after_multiple_draws" >:: fun _ ->
           test_deck_size_after_multiple_draws () );
         ("test_empty_deck_draw" >:: fun _ -> test_empty_deck_draw ());
         ("test_shuffle_randomness" >:: fun _ -> test_shuffle_randomness ());
       ]

let () =
  run_test_tt_main
    ("test suite"
    >::: [
           deck_tests;
           card_tests;
           camel_tests;
           enemy_tests;
           animation_tests;
           interaction_tests;
         ])
