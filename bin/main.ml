open Graphics

let make_hp_bar x y max_health curr_health scale =
  let hp_box_height = int_of_float (15.0 *. scale) in
  let hp_bar_height = int_of_float (11.0 *. scale) in
  set_color white;
  fill_rect
    (x - int_of_float (65.0 *. scale))
    (y + int_of_float (60.0 *. scale))
    (int_of_float (151.0 *. scale))
    hp_box_height;

  set_color red;
  let hp_width =
    int_of_float
      (145.0 *. scale *. float_of_int curr_health /. float_of_int max_health)
  in
  fill_rect
    (x - int_of_float (62.0 *. scale))
    (y + int_of_float (62.0 *. scale))
    hp_width hp_bar_height;
  set_color black;
  moveto (x - int_of_float (50.0 *. scale)) (y + int_of_float (77.0 *. scale));
  draw_string
    ("HP: " ^ string_of_int curr_health ^ "/" ^ string_of_int max_health)

let make_camel x y scale =
  let light_brown = rgb 181 101 29 in
  set_color light_brown;
  let body_width = int_of_float (50.0 *. scale) in
  let body_height = int_of_float (27.0 *. scale) in
  let hump_width = int_of_float (22.0 *. scale) in
  let hump_height = int_of_float (25.0 *. scale) in
  let neck_radius = int_of_float (20.0 *. scale) in
  let neck_height = int_of_float (30.0 *. scale) in
  let leg_offset = int_of_float (15.0 *. scale) in
  let hind_leg_offset_x = int_of_float (-60.0 *. scale) in
  let hind_leg_offset_y = int_of_float (-8.0 *. scale) in

  fill_ellipse x y body_width body_height;

  (* Camel body *)
  let hump_x = x in
  let hump_y = y + int_of_float (13.0 *. scale) in
  fill_ellipse hump_x hump_y hump_width hump_height;

  (* Camel hump *)
  set_line_width (int_of_float (19.0 *. scale));
  let neck_x = x + int_of_float (60.0 *. scale) in
  let neck_y = y + int_of_float (15.0 *. scale) in
  draw_arc neck_x neck_y neck_radius neck_height 225 360;

  (* Camel head *)
  let face_x = x + int_of_float (74.0 *. scale) in
  let face_y = y + int_of_float (37.0 *. scale) in
  let face_offset_x = int_of_float (37.5 *. scale) in
  let face_offset_y = int_of_float (24.0 *. scale) in
  let face_drop_small = int_of_float (12.5 *. scale) in
  let face_drop_large = int_of_float (5.0 *. scale) in
  fill_poly
    [|
      (face_x, face_y - face_drop_small);
      (face_x + (face_offset_x / 2), face_y - (face_drop_small / 2));
      (face_x + face_offset_x, face_y - face_drop_small);
      (face_x + face_offset_x, face_y - face_offset_y);
      (face_x, face_y - face_offset_y - face_drop_large);
    |];

  (* Camel foreleg *)
  let foreleg_x = x + int_of_float (25.0 *. scale) in
  let foreleg_y = y - leg_offset in
  fill_poly
    [|
      (foreleg_x, foreleg_y);
      (foreleg_x + int_of_float (13.0 *. scale), foreleg_y);
      ( foreleg_x + int_of_float (5.0 *. scale),
        foreleg_y - int_of_float (35.0 *. scale) );
      ( foreleg_x + int_of_float (3.0 *. scale),
        foreleg_y - int_of_float (73.0 *. scale) );
      ( foreleg_x + int_of_float (8.0 *. scale),
        foreleg_y - int_of_float (77.0 *. scale) );
      ( foreleg_x - int_of_float (6.0 *. scale),
        foreleg_y - int_of_float (77.0 *. scale) );
      ( foreleg_x - int_of_float (5.0 *. scale),
        foreleg_y - int_of_float (73.0 *. scale) );
      ( foreleg_x - int_of_float (4.0 *. scale),
        foreleg_y - int_of_float (35.0 *. scale) );
    |];

  (* Camel hindleg *)
  let hindleg_x = x + hind_leg_offset_x in
  let hindleg_y = y + hind_leg_offset_y in
  fill_poly
    [|
      (hindleg_x, hindleg_y);
      ( hindleg_x + int_of_float (25.0 *. scale),
        hindleg_y + int_of_float (24.0 *. scale) );
      ( hindleg_x + int_of_float (12.0 *. scale),
        hindleg_y - int_of_float (40.0 *. scale) );
      ( hindleg_x + int_of_float (14.0 *. scale),
        hindleg_y - int_of_float (53.0 *. scale) );
      ( hindleg_x + int_of_float (18.0 *. scale),
        hindleg_y - int_of_float (80.0 *. scale) );
      ( hindleg_x + int_of_float (22.0 *. scale),
        hindleg_y - int_of_float (85.0 *. scale) );
      ( hindleg_x + int_of_float (13.0 *. scale),
        hindleg_y - int_of_float (85.0 *. scale) );
      ( hindleg_x + int_of_float (2.0 *. scale),
        hindleg_y - int_of_float (45.0 *. scale) );
      ( hindleg_x - int_of_float (1.0 *. scale),
        hindleg_y - int_of_float (20.0 *. scale) );
      (hindleg_x, hindleg_y - int_of_float (15.0 *. scale));
    |]

let camel1A_deck =
  [
    Final_project.Card.basicA;
    Final_project.Card.basicA;
    Final_project.Card.basicA;
    Final_project.Card.basicA;
    Final_project.Card.basicD;
    Final_project.Card.basicD;
    Final_project.Card.basicD;
    Final_project.Card.basicD;
    Final_project.Card.basicAD;
  ]

let camel1A_hand =
  [
    Final_project.Card.basicA;
    Final_project.Card.basicA;
    Final_project.Card.basicD;
    Final_project.Card.basicD;
    Final_project.Card.basicAD;
  ]

let hyena_moves =
  [
    Final_project.Enemy.create_move 6 0 "None";
    Final_project.Enemy.create_move 6 0 "None";
  ]

(**[draw_one] takes in a hand and deck and appends the first element of deck
   onto the top of the hand. It returns a tuple of the updated hand and updated
   deck*)
let draw_one hand deck =
  let new_hand = Final_project.Deck.push (Final_project.Deck.peek deck) hand in
  (new_hand, Final_project.Deck.pop deck)

(**[play_card] takes in a hand and returns a tuple of the top card and the
   updated hand.*)
let play_card hand =
  let top_card = Final_project.Deck.peek hand in
  (top_card, Final_project.Deck.pop hand)

(**[make_hyena] makes a hyena GUI centered at x y.*)
let make_hyena x y =
  set_color black;
  let cpu_x = 400 in
  let cpu_y = 200 in
  fill_rect cpu_x cpu_y 10 10

(**[check_condition] checks if the user input is an int, and checks if it
   non-negative and less than the total hand.size*)
let check_conditions input hand =
  try
    let intput = int_of_string input in
    if intput > Final_project.Deck.size hand && intput >= 0 then
      failwith "Uh oh! Index out of bound"
    else input
  with Sys_error msg ->
    Printf.printf "Error: %s\n" msg;
    (*idk what to do with input so it just returns it but we prolly have to
      check this*)
    input

let rec game (player : Final_project.Character.t)
    (hyena : Final_project.Enemy.t)
    (player_hand : Final_project.Card.t Final_project.Deck.t) player_deck =
  if hyena.hp = 0 then print_endline "end"
  else
    let hand_deck_tuple = draw_one player_hand player_deck in
    let hand = fst hand_deck_tuple in
    (* let deck = snd hand_deck_tuple in *)
    Final_project.Deck.print (Final_project.Deck.to_list player_hand);
    print_endline "Play a card (type index) or End to end turn: ";
    let input = read_line () in
    let affects =
      ( (Final_project.Deck.get (int_of_string input) hand).cost,
        (Final_project.Deck.get (int_of_string input) hand).dmg,
        (Final_project.Deck.get (int_of_string input) hand).defend,
        (Final_project.Deck.get (int_of_string input) hand).effect )
    in
    match affects with
    | c, d, def, cost ->
        clear_graph ();
        set_color (rgb 208 181 154);
        fill_rect 0 0 (size_x ()) (size_y ());
        make_hyena 400 200;
        make_camel 200 200 1.0;
        make_hp_bar 400 200 20 (hyena.hp - d) 1.0;
        make_hp_bar 200 200 80 80 1.0;
        game player
          (Final_project.Enemy.create_enemy (hyena.hp - d) hyena_moves)
          player_hand player_deck

let () =
  open_graph " 640x480";
  let background_color = rgb 208 181 154 in
  set_color background_color;
  fill_rect 0 0 (size_x ()) (size_y ());

  let camel_max_hp = 80 in
  let camel_curr_hp = 80 in
  let cpu_max_hp = 40 in
  let cpu_curr_hp = 40 in
  let scale = 1.0 in
  let camel_x = 200 in
  let camel_y = 200 in
  let cpu_x = 400 in
  let cpu_y = 200 in

  make_camel camel_x camel_y scale;
  make_hyena cpu_x cpu_y;
  make_hp_bar camel_x camel_y camel_curr_hp camel_max_hp scale;
  make_hp_bar cpu_x cpu_y cpu_max_hp cpu_curr_hp scale;

  let camel = Final_project.Character.create_camel 80 3 "" in
  let hyena = Final_project.Enemy.create_enemy 20 hyena_moves in
  let deck =
    Final_project.Deck.empty
    |> List.fold_right Final_project.Deck.push camel1A_deck
  in
  (* TODO - randomization function of Deck *)
  let hand =
    Final_project.Deck.empty
    |> List.fold_right Final_project.Deck.push camel1A_hand
  in
  game camel hyena hand deck;

  let _ = read_line () in
  close_graph ()
