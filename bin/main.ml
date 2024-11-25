open Graphics

(**[pos ch] handles when current health is less than 0. If it is negative then
   it equals 0, if it isn't then it returns itself.*)
let pos ch = if ch < 0 then 0 else ch

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
      (145.0 *. scale
      *. float_of_int (pos curr_health)
      /. float_of_int max_health)
  in
  fill_rect
    (x - int_of_float (62.0 *. scale))
    (y + int_of_float (62.0 *. scale))
    hp_width hp_bar_height;
  set_color black;
  moveto (x - int_of_float (50.0 *. scale)) (y + int_of_float (77.0 *. scale));
  draw_string
    ("HP: " ^ string_of_int (pos curr_health) ^ "/" ^ string_of_int max_health)

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
    (* Final_project.Card.basicD; Final_project.Card.basicD; *)
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

(**[play_card] takes in a hand and index. Returns the played card and the new
   hand.*)
let play_card hand index =
  let played_card = Final_project.Deck.get index hand in
  let new_hand = Final_project.Deck.remove index hand in
  (played_card, new_hand)

(**[make_hyena] makes a hyena GUI centered at x y.*)
let make_hyena x y =
  let light_brown = rgb 228 204 148 in
  let brown = rgb 195 157 70 in
  let cpu_x = x + 25 in
  let cpu_y = y - 15 in

  set_color brown;
  fill_ellipse cpu_x cpu_y 35 20;
  fill_poly
    [|
      (cpu_x - 40, cpu_y);
      (cpu_x - 18, cpu_y - 10);
      (cpu_x - 18, cpu_y - 55);
      (cpu_x - 21, cpu_y - 70);
      (cpu_x - 21, cpu_y - 75);
      (cpu_x - 36, cpu_y - 75);
      (cpu_x - 40, cpu_y - 70);
      (cpu_x - 36, cpu_y - 65);
      (cpu_x - 32, cpu_y - 66);
      (cpu_x - 30, cpu_y - 60);
      (cpu_x - 32, cpu_y - 45);
      (cpu_x - 30, cpu_y - 30);
    |];
  fill_poly (* hind legs *)
    [|
      (cpu_x + 23, cpu_y - 18);
      (cpu_x + 27, cpu_y - 27);
      (cpu_x + 45, cpu_y - 48);
      (cpu_x + 45, cpu_y - 66);
      (cpu_x + 37, cpu_y - 65);
      (cpu_x + 34, cpu_y - 70);
      (cpu_x + 35, cpu_y - 75);
      (cpu_x + 51, cpu_y - 75);
      (cpu_x + 56, cpu_y - 44);
      (cpu_x + 41, cpu_y - 21);
    |];
  set_color light_brown;
  fill_poly
    [|
      (cpu_x - 40, cpu_y);
      (cpu_x - 18, cpu_y - 10);
      (cpu_x - 18, cpu_y - 55);
      (cpu_x - 21, cpu_y - 70);
      (cpu_x - 21, cpu_y - 75);
      (cpu_x - 36, cpu_y - 75);
      (cpu_x - 35, cpu_y - 70);
      (cpu_x - 31, cpu_y - 65);
      (cpu_x - 27, cpu_y - 66);
      (cpu_x - 25, cpu_y - 60);
      (cpu_x - 28, cpu_y - 45);
      (cpu_x - 25, cpu_y - 30);
      (cpu_x - 34, cpu_y - 18);
    |];
  fill_poly
    [|
      (cpu_x - 40, cpu_y);
      (cpu_x - 70, cpu_y + 20);
      (cpu_x - 55, cpu_y + 35);
      (cpu_x, cpu_y + 20);
      (cpu_x - 18, cpu_y - 10);
    |];

  fill_poly (* body *)
    [|
      (cpu_x - 40, cpu_y);
      (cpu_x - 16, cpu_y - 15);
      (cpu_x + 5, cpu_y - 25);
      (cpu_x + 28, cpu_y - 18);
    |];
  fill_poly (* hind legs *)
    [|
      (cpu_x + 28, cpu_y - 18);
      (cpu_x + 32, cpu_y - 27);
      (cpu_x + 50, cpu_y - 48);
      (cpu_x + 50, cpu_y - 66);
      (cpu_x + 42, cpu_y - 65);
      (cpu_x + 39, cpu_y - 70);
      (cpu_x + 40, cpu_y - 75);
      (cpu_x + 56, cpu_y - 75);
      (cpu_x + 61, cpu_y - 44);
      (cpu_x + 46, cpu_y - 21);
      (cpu_x + 50, cpu_y + 7);
      (cpu_x + 48, cpu_y + 14);
      (cpu_x + 15, cpu_y + 25);
      (cpu_x - 55, cpu_y + 35);
      (cpu_x - 40, cpu_y);
    |];

  fill_poly (* hind legs *)
    [|
      (cpu_x + 28, cpu_y - 18);
      (cpu_x + 32, cpu_y - 27);
      (cpu_x + 50, cpu_y - 48);
      (cpu_x + 50, cpu_y - 66);
      (cpu_x + 42, cpu_y - 65);
      (cpu_x + 39, cpu_y - 70);
      (cpu_x + 40, cpu_y - 75);
      (cpu_x + 56, cpu_y - 75);
      (cpu_x + 61, cpu_y - 44);
      (cpu_x + 46, cpu_y - 21);
      (cpu_x + 50, cpu_y + 7);
      (cpu_x + 48, cpu_y + 14);
      (cpu_x + 15, cpu_y + 25);
      (cpu_x - 55, cpu_y + 35);
      (cpu_x - 40, cpu_y);
    |];

  fill_ellipse (cpu_x - 65) (cpu_y + 20) 16 16;
  let x = cpu_x in
  let y = cpu_y + 25 in
  fill_poly
    [| (x - 63, y + 20); (x - 58, y + 10); (x - 63, y - 5); (x - 70, y + 10) |];
  let x = cpu_x - 10 in
  let y = cpu_y + 25 in
  fill_poly
    [| (x - 63, y + 20); (x - 58, y + 10); (x - 63, y - 5); (x - 70, y + 10) |];
  fill_ellipse (cpu_x - 72) (cpu_y + 8) 8 8

(**[check_condition] checks if the user input is an int, and checks if it
   non-negative and less than the total hand.size*)
let check_conditions input hand =
  try
    let index = int_of_string input in
    if index < 1 || index > Final_project.Deck.size hand then
      failwith "Uh oh! Index out of bounds"
    else index
  with
  | Failure _ -> failwith "Invalid input. Enter a valid number."
  | _ -> failwith "Unexpected error"

let rec game (player : Final_project.Character.t)
    (hyena : Final_project.Enemy.t)
    (player_hand : Final_project.Card.t Final_project.Deck.t)
    (player_deck : Final_project.Card.t Final_project.Deck.t) =
  if hyena.hp <= 0 then print_endline "You defeated the hyena! Game Over."
  else if Final_project.Character.get_hp player <= 0 then
    print_endline "You have been defeated! Game Over."
  else (
    Final_project.Deck.print (Final_project.Deck.to_list player_hand);

    print_endline "Play a card (type index) or End to end turn: ";
    let input = read_line () in
    if input = "End" then (
      (* Draw one card at the end of the turn *)
      let updated_hand, updated_deck = draw_one player_hand player_deck in
      print_endline "You drew a card!";

      (* Execute the enemy's move *)
      let enemy_attack =
        match hyena.moves with
        | [] -> raise (Failure "Enemy has no moves")
        | move :: _ -> move
      in
      let damage_taken = enemy_attack.damage in
      let new_player_hp =
        Final_project.Character.get_hp player - damage_taken
      in
      print_endline
        (Printf.sprintf "Enemy attacks! You take %d damage!" damage_taken);

      game
        (Final_project.Character.create_camel new_player_hp 3 "")
        hyena updated_hand updated_deck)
    else
      try
        let index = check_conditions input player_hand in
        let card, updated_hand = play_card player_hand index in
        let dmg = Final_project.Card.get_dmg card in
        let def = Final_project.Card.get_defend card in

        let enemy_attack =
          match hyena.moves with
          | [] -> raise (Failure "Enemy has no moves")
          | move :: _ -> move
        in

        let damage_taken = max 0 (enemy_attack.damage - def) in
        let new_player_hp =
          Final_project.Character.get_hp player - damage_taken
        in

        clear_graph ();
        set_color (rgb 208 181 154);
        fill_rect 0 0 (size_x ()) (size_y ());
        make_hyena 400 200;
        make_camel 200 200 1.0;
        make_hp_bar 400 200 20 (hyena.hp - dmg) 1.0;

        make_hp_bar 200 200 80
          (if def - enemy_attack.damage > 0 then
             Final_project.Character.get_hp player
           else new_player_hp)
          1.0;

        game
          (Final_project.Character.create_camel new_player_hp 3 "")
          (Final_project.Enemy.create_enemy (hyena.hp - dmg) hyena.moves)
          updated_hand player_deck
      with Failure msg ->
        print_endline msg;
        game player hyena player_hand player_deck)

let () =
  open_graph " 640x480";
  let background_color = rgb 219 122 74 in
  set_color background_color;
  fill_rect 0 0 (size_x ()) (size_y ());

  let camel_max_hp = 80 in
  let camel_curr_hp = 80 in
  let cpu_max_hp = 40 in
  let cpu_curr_hp = 40 in
  let scale = 1.0 in
  let camel_x = 200 in
  let camel_y = 200 in
  let cpu_x = 440 in
  let cpu_y = 200 in

  make_camel camel_x camel_y scale;
  make_hyena cpu_x cpu_y;
  make_hp_bar camel_x camel_y camel_curr_hp camel_max_hp scale;
  make_hp_bar cpu_x cpu_y cpu_max_hp cpu_curr_hp scale;

  let camel = Final_project.Character.create_camel 80 3 "" in
  let hyena = Final_project.Enemy.create_enemy 20 hyena_moves in
  Random.self_init ();

  let deck =
    List.fold_right Final_project.Deck.push camel1A_deck
      Final_project.Deck.empty
    |> Final_project.Deck.shuffle
  in
  let hand, deck = Final_project.Deck.draw 5 deck Final_project.Deck.empty in
  game camel hyena hand deck;

  let _ = read_line () in
  close_graph ()
