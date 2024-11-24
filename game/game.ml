open Graphics
open Tsdl_image
open Tsdl.Sdl
open Lib
(* open Tsdl_ttf *)

(**[pos ch] handles when current health is less than 0. If it is negative then
   it equals 0, if it isn't then it returns itself.*)
let pos ch = if ch < 0 then 0 else ch

let make_hp_bar x y max_health curr_health scale =
  let hp_box_height = int_of_float (15.0 *. scale) in
  let hp_bar_height = int_of_float (11.0 *. scale) in
  set_color white;
  Graphics.fill_rect
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
  Graphics.fill_rect
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

  (*flush with render present have different states like active - paused -etc,
    reference space invaders

    redner copy - takes rectangle (+ destination), renderer, and texture
    Rectangle : take s in sorce and desitnation create.renderer - window texture
    - path of the file

    create a texture when initializing a texture by load texture - requres
    rendere - filename, and retursn texture never change texture

    Cards - keep track of mouse cursor using get_cursor look through cursor.

    Button - for start menu and such match up *)
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
    Lib.Card.basicA;
    Lib.Card.basicA;
    Lib.Card.basicA;
    Lib.Card.basicA;
    Lib.Card.basicA;
    Lib.Card.basicD;
    Lib.Card.basicD;
    Lib.Card.basicD;
    Lib.Card.basicD;
    Lib.Card.basicAD;
  ]

let camel1A_hand =
  [
    Lib.Card.basicA;
    Lib.Card.basicA;
    Lib.Card.basicD;
    Lib.Card.basicD;
    Lib.Card.basicAD;
  ]

let hyena_moves =
  [ Lib.Enemy.create_move 7 0 "None"; Lib.Enemy.create_move 5 0 "None" ]

(**[draw_one] takes in a hand and deck and appends the first element of deck
   onto the top of the hand. It returns a tuple of the updated hand and updated
   deck*)
let draw_one hand deck =
  let new_hand = Lib.Deck.push (Lib.Deck.peek deck) hand in
  (new_hand, Lib.Deck.pop deck)

(**[play_card] takes in a hand and returns a tuple of the top card and the
   updated hand.*)
let play_card hand =
  let top_card = Lib.Deck.peek hand in
  (top_card, Lib.Deck.pop hand)

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
    let intput = int_of_string input in
    if intput > Lib.Deck.size hand && intput >= 0 then
      failwith "Uh oh! Index out of bound"
    else input
  with Sys_error msg ->
    Printf.printf "Error: %s\n" msg;
    (*idk what to do with input so it just returns it but we prolly have to
      check this*)
    input

let rec game (player : Lib.Camel.t) (hyena : Lib.Enemy.t)
    (player_hand : Lib.Card.t Lib.Deck.t) player_deck =
  if hyena.hp <= 0 then print_endline "Game Over"
  else
    let hand_deck_tuple = draw_one player_hand player_deck in
    let hand = fst hand_deck_tuple in
    (* let deck = snd hand_deck_tuple in *)
    Lib.Deck.print (Lib.Deck.to_list player_hand);
    print_endline "Play a card (type index) or End to end turn: ";
    let input = read_line () in
    let affects =
      ( Lib.Card.get_dmg (Lib.Deck.get (int_of_string input) hand),
        Lib.Card.get_defend (Lib.Deck.get (int_of_string input) hand),
        Lib.Card.get_cost (Lib.Deck.get (int_of_string input) hand),
        Lib.Card.get_effect (Lib.Deck.get (int_of_string input) hand) )
    in
    let enemy_attack =
      match hyena.moves with
      | [] -> raise (Failure "?")
      | h :: t -> h
    in
    match affects with
    | d, def, cost, eff ->
        clear_graph ();
        let background_color = rgb 219 122 74 in
        set_color background_color;
        Graphics.fill_rect 0 0 (size_x ()) (size_y ());
        make_hyena 440 200;
        make_camel 200 200 1.0;
        make_hp_bar 440 200 20 (hyena.hp - d) 1.0;

        make_hp_bar 200 200 80
          (if def - enemy_attack.damage > 0 then Lib.Camel.get_hp player
           else Lib.Camel.get_hp player + (def - enemy_attack.damage))
          1.0;
        game
          (Lib.Camel.create_camel
             (if def - enemy_attack.damage > 0 then Lib.Camel.get_hp player
              else Lib.Camel.get_hp player + (def - enemy_attack.damage))
             3 "")
          (Lib.Enemy.create_enemy (hyena.hp - d) hyena.moves)
          player_hand player_deck

let run () =
  open_graph " 640x480";
  let background_color = rgb 219 122 74 in
  set_color background_color;
  Graphics.fill_rect 0 0 (size_x ()) (size_y ());

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

  let camel = Lib.Camel.create_camel 80 3 "" in
  let hyena = Lib.Enemy.create_enemy 20 hyena_moves in
  let deck = Lib.Deck.empty |> List.fold_right Lib.Deck.push camel1A_deck in
  (* TODO - randomization function of Deck *)
  let hand = Lib.Deck.empty |> List.fold_right Lib.Deck.push camel1A_hand in
  game camel hyena hand deck;

  let _ = read_line () in
  close_graph ()

type state =
  (* | MainMenu -> (optional) add in main menu or paused state*)
  | Active

(*change this to ref MainMenu later*)
let state = ref Active
let width = 1512
let height = 850

let create_renderer window =
  match create_renderer ~index:(-1) ~flags:Renderer.presentvsync window with
  | Ok r -> r
  | Error e -> failwith ("Unable to create renderer: " ^ get_error ())

let load_texture renderer path =
  match Image.load_texture renderer path with
  | Ok texture -> texture
  | Error (`Msg e) -> failwith ("Unable to load texture: " ^ e)

(**[init] initializes window, renderer, background texture, camel texture*)
let init () =
  begin
    match init Init.everything with
    | Ok () -> ()
    | Error (`Msg e) -> failwith ("Unable to initialize SDL: " ^ e)
  end;

  match Image.init Image.Init.png with
  | _ ->
      ();
      let window =
        create_window "Camel Caravan" ~w:width ~h:height Window.shown
        |> Result.get_ok
      in
      let renderer =
        Tsdl.Sdl.create_renderer ~index:(-1)
          ~flags:Tsdl.Sdl.Renderer.presentvsync window
        |> Result.get_ok
      in
      let background_texture =
        match Image.load_texture renderer "assets/desert.png" with
        | Ok texture -> texture
        | Error (`Msg e) -> failwith ("Unable to load background texture: " ^ e)
      in
      let camel_texture =
        match Image.load_texture renderer "assets/camel.png" with
        | Ok texture -> texture
        | Error (`Msg e) -> failwith ("Unable to load camel texture: " ^ e)
      in
      let enemy_texture =
        match Image.load_texture renderer "assets/wolf.png" with
        | Ok texture -> texture
        | Error (`Msg e) -> failwith ("Unable to load enemy texture: " ^ e)
      in
      (renderer, (background_texture, camel_texture, enemy_texture))

let draw p r bg_texture camel_texture enemy_texture =
  (* clears the renderer *)
  render_clear r |> ignore;
  (* section of the png that you want *)
  Level.draw_level r bg_texture camel_texture enemy_texture 27 21;
  (* updates the renderer *)
  Level.init_players_hp p r;
  render_present r

let run () : unit =
  let renderer, (bg_texture, camel_texture, enemy_texture) = init () in

  (* temporary quit functionality *)
  let rec check_quit () =
    let event = Event.create () in
    if poll_event (Some event) then
      match Event.get event Event.typ with
      | t when t = Event.quit -> true
      | t when t = Event.key_down ->
          let keycode = Event.get event Event.keyboard_keycode in
          keycode = K.escape
      | _ -> false
    else false
  in

  let rec main_loop p h d =
    if not (check_quit ()) then begin
      draw p renderer bg_texture camel_texture enemy_texture;
      main_loop p h d
    end
    else log "Exiting the application. Goodbye!"
  in
  let players : Level.t =
    { player = Camel.init_camel; enemy = Enemy.init_enemy }
  in
  let deck = Lib.Deck.empty |> List.fold_right Lib.Deck.push camel1A_deck in
  let hand = Lib.Deck.empty |> List.fold_right Lib.Deck.push camel1A_hand in
  main_loop players hand deck;
  quit ()

let main () = run ()
