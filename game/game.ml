open Tsdl_image
open Tsdl.Sdl
open Lib
(* open Tsdl_ttf *)

(**[pos ch] handles when current health is less than 0. If it is negative then
   it equals 0, if it isn't then it returns itself.*)
let pos ch = if ch < 0 then 0 else ch

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
  [ Lib.Enemy.create_move 6 0 "None"; Lib.Enemy.create_move 6 0 "None" ]

(** [draw_one] takes in a hand and deck, shuffles the deck, and appends the
    first element of the deck onto the top of the hand if the deck is not empty.
    Returns a tuple of the updated hand and deck. *)
let draw_one hand deck =
  let shuffled_deck = Lib.Deck.shuffle deck in
  if Lib.Deck.is_empty shuffled_deck then (
    print_endline "No more cards to draw!";
    (hand, shuffled_deck))
  else
    let new_hand = Lib.Deck.push (Lib.Deck.peek shuffled_deck) hand in
    (new_hand, Lib.Deck.pop shuffled_deck)

(**[play_card] takes in a hand and index. Returns the played card and the new
   hand.*)
let play_card hand index =
  let played_card = Lib.Deck.get index hand in
  let new_hand = Lib.Deck.remove index hand in
  (played_card, new_hand)

(**[check_condition] checks if the user input is an int, and checks if it
   non-negative and less than the total hand.size*)
let check_conditions input hand =
  try
    let index = int_of_string input in
    if index < 1 || index > Lib.Deck.size hand then
      failwith "Uh oh! Index out of bounds"
    else index
  with
  | Failure _ -> failwith "Invalid input. Enter a valid number."
  | _ -> failwith "Unexpected error"

type state =
  (* | MainMenu -> (optional) add in main menu or paused state*)
  | Active

(*change this to ref MainMenu later*)
let state = ref Active
let width = 1512
let height = 850
let anim = ref "idle"

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
        match Image.load_texture renderer "assets/camelcamel.png" with
        | Ok texture -> texture
        | Error (`Msg e) -> failwith ("Unable to load camel texture: " ^ e)
      in
      let enemy_texture =
        match Image.load_texture renderer "assets/wolf.png" with
        | Ok texture -> texture
        | Error (`Msg e) -> failwith ("Unable to load enemy texture: " ^ e)
      in
      (renderer, (background_texture, camel_texture, enemy_texture))

let draw p r bg_texture camel_texture enemy_texture frame_count =
  (* Clear the renderer *)
  render_clear r |> ignore;

  (* Draw background and static elements *)
  Level.draw_level r bg_texture camel_texture enemy_texture 27 21;

  (* Draw animated camel based on the current animation state *)
  Level.draw_animation r Animations.animation_table !anim frame_count 64 64 100
    50 camel_texture;

  (* Draw player health or other UI elements *)
  Level.init_players_hp p r;

  (* Present the updated frame *)
  render_present r

let game (state : Level.t) (hand : Lib.Card.t Lib.Deck.t)
    (deck : Lib.Card.t Lib.Deck.t) =
  if Enemy.get_hp state.enemy <= 0 then (
    print_endline "You defeated the hyena! Game Over.";
    failwith "Game Over")
  else if Camel.get_hp state.player <= 0 then (
    print_endline "You have been defeated! Game Over.";
    failwith "Game Over")
  else (
    Lib.Deck.print (Lib.Deck.to_list hand);
    print_endline "Play a card (type index) or type 'End' to end turn:";

    let input = read_line () in
    if input = "End" then (
      let updated_hand, updated_deck = draw_one hand deck in
      print_endline "You drew a card!";
      let enemy_attack =
        match Enemy.get_moves state.enemy with
        | [] -> raise (Failure "Enemy has no moves")
        | move :: _ -> move
      in
      Camel.update_hp state.player enemy_attack.damage;
      print_endline
        (Printf.sprintf "Enemy attacks! You take %d damage!" enemy_attack.damage);
      (state, updated_hand, updated_deck))
    else
      try
        let index = check_conditions input hand in
        let card, updated_hand = play_card hand index in
        anim := Lib.Card.get_name card;

        (* Update animation state *)
        let dmg = Lib.Card.get_dmg card in
        let def = Lib.Card.get_defend card in

        let enemy_attack =
          match Enemy.get_moves state.enemy with
          | [] -> raise (Failure "Enemy has no moves")
          | move :: _ -> move
        in

        let total_damage_taken = max 0 (enemy_attack.damage - def) in
        Camel.update_hp state.player total_damage_taken;
        Enemy.update_hp state.enemy dmg;

        print_endline (Printf.sprintf "You dealt %d damage to the enemy!" dmg);
        if total_damage_taken > 0 then
          print_endline
            (Printf.sprintf "You took %d damage after defending %d!"
               total_damage_taken def)
        else print_endline "Enemy's attack was blocked!";

        (state, updated_hand, deck)
      with Failure msg ->
        print_endline msg;
        (state, hand, deck))

let run () : unit =
  let renderer, (bg_texture, camel_texture, enemy_texture) = init () in

  let rec check_quit () =
    let event = Event.create () in
    if poll_event (Some event) then
      match Event.get event Event.typ with
      | t when t = Event.quit -> true
      | t when t = Event.key_down ->
          Event.get event Event.keyboard_keycode = K.escape
      | _ -> false
    else false
  in

  let rec main_loop state hand deck frame_count =
    if not (check_quit ()) then (
      draw state renderer bg_texture camel_texture enemy_texture frame_count;
      let updated_state, updated_hand, updated_deck = game state hand deck in
      main_loop updated_state updated_hand updated_deck (frame_count + 1))
    else log "Quitting"
  in

  let players : Level.t =
    { player = Camel.init_camel; enemy = Enemy.init_enemy }
  in
  let full_deck = List.fold_right Lib.Deck.push camel1A_deck Lib.Deck.empty in
  let shuffled_deck = Lib.Deck.shuffle full_deck in
  let hand, deck = Lib.Deck.draw 5 shuffled_deck Lib.Deck.empty in
  main_loop players hand deck 0;
  quit ()

let main () = run ()
