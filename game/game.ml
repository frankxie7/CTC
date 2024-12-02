open Tsdl_image
open Tsdl
open Lib
open Const

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

let create_renderer window =
  match
    Sdl.create_renderer ~index:(-1) ~flags:Sdl.Renderer.presentvsync window
  with
  | Ok r -> r
  | Error e -> failwith ("Unable to create renderer: " ^ Sdl.get_error ())

let load_texture renderer path =
  match Image.load_texture renderer path with
  | Ok texture -> texture
  | Error (`Msg e) -> failwith ("Unable to load texture: " ^ e)

(**[init] initializes window, renderer, background texture, camel texture*)
let init () =
  begin
    match Sdl.init Sdl.Init.everything with
    | Ok () -> ()
    | Error (`Msg e) -> failwith ("Unable to initialize SDL: " ^ e)
  end;
  let anim = Animations.init_anim in
  Animations.set_anim anim "idle";
  match Image.init Image.Init.png with
  | _ ->
      ();
      let window =
        Sdl.create_window "Camel Caravan" ~w:Const.screen_width
          ~h:Const.screen_height Sdl.Window.shown
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
      (renderer, anim, (background_texture, camel_texture, enemy_texture))

let draw state renderer bg_texture camel_texture enemy_texture anim =
  Sdl.render_clear renderer |> ignore;

  Level.draw_level renderer bg_texture camel_texture enemy_texture;

  Level.draw_animation state renderer bg_texture camel_texture enemy_texture
    (Animations.get_anim anim);

  Level.init_players_hp state renderer;
  Sdl.render_present renderer

let game (state : Level.t) (hand : Lib.Card.t Lib.Deck.t)
    (deck : Lib.Card.t Lib.Deck.t) renderer camel_texture bg_texture
    enemy_texture anim =
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
        Animations.set_anim anim (Lib.Card.get_name card);
        print_endline ("Playing animation: " ^ Animations.get_anim anim);

        (* Level.draw_animation renderer bg_texture camel_texture enemy_texture
           !anim; *)
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

let run () =
  let renderer, anim, (bg_texture, camel_texture, enemy_texture) = init () in

  let rec main_loop (state : Level.t) hand deck =
    draw state renderer bg_texture camel_texture enemy_texture anim;
    let updated_state, updated_hand, updated_deck =
      game state hand deck renderer camel_texture bg_texture enemy_texture anim
    in
    main_loop updated_state updated_hand updated_deck
  in
  let initial_state = Level.init_player Camel.init_camel Enemy.init_enemy in
  let full_deck = List.fold_right Lib.Deck.push camel1A_deck Lib.Deck.empty in
  let shuffled_deck = Lib.Deck.shuffle full_deck in
  let hand, deck = Lib.Deck.draw 5 shuffled_deck Lib.Deck.empty in
  main_loop initial_state hand deck

let main () = run ()
