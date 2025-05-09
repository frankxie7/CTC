open Tsdl_image
open Tsdl
open Lib
open Const

(**[pos ch] handles when current health is less than 0. If it is negative then
   it equals 0, if it isn't then it returns itself.*)
let pos ch = if ch < 0 then 0 else ch

let camel1A_deck =
  [
    Lib.Card.tackle;
    Lib.Card.tackle;
    Lib.Card.tackle;
    Lib.Card.defend;
    Lib.Card.defend;
    Lib.Card.defend;
    Lib.Card.spit;
    Lib.Card.spit;
    Lib.Card.stomp;
  ]

(** [draw_one] takes in a hand and deck, shuffles the deck, and appends the
    first element of the deck onto the top of the hand if the deck is not empty.
    Returns a tuple of the updated hand and deck. *)
let draw_one hand deck =
  let shuffled_deck = Lib.Deck.shuffle deck in
  if Lib.Deck.is_empty shuffled_deck then (
    print_endline "No more cards to draw!";
    if Lib.Deck.is_empty hand then (
      print_endline "Hand is empty reshuffling and drawing 5";
      let full_deck =
        List.fold_right Lib.Deck.push camel1A_deck Lib.Deck.empty
      in
      let shuffled_deck = Lib.Deck.shuffle full_deck in
      let updated_hand, updated_deck =
        Lib.Deck.draw 5 shuffled_deck Lib.Deck.empty
      in
      (updated_hand, updated_deck))
    else (hand, deck))
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

let load_texture renderer path =
  match Image.load_texture renderer path with
  | Ok texture -> texture
  | Error (`Msg e) -> failwith ("Unable to load texture: " ^ e)

(**[init] initializes window, renderer, background texture, camel texture*)
let init (enemy_asset : string) =
  begin
    match Sdl.init Sdl.Init.everything with
    | Ok () -> ()
    | Error (`Msg e) -> failwith ("Unable to initialize SDL: " ^ e)
  end;
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
        match Image.load_texture renderer "assets/camel.png" with
        | Ok texture -> texture
        | Error (`Msg e) -> failwith ("Unable to load camel texture: " ^ e)
      in
      let enemy_texture =
        match Image.load_texture renderer enemy_asset with
        | Ok texture -> texture
        | Error (`Msg e) -> failwith ("Unable to load enemy texture: " ^ e)
      in
      (renderer, (background_texture, camel_texture, enemy_texture))

let draw state renderer bg_texture camel_texture enemy_texture level =
  Sdl.render_clear renderer |> ignore;
  Level.draw_background renderer bg_texture;
  Level.draw_camel_animation state renderer bg_texture camel_texture
    enemy_texture level;
  Level.draw_enemy_animation state renderer bg_texture camel_texture
    enemy_texture level;
  Sdl.render_present renderer;
  Level.draw_camel_base renderer camel_texture;
  Level.draw_enemy_base renderer enemy_texture;
  Sdl.render_present renderer

let enemy_moves (state : Level.t) =
  let move_list = Enemy.get_moves state.enemy in
  let len = Array.length move_list in
  let index = Random.int len in
  move_list.(index)

let player_moves (state : Level.t) (hand : Lib.Card.t Lib.Deck.t) input card
    index level =
  let dmg = Lib.Card.get_dmg card in
  let def = Lib.Card.get_defend card in
  let eff = Lib.Card.get_effect card in
  let cst = Lib.Card.get_cost card in
  let sts = Camel.get_status state.player in
  if List.mem_assoc "Stun" sts then (
    print_endline "You're stunned end your turn to get rid of the stun";
    false)
  else if cst <= Camel.get_energy state.player then (
    Camel.update_animation state.player (Lib.Card.get_name card);

    Camel.update_def state.player def;
    let enemy_status = Enemy.get_status state.enemy in

    if List.mem_assoc "Weaken" enemy_status then (
      Enemy.update_hp state.enemy (dmg * 2);
      print_endline
        (Printf.sprintf "You dealt %d damage to the enemy!" (dmg * 2)))
    else (
      Enemy.update_hp state.enemy dmg;
      print_endline (Printf.sprintf "You dealt %d damage to the enemy!" dmg));

    if eff = "Stun" || eff = "Weaken" || eff = "Bleed" then
      print_endline (Printf.sprintf "You applied %s to the enemy" eff);
    Enemy.update_status state.enemy eff;
    Camel.update_energy state.player cst;
    if dmg <> 0 then
      if level = 1 then Enemy.update_animation state.enemy "snake_damaged"
      else if level = 2 then Enemy.update_animation state.enemy "bear_damaged"
      else if level = 3 then Enemy.update_animation state.enemy "human_damaged"
      else failwith "Error with drawing damaged animations";
    true)
  else
    let _ = print_endline "You don't have enough energy to play that card!!!" in
    false

let rec game (state : Level.t) (hand : Lib.Card.t Lib.Deck.t)
    (deck : Lib.Card.t Lib.Deck.t) renderer camel_texture bg_texture
    enemy_texture level =
  if Enemy.get_hp state.enemy <= 0 then
    if level = 3 then (
      print_endline "You beat the final boss!";
      Some (state, hand, deck, true))
    else (
      print_endline "You beat the enemy!";
      Some (state, hand, deck, true))
  else if Camel.get_hp state.player <= 0 then (
    print_endline "You have been defeated! Game Over.";
    None)
  else (
    print_endline "";
    Lib.Deck.print (Lib.Deck.to_list hand);
    print_endline
      "Play a card (type index) or type 'End' to end turn: \n\
       Enter 'q' to quit out of the game:";

    let input = read_line () in
    if input = "q" then (
      print_endline "You have chosen to quit the game. Goodbye!";
      None)
    else if input = "End" then (
      print_endline "";
      let updated_hand, updated_deck = draw_one hand deck in
      print_endline "You drew a card!";
      if List.mem_assoc "Bleed" state.enemy.status then (
        Enemy.update_hp state.enemy 5;
        print_endline "Enemy took five bleed damage");
      if List.mem_assoc "Bleed" (Camel.get_status state.player) then (
        Camel.update_hp state.player 10;
        print_endline "You took 10 bleed damage");
      if List.mem_assoc "Stun" state.enemy.status then (
        let max_energy = Camel.get_energy state.player - 3 in
        let max_defense = Camel.get_def state.player in
        Camel.update_def state.player (0 - max_defense);
        Camel.update_energy state.player max_energy;
        Enemy.degrade_status state.enemy;
        Camel.degrade_status state.player;
        Some (state, updated_hand, updated_deck, false))
      else
        let enemy_attack = enemy_moves state in
        Enemy.update_animation state.enemy (Enemy.get_name enemy_attack);
        let effect = Enemy.get_effect enemy_attack in
        let max_energy = Camel.get_energy state.player - 3 in
        let max_defense = Camel.get_def state.player in
        let total_damage_taken = max 0 (enemy_attack.damage - max_defense) in
        if total_damage_taken > 0 then (
          print_endline
            (Printf.sprintf "You took %d damage after defending %d!"
               total_damage_taken max_defense);
          Camel.update_animation state.player "camel_damaged")
        else print_endline "Enemy's attack was blocked!";
        if effect = "Stun" || effect = "Bleed" then
          print_endline (Printf.sprintf "The enemy applied %s to you" effect);
        Camel.update_status state.player effect;
        Camel.update_def state.player (0 - max_defense);
        Camel.update_energy state.player max_energy;
        Camel.update_hp state.player enemy_attack.damage;

        draw state renderer bg_texture camel_texture enemy_texture level;
        Enemy.degrade_status state.enemy;
        Camel.degrade_status state.player;
        Some (state, updated_hand, updated_deck, false))
    else
      try
        let index = check_conditions input hand in
        let card, updated_hand = play_card hand index in
        if player_moves state hand input card index level then
          Some (state, updated_hand, deck, false)
        else Some (state, hand, deck, false)
      with Failure msg ->
        print_endline msg;
        Some (state, hand, deck, false))

let print_intro () =
  print_endline "🔥 Welcome to Camel Caravan! ⚔️🐪⚔️ 🔥";
  print_endline "";
  print_endline "Here are the rules:";
  print_endline "1. The goal is to choose cards to play and attack the enemy.";
  print_endline
    "2. Each card has a cost, damage or defense, and possibly a special effect!";
  print_endline "3. You can draw new cards by ending your turn.";
  print_endline
    "4. Your goal is to defeat the enemy while managing your energy and health.";
  print_endline "5. Effects Descriptions:";
  print_endline "Stun: You are unable to act; Lasts 1 turn.";
  print_endline "Bleed: You take damage over time; Lasts 3 turns.";
  print_endline "Weaken: You take more damage from attacks; Lasts 3 turns.";
  print_endline "";
  print_endline "Press Enter to start the game...";

  let _ = read_line () in
  ()

let run () =
  Random.self_init ();
  try
    print_intro ();
    let rec main_loop (state : Level.t) hand deck renderer bg_texture
        camel_texture enemy_texture level =
      draw state renderer bg_texture camel_texture enemy_texture level;
      match
        game state hand deck renderer camel_texture bg_texture enemy_texture
          level
      with
      | None -> Sdl.destroy_renderer renderer
      | Some (updated_state, updated_hand, updated_deck, ended) ->
          if not ended then
            main_loop updated_state updated_hand updated_deck renderer
              bg_texture camel_texture enemy_texture level
          else Level.draw_background renderer bg_texture;
          let level = level + 1 in
          if level = 2 then (
            let enemy_texture =
              match Image.load_texture renderer "assets/bear.png" with
              | Ok texture -> texture
              | Error (`Msg e) -> failwith ("Unable to load enemy texture: " ^ e)
            in
            Level.draw_enemy_base renderer enemy_texture;
            Level.draw_camel_base renderer camel_texture;
            let state =
              Level.init_player (Camel.init_camel ()) (Enemy.init_bear ())
            in
            let full_deck =
              List.fold_right Lib.Deck.push camel1A_deck Lib.Deck.empty
            in
            let shuffled_deck = Lib.Deck.shuffle full_deck in
            let updated_hand, updated_deck =
              Lib.Deck.draw 5 shuffled_deck Lib.Deck.empty
            in
            main_loop state updated_hand updated_deck renderer bg_texture
              camel_texture enemy_texture level)
          else if level = 3 then (
            let enemy_texture =
              match Image.load_texture renderer "assets/man.png" with
              | Ok texture -> texture
              | Error (`Msg e) -> failwith ("Unable to load enemy texture: " ^ e)
            in
            Level.draw_enemy_base renderer enemy_texture;
            Level.draw_camel_base renderer camel_texture;
            let state =
              Level.init_player (Camel.init_camel ()) (Enemy.init_man ())
            in
            let full_deck =
              List.fold_right Lib.Deck.push camel1A_deck Lib.Deck.empty
            in
            let shuffled_deck = Lib.Deck.shuffle full_deck in
            let updated_hand, updated_deck =
              Lib.Deck.draw 5 shuffled_deck Lib.Deck.empty
            in
            main_loop state updated_hand updated_deck renderer bg_texture
              camel_texture enemy_texture level);
          Sdl.destroy_renderer renderer
    in

    let renderer, (bg_texture, camel_texture, enemy_texture) =
      init "assets/snake.png"
    in
    let initial_state =
      Level.init_player (Camel.init_camel ()) (Enemy.init_snake ())
    in
    let full_deck = List.fold_right Lib.Deck.push camel1A_deck Lib.Deck.empty in
    let shuffled_deck = Lib.Deck.shuffle full_deck in
    let hand, deck = Lib.Deck.draw 5 shuffled_deck Lib.Deck.empty in
    main_loop initial_state hand deck renderer bg_texture camel_texture
      enemy_texture 1
  with _ -> ()

let main () = run ()
