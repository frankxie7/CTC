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

let game (t : Level.t) h d =
  if Enemy.get_hp t.enemy <= 0 then failwith "Game Over"
  else
    let hand_deck_tuple = draw_one h d in
    let hand = fst hand_deck_tuple in
    (* let deck = snd hand_deck_tuple in *)
    Lib.Deck.print (Lib.Deck.to_list h);
    print_endline "Play a card (type index) or End to end turn: ";
    let input = read_line () in
    let affects =
      ( Lib.Card.get_dmg (Lib.Deck.get (int_of_string input) hand),
        Lib.Card.get_defend (Lib.Deck.get (int_of_string input) hand),
        Lib.Card.get_cost (Lib.Deck.get (int_of_string input) hand),
        Lib.Card.get_effect (Lib.Deck.get (int_of_string input) hand) )
    in
    let enemy_attack =
      match Enemy.get_moves t.enemy with
      | [] -> raise (Failure "?")
      | h :: t -> h
    in
    match affects with
    | d, def, cost, eff ->
        Enemy.update_hp t.enemy d;
        let total_damage_taken = enemy_attack.damage - def in
        if total_damage_taken > 0 then
          Camel.update_hp t.player total_damage_taken

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
      game p h d;
      main_loop p h d
    end
    else log "quitting"
  in
  let players : Level.t =
    { player = Camel.init_camel; enemy = Enemy.init_enemy }
  in
  let deck = Lib.Deck.empty |> List.fold_right Lib.Deck.push camel1A_deck in
  let hand = Lib.Deck.empty |> List.fold_right Lib.Deck.push camel1A_hand in
  main_loop players hand deck;
  quit ()

let main () = run ()
