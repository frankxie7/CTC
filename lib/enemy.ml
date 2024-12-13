open Tsdl.Sdl
open Tsdl_image

type m = {
  name : string;
  damage : int;
  defend : int;
  effect : string;
}

type t = {
  mutable hp : int;
  moves : m array;
  mutable status : (string * int) list;
  mutable animation : string;
}

let get_name m = m.name
let get_dmg m = m.damage
let get_effect m = m.effect

let create_move (n : string) (dmg : int) (def : int) (eff : string) : m =
  { name = n; damage = dmg; defend = def; effect = eff }

let snake_moves =
  [| create_move "bite" 7 0 "Bleed"; create_move "flick" 5 0 "None" |]

let bear_moves =
  [| create_move "roar" 7 0 "Stun"; create_move "maul" 5 0 "Bleed" |]

let man_moves =
  [|
    create_move "mohan" 30 1 "Bleed";
    create_move "chuck" 10 1 "None";
    create_move "music" 15 1 "Stun";
    create_move "cupid" 10 1 "Bleed";
  |]

let create_enemy h m : t =
  { hp = h; moves = m; status = []; animation = "idle" }

let init_snake () : t =
  { hp = 50; moves = snake_moves; status = []; animation = "idle" }

let init_bear () : t =
  { hp = 70; moves = bear_moves; status = []; animation = "idle" }

let init_man () : t =
  { hp = 100; moves = man_moves; status = []; animation = "idle" }

let get_hp t = t.hp
let get_moves t = t.moves
let get_status t = t.status
let get_animation t = t.animation
let update_animation t x = t.animation <- x
let update_hp t x = t.hp <- t.hp - x

let rec degrade_status t =
  t.status <- List.map (fun (s, i) -> (s, i - 1)) t.status;
  t.status <- List.filter (fun (s, i) -> i != 0) t.status

let update_status t s =
  if s = "Bleed" then
    if List.mem_assoc s t.status then
      t.status <- (s, 3) :: List.remove_assoc s t.status
    else t.status <- (s, 3) :: t.status
  else if s = "Weaken" then
    if List.mem_assoc s t.status then
      t.status <- (s, 3) :: List.remove_assoc s t.status
    else t.status <- (s, 3) :: t.status
  else if s = "Stun" then
    if List.mem_assoc s t.status then
      t.status <- (s, 1) :: List.remove_assoc s t.status
    else t.status <- (s, 1) :: t.status
  else ()
