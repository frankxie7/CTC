open Tsdl.Sdl
open Tsdl_image
open Const

type m = {
  name : string;
  damage : int;
  defend : int;
  effect : string;
}

type t = {
  mutable hp : int;
  moves : m array;
  mutable animation : string;
}

let get_name m = m.name
let get_dmg m = m.damage

let create_move (n : string) (dmg : int) (def : int) (eff : string) : m =
  { name = n; damage = dmg; defend = def; effect = eff }

let snake_moves =
  [| create_move "bite" 7 0 "Bleed"; create_move "flick" 5 0 "None" |]

let bear_moves =
  [| create_move "roar" 7 0 "Stun"; create_move "maul" 5 0 "Bleed" |]

let man_moves =
  [|
    create_move "mohan" 10 1 "None";
    create_move "chuck" 10 1 "None";
    create_move "music" 10 1 "None";
    create_move "cupid" 10 1 "None";
  |]

let create_enemy h m : t = { hp = h; moves = m; animation = "idle" }
let init_snake () : t = { hp = 20; moves = snake_moves; animation = "idle" }
let init_bear () : t = { hp = 20; moves = bear_moves; animation = "idle" }
let init_man () : t = { hp = 20; moves = man_moves; animation = "idle" }
let get_hp t = t.hp
let get_moves t = t.moves
let get_animation t = t.animation
let update_animation t x = t.animation <- x
let update_hp t x = t.hp <- t.hp - x
