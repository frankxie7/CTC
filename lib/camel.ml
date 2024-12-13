open Tsdl.Sdl
open Deck
open Card

type t = {
  mutable hp : int;
  mutable energy : int;
  mutable defense : int;
  mutable status : (string * int) list;
  mutable animation : string;
}

let init_camel () : t =
  { hp = 100; energy = 3; defense = 0; status = []; animation = "idle" }

let update_def t x = t.defense <- t.defense + x
let update_hp t x = t.hp <- t.hp - x
let update_energy t x = t.energy <- t.energy - x

let rec degrade_status t =
  t.status <- List.map (fun (s, i) -> (s, i - 1)) t.status;
  t.status <- List.filter (fun (s, i) -> i != 0) t.status

let update_status t s =
  if s = "Bleed" then
    if List.mem_assoc s t.status then
      t.status <- (s, 3) :: List.remove_assoc s t.status
    else t.status <- (s, 3) :: t.status
  else if s = "Stun" then
    if List.mem_assoc s t.status then
      t.status <- (s, 2) :: List.remove_assoc s t.status
    else t.status <- (s, 2) :: t.status
  else ()

let update_animation t s = t.animation <- s
let get_def t = t.defense
let get_hp t = t.hp
let get_energy t = t.energy
let get_status t = t.status
let get_animation t = t.animation
