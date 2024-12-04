open Tsdl.Sdl
open Deck
open Card
open Const

type t = {
  mutable hp : int;
  mutable energy : int;
  mutable defense : int;
  mutable status : string;
  mutable animation : string;
}

let init_camel : t =
  { hp = 100; energy = 3; defense = 0; status = "None"; animation = "idle" }

let update_def t x = t.defense <- t.defense + x
let update_hp t x = t.hp <- t.hp - x
let update_energy t x = t.energy <- t.energy - x
let update_status t x = t.status <- x
let update_animation t s = t.animation <- s
let get_hp t = t.hp
let get_energy t = t.energy
let get_status t = t.status
let get_animation t = t.animation
