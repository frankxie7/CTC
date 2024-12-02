open Tsdl.Sdl
open Deck
open Card
open Const

type t = {
  mutable hp : int;
  mutable energy : int;
  mutable status : string; (* mutable animation : renderer; *)
}

let create_camel h e s : t = { hp = h; energy = e; status = s }
let init_camel : t = { hp = 100; energy = 100; status = "None" }
let update_hp t x = t.hp <- t.hp - x
let update_energy t x = t.energy <- t.energy - x
let update_status t x = t.status <- x
let get_hp t = t.hp
let get_energy t = t.energy
let get_status t = t.status

(* let draw_camel r t = (* TODO : theres something to do here - we don't need to
   draw the camel if it is the idle animation, and row + frame_height
   calculations are weird*) let src = Rect.create ~x:(camel_init_height +
   frame_height) ~y:(camel_init_width + frame_width) ~w:frame_width
   ~h:frame_height in (* changes the camel size & pos: (x,y) = location, (w,h) =
   size scaling *) let dest = Rect.create ~x:camel_x ~y:camel_y
   ~w:camel_width_scaling ~h:camel_height_scaling in match render_copy ~src
   ~dst:dest r t with | Ok () -> () | Error (`Msg e) -> failwith ("Failed to
   draw camel: " ^ e) *)

(* let update_camel k dt t = (* if the card is selected *) *)
