open Tsdl.Sdl
open Deck
open Card

type t = {
  mutable hp : int;
  mutable energy : int;
  mutable status : string;
}

let create_camel h e s : t = { hp = h; energy = e; status = s }
let init_camel : t = { hp = 100; energy = 100; status = "None" }
let update_hp t x = t.hp <- x
let update_energy t x = t.energy <- x
let update_status t x = t.status <- x
let get_hp t = t.hp
let get_energy t = t.energy
let get_status t = t.status

let draw_camel row col src_width src_height r t =
  let src = Rect.create ~x:row ~y:col ~w:src_width ~h:src_height in
  (* changes the camel size & pos: (x,y) = location, (w,h) = size scaling *)
  let dest = Rect.create ~x:250 ~y:500 ~w:300 ~h:300 in
  match render_copy ~src ~dst:dest r t with
  | Ok () -> ()
  | Error (`Msg e) -> failwith ("Failed to draw camel: " ^ e)
