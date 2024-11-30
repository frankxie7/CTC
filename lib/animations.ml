open Tsdl
open Tsdl_image
open Camel
open Enemy

type t = {
  name : string;
  col : int;
  frame_num : int;
}

type animation = t

let get_name t = t.name
let get_col t = t.col
let get_frame_num t = t.frame_num

(* distance between each col of frame *)
let col_space = 3
let row_space = 3
let animate name col frame_num : t = { name; col; frame_num }
let spit = animate "spit" 3 6
let idle = animate "idle" 1 1
let defend = animate "defend" 2 9
let stomp = animate "stomp" 1 8
let animation_table = Hashtbl.create 5

let () =
  Hashtbl.add animation_table "spit" spit;
  Hashtbl.add animation_table "idle" idle;
  Hashtbl.add animation_table "defend" defend;
  Hashtbl.add animation_table "stomp" stomp

let camel_take_damage row col src_width src_height r =
  let src =
    Sdl.Rect.create ~x:row ~y:col ~w:(src_width + 100) ~h:(src_height + 100)
  in
  (* changes the camel size & pos: (x,y) = location, (w,h) = size scaling *)
  let dest = Sdl.Rect.create ~x:camel_x ~y:camel_y ~w:350 ~h:300 in
  let t =
    match Image.load_texture r "assets/camelcamel.png" with
    | Ok texture -> texture
    | Error (`Msg e) -> failwith ("Unable to load enemy texture: " ^ e)
  in
  match Sdl.render_copy ~src ~dst:dest r t with
  | Ok () -> ()
  | Error (`Msg e) -> failwith ("Failed to draw camel: " ^ e)
