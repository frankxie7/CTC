open Tsdl
open Tsdl_image
open Camel
open Enemy
open Const

type t = {
  name : string;
  col : int;
  total_frames : int;
}

type animation = t

let rec lookup k = function
  | [] -> failwith "Error: Not found"
  | (k', v) :: t -> if k = k' then v else lookup k t

let get_name t k =
  let x = lookup k t in
  x.name

let get_col t k =
  let x = lookup k t in
  x.col

let get_frame_num t k =
  let x = lookup k t in
  x.total_frames

let animate name col total_frames : t = { name; col; total_frames }
let spit = animate "spit" 2 17
let idle = animate "idle" 1 1
let defend = animate "defend" 3 13
let stomp = animate "stomp" 1 15
let throw = animate "throw" 4 17
let tackle = animate "tackle" 5 1

let animation_table =
  [
    ("spit", spit);
    ("idle", idle);
    ("defend", defend);
    ("stomp", stomp);
    ("throw", throw);
    ("tackle", tackle);
  ]

(* let camel_take_damage row col src_width src_height r = let src =
   Sdl.Rect.create ~x:row ~y:col ~w:(src_width + 100) ~h:(src_height + 100) in
   (* changes the camel size & pos: (x,y) = location, (w,h) = size scaling *)
   let dest = Sdl.Rect.create ~x:camel_x ~y:camel_y ~w:Const.camel_width_scaling
   ~h:Const.camel_height_scaling in let t = match Image.load_texture r
   "assets/camelcamel.png" with | Ok texture -> texture | Error (`Msg e) ->
   failwith ("Unable to load enemy texture: " ^ e) in match Sdl.render_copy ~src
   ~dst:dest r t with | Ok () -> () | Error (`Msg e) -> failwith ("Failed to
   draw camel: " ^ e) *)
