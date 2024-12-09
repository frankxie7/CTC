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
let throw = animate "throw" 4 19
let tackle = animate "tackle" 5 10
(* let damaged = animate "damaged" 6 1 *)

let animation_table =
  [
    ("spit", spit);
    ("idle", idle);
    ("defend", defend);
    ("stomp", stomp);
    ("throw", throw);
    ("tackle", tackle);
    (* ("damaged", damaged); *)
  ]
