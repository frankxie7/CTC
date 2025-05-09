open Tsdl
open Tsdl_image
open Camel
open Enemy

type t = {
  name : string;
  col : int;
  total_frames : int;
}

type animation = t

let get_name t k =
  let rec lookup k = function
    | [] -> failwith "Error: Get_name function: Animation not found"
    | (k', v) :: t -> if k = k' then v else lookup k t
  in
  let x = lookup k t in
  x.name

let get_col t k =
  let rec lookup k = function
    | [] -> failwith "Error: Get_col function: Animation not found"
    | (k', v) :: t -> if k = k' then v else lookup k t
  in
  let x = lookup k t in
  x.col

let get_frame_num t k =
  let rec lookup k = function
    | [] -> failwith "Error: Frame_num function: Animation not found"
    | (k', v) :: t -> if k = k' then v else lookup k t
  in
  let x = lookup k t in
  x.total_frames

let animate name col total_frames : t = { name; col; total_frames }
let spit = animate "spit" 2 17
let idle = animate "idle" 1 1
let defend = animate "defend" 3 13
let stomp = animate "stomp" 1 15
let throw = animate "throw" 4 19
let tackle = animate "tackle" 5 10
let camel_damaged = animate "camel_damaged" 6 3

let camel_animation_table =
  [
    ("spit", spit);
    ("idle", idle);
    ("defend", defend);
    ("stomp", stomp);
    ("throw", throw);
    ("tackle", tackle);
    ("camel_damaged", camel_damaged);
  ]

let flick = animate "flick" 1 5
let bite = animate "bite" 2 13
let snake_damaged = animate "snake_damaged" 3 3
let idle = animate "idle" 1 1

let snake_animation_table =
  [
    ("flick", flick);
    ("idle", idle);
    ("bite", bite);
    ("snake_damaged", snake_damaged);
  ]

let roar = animate "roar" 1 13
let maul = animate "maul" 2 17
let idle = animate "idle" 1 1
let bear_damaged = animate "bear_damaged" 3 3

let bear_animation_table =
  [
    ("idle", idle);
    ("roar", roar);
    ("maul", maul);
    ("bear_damaged", bear_damaged);
  ]

let chuck = animate "chuck" 1 34
let cupid = animate "cupid" 3 35
let mohan = animate "mohan" 4 34
let music = animate "music" 2 29
let idle = animate "idle" 1 1
let human_damaged = animate "human_damaged" 5 3

let human_animation_table =
  [
    ("idle", idle);
    ("mohan", mohan);
    ("music", music);
    ("chuck", chuck);
    ("cupid", cupid);
    ("human_damaged", human_damaged);
  ]
