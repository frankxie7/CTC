type 'a t = 'a list

exception Empty

let empty = []

let is_empty = function
  | [] -> true
  | _ -> false

let push = List.cons

let peek = function
  | [] -> raise Empty
  | x :: _ -> x

let pop = function
  | [] -> raise Empty
  | _ :: s -> s

let size = List.length
let to_list = Fun.id

let rec get (n : int) = function
  | [] -> raise Empty
  | t :: s -> if n = 1 then t else get (n - 1) s

let rec remove (n : int) = function
  | [] -> raise Empty
  | s :: d -> if n = 1 then d else s :: remove (n - 1) d
