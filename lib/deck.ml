open Card

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

let print (deck_lst : Card.t list) =
  List.iteri
    (fun i x ->
      Printf.printf "%d -> " (i + 1);
      Card.print_card x)
    deck_lst
