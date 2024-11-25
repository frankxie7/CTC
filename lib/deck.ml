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

let rec get (n : int) = function
  | [] -> raise Empty
  | t :: s -> if n = 1 then t else get (n - 1) s

let rec remove (n : int) = function
  | [] -> raise Empty
  | s :: d -> if n = 1 then d else s :: remove (n - 1) d

let shuffle deck =
  let deck_array = Array.of_list deck in
  let n = Array.length deck_array in
  for i = n - 1 downto 1 do
    let j = Random.int (i + 1) in
    let temp = deck_array.(i) in
    deck_array.(i) <- deck_array.(j);
    deck_array.(j) <- temp
  done;
  Array.to_list deck_array

let rec draw n deck hand =
  if n = 0 || is_empty deck then (hand, deck)
  else
    let card = peek deck in
    draw (n - 1) (pop deck) (push card hand)

let print (deck_lst : Card.t list) =
  List.iteri
    (fun i x ->
      Printf.printf "%d -> " (i + 1);
      Card.print_card x)
    deck_lst
