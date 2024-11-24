type m = {
  damage : int;
  defend : int;
  effect : string;
}

type t = {
  mutable hp : int;
  moves : m list;
}

let create_move (dmg : int) (def : int) (eff : string) : m =
  { damage = dmg; defend = def; effect = eff }

let hyena_moves = [ create_move 7 0 "None"; create_move 5 0 "None" ]
let create_enemy h m : t = { hp = h; moves = m }
let init_enemy : t = { hp = 50; moves = hyena_moves }
