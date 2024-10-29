type m = {
  damage : int;
  defend : int;
  effect : string;
}

type t = {
  hp : int;
  moves : m list;
}

let create_move (dmg : int) (def : int) (eff : string) : m =
  { damage = dmg; defend = def; effect = eff }

let create_enemy h m : t = { hp = h; moves = m }
