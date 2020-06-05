
type rd = {
  is_there: bool;
  player: Player.t option;
}

type t = {
  mutable road: rd;
  index: int;
}

let make_edge n = 
  {
    road = {
      is_there = false;
      player = None;
    };
    index = n;
  }

let add_road t player = 
  t.road <- {
    is_there = true;
    player = player;
  }

let remove_road t = 
  t.road <- {
    is_there = false;
    player = None;
  }

let get_index t = 
  t.index

let get_player t =
  t.road.player