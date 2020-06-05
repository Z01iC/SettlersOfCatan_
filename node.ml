
type s = None | Settlement | City


type port = ThreeToOne of bool | TwoToRes of string | NoPort

type t = {
  neigh_tiles: Tile.t list;
  mutable settlement : s;
  mutable player : Player.t option;
  index: int;
  edges: Edge.t list;
  port: port;
}

let make_node list n edge three_to_one res_port= 
  {
    neigh_tiles = list;
    settlement = None;
    player = None;
    index = n;
    edges = edge;
    port = if three_to_one 
      then ThreeToOne true
      else if res_port != "" 
      then TwoToRes res_port
      else NoPort
  }

let add_settlement name player t = 
  try 
    match name with
    |x when x="settlement"-> t.settlement <- Settlement; 
      Player.add_points player 1;
      t.player <- Some player;
    |x when x="city"-> t.settlement <- City; 
      Player.add_points player 1;
      t.player <- Some player
    |_ -> failwith "invalid settlement type"
  with
  |Failure x -> ()

let remove_settlement t =
  t.settlement <- None; 
  t.player <- None

(**[give_resource_helper dr lst] fails if there are no tiles with that 
   die roll around a node. Otherwise it returns the resource from [lst] of
   tiles with number [dr] *)
let rec give_resource_helper (dr:int) (lst: Tile.t list)=
  match lst with 
  |[]-> failwith "none"
  |h::t-> if((Tile.get_number h) = dr) 
    then (Tile.get_resource h)
    else give_resource_helper dr t

let give_resource (dr:int) (node:t)=
  match node.player with 
  |None -> failwith "No player"
  |Some n-> 
    try 
      match (give_resource_helper dr node.neigh_tiles) with
      |"wood"->Player.give_wood n; 
        if node.settlement=City 
        then Player.give_wood n 
        else ();
      |"sheep"->Player.give_sheep n; 
        if node.settlement=City 
        then Player.give_sheep n 
        else ();
      |"wheat"->Player.give_wheat n; 
        if node.settlement=City 
        then Player.give_wheat n 
        else ();
      |"rock"->Player.give_rock n; 
        if node.settlement=City 
        then Player.give_rock n 
        else ();
      |"brick"->Player.give_brick n; 
        if node.settlement=City 
        then Player.give_brick n else ();
      |_->failwith "invalid resource type"
    with
    |Failure x->()

(**[has_three_to_one node] is true if this node has a three to one port*)
let has_three_to_one (node:t) = 
  match node.port with 
  |ThreeToOne x -> x
  |_->false

(**[has_res_port node] returns the type of resource port the node has or 
   an empty string if the node does not have a port *)
let has_res_port (node:t) = 
  match node.port with 
  |TwoToRes x -> x
  |_-> ""

(**[give_resource_start_helper] is a helper method for give_resource_start
   which takes in a [player] and a list of tiles [lst] in order to 
   give the player one of every resource from the neighboring tiles *)
let rec give_resource_start_helper (player:Player.t) (lst: Tile.t list) = 
  match lst with 
  |[]->()
  |h::t->  
    if Tile.is_there_robber h = true 
    then raise(Failure("robber is present"))
    else
      (match Tile.get_resource h with 
       |"wood"->Player.give_wood player
       |"sheep"->Player.give_sheep player
       |"wheat"->Player.give_wheat player
       |"rock"->Player.give_rock player
       |"brick"->Player.give_brick player
       |_->failwith "invalid resource type");
    give_resource_start_helper player t


let give_resource_start (node:t)= 
  match node.player with 
  |None -> failwith "No player"
  |Some n -> 
    try 
      give_resource_start_helper n node.neigh_tiles
    with 
    |Failure x-> ()

let get_index t = 
  t.index

let get_tiles t = 
  t.neigh_tiles

let get_edges t = 
  t.edges

let get_settlement t =
  match t.settlement with 
  |None -> raise(Not_found)
  |Settlement -> "settlement"
  |City -> "city"

let get_player t =
  match t.player with
  |None -> raise(Not_found)
  |Some p -> p

let rec add_nodes acc counter=
  if counter=54 
  then acc 
  else add_nodes ((make_node [] counter [] false "")::acc) (counter+1)

let generate_empty_nodes () = 
  add_nodes [] 0

(**[find_edges elist e] is the player at position [e] in [elist] or None *)
let rec find_edges elist e = 
  match elist with 
  | [] -> raise(Failure("No edge at position"))
  | h::t -> 
    if (Edge.get_index h) == e 
    then Edge.get_player h 
    else 
      find_edges t e

let has_edge node = function 
  |(x,y) -> find_edges (get_edges node) y

let rec get_edge_helper neigh edge_list = 
  match edge_list with 
  |[]->failwith"no such neighboring node"
  |h::t-> 
    if Edge.get_index h = neigh 
    then h 
    else get_edge_helper neigh t

let rec get_edge neigh node = 
  get_edge_helper neigh (get_edges node)

let generate_nodes board= 
  [make_node [Board.get_tile board 0] 0 [Edge.make_edge 1; Edge.make_edge 3] true "";
   make_node [Board.get_tile board 0] 1 [Edge.make_edge 0; Edge.make_edge 4] true "";
   make_node [Board.get_tile board 1] 2 [Edge.make_edge 3; Edge.make_edge 7] false "sheep";
   make_node [Board.get_tile board 0; Board.get_tile board 1] 3 [Edge.make_edge 0; Edge.make_edge 2; Edge.make_edge 8] false "";
   make_node [Board.get_tile board 0; Board.get_tile board 2] 4 [Edge.make_edge 1; Edge.make_edge 5; Edge.make_edge 9] false "";
   make_node [Board.get_tile board 2] 5 [Edge.make_edge 4; Edge.make_edge 10] false "rock";
   make_node [Board.get_tile board 3] 6 [Edge.make_edge 7; Edge.make_edge 12] false "";
   make_node [Board.get_tile board 1; Board.get_tile board 3] 7 [Edge.make_edge 2; Edge.make_edge 6; Edge.make_edge 13] false "sheep";
   make_node [Board.get_tile board 0; Board.get_tile board 1; Board.get_tile board 4] 8 [Edge.make_edge 3; Edge.make_edge 9; Edge.make_edge 14] false "";
   make_node [Board.get_tile board 0; Board.get_tile board 2; Board.get_tile board 4] 9 [Edge.make_edge 4; Edge.make_edge 8; Edge.make_edge 15] false "";
   make_node [Board.get_tile board 2; Board.get_tile board 5] 10 [Edge.make_edge 5; Edge.make_edge 11; Edge.make_edge 16] false "rock";
   make_node [Board.get_tile board 5] 11 [Edge.make_edge 10; Edge.make_edge 17] false "" ;
   make_node [Board.get_tile board 3] 12 [Edge.make_edge 6; Edge.make_edge 18] false "";
   make_node [Board.get_tile board 1; Board.get_tile board 3; Board.get_tile board 6] 13 [Edge.make_edge 7; Edge.make_edge 14; Edge.make_edge 19] false "";
   make_node [Board.get_tile board 1; Board.get_tile board 4; Board.get_tile board 6] 14 [Edge.make_edge 8; Edge.make_edge 13; Edge.make_edge 20] false "";
   make_node [Board.get_tile board 2; Board.get_tile board 4; Board.get_tile board 7] 15 [Edge.make_edge 9; Edge.make_edge 16; Edge.make_edge 21] false "";
   make_node [Board.get_tile board 2; Board.get_tile board 5; Board.get_tile board 7] 16 [Edge.make_edge 10; Edge.make_edge 15; Edge.make_edge 22] false "";
   make_node [Board.get_tile board 5] 17 [Edge.make_edge 11; Edge.make_edge 23] false "";
   make_node [Board.get_tile board 3; Board.get_tile board 8] 18 [Edge.make_edge 12; Edge.make_edge 19; Edge.make_edge 24] false "wood";
   make_node [Board.get_tile board 3; Board.get_tile board 6; Board.get_tile board 8] 19 [Edge.make_edge 13; Edge.make_edge 18; Edge.make_edge 25] false "";
   make_node [Board.get_tile board 4; Board.get_tile board 6; Board.get_tile board 9] 20 [Edge.make_edge 14; Edge.make_edge 21; Edge.make_edge 26] false "";
   make_node [Board.get_tile board 4; Board.get_tile board 7; Board.get_tile board 9] 21 [Edge.make_edge 15; Edge.make_edge 20; Edge.make_edge 27] false "";
   make_node [Board.get_tile board 5; Board.get_tile board 7; Board.get_tile board 10] 22 [Edge.make_edge 16; Edge.make_edge 23; Edge.make_edge 28] false "";
   make_node [Board.get_tile board 5; Board.get_tile board 10] 23 [Edge.make_edge 17; Edge.make_edge 22; Edge.make_edge 29] false "rock";
   make_node [Board.get_tile board 8] 24 [Edge.make_edge 18; Edge.make_edge 30] false "wood";
   make_node [Board.get_tile board 6; Board.get_tile board 8; Board.get_tile board 11] 25 [Edge.make_edge 19; Edge.make_edge 26; Edge.make_edge 31] false "";
   make_node [Board.get_tile board 6; Board.get_tile board 9; Board.get_tile board 11] 26 [Edge.make_edge 20; Edge.make_edge 25; Edge.make_edge 32] false "";
   make_node [Board.get_tile board 7; Board.get_tile board 9; Board.get_tile board 12] 27 [Edge.make_edge 21; Edge.make_edge 28; Edge.make_edge 33] false "";
   make_node [Board.get_tile board 7; Board.get_tile board 10; Board.get_tile board 12] 28 [Edge.make_edge 22; Edge.make_edge 27; Edge.make_edge 34] false "";
   make_node [Board.get_tile board 10] 29 [Edge.make_edge 23; Edge.make_edge 35] false "rock";
   make_node [Board.get_tile board 8; Board.get_tile board 13] 30 [Edge.make_edge 24;Edge.make_edge 31; Edge.make_edge 36] false "";
   make_node [Board.get_tile board 8; Board.get_tile board 11; Board.get_tile board 13] 31 [Edge.make_edge 25; Edge.make_edge 30; Edge.make_edge 37] false "";
   make_node [Board.get_tile board 9; Board.get_tile board 11; Board.get_tile board 14] 32 [Edge.make_edge 26; Edge.make_edge 33; Edge.make_edge 38] false "";
   make_node [Board.get_tile board 9; Board.get_tile board 12; Board.get_tile board 14] 33 [Edge.make_edge 27; Edge.make_edge 32; Edge.make_edge 39] false "";
   make_node [Board.get_tile board 10; Board.get_tile board 12; Board.get_tile board 15] 34 [Edge.make_edge 28; Edge.make_edge 35; Edge.make_edge 40] false "";
   make_node [Board.get_tile board 10; Board.get_tile board 15] 35 [Edge.make_edge 29; Edge.make_edge 41] false "";
   make_node [Board.get_tile board 13] 36 [Edge.make_edge 30; Edge.make_edge 42] true "";
   make_node [Board.get_tile board 11; Board.get_tile board 13; Board.get_tile board 16] 37 [Edge.make_edge 31; Edge.make_edge 38; Edge.make_edge 43] false "";
   make_node [Board.get_tile board 11; Board.get_tile board 14; Board.get_tile board 16] 38 [Edge.make_edge 32; Edge.make_edge 37; Edge.make_edge 44] false "";
   make_node [Board.get_tile board 12; Board.get_tile board 14; Board.get_tile board 17] 39 [Edge.make_edge 33; Edge.make_edge 40; Edge.make_edge 45] false "";
   make_node [Board.get_tile board 12; Board.get_tile board 15; Board.get_tile board 17] 40 [Edge.make_edge 34; Edge.make_edge 39; Edge.make_edge 46] false "";
   make_node [Board.get_tile board 15] 41 [Edge.make_edge 35; Edge.make_edge 47] true "";
   make_node [Board.get_tile board 13] 42 [Edge.make_edge 36; Edge.make_edge 43] true "";
   make_node [Board.get_tile board 13; Board.get_tile board 16] 43 [Edge.make_edge 37; Edge.make_edge 42; Edge.make_edge 48] false "";
   make_node [Board.get_tile board 14; Board.get_tile board 16; Board.get_tile board 18] 44 [Edge.make_edge 38; Edge.make_edge 45; Edge.make_edge 49] false "";
   make_node [Board.get_tile board 14; Board.get_tile board 17; Board.get_tile board 18] 45 [Edge.make_edge 39; Edge.make_edge 44; Edge.make_edge 50] false "";
   make_node [Board.get_tile board 15; Board.get_tile board 17] 46 [Edge.make_edge 40; Edge.make_edge 47; Edge.make_edge 51] false "";
   make_node [Board.get_tile board 15] 47 [Edge.make_edge 41; Edge.make_edge 46] true "";
   make_node [Board.get_tile board 16] 48 [Edge.make_edge 43; Edge.make_edge 49] true "";
   make_node [Board.get_tile board 16; Board.get_tile board 18] 49 [Edge.make_edge 44; Edge.make_edge 48; Edge.make_edge 52] true "";
   make_node [Board.get_tile board 17; Board.get_tile board 18] 50 [Edge.make_edge 45; Edge.make_edge 51; Edge.make_edge 53] false "wheat";
   make_node [Board.get_tile board 17] 51 [Edge.make_edge 46; Edge.make_edge 50] false "wheat";
   make_node [Board.get_tile board 18] 52 [Edge.make_edge 49; Edge.make_edge 53] false "";
   make_node [Board.get_tile board 18] 53 [Edge.make_edge 50; Edge.make_edge 52] false "";
  ]
