
(**The type of a settlement *)
type s

(**the abstract type of a port *)
type port

(**The type of a Node *)
type t

(**[make_node list n edge] returns a new node t.*)
val make_node : Tile.t list -> int -> Edge.t list -> bool -> string -> t

(** [add_settlement a p t] can add a new settlement belonging to p to t.*)
val add_settlement : string -> Player.t -> t -> unit

(** [remove_settlement] can remove the settlement on t.*)
val remove_settlement : t -> unit

(**[has_three_to_one node] is true if this node has a three to one port*)
val has_three_to_one : t -> bool

(**[has_res_port node] returns the type of resource port the node has or 
   an empty string if the node does not have a port *)
val has_res_port : t -> string

(**[give_resource dr node] give the player who has a settlement on node [node] 
   all resources corresponding to die roll [dr]*)
val give_resource : int -> t -> unit

(**[get_index t] returns the index of [t] *)
val get_index : t -> int

(**[get_tiles t] returns the neighbor tiles of [t] *)
val get_tiles : t -> Tile.t list

(**[get_edges t] returns the neighbor edges of [t] *)
val get_edges : t -> Edge.t list

(**[get_player t] is the player in [t] *)
val get_player : t -> Player.t

(**[get_settlement t] is the settlement in [t] *)
val get_settlement : t -> string

(**[has_edge pos] is the player that owns the edge at [pos]*)
val has_edge : t -> (int*int) -> Player.t option

(*[get_edge neigh node] is the edge leaving node [node] going to 
  a neighboring node with index [neigh]*)
val get_edge : int -> t -> Edge.t

(**[generateNodes ()] is a generated list of nodes indexed from 1-54 *)
val generate_nodes : Board.t -> t list

(**[generateNodes ()] is a generated list of nodes indexed from 1-54 *)
val generate_empty_nodes : unit -> t list

(**[give_resource_start] gives the player who has placed a settlement at
   node [node] all the resources from the surrounding tiles *)
val give_resource_start: t -> unit
