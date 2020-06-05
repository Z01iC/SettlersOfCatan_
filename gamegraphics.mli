(*  A module that creates a graphical user interface by displaying all 
    the tiles on the board and drawing out all the nodes and edges with houses,
     cities or roads on them.
*)

(**[draw_board t] draws the board [t] in the terminal *) 
val draw_board : Board.t -> Node.t list -> unit

(**[draw_win wp] draws the winning display for [wp] *)
val draw_win : string -> unit

(**[rc_to_node rc] is the node index of the row,column pair [rc]
   Raises: Failure "Not a Node" if [rc] does not correspond to a node *)
val rc_to_node: (int*int) -> int

(**[rc_to_edge rc] is the index pair nodes that correspond to the edge at
   [rc] 
   Raises: Failure "Not an Edge" if the (row,col) is not an edge *)
val rc_to_edge: (int*int) -> (int*int)

(**[rc_to_tile rc] is the tile number of the tile corresponding to the 
   row,column pair [rc]
   Raises: Failure "Not a Tile" if [rc] does not correspond to a tile *)
val rc_to_tile: (int*int) -> (int)