(**[type t] represents a list of all of the tiles in the board*)
(**RI: the index of the list represents what tile it is**)
type t = Tile.t list

(**[initial_board] becomes a pre-built standard board of type t*)
val initial_board : t

(**[rand_board] becomes a randomized board of type t*)
val rand_board : unit -> t

(**[get_tile] is a the type located at an index*)
val get_tile : t -> int -> Tile.t

(**[get_tiles_with_num] is the list of tiles with a certain int *)
val get_tiles_with_num : t -> int -> Tile.t list

(**[remove index start index lst] ist a [lst] without the element at [index]*)
val remove_index : int -> int -> 'a list -> 'a list

(**[get_index start index lst] is the entry of [lst] at [index]*)
val get_index : int -> int -> 'a list -> 'a

(**[random_resources acc lst] is [lst] but with its entries in a random order*)
val random_resources : 'a list -> 'a list -> 'a list

(**[rand_board_helper start num_lst rand_res_lst acc] is generates a 
   random board from [rand_res_lst] using the indexing of [num_lst]*)
val rand_board_helper : int -> int list -> string list -> Tile.t list -> Tile.t list

(**[grab_resource t n] is the resource of the tile at position [n] in [t] *)
val grab_resource : t -> int -> string

(**[grab_number t n] is the number of the tile at position [n] in [t] *)
val grab_num : t -> int -> int

(**[robbers_false t] sets all the robbers in [t] to false *)
val robbers_false: t -> unit