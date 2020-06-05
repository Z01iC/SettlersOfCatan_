(*  Consists of attributes of a list of resources, points, development card list and if this person has the longest road.*)

(** The abstract type of values representing player. *)
type t

(** The abstract type representing resources.*)
type r

(* The abstract type reresenting a a players port *)
type port

(** The abstract type representing color*)
type color

(** The abstract type representing development card*)
type card

(**[ini_card] returns the initial development cards.*)
val ini_card : card list

(**[make_player color] creates a new player with color [color] *)
val make_player : string -> t

(**[add_points t p] adds [p] points to [t] *)
val add_points : t -> int -> unit

(**[add_army t] adds one to the army of [t] *)
val add_army : t -> unit

(**[add_road t] adds one to the longest road of [t] *)
val add_road : t -> unit

(**[get_army t] is the size of the army of [t] *)
val get_army : t -> int

(**[get_longest_road t] is the length of the longest road of [t] *)
val get_longest_road : t -> int

(**[get_army_l t] is true if the army is the largest in the game or false 
   otherwise *)
val get_army_l : t -> bool

(**[get_road_l t] is true if the road is the longest in the game or false 
   otherwise *)
val get_road_l : t -> bool

(**[get_points t] is the points of [t] which is a combination of unlosable points
   from settlements/cities/development cards, and if they currently have the longest
   road and/or largest army.*)
val get_points : t -> int

(**[get_color t] is the color of player [t]  *)
val get_color : t -> color

(**[set_l_army t b] sets the value of having the largest army in [t] to [b] *)
val set_l_army : t -> bool -> unit

(**[set_l_road b t] set the value of having the longest road in [t] to [b] *)
val set_l_road : bool-> t -> unit

(**[color_to_string c] is the string representing [c] *)
val color_to_string : color -> string

(**[get_resources t] returns the resouces number list of [t]*)
val get_resources : t -> r list

(**[give_sheep t] gives the [t] sheep.*)
val give_sheep : t -> unit

(**[give_rock t] gives the player [t] rock.*)
val give_rock : t -> unit

(**[give_ brick t] gives [t] brick.*)
val give_brick : t -> unit

(**[give_sheep t] gives [t] wood.*)
val give_wood  : t -> unit

(**[give_sheep t] gives [t] wheat.*)
val give_wheat : t -> unit

(**[give_card t] gives [t] [card].*)
val give_card : card -> t -> unit

(**[give_port t threeToOne res] gives player [t] a three to one port if
   [threeToOne] and if  [res] is not empty it gives plyaer a [res] port otherwise 
   fails*)
val give_port : t -> bool -> string -> unit

(**[has_three_to_one t] returns true if the player [t] has a three to one port *)
val has_three_to_one : t -> bool

(**[has_two_to_one t res] is true if [t] has a three to one port 
   corresponding to resource [res]*)
val has_two_to_one : t-> string -> bool

(**[has_trade_res player x res] is true if [player] has [x] of [res]*)
val has_trade_res : t -> int -> string -> bool

(**[player_to_string player] is a string representation of [player]*)
val player_to_string : t -> string

(**[resources_to_string player] is a string representation of [player.resources]*)
val resources_to_string : t -> string list

(**[cards_to_string player] is a string representation of [player.card_list]*)
val cards_to_string : t -> string list

(**[subset lst1 lst2] is true if lst 2 is a subset of lst1 and false
   otherwise *)
val subset : r list -> r list -> bool

(**[rob_player t] mutates player [t] so that they lose half of their resources 
   if the number of resources they have is greater than seven*)
val rob_player : t -> unit

(**[can_build_set player] is true or false if [player] can build
   a settlement*)
val can_build_set: t -> bool 

(**[can_build_city player] is true or false if [player] can build
   a city*)
val can_build_city: t -> bool 

(**[can_build_road player] is true or false if [player] can build
   a road*)
val can_build_road: t -> bool

(**[can_buy_card player] is true or false if [player] 
   can buy a card*)
val can_buy_card: t -> bool

(**[can_use_knight player] is true or false if [player] 
   can use card Knight*)
val can_use_knight: t -> bool

(**[can_use_victory player] is true or false if [player] 
   can use card Victory*)
val can_use_victory: t -> bool

(**[can_use_progress player] is true or false if [player] 
   can use card Progress*)
val can_use_progress: t -> bool

(**[avail_card list] is true or false if there are available cards*)
val avail_card : card list -> bool

(**[build_settlement player] checks if [player] has enough resources to build
   a settlement and if they do, then removes those resources from the players
   inventory*)
val build_settlement: t->unit

(**[build_city player] checks if [player] has enough resources to build
   a city and if they do, then removes those resources from the players
   inventory*)
val build_city: t->unit

(**[build_road player] checks if [player] has enough resources to build a road,
   if they do then it removes those resources from the players inventory*)
val build_road: t-> unit

(**[buy_card player card] checks if [player] has enough resources to buy a card,
   if they do then it removes those resources from the players inventory and add
   a card to its card list*)
val buy_card: t-> card -> unit

(**[bank_trade player x res1 y res2] takes [x] of [res1] from player [player]
   and gives [y] of [res2] to player*)
val bank_trade: t -> int -> string -> int -> string -> unit

(**[take_player_trade player x res1] takes [x] of [res1] from [player] *)
val take_player_trade : t -> int -> string -> unit

(**[give_player_trade player y res2] gives [y] of [res2] to [player]*)
val give_player_trade : t -> int -> string-> unit 

(**[take_victory t] takes a victory card from [t]*)
val take_victory: t -> unit

(**[take_progress t] takes a progress card from [t]*)
val take_progress: t -> unit

(**[take_knight t] takes a knight card from [t]*)
val take_knight: t -> unit
