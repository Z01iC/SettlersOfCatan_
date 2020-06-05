type r = Wood | Brick | Wheat | Sheep | Rock | Desert

type t = {
  number:int;
  resource:r;
  mutable robber:bool;
}


let make_tile (number:int) (resource:string) (robber:bool)= 
  {
    number=number;
    resource=
      (match resource with 
       |x when x="wood" -> Wood
       |x when x="brick" -> Brick
       |x when x="sheep" -> Sheep
       |x when x="wheat" -> Wheat
       |x when x="rock" -> Rock
       |x when x="desert" -> Desert
       |_-> failwith "invalid resource"
      );
    robber=robber;
  }

let add_robber tile =
  tile.robber <- true

let remove_robber tile = 
  tile.robber <- false

let is_there_robber tile = 
  if tile.robber = true 
  then true 
  else false

let get_resource (tile: t)= 
  match tile.resource with
  |Wood->"wood"
  |Brick->"brick"
  |Sheep->"sheep"
  |Wheat->"wheat"
  |Rock->"rock"
  |Desert->"desert"

let string_of_tile t = 
  if t.robber = true 
  then "robber" 
  else
    get_resource t

let get_number (tile: t)= 
  tile.number