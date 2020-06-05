type trade = int*string*int*string

type command = 
  | Quit
  | Inventory
  | AddCity
  | AddSettle
  | AddRoad
  | Done
  | Help
  | Points
  | Malformed
  | TradeBank of trade 
  | TradeGreen of trade
  | TradeBlue of trade
  | TradeMagenta of trade
  | TradeYellow of trade
  | BuyCard
  | Cards
  | UseKnight
  | UseVictory
  | UseProgress

exception Empty

exception Malformed

(** [nonblank a] is helper function that checks if a string is blank *)
let nonblank string = 
  string <> ""

let to_data command = 
  match command with 
  |Quit -> ("quit",0,"",0,"")
  |Inventory -> ("inventory",0,"",0,"")
  |AddCity -> ("addcity",0,"",0,"")
  |AddSettle -> ("addsettle",0,"",0,"")
  |AddRoad -> ("addroad",0,"",0,"")
  |Done-> ("done",0,"",0,"")
  |Help -> ("help",0,"",0,"")
  |Points -> ("points",0,"",0,"")
  |Malformed -> ("malformed",0,"",0,"")
  |TradeBank (x,res1,y,res2)-> ("tradebank",x,res1,y,res2)
  |TradeGreen (x,res1,y,res2)-> ("tradegreen",x,res1,y,res2)
  |TradeBlue (x,res1,y,res2)-> ("tradeblue",x,res1,y,res2)
  |TradeMagenta (x,res1,y,res2)-> ("trademagenta",x,res1,y,res2)
  |TradeYellow (x,res1,y,res2)-> ("tradeyellow",x,res1,y,res2)
  |BuyCard -> ("buycard",0,"",0,"")
  |Cards -> ("cards",0,"",0,"")
  |UseKnight -> ("useknight",0,"",0,"")
  |UseVictory -> ("usevictory",0,"",0,"")
  |UseProgress -> ("useprogress",0,"",0,"")

(**[valid_resouces res1 res2] return true if [res1] and [res2] are
   valid resources and false otherwise *)
let valid_resources (res1:string) (res2:string) : bool=
  (res1="sheep" || res1="wheat" || res1="wood" || res1="brick" || res1="rock")
  && (res2="sheep" || res2="wheat" || res2="wood" || res2="brick" || 
      res2="rock") 

let parse str =
  try (
    match str |> String.split_on_char ' ' |> List.filter nonblank with
    | [] -> raise Empty
    | h::t -> if h = "quit" || h = "Quit" then begin
        if t = [] then Quit
        else raise Malformed
      end
      else if h = "inventory" || h = "Inventory" then begin
        if t = [] then Inventory
        else raise Malformed
      end
      else if h = "cards" || h = "Cards" then begin
        if t = [] then Cards
        else raise Malformed
      end
      else if h = "buycard" || h = "Buycard" then begin
        if t = [] then BuyCard
        else raise Malformed
      end
      else if h = "use" || h = "Use" then begin
        match t with
        | [] -> raise Malformed
        | h::t -> if (h = "knight" || h="Knight")&& t=[] then UseKnight
          else if (h = "progress"|| h= "Progress") && t=[] then UseProgress
          else if (h= "Victory" || h = "victory") && t=[] then UseVictory
          else raise Malformed
      end
      else if h = "build" || h = "Build" then begin
        match t with
        | [] -> raise Malformed
        | h::t -> if (h = "city" || h="City")&& t=[] then AddCity
          else if (h = "settlement"|| h= "Settlement") && t=[] then AddSettle 
          else if (h= "road" || h = "Road") && t=[] then AddRoad
          else raise Malformed
      end
      else if (h = "trade" || h = "Trade") then begin
        match t with
        | [] -> raise Malformed
        | h::t -> 
          if (h = "bank" || h="Bank") then 
            match t with 
            |x::res1::y::res2::t when t=[]-> 
              if(valid_resources res2 res2) then 
                TradeBank (int_of_string x,res1, int_of_string y,res2)
              else raise Malformed
            |_-> raise Malformed
          else if (h = "blue" || h="Blue")then 
            match t with 
            |x::res1::y::res2::t when t=[]-> 
              if(valid_resources res2 res2) then 
                TradeBlue (int_of_string x,res1, int_of_string y,res2)
              else raise Malformed
            |_-> raise Malformed
          else if (h = "magenta" || h="Magenta") then 
            match t with 
            |x::res1::y::res2::t when t=[]-> 
              if(valid_resources res2 res2) then 
                TradeMagenta (int_of_string x,res1, int_of_string y,res2)
              else raise Malformed
            |_-> raise Malformed
          else if (h = "green" || h="Green") then 
            match t with 
            |x::res1::y::res2::t when t=[]-> 
              if(valid_resources res2 res2) then 
                TradeGreen (int_of_string x,res1, int_of_string y,res2)
              else raise Malformed
            |_-> raise Malformed
          else if (h = "yellow" || h="Yellow") then 
            match t with 
            |x::res1::y::res2::t when t=[]-> 
              if(valid_resources res2 res2) then 
                TradeYellow (int_of_string x,res1, int_of_string y,res2)
              else raise Malformed
            |_-> raise Malformed
          else raise Malformed
      end
      else if h= "done" || h = "Done" then begin 
        if t = [] then Done
        else raise Malformed
      end
      else if h="help" || h = "Help" then begin 
        if t = [] then Help 
        else raise Malformed
      end
      else if h="points" || h = "Points" then begin 
        if t = [] then Points
        else raise Malformed
      end
      else raise Malformed)
  with 
  |Malformed -> Malformed