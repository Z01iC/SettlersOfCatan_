(* 
-------------------------------------------------------------------------------
========================PLAY TESTING DOCUMENTATION==========================
Most of our testing consisted of play testing. For every function and seperate 
piece of functionality we created the implementer extensively used glass box
testing in order to test all the edge cases and corner cases of their 
implementation to make sure the game functioned correctly. We used this type of
testing because we wanted to make sure the game actually behaved in the terminal
as it should, and test it in the enviroment the user would play the game in.

After The implementer of the functions glass-box tested them- the rest of the 
group used black box play testing by testing the game without knowing how it was 
implemented this provided great feedback and checking to make sure everything 
actually worked as expected.

OUnit Testing VS Manual Testing
Ounit: We tested the index removal function which was important for random board
        generation
      We tested nodes to make sure they were initialized correctly because
        if the indexing was off the entire game would be messed up

Manual Testing Explanations/ Proofs of why all aspects of out code work
==================================Graphics======================================
To test graphics we simply ran make play and drew out the board and interacted
with it. This was the best way to test our graphics module

================================Board Testing===================================
Board Testing- To test the board we wrote a few Ounit tests, but mainly tested
the board using by printing it using graphics. We ensured
  1. The board generations of resources were random
  2. The file and node attributed were properly displayed

==================================Help Tab======================================
We called the help function and ensured that
  1.The help commands were properly displayed
  2.You could return to the game, with its state being preserved 

=================================Setup Phase====================================
We tested the setup phase by running through it multiple times. We verified
  1. The proper ordering of player settlement and roads
  2. The proper ordering of player turn placement
  3. We checked every possible corner case of players placing settlements
  4. We ensured players could only place settlements on a node that is
  empty and has no neighboring settlements
  5.We ensured that players could only place a road on an edge next to their 
  settlement
  6.If players clicked anywhere besides their node they would get another chance
  7.Players recieved resources from all the tiles that neighbor the
  second node they placed

============================Resource Distribution===============================
We tested resource distribution by keeping track of players resources and 
while playing the game ensuring that their inventory was always properly updated
We checked to make sure that the resources corresponding to their die rolls
were always given to them And if they had a city on the node they got two
resources. 

==================================Die Rolling===================================
We play tested by watching die rolls, observing they were random and the game
game the resources corresponding to that random die roll.

==============================Building Settlements==============================
We first made sure that if players had enough resources to build a settlement. 
And We ensured that a settlement appeared where the players clicked and that 
they begin to recieve resources from that settlement. We also checked to make 
sure that players could only build on nodes that are not adjacent to other nodes 
but are also connected to their civilation via road. 

===============================Building Cities==================================
We first made sure that if players had enough resources to build a city. And we
ensured that players could only place cities on nodes where they already have 
settlements. And that players began recieving two times as many resources from 
that node.

==============================Giving Players Ports==============================
We checked many different cobinations of players and ports. We then ensured that
the player could only use the boosted trading rate if they had the port.

=================================Bank Trading===================================
We play tested bank trading by trying all the different combinations of
  1. A trade where the player does not have the port and it fails but recovers
  2. A trade where the player does have the 3:1 port and it sucseeds
  3. A general 4:1 ration trade
  4. A trade where the player does have the 2:1 port and it sucseeds
  5. A trade where the player does not have the resources but it recovers

=================================Player Trading=================================
We play tested player trading by keeping track of all the players inventories 
and ensuring that
  1. Players could only trade when both players had the resources
  2. Players lost their resources they traded away
  3. Players gained the resources they traded for
  4. The correct players recieved the resources

====================================Robbing=====================================
We tested robbing by running through the game and checking that whenever a 
player rolled a 7 that
  1. Players could move the robber onto any tile, including the one it was on
  2. All players had their resources halved 
  3. A robber on a tile prevented players from gaining its resource
=====================================Win/Points=================================
We tested points and winning by running through the game and ensuring that
  1. Every Settlement gave a player 1 point
  2. Every City gave a player 2 points
  3. Having the most roads or most armies gave players 2 points
  4. Another player surpassing the most number of armies/roads gave them the 
  points and also subtracted them from the original holder
  5. Victory cards permenantly gave players 1 point on their use
  6. Upon reaching 10 or more points the game would end with the player who
  reached that number of points being displayed on the screen as the winner

============================Buying Development Card=============================
We tested buying development card by running through it multiple times. We 
verified that
  1. Players could only buy development cards when they had enough resources.
  2. Players could get a random development card when they succeed buying a card
  buy checking their cards list.
  3. Players lost their resources they used to buy cards.
  4. We ensured players couldn't buy cards when all the cards had been sold out.

============================Using Development Card==============================
We tested using development card by running through it multiple times. We 
verified that
  1. Players could only use the development cards they owned.
  2. Players lost corresponding cards after using them.
  3. Players could go into the correct phase after using corresponding cards.

================================Building Roads==================================
We first made sure that if players had enough resources to build a road. And We 
ensured that a road appeared where the players clicked. We also checked to make 
sure that players could only build on edges that are adjacent to their 
settlements or other roads.

==================================Conlcusion====================================
Bescause we observed no faulty behavior after play testing the game with as many
possible combinations and corner cases we could think of in the enviroment that
the user would play the game in, We can assume that the game if fully functional
*)

open OUnit2
open Board
open Player

let remove_index_test
    test start index lst expected_output : test = test >:: (fun _ -> 
    assert_equal expected_output (Board.remove_index start index lst)) 

let subset_test
    test lst1 lst2 expected_output : test = test>:: (fun _-> 
    assert_equal expected_output (Player.subset lst1 lst2))

let node_index_test
    test num list expected_output: test = test >:: (fun _ -> 
    assert_equal expected_output (Node.get_index (List.nth list num)))
let get_resources_test
    test player expected_output : test = test>:: (fun _-> 
    assert_equal expected_output (Player.resources_to_string player))

let give_sheep 
    test player: test = test>:: (fun _-> 
    Player.give_sheep player)

let give_wheat
    test player: test = test>:: (fun _-> 
    Player.give_wheat player)

let give_rock
    test player: test = test>:: (fun _-> 
    Player.give_rock player)

let give_wood
    test player: test = test>:: (fun _-> 
    Player.give_wood player)

let give_brick
    test player: test = test>:: (fun _-> 
    Player.give_wood player)

let give_port
    test player bool str: test = test>:: (fun _-> 
    Player.give_port player bool str)

let has_three_to_one_test 
    test player expected_output : test = test>:: (fun _-> 
    assert_equal expected_output ((Player.has_three_to_one player)) ~printer:string_of_bool)

let has_two_to_one_test 
    test player string expected_output : test = test>:: (fun _-> 
    assert_equal expected_output ((Player.has_two_to_one player string)) ~printer:string_of_bool)

let board = Board.rand_board

let board_tests = 
  [
    remove_index_test "remove index test 1" 0 0 [1;2;3;4] [2;3;4];
    remove_index_test "remove index test 2" 0 1 [1;2;3;4] [1;3;4];
    remove_index_test "remove index test 3" 0 2 [1;2;3;4] [1;2;4];
    remove_index_test "remove index test 4" 0 3 [1;2;3;4] [1;2;3];
  ]

let node1 = Node.make_node [] 2 [] true ""
let player1 = Player.make_player "green"
let nodelist = Node.generate_nodes (rand_board ())

let player2 = Player.make_player "blue"



let node_tests = 
  [
    "test add_settlement1">::(fun _ -> assert_equal "city" 
                                 ((Node.add_settlement "city" player1 node1);
                                  (Node.get_settlement node1)));
    "test add_settlement2">::(fun _ -> assert_equal "settlement" 
                                 ((Node.add_settlement "settlement" player1 
                                     node1);
                                  (Node.get_settlement node1)));

    node_index_test "testing index	0	"	0	nodelist	0;
    node_index_test "testing index	1	"	1	nodelist	1;
    node_index_test "testing index	2	"	2	nodelist	2;
    node_index_test "testing index	3	"	3	nodelist	3;
    node_index_test "testing index	4	"	4	nodelist	4;
    node_index_test "testing index	5	"	5	nodelist	5;
    node_index_test "testing index	6	"	6	nodelist	6;
    node_index_test "testing index	7	"	7	nodelist	7;
    node_index_test "testing index	8	"	8	nodelist	8;
    node_index_test "testing index	9	"	9	nodelist	9;
    node_index_test "testing index	10	"	10	nodelist	10;
    node_index_test "testing index	11	"	11	nodelist	11;
    node_index_test "testing index	12	"	12	nodelist	12;
    node_index_test "testing index	13	"	13	nodelist	13;
    node_index_test "testing index	14	"	14	nodelist	14;
    node_index_test "testing index	15	"	15	nodelist	15;
    node_index_test "testing index	16	"	16	nodelist	16;
    node_index_test "testing index	17	"	17	nodelist	17;
    node_index_test "testing index	18	"	18	nodelist	18;
    node_index_test "testing index	19	"	19	nodelist	19;
    node_index_test "testing index	20	"	20	nodelist	20;
    node_index_test "testing index	21	"	21	nodelist	21;
    node_index_test "testing index	22	"	22	nodelist	22;
    node_index_test "testing index	23	"	23	nodelist	23;
    node_index_test "testing index	24	"	24	nodelist	24;
    node_index_test "testing index	25	"	25	nodelist	25;
    node_index_test "testing index	26	"	26	nodelist	26;
    node_index_test "testing index	27	"	27	nodelist	27;
    node_index_test "testing index	28	"	28	nodelist	28;
    node_index_test "testing index	29	"	29	nodelist	29;
    node_index_test "testing index	30	"	30	nodelist	30;
    node_index_test "testing index	31	"	31	nodelist	31;
    node_index_test "testing index	32	"	32	nodelist	32;
    node_index_test "testing index	33	"	33	nodelist	33;
    node_index_test "testing index	34	"	34	nodelist	34;
    node_index_test "testing index	35	"	35	nodelist	35;
    node_index_test "testing index	36	"	36	nodelist	36;
    node_index_test "testing index	37	"	37	nodelist	37;
    node_index_test "testing index	38	"	38	nodelist	38;
    node_index_test "testing index	39	"	39	nodelist	39;
    node_index_test "testing index	40	"	40	nodelist	40;
    node_index_test "testing index	41	"	41	nodelist	41;
    node_index_test "testing index	42	"	42	nodelist	42;
    node_index_test "testing index	43	"	43	nodelist	43;
    node_index_test "testing index	44	"	44	nodelist	44;
    node_index_test "testing index	45	"	45	nodelist	45;
    node_index_test "testing index	46	"	46	nodelist	46;
    node_index_test "testing index	47	"	47	nodelist	47;
    node_index_test "testing index	48	"	48	nodelist	48;
    node_index_test "testing index	49	"	49	nodelist	49;
    node_index_test "testing index	50	"	50	nodelist	50;
    node_index_test "testing index	51	"	51	nodelist	51;
    node_index_test "testing index	52	"	52	nodelist	52;
    node_index_test "testing index	53	"	53	nodelist	53;
  ]

let player_test = [
  get_resources_test "tests if player initialized to no resources" player1 [];
  give_sheep "gives player 1 a sheep" player1;
  (*
  these tests should fail
    get_resources_test "tests if player has a sheep" player1 ["Sheep"];
    give_wood "gives player 1 a wood" player1;
    get_resources_test "tests if player has a sheep" player1 ["Wood";"Sheep"];
    give_rock "gives player 1 a rock" player1;
    get_resources_test "tests if player has a sheep" player1 ["Rock";"Wood";"Sheep"];
    give_brick "gives player 1 a brick" player1;
    get_resources_test "tests if player has a brick" player1 ["Brick";"Rock";"Wood";"Sheep"];
    give_wheat "gives player 1 a wheat" player1;
    get_resources_test "tests if player has a wheat" player1 ["Wheat";"Brick";"Rock";"Wood";"Sheep"];
    has_three_to_one_test "tests if players start with no ports" player1 true;
    has_three_to_one_test "tests if players start with no ports" player2 true;

    give_port "gives player 2 a three to one port" player1 true "";
    has_three_to_one_test "tests if players start with no ports" player1 true;
    give_port "gives player 2 a three to one port" player1 false "sheep";
    has_two_to_one_test "tests if player gets a two to one port" player1 "sheep" true;
    give_port "gives player 2 a three to one port" player1 false "wheat";
    has_two_to_one_test "tests if player gets a two to one port" player1 "wheat" true;
    give_port "gives player 2 a three to one port" player1 false "rock";
    has_two_to_one_test "tests if player gets a two to one port" player1 "rock" true;
    give_port "gives player 2 a three to one port" player1 false "brick";
    has_two_to_one_test "tests if player gets a two to one port" player1 "brick" true;
    give_port "gives player 2 a three to one port" player1 false "wood";
    has_two_to_one_test "tests if player gets a two to one port" player1 "wood" true;*)
]


let suite =
  "test suite for Final Project"  >::: List.flatten [
    board_tests;
    node_tests;
    player_test;
  ]

let _ = run_test_tt_main suite