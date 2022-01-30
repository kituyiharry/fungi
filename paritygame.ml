(**********************************************
 *  Start a Paritygame from our graph impl
 *  Uses graph impl
 *  Odd  player: 1
 *  Even player: 0
 **********************************************)

module PGame = struct

  (* Use integer labels and priorities *)
  module Graph = Mygraph.MakeGraph(Int)

  module AdjSet = Graph.AdjSet

  module Nodes  = Graph.NodeMap

  (* Empty Game is just an empty Graph *)
  let empty = Graph.empty


  (*
    What game size does the collision probability equal or > 50%
   *)
  let uniqlabel player = (Random.int 0x3FFF0) * (Random.int 0x3FFF0) * player

  (* Check if player is even*)
  let isevn player = ((Int.rem player 2) = 0)

  (* Adds a node as a mapping from a uniqlabel to a triple of incoming, outgoing
    and priority. Player information is contained in the label *)
  let add_node player priority game =
    let label = (uniqlabel 2) in
    if isevn player then
     (label, Graph.add_node (label) priority game)
    else
     ((label + 1), Graph.add_node (label + 1) priority game)
  ;;

  (* Add an Edge between nodes *)
  let add_edge = Graph.add_edge;;

  (* Incoming set of nodes *)
  let incomingof node game = let (inc, _, _) = Nodes.find node game in inc

  (* Outgoing set of nodes *)
  let outgoingof node game = let (_, out, _) = Nodes.find node game in out

  (* If 2 nodes have the same player *)
  let sameplayer node_a node_b = (isevn node_a) = (isevn node_b)

  (* If 2 nodes have different players *)
  let diffplayer node_a node_b = (isevn node_a) != (isevn node_b)

 (* Get the checked node outgoing set *)
 (* what if it points to same player but opposing team *)
  let hassafeoutgoing matchnode currentnode visited game =
    let outgoing = (outgoingof currentnode game) in
      let leaver = AdjSet.filter (diffplayer matchnode) outgoing in
            AdjSet.subset leaver visited
  ;;

  (*
    attractive if same player or outgoing nodes are attractive
    i.e attractive in relation to the basenode
   *)
  let attractive visited basenode game agivennode =
    (sameplayer basenode agivennode)
      ||
    (hassafeoutgoing basenode agivennode visited game)
  ;;

  (*Push incoming nodes from each*)
  (*Is an node part of its own attractor ??*)
  let attract visited incomingset basenode game =
    (*
       If its already visited in the accumulator then no need to check it
       If its in the incomingset and same player then there is a path so add
       If its outgoing nodes are also attractive then there is a path so add it
    *)
    let unvisited = AdjSet.diff incomingset visited in
      AdjSet.filter (attractive visited basenode game) unvisited
  ;;


  (* Attractor *)
  (* Get the attractor of a set of nodes *)
  let rec attractor nodelist initial game accumulator =
    (* Find the Node in the graph *)
    match nodelist with
    | node :: tail ->
      (* Concatenate the attractive non-visited incoming neighbours *)
      let morework = (
        (AdjSet.elements (attract accumulator (incomingof node game) initial game)) @ tail) in
        (* Recursively build the attractor set in the accumulator *)
        attractor morework initial game (AdjSet.add node accumulator)
    | [] -> accumulator
  ;;


  (*
  Convenience functions for printing in the REPl
   *)
  let whatplayer node =
    if isevn node then
      0
    else
      1

  let asplayerprio game node =
    let (_,_,prio) = Nodes.find node game in
      ((whatplayer node), prio)

  (* A node is part of its own attractor *)
  let buildattractor node game =
    (* Convenience method to make it printable in the REPL *)
    List.map (asplayerprio game) (AdjSet.elements
      (* Kick off the main attractor generator  *)
      (attractor [node] node game (AdjSet.add node AdjSet.empty))
    )
  ;;

end


  let p = PGame.empty;;

  (* I also assume there are no same priority nodes to make life easy *)
  let (l_2, p) = PGame.add_node 0 2 p;;
  let (l_4, p) = PGame.add_node 0 4 p;;
  let (l_6, p) = PGame.add_node 0 6 p;;
  let (l_8, p) = PGame.add_node 0 8 p;;
  let (l_3, p) = PGame.add_node 1 3 p;;
  let (l_5, p) = PGame.add_node 1 5 p;;
  let (l_7, p) = PGame.add_node 1 7 p;;
  let (l_9, p) = PGame.add_node 1 9 p;;
  (**
    A non-empty Parity game will always have a Even labeled node 2 or
    an Odd labeled node 3
  **)
  let p = PGame.add_edge l_2 l_4 p;; (* 2 connects to even and 1 attractive odd *)
  let p = PGame.add_edge l_4 l_2 p;;
  let p = PGame.add_edge l_6 l_3 p;; (* 3 connects only to even nodes *)
  let p = PGame.add_edge l_3 l_2 p;;
  let p = PGame.add_edge l_3 l_4 p;;
  let p = PGame.add_edge l_4 l_8 p;;
  let p = PGame.add_edge l_2 l_8 p;;
  let p = PGame.add_edge l_6 l_5 p;; (* 6 connects only to odd nodes *)
  let p = PGame.add_edge l_6 l_7 p;;
  let p = PGame.add_edge l_6 l_9 p;;
  let p = PGame.add_edge l_9 l_3 p;;
  let p = PGame.add_edge l_8 l_7 p;;
  let p = PGame.add_edge l_8 l_5 p;;
  let p = PGame.add_edge l_5 l_5 p;; (* 5 connects only to odd supportive nodes *)
  let p = PGame.add_edge l_5 l_7 p;;
  let p = PGame.add_edge l_5 l_9 p;;
  let p = PGame.add_edge l_7 l_7 p;; (* 7 self references *)
  let p = PGame.add_edge l_9 l_5 p;;
  let p = PGame.add_edge l_8 l_2 p;;
  (*self referencing graph*)
  (*let p = PGame.add_edge 33 33 p;;*)

  (*p = PGame.add_edge 101 303 p;;*)

  (*PGame.to_list p;;*)
