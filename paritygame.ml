(**********************************************
 *  Start a Paritygame from our graph impl
 **********************************************)

(* Why ocaml, why?? https://stackoverflow.com/questions/6518436/where-how-to-declare-the-unique-key-of-variables-in-a-compiler-written-in-ocaml *)
(* God bless Jane Street :D
    https://github.com/janestreet/core_kernel/search?q=Unique_Id
    https://ocaml.janestreet.com/ocaml-core/109.55.00/tmp/core_kernel/Unique_id.Int.html
    i hear its thread safe
 *)
module RAND: Core_kernel.Unique_id.Id = Core_kernel.Unique_id.Int63 ()

module PGame = struct

  (* A parity game has an Odd and Even player *)
  type player =
    | Even
    | Odd

  (* Each node in a parity game has an Integer priority *)
  type priority =
    | Priority of int

  (* Each node is given a unique label for identification purposes *)
  type identity =
    | Label of (player * RAND.t)

  (* label -> [(incominglabels * outgoinglabels * (player, priority)),...] *)
  module Graph  = Mygraph.MakeGraph
    (struct
      type t      = priority      (* The type of the internal data *)
      let compare = fun (Priority l) (Priority r) -> Int.compare l r
    end)
    (struct
      type t      = identity      (* The type to uniquely identify a node *)
      let compare = fun (Label (_,l)) (Label (_,r)) -> Int.compare (RAND.to_int_exn l) (RAND.to_int_exn r)
    end)

  module AdjSet = Graph.AdjSet

  module Nodes  = Graph.NodeMap

  (* Empty Game is just an empty Graph *)
  let empty = Graph.empty

  let uniqlabel _ = RAND.create();;

  (* Adds a node as a mapping from a uniqlabel to a triple of incoming,
     outgoing
    and priority. Player information is contained in the label *)
  let add_node player priority game =
    let
      label     = Label (player, (uniqlabel ()))
        and
      nodedata  = Priority priority
    in
      (label, Graph.add_node label nodedata game)
  ;;

  (* Add an Edge between nodes *)
  let add_edge = Graph.add_edge;;

  (* Incoming set of nodes *)
  let incomingof node game = let (inc, _, _) = Nodes.find node game in inc

  (* Outgoing set of nodes *)
  let outgoingof node game = let (_, out, _) = Nodes.find node game in out

  let sameplayer labela (Label (labelb, _)) = labela = labelb

  let diffplayer labela (Label (labelb, _)) = labela != labelb

 (* Get the checked node outgoing set *)
 (* Checks if that set has leavers that aren't controlled by forplayer *)
  let hassafeoutgoing visited game currentnode =
      (* Add 'sure' nodes early into the accumulator so this doesn't spook *)
      AdjSet.subset  (outgoingof currentnode game) visited
  ;;

  (*
    attractive if same player or outgoing nodes are attractive
    i.e attractive in relation to the basenode
   *)
  let attractive visited forplayer game agivennode =
      (* Controlled by the player and can reach the predecessor node *)
      (sameplayer forplayer agivennode)
        ||
      (* all outgoing members are in the accumulator *)
      (hassafeoutgoing visited game agivennode)
  ;;

  (*Push incoming nodes from each*)
  (*Is an node part of its own attractor ??*)
  let attract visited incomingset player game =
    (*
    Check for attractiveness:
       If its already visited in the accumulator then no need to check it
       If its in the incomingset and same player then there is a path so add
       If its outgoing nodes are also attractive then there is a path so add it

       It is OK to add it in the accumulator now so that later checks don't miss
       it!
    *)
    let oktoadd =
      AdjSet.diff incomingset visited |> AdjSet.filter (attractive visited player game)
    in
      (oktoadd, AdjSet.union oktoadd visited)
  ;;


  (* Attractor *)
  (* Get the attractor of a set of nodes *)
  let rec attractor player game accumulator nodelist =
    match nodelist with
    | node :: tail ->
        (*
         Concatenate the attractive non-visited incoming neighbours
         while ensuring they aren't treachorous
        *)
        let (newels, newaccumulator) =
          (attract accumulator (incomingof node game) player game)
        in
        newels
        |> AdjSet.elements
        |> (@) tail
        |> attractor player game (AdjSet.add node newaccumulator)
        (* NB: The information in the accumulator needs to be as recent as
           possible *)
    | [] -> accumulator
  ;;

  (* Convenience functions for printing in the REPl *)

  let asplayerprio game node =
    let
      (_,_, value) = Graph.NodeMap.find node game
        and
      Label (player, _rand) = node
    in
      (player, value)
  ;;

  (* A node is part of its own attractor *)
  let buildattractor node player game =
    (* Convenience method to make it printable in the REPL *)
    List.map (asplayerprio game)
    @@ AdjSet.elements
    @@ attractor player game AdjSet.empty
    @@ [node]
  ;;

end


let p = PGame.empty;;

(* I also assume there are no same priority nodes to make life easy *)
let (l_2, p)  = PGame.add_node  Even 2 p;;
let (l_4, p)  = PGame.add_node  Even 4 p;;
let (l_6, p)  = PGame.add_node  Even 6 p;;
let (l_8, p)  = PGame.add_node  Even 8 p;;
let (l_10, p) = PGame.add_node  Even 10 p;;
let (l_3, p)  = PGame.add_node  Odd  3 p;;
let (l_5, p)  = PGame.add_node  Odd  5 p;;
let (l_7, p)  = PGame.add_node  Odd  7 p;;
let (l_9, p)  = PGame.add_node  Odd  9 p;;
let (l_11,p)  = PGame.add_node  Odd  11 p;;
let (l_13,p)  = PGame.add_node  Odd  13 p;;
let (l_15,p)  = PGame.add_node  Odd  15 p;;
(**
  A non-empty Parity game will always have a Even labeled node 2 or
  an Odd labeled node 3
**)
let p = PGame.add_edge l_2  l_4   p;; (* 2 connects to even and 1 attractive odd *)
let p = PGame.add_edge l_2  l_11  p;;
let p = PGame.add_edge l_4  l_2   p;;
let p = PGame.add_edge l_4  l_8   p;;
let p = PGame.add_edge l_4  l_6   p;; (* 6 has 1 incoming odd node *)
let p = PGame.add_edge l_6  l_3   p;; (* 3 connects only to even nodes *)
let p = PGame.add_edge l_6  l_5   p;; (* 6 connects only to odd nodes *)
let p = PGame.add_edge l_6  l_7   p;;
let p = PGame.add_edge l_6  l_9   p;;
let p = PGame.add_edge l_8  l_7   p;;
let p = PGame.add_edge l_8  l_5   p;;
let p = PGame.add_edge l_8  l_2   p;;
let p = PGame.add_edge l_10 l_13  p;;
let p = PGame.add_edge l_10 l_15  p;;
let p = PGame.add_edge l_3  l_2   p;;
let p = PGame.add_edge l_3  l_4   p;;
let p = PGame.add_edge l_5  l_5   p;; (* 5 connects only to odd nodes *)
let p = PGame.add_edge l_5  l_7   p;;
let p = PGame.add_edge l_5  l_9   p;;
let p = PGame.add_edge l_7  l_7   p;; (* 7 self references *)
let p = PGame.add_edge l_7  l_10  p;;
let p = PGame.add_edge l_9  l_5   p;;
let p = PGame.add_edge l_9  l_10  p;;
let p = PGame.add_edge l_9  l_3   p;;
let p = PGame.add_edge l_11 l_8   p;;
let p = PGame.add_edge l_13 l_15  p;;
let p = PGame.add_edge l_15 l_13  p;;


(*let p = PGame.add_edge l_2 l_8 p;;*)
(*let p = PGame.add_edge l_15 l_2 p;;*)
