(**********************************************
 *  Start a Paritygame from our graph impl
 **********************************************)

(* Why ocaml, why??
   https://stackoverflow.com/questions/6518436/where-how-to-declare-the-unique-key-of-variables-in-a-compiler-written-in-ocaml *)
(*
    https://github.com/janestreet/core_kernel/search?q=Unique_Id
    https://ocaml.janestreet.com/ocaml-core/109.55.00/tmp/core_kernel/Unique_id.Int.html
 *)
module RAND: Core.Unique_id.Id = Core.Unique_id.Int63 ()

module PGame = struct

  (* A parity game has an Odd and Even player *)
  type player =
    | Even
    | Odd

  (* Each node in a parity game has an Integer priority *)
  type priority =
    | Priority of int

  (* Each node is given a unique label for identification purposes consisting of
   the player type and identifier *)
  type identity =
    | Label of (player * RAND.t)

  (* label -> [(incominglabels * outgoinglabels * (player, priority)),...] .. *)
  module Graph  = Mygraph.MakeGraph
    (struct
      type t      = priority      (* The type of the internal data *)
      let compare = fun (Priority l) (Priority r) -> Int.compare l r
    end)
    (struct
      type t      = identity      (* The type to uniquely identify a node *)
      let compare = fun (Label (_,l)) (Label (_,r)) -> Int.compare (RAND.to_int_exn l) (RAND.to_int_exn r)
    end)

  type t = Graph.t

  module AdjSet = Graph.AdjSet

  module Nodes  = Graph.NodeMap

  (* Empty Game is just an empty Graph *)
  let empty = Graph.empty

  (** [ add_node player int PGame.t]
     Adds a node as a mapping from a uniqlabel to a triple of incoming,
     outgoing and priority. Player information is contained in the label
     this uses the underlying graph  while handling the setup boilerplate
     return (label id * internal graph)*)
  let add_node player priority game =
    let
      label    = Label (player, RAND.create())
        and
      nodedata = Priority priority
    in
      (label, Graph.add_node label nodedata game)
  ;;

  (** [ add_edge identity identity (PGame.t)  (AdjSet.t AdjSet.t priority) Nodes.t]
     Add an Edge between nodes *)
  let add_edge = Graph.add_edge;;

  (** [ incomingof identity (PGame.t) AdjSet.t]
  Incoming set of nodes *)
  let incomingof node game = let (inc, _, _) = Nodes.find node game in inc

  (** [ incomingof identity (PGame.t) AdjSet.t]
  Outgoing set of nodes *)
  let outgoingof node game = let (_, out, _) = Nodes.find node game in out

  (* Structural equality i.e Odd = Odd or Even = Even *)
  let sameplayer player_a (Label (player_b, _)) = player_a = player_b

  (** [diffplayer player identity bool]
  Structural difference i.e Odd != Even or Even != Odd *)
  let diffplayer player_a (Label (player_b, _)) = player_a <> player_b

  (** [invertplayer identity identity]
  Invert player switches between players but maintains structure *)
  let invertplayer (Label (someplayer, itsuniqness)) = match someplayer with
  | Odd  -> Label(Even, itsuniqness)
  | Even -> Label(Odd, itsuniqness)

  (** [playerof identity player]
  Destructure the player from a label and its unique component *)
  let playerof (Label (curplayer, _)) = curplayer

  (** [pluck AdjSet.t (identity option)]
  Destructure an element from a set *)
  let pluck fromset =
    match AdjSet.max_elt_opt fromset with
    | Some(node) -> Some (node, (AdjSet.remove node fromset))
    | _ -> None

  (** [hassafeoutgoing AdjSet.t PGame.t identity]
  Get the checked node outgoing set
  Checks if that set has leavers that aren't controlled by current player *)
  let hassafeoutgoing attractorset game currentnode =
      AdjSet.subset  (outgoingof currentnode game) attractorset
  ;;

  (** [attractive AdjSet.t player PGame.t identity]
    attractive if same player or outgoing nodes are attractive
    i.e attractive in relation to the basenode
   *)
  let attractive attractorset forplayer game agivennode =
      (* Controlled by the player and can reach the predecessor node *)
      (sameplayer forplayer agivennode)
        ||
      (* all outgoing members lead into the accumulator which is also an attractor *)
      (hassafeoutgoing attractorset game agivennode)
  ;;

  (** [attract AdjSet.t AdjSet.t player PGame.t identity (AdjSet.t * AdjSet.t)]
    Check for attractiveness:
     If its already visited in the accumulated attractor then no need to check it
     If its in the incomingset and same player then its reachable so add
     If its outgoing nodes are also attractive then its reachable so add it
     It is OK to add it in the accumulator now so that later checks don't miss it!
     - Returns a pair of newly found attractive nodes and a union of that set
     with the previously accumulated attractor
  *)
  let attract attractorset incomingset player game =
    let oktoadd =
      AdjSet.diff incomingset attractorset |> AdjSet.filter (attractive attractorset player game)
    in
      (oktoadd, AdjSet.union oktoadd attractorset)
  ;;


  (** [attractor player PGame.t AdjSet.t AdjSet.t AdjSet.t]
  Get the attractor nodes of a player from a node *)
  let rec attractor player game attractorset nodeset =
    match (pluck nodeset) with
    | Some(node, rest) ->
        (*
         Concatenate the attractive non-visited incoming neighbours
         while ensuring they aren't treachorous
        *)
        let (newels, newaccumulator) =
          attract attractorset (incomingof node game) player game
        in
          attractor player game (AdjSet.add node newaccumulator) (AdjSet.union newels rest)
    | _ -> attractorset
  ;;

  (* Convenience functions for printing in the REPl *)

  (** [asplayerprio PGame.t (identity * priority)]
  helper to show parity game as a graph without the random generated ids *)
  let asplayerprio game node =
    let
      (_,_, value) = Graph.NodeMap.find node game
        and
      Label (player, _rand) = node
    in
      (player, value)
  ;;

  (** [buildattractor identity player PGame.t AdjSet.t]
    A node is part of its own attractor
    To print the game as a adjacency list graph in a REPL use
     List.map (asplayerprio game) @@ AdjSet.elements
     @@ ...
   *)
  let buildattractor node player game =
    let startset = (AdjSet.add node AdjSet.empty) in
      attractor player game startset startset
  ;;

  (** [carve PGame.t AdjSet.t PGame.t]
    Removes nodes from a game
  *)
  let carve game nodeset =
    AdjSet.fold (
      fun node curgame ->
        Graph.delete_node node curgame
    ) nodeset game
  ;;

  (** [zielonka PGame.t (AdjSet.t * AdjSet.t)]
    Recursive algorithm which produces winning sets of the game
    https://oliverfriedmann.com/downloads/papers/recursive_lower_bound.pdf
  *)
  let rec zielonka: (AdjSet.t * AdjSet.t * priority) Nodes.t -> (AdjSet.t * AdjSet.t) = fun game ->
    if Nodes.is_empty game then
      (AdjSet.empty, AdjSet.empty)
    else
      let (playerident, _) = Nodes.max_binding game in
      let myattractor      = buildattractor playerident (playerof playerident) game in
      let subgame          = (carve game myattractor) in
      let (_, w1)          = zielonka subgame in
      if AdjSet.is_empty w1 then
        (myattractor, w1)
      else
        let oppatrractor   = buildattractor playerident  (playerof (invertplayer playerident)) game in
        let invsubgame     = (carve game oppatrractor) in
        let (w0, w1_ii)    = zielonka invsubgame in
        (w0, (AdjSet.union w1_ii oppatrractor))
  ;;

end
