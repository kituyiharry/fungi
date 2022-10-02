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
    | Priority of (int * player)

  (* Each node is given a unique label for identification purposes consisting of
   the player type and identifier *)
  type identity =
    | Label of (priority * RAND.t)
  ;;

  let cmprands  (Label(_, l)) (Label(_, r)) = Int.compare (RAND.to_int_exn l)  (RAND.to_int_exn r)

  let cmpprio (Priority lp) (Priority rp) = (compare lp rp)

  (* label -> [(incominglabels * outgoinglabels * (player, priority)),...] .. *)
  (* Noted: there is duplication of priority type -> doesn't help at all *)
  module Graph  = Mygraph.MakeGraph
    (struct
      type t      = priority   (* The type of the internal data *)
      let compare = cmpprio
    end)
    (struct
      type t      = identity   (* The type to uniquely identify a node *)
      let compare = cmprands
    end)

  module AdjSet = Graph.AdjSet

  module Nodes  = Graph.NodeMap

  type t = Graph.t

  (* Empty Game is just an empty Graph *)
  let empty = Graph.empty

  (** [ add_node player int PGame.t]
     Adds a node as a mapping from a uniqlabel to a triple of incoming,
     outgoing and priority. Player information is contained in the label
     this uses the underlying graph  while handling the setup boilerplate
     return (label id * internal graph)*)
  let add_node player priority game =
    let
      label    = Label((Priority (priority, player)), RAND.create())
        and
      nodedata = (Priority (priority, player))
    in
      (label, Graph.add_node label nodedata game)
  ;;

  (** [ add_edge identity identity (PGame.t)  (AdjSet.t AdjSet.t priority) Nodes.t]
     Add an Edge between nodes *)
  let add_edge = Graph.add_edge;;

  let add_edge_all = Graph.add_all;;

  let use_adjlist_desc = Graph.from_list_description;;

  (** [ incomingof identity (PGame.t) AdjSet.t]
  Incoming set of nodes *)
  let incomingof node game = let (inc, _, _) = Nodes.find node game in inc

  (** [ incomingof identity (PGame.t) AdjSet.t]
  Outgoing set of nodes *)
  let outgoingof node game = let (_, out, _) = Nodes.find node game in out

  (* Structural equality i.e Odd = Odd or Even = Even *)
  (* Which player do we consider!!! *)
  let sameplayer player_a (Label (Priority (_, player_b), _)) = player_a = player_b

  (** [diffplayer player identity bool]
  Structural difference i.e Odd != Even or Even != Odd *)
  let diffplayer player_a (Label (Priority (_, player_b), _)) = player_a <> player_b

  (** [invertplayer identity identity]
  Invert player switches between players but maintains structure *)
  let invert = function
    | Odd  -> Even
    | Even -> Odd
  ;;

  (** [playerof identity player]
  Destructure the player from a label and its unique component *)
  let playerof (Priority (_, curplayer)) = curplayer

  (** [pluck AdjSet.t (identity option)]
  Destructure an element from a set *)
  let pluck fromset =
    match AdjSet.max_elt_opt fromset with
    | Some(node) -> Some (node, (AdjSet.remove node fromset))
    | _ -> None
  ;;

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
      AdjSet.diff incomingset attractorset
      |> AdjSet.filter (attractive attractorset player game)
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

  (** [buildattractor ?set:(AdjSet.t) identity player PGame.t AdjSet.t]
    A node is part of its own attractor
    To print the game as a adjacency list graph in a REPL use
     List.map (asplayerprio game) @@ AdjSet.elements
     @@ ...
   *)
  let buildattractor withnode player ?set:(startset=AdjSet.empty) game =
    match withnode with
    | Some(node) ->
        let istart = AdjSet.add node startset in
          attractor player game istart istart
    | _ ->
        attractor player game startset startset
  ;;

  (** [carve PGame.t AdjSet.t PGame.t]
    Removes nodes from a game
  *)
  let carve game nodeset =
    AdjSet.fold (Graph.delete_node) nodeset game
  ;;

  let omega (Priority (ofprio, _)) =
    if ofprio mod 2 == 0 then Even else Odd

  (* Parity based on priority integer *)
  let parity ofprio =
      if (playerof ofprio) = (omega ofprio) then
       (playerof ofprio)
      else
        invert (playerof ofprio)
  ;;

  (* Cluster max priority nodes *)
  let cluster (Label ((Priority (l, pl)),_)) game =
    AdjSet.of_list
    @@ List.map (fun (key, _) -> key)
    @@ Nodes.bindings
    @@ Nodes.filter (fun (Label ((Priority (r, pr)), _)) _  -> ((r = l) && (pl = pr))) game
  ;;

  (* Collect nodes forming the game into a set based on controlling player *)
  let collective: player option -> ((AdjSet.t * AdjSet.t * priority) Nodes.t) ->
    AdjSet.t = fun plyr game ->
      match plyr with
      | Some(cntrl) -> (Nodes.fold
        (fun node _ neighbours ->
          if sameplayer cntrl node then
            AdjSet.add node neighbours
          else
            neighbours
        ) game) AdjSet.empty
      | _ -> (Nodes.fold
        (fun node _ neighbours ->
          AdjSet.add node neighbours) game) AdjSet.empty
  ;;

  (* Elect(?) nodes forming the game into a set based on controller and priority *)
  let elective: player option -> ((AdjSet.t * AdjSet.t * priority) Nodes.t) ->
    AdjSet.t = fun plyr game ->
      match plyr with
      | Some(cntrl) -> (Nodes.fold
        (fun ((Label (p, _)) as node)  _ neighbours ->
          if (parity p) = cntrl then
            AdjSet.add node neighbours
          else
            neighbours
        ) game) AdjSet.empty
      | _ -> (Nodes.fold
        (fun node _ neighbours ->
          AdjSet.add node neighbours) game) AdjSet.empty
  ;;

  (** [ max_priority_node (PGame.t)  (Nodes.t * priority) ]
    Largest priority node in the game *)
  let max_priority_node = Graph.max_elt

  let assign:
    (identity * player * AdjSet.t *  (AdjSet.t * AdjSet.t * priority) Nodes.t * (AdjSet.t * AdjSet.t)) ->
      (AdjSet.t * AdjSet.t * AdjSet.t) =
  fun (maxnode, _iplyr, maxnodeattr, fullgame, (winset0, winset1)) ->
    let (_, out, _ ) = Nodes.find maxnode fullgame in
      if AdjSet.subset out winset0 then
        (AdjSet.union maxnodeattr winset0, winset1, winset1)
      else if AdjSet.subset out winset1 then
        (winset0, AdjSet.union maxnodeattr winset1, winset0)
      else
        (winset0, winset1, maxnodeattr)
  ;;

  let anynonempty (w0, w1) =
    if AdjSet.is_empty w0 then w1 else w0
  ;;

  (** [zielonka PGame.t (AdjSet.t * AdjSet.t)]
    Recursive algorithm which produces winning sets of the game
    https://oliverfriedmann.com/downloads/papers/recursive_lower_bound.pdf

    Notes:
      - Why is the  last 'switcheroo' needed
      - Does building the attractor really need a node! Seems to  cause issues
      but during development earlier it was requested.
      -
  *)
  let rec zielonka:(AdjSet.t * AdjSet.t * priority) Nodes.t -> (AdjSet.t * AdjSet.t) =
    fun game ->
    if Nodes.is_empty game then
      (AdjSet.empty, AdjSet.empty)
    else
      let node, prio = max_priority_node game in
      let i          = omega prio in
      let u          = cluster node game in
      let a          = buildattractor None  (i) ?set:(Some u) game in
      let g_a        = carve game a in
      let (w_0, w_1) = zielonka g_a in
      let (_w_i, w_1_i) = (
        match i with
         | Even -> (w_0, w_1)
         | Odd  -> (w_1, w_0)
      ) in
    if AdjSet.is_empty w_1_i then
      ((collective None game), AdjSet.empty)
    else
      let b            = buildattractor None (invert i) ?set:(Some w_1_i) game in
      let g_b          = carve game b in
      let (w_0, w_1)   = zielonka g_b in
      (AdjSet.union w_1 b, w_0)
  ;;

  (**
     Break this ??
  *)
  let rec solve:(AdjSet.t * AdjSet.t * priority) Nodes.t -> (AdjSet.t * AdjSet.t) =
    fun game ->
    if Nodes.is_empty game then
      (AdjSet.empty, AdjSet.empty)
    else
    let node, prio  = max_priority_node game in
    let u           = cluster node game in
    let i           = omega prio in
    let a           = buildattractor None i ?set:(Some u) game in
    let subgame     = carve game a in
    let (w0, w1)    = (solve subgame) in
    let (wi, w_1_i) = match i with
      | Even -> (w0, w1)
      | Odd  -> (w1, w0) in
    let (w', w'', w1_i) = assign (node, i, a, game, (wi, w_1_i)) in
      if AdjSet.is_empty w1_i then
        (w', w'')
      else
        (anynonempty (w', w''), w1_i)

end
