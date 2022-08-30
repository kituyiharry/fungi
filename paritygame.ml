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

  let cmpplayers l r = if l = r then 0 else -2

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

  (** [ max_priority_node (PGame.t)  (Nodes.t * priority) ]
    Largest priority node in the game *)
  let max_priority_node = Graph.max_elt

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

  (** [playerof identity player]
  Destructure the player from a label and its unique component *)
  let playerof (Priority (_, curplayer)) = curplayer

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

  (** [buildattractor ?set:(AdjSet.t) identity player PGame.t AdjSet.t]
    A node is part of its own attractor
    To print the game as a adjacency list graph in a REPL use
     List.map (asplayerprio game) @@ AdjSet.elements
     @@ ...
   *)
  let buildattractor node player ?set:(startset=(AdjSet.empty)) game =
    let istart = AdjSet.add node startset in attractor player game istart istart
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
  let cluster (Label ((Priority (l, _)),_)) game =
    AdjSet.of_list
    @@ List.map (fun (key, _) -> key)
    @@ Nodes.bindings
    @@ Nodes.filter (fun (Label ((Priority (p, _)), _)) _  -> (p = l)) game
  ;;

  let assignregion (fprio, attr, (wi, wii)) =
    (if AdjSet.subset attr wi then
      match omega fprio with
      | Even -> ((AdjSet.union wi attr), wii)
      | Odd ->  (wii, (AdjSet.union wi attr))
      else
      match omega fprio with
      | Even -> ((AdjSet.union wii attr), wi)
      | Odd ->  (wi, (AdjSet.union wii attr))
    )
  ;;

  let oppose prio (we, wo) =
    match omega prio with
    | Even -> wo
    | Odd  -> we

  (** [zielonka PGame.t (AdjSet.t * AdjSet.t)]
    Recursive algorithm which produces winning sets of the game
    https://oliverfriedmann.com/downloads/papers/recursive_lower_bound.pdf
  *)
  let rec zielonka :(AdjSet.t * AdjSet.t * priority ) Nodes.t -> (AdjSet.t * AdjSet.t) =
    fun game ->
      if Nodes.is_empty game then
        (AdjSet.empty, AdjSet.empty)
      else
        let node, prio     = max_priority_node game in
        let u              = cluster node game in
        let i              = omega prio in
        let i_attractor    = buildattractor node i ?set:(Some u) game in
        let subgame        = carve game i_attractor in
        let winsets        = zielonka subgame in
        let (w0, w1)       = assignregion (prio, i_attractor, winsets) in
        let opposition     = (oppose prio (w0, w1)) in
          if AdjSet.is_empty opposition then
            (w0, w1)
          else
        let ii             = invert i in
        let oppatrractor   = buildattractor node ii ?set:(Some opposition) game in
        let invsubgame     = carve game oppatrractor in
        let invwinsets     = zielonka invsubgame in
        let overlap        = (AdjSet.inter i_attractor oppatrractor) in
        let (w00, w11)     = assignregion (prio, overlap, invwinsets) in
          (w0, AdjSet.union w00 w11)
  ;;

end
