(**********************************************
 *  Start a Paritygame from our graph impl
 **********************************************)

(* Why ocaml, why??
   https://stackoverflow.com/questions/6518436/where-how-to-declare-the-unique-key-of-variables-in-a-compiler-written-in-ocaml *)
(*
    https://github.com/janestreet/core_kernel/search?q=Unique_Id
    https://ocaml.janestreet.com/ocaml-core/109.55.00/tmp/core_kernel/Unique_id.Int.html
 *)
module RAND = Random.State

module PGame = struct

  (* A parity game has an Odd and Even player *)
  type player =
    | Even
    | Odd
  ;;

  (* Each node in a parity game has an Integer priority *)
  type priority =
    | Priority of (int * player)
  ;;

  (* Each node is given a unique label for identification purposes consisting of
   the player type and identifier *)
  type node =
    | Label of (priority * RAND.t)
  ;;

  let labelof (Label(_, l)) = l

  let cmprands  (Label(_, l)) (Label(_, r)) = (compare l r)

  let cmpprio (Priority lp) (Priority rp) = (compare lp rp)

  (* label -> [(incominglabels * outgoinglabels * (player, priority)),...] .. *)
  (* Noted: there is duplication of priority type -> doesn't help at all *)
  module Graph  = Mygraph.MakeGraph(struct
      type t      = priority   (* The type of the internal data *)
      let compare = cmpprio
    end)
    (struct
      type t      = node   (* The type to uniquely identify a node *)
      let compare = cmprands
    end)

  module AdjSet = Graph.AdjSet

  module Nodes  = Graph.NodeMap

  type t = Graph.t

  (* Empty Game is just an empty Graph *)
  let empty = Graph.empty

  type play = (node * node)

  (* This play implicitly makes the assumption that a strategy can ONLY be used
   to pick one path - so there is no need to check the 2nd one because a player
   cannot play 2 strategies! i.e this does not properly compare the RAND parts

   Consultation needed *)
  let cmpplays (lf, _lt) (rf, _rt) = compare (labelof lf) (labelof rf)

  module StrSet = Set.Make(struct
    type t = play
    let compare = cmpplays
  end)

  (* A parity game solution is a product of the winning regions and
     corresponding strategies for each player *)
  type solution = {
    regions:  (AdjSet.t * AdjSet.t); (* W0 , W1 *)
    strategy: (StrSet.t* StrSet.t) (* [0 x -> x+1 -> ... ], [1 y -> y+1 -> ... ] *)
  }

  (** [ add_node player int PGame.t]
     Adds a node as a mapping from a uniqlabel to a triple of incoming,
     outgoing and priority. Player information is contained in the label
     this uses the underlying graph  while handling the setup boilerplate
     return (label id * internal graph)*)
  let add_node player priority game =
    let
      label    = Label ((Priority (priority, player)), (RAND.make_self_init ()))
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

  let playerof (Label (Priority (_, playeri), _)) = playeri

  let priorityof (Label ((p, _))) = p

  (** [invertplayer identity identity]
  Invert player switches between players but maintains structure *)
  let invert = function
    | Odd  -> Even
    | Even -> Odd
  ;;

  let into_strat attractor game node =
    List.map (fun pair -> (node, pair))
    @@ AdjSet.elements
    @@ AdjSet.filter (fun y -> AdjSet.mem y attractor) (outgoingof node game)
  ;;

  (* Checks whether the a play can be added as a strategy for owner into the
     strategy set *)
  let validstrategy stratset owner (protagonist, foreigner) =
    if StrSet.mem (protagonist, foreigner) stratset then
      stratset
    else if sameplayer (playerof protagonist) foreigner then
      StrSet.add (protagonist, foreigner) stratset
    else
      if sameplayer owner protagonist then
        if (cmpprio (priorityof protagonist) (priorityof foreigner)) > 0 then
          StrSet.add (protagonist, foreigner) stratset
        else
          stratset
      else
        if (cmpprio (priorityof protagonist) (priorityof foreigner)) > 0 then
          stratset
        else
          StrSet.add (protagonist, foreigner) stratset
  ;;


  (**
    * In the attractor
    * - start from max same player node
    * - keep a visited set (max is the first in this node)
    * - keep a cycle set for a node (max is also in this node)
    * - from incoming of max player node
    * - same player as attractor pointing back can be added
    * - different player must take priority value into account
    * - priority of different must be less than max to be added
    * - different player pointing back with smaller cardinality can be added
    * - cardinal priority with respect to the priority

    (*let biggest = AdjSet.max_elt attractor in*)
      (*(*let  (visited, cycle) = (AdjSet.empty, AdjSet.empty) in*)*)
        (*let inc = incomingof biggest game in*)
          (*AdjSet.fold  (fun enc acc -> *)
              (*if sameplayer player enc then *)
                (*validstrategy acc player (enc, biggest)*)
              (*else*)
                (*acc*)
          (*) inc stratstate*)

    *)
  let strategy player attractor game stratstate =
    stratstate
    |> StrSet.fold (fun ply acc -> validstrategy acc player ply)
    @@ StrSet.of_list
    @@ List.flatten
    @@ List.map (into_strat attractor game)
    @@ AdjSet.elements attractor (* from attractor *)
  ;;

  (** [playerof identity player]
  Destructure the player from a label and its unique component *)
  let playerof  (Label ((Priority (_, curplayer), _))) = curplayer

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
      AdjSet.filter (attractive attractorset player game)
      @@ AdjSet.diff incomingset attractorset
    in
    let accumulator = (AdjSet.union oktoadd attractorset)
    in
      (oktoadd, accumulator)
  ;;


  (** [attractor player PGame.t AdjSet.t AdjSet.t AdjSet.t]
  Get the attractor nodes of a player from a node
  attractorset is the current state of the attractor
  nodeset are the unvisited nodes*)
  let rec attractor player game attractorset nodeset strats =
    match (pluck nodeset) with
    | Some(node, rest) ->
        (*
         Concatenate the attractive non-visited incoming neighbours
         while ensuring they aren't treachorous
        *)
        let (newels, accum) =
          attract attractorset (incomingof node game) player game
        in
        let (newattr, morenodes) =
          (AdjSet.add node accum, AdjSet.union newels rest)
        in
        let newstrat =  (strategy player newattr game strats)
        in
          attractor player game newattr morenodes (StrSet.union newstrat strats)
    | _ -> (attractorset, strats)
  ;;

  (** [buildattractor ?set:(AdjSet.t) identity player PGame.t AdjSet.t]
    A node is part of its own attractor
   *)
  let attr player ?set:(startset=AdjSet.empty) game =
    attractor player game startset startset StrSet.empty
  ;;

  (** [carve PGame.t AdjSet.t PGame.t]
    Removes a set of nodes from a game
  *)
  let carve game nodeset =
    AdjSet.fold (Graph.delete_node) nodeset game
  ;;

  let omega (Label ((Priority (ofprio, _)), _)) =
    if ofprio mod 2 == 0 then Even else Odd

  (* Parity based on priority integer *)
  let parity ofprio =
      if (playerof ofprio) = (omega ofprio) then
       (playerof ofprio)
      else
        invert (playerof ofprio)
  ;;

  (* Cluster max priority nodes
     NB: AdjSet.of_list doesn't check for equality in  some cases
   *)
  let cluster (Label ((Priority (l, pl)),_)) game =
    AdjSet.of_list
    @@ List.map (fst)
    @@ Nodes.bindings
    @@ Nodes.filter (fun (Label ((Priority (r, pr)), _)) _  -> ((r = l) && (pl = pr))) game
  ;;

  (* Collect nodes forming the game into a set *)
  let collective game =
      (Nodes.fold
        (fun node _ neighbours -> AdjSet.add node neighbours) game)
      AdjSet.empty
  ;;

  (** [ max_priority_node (PGame.t)  (Nodes.t * priority) ]
    Largest priority node in the game *)
  let max_priority_node = Graph.max_elt
  let empty_strategy  = (StrSet.empty, StrSet.empty)
  let empty_region  = (AdjSet.empty, AdjSet.empty)

  let rec ustrat strategies =
    match strategies with
    | [] -> StrSet.empty
    | next :: rest -> StrSet.union next (ustrat rest)
  ;;

  let rec uadj adjacencies =
    match adjacencies  with
    | [] -> AdjSet.empty
    | next :: rest -> AdjSet.union next (uadj rest)
  ;;

  (* TODO: Figure out how to do Set Ops generically over the modules as bellow  *)
  (*module type SetOps = sig*)
    (*type set*)
    (*val union : set -> set -> set*)
  (*end*)
  (*let (<@>) (type a) (module S : SetOps with type set = a) (set1 : a) (set2 : a) =
    S.union set1 set2*)

  (* Union shorthand *)
  let (<+>) x y = StrSet.union x y
  let (<->) x y = AdjSet.union x y

  (** [zielonka PGame.t PGame.solution]
    Recursive algorithm which produces winning sets of the game
    https://oliverfriedmann.com/downloads/papers/recursive_lower_bound.pdf
  *)
  let rec zielonka:'a Nodes.t -> solution = fun game ->
    if Nodes.is_empty game then
      { regions=empty_region; strategy=empty_strategy; }
    else
      let node, _       = max_priority_node game in
      let i             = omega node in
      let u             = cluster node game in
      let tau           = strategy i u game StrSet.empty in
      let (a, tau')     = attr (i) ?set:(Some u) game in
      let g_a           = carve game a in
      let { regions=(w_0, w_1); strategy=(s_0, s_1); } = zielonka g_a in
        let (_w_i, w_1_i) = (
          match i with
          | Even -> (w_0, w_1)
          | Odd  -> (w_1, w_0)
        ) in
          if AdjSet.is_empty w_1_i then
            let strat = match i with
              | Even -> ((s_0 <+> tau <+> tau'), StrSet.empty)
              | Odd  -> (StrSet.empty, (s_1 <+> tau <+> tau')) in
            { regions=((collective game), AdjSet.empty); strategy=strat }
          else
            let (b, rho) = attr (invert i) ?set:(Some w_1_i) game in
            let g_b      = carve game b in
            let { regions=(w_0', w_1'); strategy=(s_0', s_1') } = zielonka g_b in
            let strat' = match invert i with
              | Even -> ((rho <+> s_0' <+> s_0), s_1')
              | Odd  -> (s_0', (rho <+> s_1' <+> s_1)) in
            { regions=(w_1' <-> b, w_0'); strategy=strat' }
  ;;

end
