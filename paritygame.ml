(*****************************************************************************
 *                                                                           *
 *                                                                           *
 *                A Dead Simple Parity Game Implementation                   *
 *                                                                           *
 *                                                                           *
 *****************************************************************************)

(* Caveat Emptor:

   I use floating points as way to escape the integer limit for unique nodes.
   Of course that brings its own challenges -> maybe the float init can break 
   monotonicity idk lol!

   This implementation is only meant to be simple with a focus on learning

   At the end of the day Stdlib.compare decides your fate.

   TODO: Also I think the strategy implementation may not be correct in some games
*)
open Mygraph;;

let precision         = 0.01     (* Set to precision of the universe if necessary :-) *)
let entropy           = ref 0.00
let strictmonotonic x = let () = x := !x+.precision in !x+.precision;;

module ParityGame = struct

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
    the player type and identifier - the second int makes it unique in a graph
    or set as 2 or more nodes in a graph can have the same integer priority and
    player from above ^*)
    type node =
        | Label of (priority * float)
    ;;

    let labelof (Label(_, l)) = l

    let cmprands (Label(_, l)) (Label(_, r)) = (compare l r)

    let cmpprios (Priority lp) (Priority rp) = (compare lp rp)

    (* Compare only the structural priority part of the nodes relevant to parity games  *)
    let compare  (Label ((Priority lp), _)) (Label ((Priority rp), _)) = (compare rp lp)

    module GraphNode = struct
        type t      = node       (* The type to uniquely identify a node *)
        let compare = cmprands
    end

    (* label -> [(incominglabels * outgoinglabels * (player, priority)),...] .. *)
    (* Noted: there is duplication of priority type -> doesn't help at all *)
    (* So for the graph we only compare at the random level *)
    module Graph  = MakeGraph(GraphNode)

    module AdjSet = Graph.AdjSet

    module Nodes  = Graph.NodeMap

    type t        = Graph.t

    (* Empty Game is just an empty Graph *)
    let empty = Graph.empty

    (* Basically denoting an edge where the token moves *)
    type play = (node * node)

    (* This play implicitly makes the assumption that a strategy can ONLY be used
    to pick one path - so there is no need to check the 2nd one because a player
    cannot play 2 strategies! i.e this does not properly compare the RAND parts
    ...Consultation needed *)
    let cmpplays (lf, _lt) (rf, _rt) = Stdlib.compare (labelof lf) (labelof rf)

    module StrSet = Myset.TreeSet(struct
        type t = play
        let compare = cmpplays
    end)

    (* A parity game solution is a product of the winning regions and
     corresponding strategies for each player *)
    type solution = {
        regions:  (AdjSet.t * AdjSet.t); (* W0 , W1 *)
        strategy: (StrSet.t * StrSet.t); (* [0 x -> x+1 -> ... ], [1 y -> y+1 -> ... ] *)
    }

    (** [ add_node player int PGame.t]
     Adds a node as a mapping from a uniqlabel to a triple of incoming,
     outgoing and priority. Player information is contained in the label
     this uses the underlying graph  while handling the setup boilerplate
     return (label id * internal graph)
     returns back the label in case you want to add edges *)
    let add_node player priority game =
        let
            label = Label ((Priority (priority, player)), (strictmonotonic entropy))
        in
            (label, Graph.add label game)
    ;;


    (** [ add player int PGame.t]
     like add_node but doesn't return the node *)
    let add player priority game =
        Graph.add (Label ((Priority (priority, player)), (strictmonotonic entropy))) game
    ;;


    (* Structural equality i.e Odd = Odd or Even = Even *)
    let sameplayer player_a (Label (Priority (_, player_b), _)) = player_a = player_b

    (** [diffplayer player identity bool] Structural difference i.e Odd != Even or Even != Odd *)
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
        @@ AdjSet.filter (fun y -> AdjSet.mem y attractor) (Graph.outgoingof node game)
    ;;

    let into_strat_lazy attractor game node =
        Seq.map (fun pair -> (node, pair))
        @@ AdjSet.to_seq
        @@ AdjSet.filter (fun y -> AdjSet.mem y attractor) (Graph.outgoingof node game)
    ;;

    (* Checks whether the a play can be added as a strategy for owner into the
     strategy set *)
    let validstrategy owner (protagonist, foreigner) stratset  =
        if StrSet.mem (protagonist, foreigner) stratset then stratset else 
        if sameplayer (playerof protagonist) foreigner then StrSet.add (protagonist, foreigner) stratset
        else let parity = compare protagonist foreigner in
            if sameplayer owner protagonist then
                if parity > 0 then StrSet.add (protagonist, foreigner) stratset else stratset
            else
                if parity > 0 then stratset else StrSet.add (protagonist, foreigner) stratset
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
    *)
    let strategy player attractor game stratstate = 
        StrSet.fold (validstrategy player) stratstate
        @@ StrSet.of_list
        @@ List.flatten
        @@ List.map (into_strat attractor game)
        @@ AdjSet.elements attractor (* from attractor *)
    ;;

    let lazy_strategy attractor game =
        Seq.concat
        @@ Seq.map (into_strat_lazy attractor game)
        @@ AdjSet.to_seq attractor (* from attractor *)
    ;;

    let resolve player stratstate seq =
        StrSet.fold (validstrategy player) stratstate
        @@ StrSet.of_seq seq

    (** [playerof identity player]
    Destructure the player from a label and its unique component *)
    let playerof (Label ((Priority (_, curplayer), _))) = curplayer

    (** [hassafeoutgoing AdjSet.t PGame.t identity]
    Get the checked node outgoing set
    Checks if that set has leavers that aren't controlled by current player *)
    let hassafeoutgoing attractorset game currentnode =
        AdjSet.subset (Graph.outgoingof currentnode game) attractorset
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
        let oktoadd = AdjSet.diff incomingset attractorset
            |> AdjSet.filter (attractive attractorset player game)
        in  (oktoadd, (AdjSet.union oktoadd attractorset))
    ;;

    (** [attractor player PGame.t AdjSet.t AdjSet.t AdjSet.t]
    Get the attractor nodes of a player from a node
    attractorset is the current state of the attractor
    nodeset are the unvisited nodes*)
    let rec attractor player game attractorset nodeset strats =
        match (AdjSet.take_max nodeset) with
        | (Some(node), rest) ->
            (*
                Concatenate the attractive non-visited incoming neighbours
                while ensuring they aren't treachorous
            *)
            let (newels, accum) = attract attractorset (Graph.incomingof node game) player game in
            let (newattr, morenodes) = (AdjSet.add node accum, AdjSet.union newels rest) in
            let newstrat =  (strategy player newattr game strats) in
                attractor player game newattr morenodes (StrSet.union newstrat strats)
        | _ ->
            (attractorset, strats)
    ;;

    (** [lazy_attractor player PGame.t AdjSet.t AdjSet.t AdjSet.t]
    same as attractor but lazy on getting strategies *)
    let rec lazy_attractor player game attractorset nodeset strats =
        match (AdjSet.take_max nodeset) with
        | (Some(node), rest) ->
            (*
                Concatenate the attractive non-visited incoming neighbours
                while ensuring they aren't treachorous
            *)
            let (newels, accum) = attract attractorset (Graph.incomingof node game) player game in
            let (newattr, morenodes) = (AdjSet.add node accum, AdjSet.union newels rest) in
            let newstrat =  (lazy_strategy newattr game) in
                lazy_attractor player game newattr morenodes (Seq.append newstrat strats)
        | _ ->
            (attractorset, strats)
    ;;



    (** [buildattractor ?set:(AdjSet.t) identity player PGame.t AdjSet.t]
        A node is part of its own attractor
    *)
    let attr player startset game =
        attractor player game startset startset StrSet.empty
    ;;

    (** [buildattractor ?set:(AdjSet.t) identity player PGame.t AdjSet.t]
        A node is part of its own attractor
    *)
    let lazy_attr player startset game =
        lazy_attractor player game startset startset (StrSet.to_seq StrSet.empty)
    ;;

    (** [carve PGame.t AdjSet.t PGame.t] Removes a set of nodes from a game *)
    let carve game nodeset =
        AdjSet.fold (Graph.remove) nodeset game
    ;;

    let omega (Label ((Priority (ofprio, _)), _)) =
        if ofprio mod 2 == 0 then Even else Odd
    ;;

    (* Cluster max priority nodes (same player and priority) *)
    let cluster (Label ((Priority (l, pl)),_)) game =
        let nodes = Nodes.filter (fun (Label ((Priority (r, pr)), _)) _  -> ((r = l) && (pl = pr))) game in
        Nodes.fold (fun x _y acc -> AdjSet.add x acc) nodes AdjSet.empty
    ;;

    (* Collect nodes forming the game into a set *)
    let collective game =
        (Nodes.fold
            (fun node _ neighbours -> AdjSet.add node neighbours) 
        game) AdjSet.empty
    ;;

    (*let bindings nodeMap: (AdjSet.t * AdjSet.t * node) Nodes.t =*)
    let bindings nodeMap  =
        (
            (*Have to sort by the priority and not the internal 'entropy' representation *)
            List.sort (compare)
            @@ List.map (fst)
            @@ Graph.NodeMap.bindings nodeMap
        )
    ;;

    (*Max element of the Map but using its internal elements and not keys *)
    let max_elt nodeMap =
        List.hd (bindings nodeMap)
    ;;

end

