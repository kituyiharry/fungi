(**********************************************
 *  A Parity Game implementation
 *
 *
 *
 *
 *
 *
 **********************************************)

let monotonic x = let () = x := !x+1 in !x + 1;;
let entropy = ref 0

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
        | Label of (priority * int)
    ;;

    let labelof (Label(_, l)) = l

    let cmprands (Label(_, l)) (Label(_, r)) = (compare l r)

    let cmpprios (Priority lp) (Priority rp) = (compare lp rp)

    module NodePriority = struct 
        type t      = priority   (* The type of the internal data *)
        let compare = cmpprios
    end

    module NodeValue = struct
        type t      = node       (* The type to uniquely identify a node *)
        let compare = cmprands
    end

    (* label -> [(incominglabels * outgoinglabels * (player, priority)),...] .. *)
    (* Noted: there is duplication of priority type -> doesn't help at all *)
    (*module Graph  = Mygraph.MakeGraph(NodePriority)(NodeValue)*)
    module Graph  = Mygraph.MakeGraph(struct 
        type t = NodeValue.t
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
    ...Consultation needed *)
    let cmpplays (lf, _lt) (rf, _rt) = compare (labelof lf) (labelof rf)

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
     return (label id * internal graph)*)
    let add_node player priority game =
        let
            label = Label ((Priority (priority, player)), (monotonic entropy))
        in
            (label, Graph.add_node label game)
    ;;

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
        @@ AdjSet.filter (fun y -> AdjSet.mem y attractor) (Graph.outgoingof node game)
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
            if (cmpprios (priorityof protagonist) (priorityof foreigner)) > 0 then
                StrSet.add (protagonist, foreigner) stratset
            else
                stratset
        else
            if (cmpprios (priorityof protagonist) (priorityof foreigner)) > 0 then
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
            let (newels, accum) =
                attract attractorset (Graph.incomingof node game) player game
            in
            let (newattr, morenodes) =
                (AdjSet.add node accum, AdjSet.union newels rest)
            in
            let newstrat =  (strategy player newattr game strats)
            in
            attractor player game newattr morenodes (StrSet.union newstrat strats)
        | _ ->
            (attractorset, strats)
    ;;

    (** [buildattractor ?set:(AdjSet.t) identity player PGame.t AdjSet.t]
        A node is part of its own attractor
    *)
    let attr player startset game =
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
    ;;

    (* Cluster max priority nodes
    *)
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
        (List.rev
            (*Have to sort by the priority and not the internal representation *)
            @@ List.sort (fun ((Label (lp, _)), _) ((Label (rp, _)), _) -> cmpprios lp rp)
            @@ Graph.NodeMap.bindings nodeMap 
            (*@@ Graph.NodeMap.map(fun (_, _, label) -> label) nodeMap*)
        )
    ;;

    (*Max element of the Map but using its internal elements and not keys *)
    let max_elt nodeMap =
        List.hd (bindings nodeMap)
    ;;

    (** [ max_priority_node (PGame.t)  (Nodes.t * priority) ]
    Largest priority node in the game *)
    let max_priority_node = max_elt

    let empty_strategy    = (StrSet.empty, StrSet.empty)
    let empty_region      = (AdjSet.empty, AdjSet.empty)

    (* Union shorthand *)
    let (<->) x y = StrSet.union x y
    let (<+>) x y = AdjSet.union x y

    (** [zielonka PGame.t PGame.solution]
    Recursive algorithm which produces winning sets of the game
    https://oliverfriedmann.com/downloads/papers/recursive_lower_bound.pdf
    *)
    let rec zielonka:'a Nodes.t -> solution = fun game ->
        if Nodes.is_empty game then
            { regions=empty_region; strategy=empty_strategy; }
        else
            let node, _      = max_priority_node game in
            let i            = omega node in
            let u            = cluster node game in
            let tau          = strategy i u game StrSet.empty in
            let (a, tau')    = attr (i) u game in
            let g_a          = carve game a in
            let { regions=(w_0, w_1); strategy=(s_0, s_1); } = zielonka g_a in
            let (_wi, w_1_i) = (
                match i with
                | Even -> (w_0, w_1)
                | Odd  -> (w_1, w_0)
            ) in
            if AdjSet.is_empty w_1_i then
                let strat = match i with
                    | Even -> ((s_0 <-> tau <-> tau'), StrSet.empty)
                    | Odd  -> (StrSet.empty, (s_1 <-> tau <-> tau')) in
                { regions=((collective game), AdjSet.empty); strategy=strat }
            else
                let flip     = invert i in
                let (b, rho) = attr (flip) w_1_i game in
                let g_b      = carve game b in
                let { regions=(w_0', w_1'); strategy=(s_0', s_1') } = zielonka g_b in
                let strat' = match flip with
                    | Even -> ((rho <-> s_0' <-> s_0), s_1')
                    | Odd  -> (s_0', (rho <-> s_1' <-> s_1)) in
                { regions=(w_1' <+> b, w_0'); strategy=strat' }
    ;;

end

