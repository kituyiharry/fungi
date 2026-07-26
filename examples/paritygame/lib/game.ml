(*****************************************************************************
 *                                                                           *
 *                                                                           *
 *            A Dead Simple Functional Parity Game Implementation            *
 *                                                                           *
 *                                                                           *
 *****************************************************************************)

(* Caveat Emptor: This implementation is only meant to be simple with a focus on learning 
   For reference please cite this repository if you are using it for your own
   purposes!
*)
open Fungi.Graph  ;;
open Fungi.Treeset;;

let entropy           = ref 0
let strictmonotonic x = let () = incr x in !x;;

module ParityGame = struct

    (* A parity game has an Odd and Even player *)
    type player =
        | Odd
        | Even
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

    (* Compare only the structural priority part of the nodes relevant to parity games  *)
    let compare  (Label ((Priority lp), _)) (Label ((Priority rp), _)) = (compare rp lp)

    module GraphNode = struct
        type t      = node       (* The type to uniquely identify a node *)
        type edge   = unit
        let compare = cmprands
    end

    (* label -> [(incominglabels * outgoinglabels * (player, priority)),...] .. *)
    module Graph  = MakeGraph(GraphNode) 

    module AdjSet = Graph.AdjSet

    module Nodes  = Graph.NodeMap

    type   t      = node Graph.t

    (* Empty Game is just an empty Graph *)
    let empty     = Graph.empty

    (* Basically denoting an edge where the token moves *)
    type play     = (node * node)

    (* integer priority value of a node *)
    let valueof (Label((Priority (d, _)), _)) = d

    (* compare two paths of a strategy *)
    let cmpplays (lf, _lt) (rf, _rt) = Int.compare (labelof lf) (labelof rf)

    (* A set of edges which a token follows in a graph *)
    module Strategy = struct
        type t      = play
        let compare = cmpplays
    end

    module StrSet = TreeSet(Strategy)

    (* A parity game solution is a product of the winning regions and
     corresponding strategies for each player *)
    type solution = {
        regions:  (GraphNode.t AdjSet.set * GraphNode.t AdjSet.set); (* W0 , W1 *)
        strategy: (play StrSet.set * play StrSet.set); (* [0 x -> x+1 -> ... ], [1 y -> y+1 -> ... ] *)
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

    let priorityof (Label ((p, _))) = p

    (** [invertplayer identity identity]
    Invert player switches between players but maintains structure *)
    let invert = function
        | Odd  -> Even
        | Even -> Odd
    ;;

    (** [playerof identity player]
    Destructure the player from a label and its unique component *)
    let playerof (Label ((Priority (_, curplayer), _))) = curplayer

    (** [attr player startset game]  Standard i-attractor of [startset]: the set
        of nodes from which [player] can force the token into [startset]. Computed
        as a least fixpoint - a [player]-owned node joins when it has some
        successor already inside; an opponent node joins when all of its
        (non-empty) successors are inside. Returns the attractor SET only.

        A correct positional attractor STRATEGY needs the move that carries each
        node one step toward the target, which a set-level fixpoint cannot see, so
        strategies are synthesised separately (see [Solve.winning_strategy]). The
        old [validstrategy]/[strategy] heuristic tried to recover it post-hoc and
        produced unsound strategies (it could drop a player's own forced move); it
        has been removed. *)
    let attr player startset game =
        let rec fix current =
            let added = Nodes.fold (fun v _ acc ->
                if AdjSet.mem v current then acc
                else
                    let outs = Graph.outgoingof v game in
                    if playerof v = player then
                        if AdjSet.exists (fun w -> AdjSet.mem w current) outs
                        then AdjSet.add v acc else acc
                    else
                        if (not (AdjSet.is_empty outs)) && AdjSet.subset outs current
                        then AdjSet.add v acc else acc
            ) game AdjSet.empty in
            if AdjSet.is_empty added then current else fix (AdjSet.union current added)
        in fix startset
    ;;

    (** [carve PGame.t AdjSet.t PGame.t] Removes a set of nodes from a game *)
    let carve game nodeset =
        AdjSet.fold (Graph.remove) nodeset game
    ;;

    let omega (Label ((Priority (ofprio, _)), _)) =
        if ofprio mod 2 == 0 then Even else Odd
    ;;

    (* Collect nodes forming the game into a set *)
    let collective game =
        (Nodes.fold
            (fun node _ neighbours -> AdjSet.add node neighbours) 
        game) AdjSet.empty
    ;;

    (* Cluster max priority nodes (same player and priority) *)
    let cluster (Label ((Priority (l, pl)),_)) game =
        (Nodes.filter (fun (Label ((Priority (r, pr)), _)) _  -> ((r = l) && (pl = pr))) game)
        |> collective
    ;;

    (*let bindings nodeMap: (AdjSet.t * AdjSet.t * node) Nodes.t =*)
    let bindings nodeMap  =
        (*Have to sort by the priority and not the internal 'entropy' representation *)
        List.sort (compare)
        @@ List.map (fst)
        @@ Graph.NodeMap.bindings nodeMap
    ;;

    (*Max element of the Map but using its internal elements and not keys *)
    let max_elt nodeMap =
        List.hd (bindings nodeMap)
    ;;

end
