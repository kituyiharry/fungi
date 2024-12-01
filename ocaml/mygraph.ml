(******************************************************************************
*     Simplest Functional Directed Graph Implementation (Adjacency list)      *
*                                                                             *
* https://www.lri.fr/~filliatr/ftp/publis/ocamlgraph-tfp-8.pdf                *
* https://bryangarza.github.io/blog/basic-graph-traversal-in-ocaml            *
* https://gist.github.com/theawesomestllama/7d1c0961a2c4446ef40b              *
* https://github.com/backtracking/ocamlgraph                                  *
*                                                                             *
* TODO: use Weak module instead of label-key duplication                      *
*                                                                             *
*******************************************************************************)
open Myset;;

let (let*) = Option.bind

module type Graph = sig 
    type 'a t
    type elt
    module NodeMap: Map.S with type key := elt
    module AdjSet: TSet with type t := elt
    type adj := (elt AdjSet.set * elt AdjSet.set * elt)
    module Vertex: Set.OrderedType with type t := adj
    val empty: adj NodeMap.t
    val add: elt -> adj NodeMap.t -> adj NodeMap.t
    val add_edge: elt -> elt -> adj NodeMap.t -> adj NodeMap.t
    val add_all: elt -> elt list -> adj NodeMap.t -> adj NodeMap.t
    val of_list: (elt * elt list) list -> adj NodeMap.t -> adj NodeMap.t
    val incomingof: elt -> adj NodeMap.t -> (elt AdjSet.set)
    val outgoingof: elt -> adj NodeMap.t -> (elt AdjSet.set)
    val remove: elt -> adj NodeMap.t -> adj NodeMap.t
    val bfs: (elt -> bool) -> elt -> adj NodeMap.t -> bool option
    val dfs: (elt -> bool) -> elt -> adj NodeMap.t -> bool option
    val adj_list_of: elt -> adj NodeMap.t -> elt list
    val transpose: adj NodeMap.t -> adj NodeMap.t
end

module MakeGraph(Unique: Set.OrderedType): Graph with type elt = Unique.t = struct

    type elt = Unique.t

    (** Module for manipulating the Set structure holding the Adjacency list *)
    module AdjSet  = TreeSet(Unique)
    type   adj     = (elt AdjSet.set * elt AdjSet.set * elt)

    (*  Incoming nodes  Outgoing nodes data *)
    module Vertex  = struct 
        type t      = adj
        let compare = fun (_, _, lnode) (_, _, rnode) -> Unique.compare lnode rnode
    end

    (** Adjacency list graph definition **)
    (* Map from NodeType.t to (incoming outgoing label) *)
    type 'a t      = (Vertex.t) Map.Make(Unique).t

    (** Module for manipulating the Map (Node -> (set , set , label)) *)
    module NodeMap = Map.Make(Unique)

    (** An empty graph **)
    let empty      = NodeMap.empty

    (** Add a new node with its label -> ( ... , nodedata) *)
    let add nodekey nodeMap =
        NodeMap.add nodekey (AdjSet.empty, AdjSet.empty, nodekey) nodeMap
    ;;

    (**
    Add a directed edge [(tail)] --> [(head)] such that the tails outgoing
    set points to the heads incoming set.

    (*Find the tail of the directed edge*)
      (*Update with outgoing*)
        (*Find the head of the directed edge*)
          (*Update with incoming*)
    *)
    let add_edge nodeFrom nodeTo nodeMap =
        (NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label) = x in 
            Some (fromIncoming, (AdjSet.add nodeTo fromOutgoing), label)) nodeMap)
        |>  NodeMap.update nodeTo (fun x -> let* (toIncoming, toOutgoing, tolabel) = x in
            Some ((AdjSet.add nodeFrom toIncoming), (toOutgoing), tolabel))
    ;;

    let rec add_all nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | nodeTo :: rest -> add_edge nodeFrom nodeTo (add_all nodeFrom rest nodeMap)
    ;;

    (* Creates a graph given a node and outgoing edge, incoming edges will be
       resolved naturally. All nodes should already be available in the map  *)
    let rec of_list adjList nodeMap = match adjList with
        | [] -> nodeMap
        | (nodeFrom, nodeJoinList) :: rest ->
            add_all nodeFrom nodeJoinList (of_list rest nodeMap)
    ;;

    (** [ incomingof identity (Graph.t) AdjSet.t]
    Incoming set of nodes *)
    let incomingof node game = let (inc, _, _) = NodeMap.find node game in inc

    (** [ incomingof identity (Graph.t) AdjSet.t]
    Outgoing set of nodes *)
    let outgoingof node game = let (_, out, _) = NodeMap.find node game in out

    (** Removes a node from the graph *)
    let remove delnode nodeMap =
        let (incoming, outgoing, _label) = (NodeMap.find delnode nodeMap) in
        NodeMap.remove delnode (AdjSet.fold (fun nodelabel updatemap ->
            NodeMap.update nodelabel (fun x -> 
                let* (deepinc, deepout, deeplabel) = x in
                    Some (AdjSet.remove delnode deepinc, AdjSet.remove delnode deepout, deeplabel)
            ) updatemap
        ) (AdjSet.union incoming outgoing) nodeMap)
    ;;

    (*breadth first search starting from start node applying f until returns true*)
    let bfs f start game = 
        let que     = Queue.create () in
        let _       = Queue.add start que in
        let visited = AdjSet.singleton start in
        let rec iter vis nxt =
            let* label = Queue.take_opt nxt in
            if f label then
                Some true
            else
                let out =  outgoingof label game in 
                let _   = AdjSet.iter_inorder (fun x -> Queue.add x nxt) (AdjSet.diff out vis) in
                iter (AdjSet.union out vis) nxt
        in iter visited que
    ;;

    (*depth first search starting from start node applying f until returns true*)
    let dfs f start game =
        let stck    = Stack.create () in
        let _       = Stack.push start stck in
        let visited = AdjSet.singleton start in
        let rec iter vis nxt =
            let* label = Stack.pop_opt nxt in
            if f label then
                Some true
            else
                let out = outgoingof label game in 
                let _   = AdjSet.iter_inorder (fun x -> Stack.push x nxt) (AdjSet.diff out vis) in
                iter (AdjSet.union out vis) nxt
        in iter visited stck
    ;;

    (* Get adjacency list of a node *)
    let adj_list_of node nodeMap =
        let (incoming, outgoing, _label) = NodeMap.find node nodeMap in
        AdjSet.fold (
            fun anode alist -> anode :: alist
        )  (AdjSet.union incoming outgoing) []
    ;;

    let transpose nodeMap =
        NodeMap.map (fun (inc, out, label) -> (out, inc, label)) nodeMap
    ;;

    type state = { label: Unique.t; mutable slot: Unique.t option };;

    (** Tarjans SCC algorithm *)
    let tarjan nodeMap =
        let visited    = AdjSet.empty in
        let invar      = Stack.create () in
        NodeMap.fold (fun key (_inc, out, _label) visit ->
            if (AdjSet.mem key visit) then visit else  
                let space = AdjSet.add key out in
                (** why is a postorder traversal recommended ?? *)
                let _     = AdjSet.iter_postorder (fun elt ->
                    let c = AdjSet.(singleton elt |> AdjSet.union @@ AdjSet.of_seq 
                        (Seq.map (fun { label;_ } -> label) (Stack.to_seq invar))) in
                    let _ = dfs (fun x -> let loop = (AdjSet.mem x c) in
                        if loop then
                            (** Cycle found - iter and update mins and halt dfs *)
                            (*backtrack*)
                            (*Stack.iter (fun { slot; _ } -> slot := () )*)
                            true
                        else
                            (** Update the stack *)
                            let _ = Stack.push {label=x; slot=Some x} invar in
                            false
                    ) elt nodeMap in ()
                ) space in AdjSet.union space visit
        ) nodeMap visited
    ;;

end;;
