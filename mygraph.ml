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
    module Vertex: Set.OrderedType with type t := (elt AdjSet.set * elt AdjSet.set * elt)
    val empty: (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t
    val add: elt -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t
    val add_edge: elt -> elt -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t
    val add_all: elt -> elt list -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t
    val of_list: (elt * elt list) list -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t
    val incomingof: elt -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> (elt AdjSet.set)
    val outgoingof: elt -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> (elt AdjSet.set)
    val remove: elt -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t
    val bfs: (elt -> bool) -> elt -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> bool option
    val dfs: (elt -> bool) -> elt -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> bool option
    val adj_list_of: elt -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t -> elt list 
end

module MakeGraph(Unique: Set.OrderedType): Graph with type elt := Unique.t = struct

    type elt = Unique.t

    (** Module for manipulating the Set structure holding the Adjacency list *)
    module AdjSet  = TreeSet(Unique)

    (*  Incoming nodes  Outgoing nodes data *)
    module Vertex = struct 
        type t      = (elt AdjSet.set * elt AdjSet.set * elt)
        let compare = fun (_, _, lnode) (_, _, rnode) -> Unique.compare lnode rnode
    end

    (** Adjacency list graph definition **)
    (* Map from NodeType.t to (incoming outgoing label) *)
    type 'a t = (Vertex.t) Map.Make(Unique).t

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

end;;
