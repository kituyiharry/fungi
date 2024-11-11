(************************
 * @author Harry K kituyiharry@github.com
 * @date   2021-02-19

 Simplest Graph Implementation (Adjacency list)

 https://www.lri.fr/~filliatr/ftp/publis/ocamlgraph-tfp-8.pdf
 https://bryangarza.github.io/basic-graph-traversal-in-ocaml.html
 https://gist.github.com/theawesomestllama/7d1c0961a2c4446ef40b
************************)

(**
 * Functor to produce a graph from any ordinal type
 * using Node as the data and labels as key for identifying the node
 **)

(*module MakeGraph(Node: Myset.Ord)(Label: Myset.Ord) = struct*)
module MakeGraph(Label: Myset.Ord) = struct

    (* A given set of nodes *)
    type elt = Myset.TreeSet (Label).t

    (*  Incoming nodes  Outgoing nodes data *)
    module Vertex = struct 
        type t = (elt * elt * Label.t)
        let compare = fun (_, _, lnode) (_, _, rnode) -> Label.compare lnode rnode
    end

    (** Adjacency list graph definition **)
    (* Map from NodeType.t to (incoming outgoing label) *)
    type t = (Vertex.t) Map.Make(Label).t

    (** Module for manipulating the Set structure holding the Adjacency list
      holding Label.t 
   *)
    module AdjSet  = Myset.TreeSet (Label)

    (** Module for manipulating the Map (Node -> (set , set , label)) *)
    module NodeMap = Map.Make (Label)

    (** An empty graph **)
    let empty = NodeMap.empty

    (** Add a new node with its label -> ( ... , nodedata) *)
    let add_node nodekey nodeMap =
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
        let (fromIncoming, fromOutgoing, label) = NodeMap.find nodeFrom nodeMap in
        let finMap = (NodeMap.add nodeFrom (fromIncoming, (AdjSet.add nodeTo fromOutgoing), label) nodeMap) in
        let (toIncoming, toOutgoing, label) = NodeMap.find nodeTo finMap in
            NodeMap.add nodeTo (
                (AdjSet.add nodeFrom toIncoming), toOutgoing, label) 
            finMap
    ;;

    let rec add_all nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | nodeTo :: rest -> add_edge nodeFrom nodeTo (add_all nodeFrom rest nodeMap)
    ;;

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

    (*
    * Removes a node from the graph
    *
    * I realized since a node can be in the incoming or outgoing (or both),
    * then a function over the union of both sets while removing in both
    * still works and is simple enough
    *
    * This way i don't have to do an O(n) on both nodes
    *)
    let delete_node delnode nodeMap =
        let (incoming, outgoing, _label) = (NodeMap.find delnode nodeMap) in
        NodeMap.remove delnode (
            AdjSet.fold ((fun nodelabel updatemap ->
                let (deepinc, deepout, deeplabel) = (NodeMap.find nodelabel updatemap) in
                NodeMap.add nodelabel (
                    (AdjSet.remove delnode deepinc),
                    (AdjSet.remove delnode deepout),
                    deeplabel
                ) updatemap
            )
            ) (AdjSet.union incoming outgoing) nodeMap
        )
    ;;

    (* Get adjacency list of a node *)
    let adj_list_of node nodeMap =
        let (incoming, outgoing, _label) = NodeMap.find node nodeMap in
        AdjSet.fold (
            fun anode alist ->
            anode :: alist
        )  (AdjSet.union incoming outgoing) []
    ;;

    (* Get adjacency list of a node *)
    let adj_set_of node nodeMap =
        let (incoming, outgoing, _label) = NodeMap.find node nodeMap in
        (AdjSet.union incoming outgoing)
    ;;
end;;
