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
module MakeGraph(Node: Set.OrderedType)(Label: Map.OrderedType) = struct

  (** Adjacency list graph definition **)
  type t = (
    (* Incoming nodes      Outgoing nodes         label *)
    Set.Make(Label).t * Set.Make(Label).t * Node.t
    (* Map from NodeType.t to (incoming outgoing label) *)
  ) Map.Make(Label).t

  (** Module for manipulating the Set structure holding the Adjacency list
      holding Label.t *)
  module AdjSet  = Set.Make (Label)

  (** Module for manipulating the Map (Node -> (set , set , label)) *)
  module NodeMap = Map.Make (Label)

  (** An empty graph **)
  let empty = NodeMap.empty

  (** Add a new node with its label -> ( ... , nodedata) *)
  let add_node nodekey nodedata nodeMap =
    NodeMap.add nodekey (AdjSet.empty, AdjSet.empty, nodedata) nodeMap
  ;;

  (**
      Add a directed edge [(tail)] --> [(head)] such that the tails outgoing
      set points to the heads incoming set.
  *)
  let add_edge nodeFrom nodeTo nodeMap =
    (*Find the tail of the directed edge*)
    let (fromIncoming, fromOutgoing, label) = NodeMap.find nodeFrom nodeMap in
      (*Update with outgoing*)
      let finMap = (NodeMap.add nodeFrom (fromIncoming, (AdjSet.add nodeTo fromOutgoing), label) nodeMap) in
        (*Find the head of the directed edge*)
        let (toIncoming, toOutgoing, label) = NodeMap.find nodeTo finMap in
          (*Update with incoming*)
          NodeMap.add nodeTo ((AdjSet.add nodeFrom toIncoming), toOutgoing, label) finMap
  ;;

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
        AdjSet.fold(
          (fun nodelabel updatemap ->
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
      ) (AdjSet.union incoming outgoing) []
  ;;

  (* Get adjacency list of a node *)
  let adj_set_of node nodeMap =
    let (incoming, outgoing, _label) = NodeMap.find node nodeMap in
      (AdjSet.union incoming outgoing)
  ;;

  (* Print the graph as an adjacency list *)
  (* [
      ....
      (key * label * [ adjacency list ])
      ....
  ] *)
  let render nodeMap =
    NodeMap.fold (
      fun key (_, _, label) acc  ->
        (key, label, (adj_list_of key nodeMap)) :: acc
    ) nodeMap []
  ;;

  let elt_bindings nodeMap =
    (List.rev
      @@ List.sort (fun (_, lnode) (_, rnode) -> Node.compare lnode rnode)
      @@ NodeMap.bindings
      @@ NodeMap.map(fun (_ , _, label) -> label)
    nodeMap)

  (* Max element of the Map but using its internal elements and not keys *)
  let max_elt nodeMap =
    List.hd (elt_bindings nodeMap)
  ;;

end;;
