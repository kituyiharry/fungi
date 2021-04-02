(************************
 * Author: Harry K
 * Date  : 2021-02-19

 Graph Implementation 


 https://www.lri.fr/~filliatr/ftp/publis/ocamlgraph-tfp-8.pdf
 https://bryangarza.github.io/basic-graph-traversal-in-ocaml.html
 https://gist.github.com/theawesomestllama/7d1c0961a2c4446ef40b

************************)

(*
 * Functor to produce a graph from an ordinal type
 *)
module MakeGraph (NodeType : Map.OrderedType) = struct

  type node = NodeType

  type 'a t      = ( Set.Make(NodeType).t * Set.Make(NodeType).t * 'a ) Map.Make(NodeType).t

  module NodeMap = Map.Make (NodeType)
  (*Hold adjacency list in a set structure*)
  module AdjSet  = Set.Make (NodeType)

  let empty = NodeMap.empty

  let add_node node label nodeMap = NodeMap.add node ( AdjSet.empty, AdjSet.empty, label) nodeMap

  (*   (tail) -----> (head)  *)
  let add_edge nodeFrom nodeTo nodeMap =
    (*Find the tail of the directed edge*)
    let (fromIncoming, fromOutgoing, label) = NodeMap.find nodeFrom nodeMap in
      (*Update with outgoing*)
      let finMap = (NodeMap.add nodeFrom (fromIncoming, (AdjSet.add nodeTo fromOutgoing), label) nodeMap) in
        (*Find the head of the directed edge*)
        let (toIncoming, toOutgoing, label) = NodeMap.find nodeTo nodeMap in
          (*Update with incoming*)
          NodeMap.add nodeTo ((AdjSet.add nodeFrom toIncoming), toOutgoing, label) finMap

  (*
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
          (fun anode updatemap ->
            let (deepinc, deepout, olabel) = (NodeMap.find anode updatemap) in
            NodeMap.add anode (
              (* Might be in either *)
              (* Can i add more context to make this efficient ? *)
              (AdjSet.remove delnode deepinc), 
              (AdjSet.remove delnode deepout), 
              olabel) updatemap
          )
        ) (AdjSet.union incoming outgoing) nodeMap
      )
      (* Have i been doing math this whole time ? *)

  (* Get adjacency list of a node *)
  let adj_list_of node nodeMap =
    let (incoming, outgoing, _label) = NodeMap.find node nodeMap in
      AdjSet.fold(
        fun anode alist ->
          anode :: alist
      ) (AdjSet.union incoming outgoing) []

  (* Print the graph as an adjacency list *)
  let render nodeMap =
    NodeMap.fold (
      fun key _value acc  ->
        (key, (adj_list_of key nodeMap)) :: acc
    ) nodeMap []

end;;
