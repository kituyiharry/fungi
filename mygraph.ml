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

  (*Feels too complex for small inputs*)
  let delete_node delnode nodeMap =
    let (incoming, _outgoing, _label) = NodeMap.find delnode nodeMap in
    (* Cascade all incoming nodes and delete node from each *)
    finalMap = AdjSet.fold(
        (* Using an "accumulative map" to update where removed *)
        fun incomenode prevMap  ->
          (* Find the incoming and outgoing of the node *)
          let (i_inc, i_outgoing, label) = NodeMap.find incomenode prevMap in
            (* Update with necessary data *)
            NodeMap.add incomenode (i_inc, (AdjSet.remove delnode i_outgoing), label) prevMap
     ) incoming nodeMap (* Error happens here!! *)

    (* Do the same for outgoing *)
    (*...*)

    (* NodeMap.filter_map (fun entry (edges, label) ->
      if entry = node then
        None
    else (
      Some ((AdjSet.remove node edges), label )
         )
       ) nodeMap
    *)
end;;
