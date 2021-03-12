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

  (*type 'a t = (node list * 'a) Map.Make(NodeType).t*)
  (*type 'a t      = ( Set.Make(NodeType).t * 'a ) Map.Make(NodeType).t*)
  type 'a t    = ( Set.Make(NodeType).t * bool * 'a ) Map.Make(NodeType).t

  module NodeMap = Map.Make (NodeType)
  (*Hold adjacency list in a set structure*)
  module AdjSet  = Set.Make (NodeType)

  let empty = NodeMap.empty

  let add_node node label nodeMap = NodeMap.add node ( AdjSet.empty , true , label) nodeMap

  (* Error Handling if node isn't there *)
  let add_edge nodeFrom nodeTo nodeMap =
    let (edges, active, label) = NodeMap.find nodeFrom nodeMap in
    NodeMap.add nodeFrom ( (AdjSet.add nodeTo edges), active, label) nodeMap

  let delete_node delnode nodeMap =
    (*set active flag to false*)
    (*NodeMap.update node (fun _nodeoption ->  None) nodeMap*)
    (*NodeMap.add node ( edges, false, label) nodeMap*)
    let (edges, _active, label) = NodeMap.find delnode nodeMap in
    NodeMap.add delnode (edges, false , label) nodeMap

    (* NodeMap.filter_map (fun entry (edges, label) ->
         if entry = node then
           None
         else (
           Some ((AdjSet.remove node edges), label )
         )
       ) nodeMap
    *)
end;;
