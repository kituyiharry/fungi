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

  type 'a t = (node list * 'a) Map.Make(NodeType).t

  module NodeMap = Map.Make(NodeType)

  let empty = NodeMap.empty

  let add_node node label g = NodeMap.add node ([], label) g

  let add_edge nodeFrom nodeTo g =
    let (edges, label) = NodeMap.find nodeFrom g in
    NodeMap.add nodeFrom (nodeTo::edges, label) g

  (***
  let display = ()

  let dfs = ()
  ***)

end;;
