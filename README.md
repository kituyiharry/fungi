## Fungi (Fun-ctional G-raph Implementation)

This is an adjacency set based graph implementation with algorithms written in
a functional style. It supports various path, flow, matching and scc algorithms
with more to be added in the future. Some data structures will also be added to
support the graph (Heap (done), LC-tree, UnionFind...etc).

### Usage

to use the ocaml implementation

```ocaml
    open Fungi;;
    (* create a graph with string nodes and float edges *)
    module SGraph = Graph.MakeGraph(struct 
        type t     = string
        type edge  = float
        let compare= String.compare
    end);;

    (* empty graph *)
    let s  = SGraph.empty;;

    (* add nodes *)
    let s'  = SGraph.add "A" s;;
    let s'' = SGraph.add "B" s';;

    (* add edge without weight *)
    let s'' = SGraph.add_edge "B" "A" s'';;
    (* add directed edges (with weights on them) *)
    let s'' = SGraph.add_weight 2. "A" "B" s'';;
    (* add bidirectional weighted edges (with weights on them) *)
    let s'' = SGraph.add_weight 2. "A" "B" s'';;

    (* Use dijkstra to find the shortest path - we need the AST of our edge type
    to compute paths (i.e Float.sub, Float.min ... etc) *)
    module SPath = SGraph.Path.Compute(Graph.Biject(Float));;
    let path = SPath.dijkstra "A" "B" s'';;

    (* export graph to dot - build a serializer *)
    module Ser = struct 
        let string_of_elt = Fun.id
        let string_of_wgt = (Float.to_string)
        let elt_of_string = Fun.id
        let wgt_of_string = Float.of_string
    end;;
    module SGSer = SGraph.Serialize (Ser);;

    (* global attributes *)
    let gt   = SGSer.StyleTbl.create 1;;
    (* per edge style attributes *)
    let et   = SGSer.AttrbTbl.create 1;;
    (* per node style attributes *)
    let nt   = SGSer.AttrbTbl.create 1;;
    (* add some attributes *)
    SGSer.StyleTbl.add gt "rankdir" "LR";;
    SGSer.StyleTbl.add st "color" "green";;

    (* create a sequence of strings for the dot fil *)
    let z = SGSer.to_dot ~dir:true "toposort" gt nt et s'';;
    (* print the  graph dot sequence *)
    z |> Seq.concat |> Seq.iter (fun s -> Format.printf "%s" (s ())) ;;

```
