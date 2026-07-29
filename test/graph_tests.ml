open Fungi
open Alcotest

module G = Graph.MakeGraph (struct
    type t      = string
    type edge   = float
    let compare = String.compare
end)

module P = G.Path.Compute (Graph.Adapt (Float))

let cost_of = function
    | `Val f  -> f
    | `Inf    -> infinity
    | `NegInf -> neg_infinity
    | `Nan    -> nan
;;

let members set = List.sort compare (G.AdjSet.to_list set)

(* nodes and cardinality *)
let structure _cx =
    let g = G.empty |> G.add "A" |> G.add "B" |> G.add "C" in
    check int "three nodes" 3 (G.cardinal g)
;;

(* a directed edge shows up in the tail's out-set and the head's in-set *)
let directed_edge _cx =
    let g = G.empty |> G.add "A" |> G.add "B" |> G.add_edge "A" "B" in
    check bool "has_edge A->B"           true  (G.has_edge "A" "B" g);
    check bool "no reverse edge B->A"    false (G.has_edge "B" "A" g);
    check (list string) "A out-set"  ["B"] (members (G.outgoingof "A" g));
    check (list string) "B in-set"   ["A"] (members (G.incomingof "B" g));
    check int "out-degree of A" 1 (G.outdeg "A" g)
;;

(* edge weights are persistent: deriving a graph must not mutate the original *)
let weights_persistent _cx =
    let g0 = G.empty |> G.add "A" |> G.add "B" in
    let g1 = G.add_weight 2.0 "A" "B" g0 in
    check (option (float 0.0)) "original graph unchanged"
        None (G.Vertex.edgeo "A" "B" g0);
    check (option (float 0.0)) "derived graph has the weight"
        (Some 2.0) (G.Vertex.edgeo "A" "B" g1);
    (* re-adding the same edge replaces rather than accumulating *)
    let g2 = G.add_weight 5.0 "A" "B" g1 in
    check (float 0.0) "re-add replaces on derived" 5.0 (G.Vertex.edge "A" "B" g2);
    check (float 0.0) "prior version untouched"    2.0 (G.Vertex.edge "A" "B" g1)
;;

(* transpose swaps edge direction while preserving weights *)
let transpose_preserves _cx =
    let g = G.empty |> G.add "A" |> G.add "B" |> G.add_weight 3.0 "A" "B" in
    let t = G.transpose g in
    check bool "edge reversed to B->A" true (G.has_edge "B" "A" t);
    check (float 0.0) "weight carried over on B->A" 3.0 (G.Vertex.edge "B" "A" t)
;;

(* toposort must respect every edge: tail before head (the doc calls this a
   "happens-before" order). *)
let toposort_valid _cx =
    let g =
        G.empty
        |> G.add "A" |> G.add "B" |> G.add "C" |> G.add "D"
        |> G.add_edge "A" "B"
        |> G.add_edge "A" "C"
        |> G.add_edge "B" "D"
        |> G.add_edge "C" "D"
    in
    let order = G.toposort g in
    check int "all nodes present in ordering" 4 (List.length order);
    let pos x =
        let rec go i = function
            | []                       -> raise Not_found
            | y :: _  when String.equal x y -> i
            | _ :: tl                  -> go (i + 1) tl
        in go 0 order
    in
    let ok = G.edgeseq g |> Seq.for_all (fun (u, v) -> pos u < pos v) in
    check bool "every edge points forward in the order" true ok
;;

let acyclic _cx =
    let dag =
        G.empty |> G.add "A" |> G.add "B" |> G.add "C"
        |> G.add_edge "A" "B" |> G.add_edge "B" "C"
    in
    let cyc =
        G.empty |> G.add "A" |> G.add "B" |> G.add "C"
        |> G.add_edge "A" "B" |> G.add_edge "B" "C" |> G.add_edge "C" "A"
    in
    check bool "DAG is acyclic"      true  (G.is_acyclic dag);
    check bool "cycle is not acyclic" false (G.is_acyclic cyc)
;;

(* shortest path should route A->B->C (cost 3) rather than the direct A->C (5) *)
let dijkstra_shortest _cx =
    let g =
        G.empty |> G.add "A" |> G.add "B" |> G.add "C"
        |> G.add_weight 1.0 "A" "B"
        |> G.add_weight 2.0 "B" "C"
        |> G.add_weight 5.0 "A" "C"
    in
    let path = P.dijkstra "A" "C" g in
    let c    = cost_of (List.assoc "C" path) in
    check (float 0.0001) "shortest cost to C is 3" 3.0 c
;;

let () =
    run "Graph" [
        "structure", [
            test_case "nodes"          `Quick structure;
            test_case "directed edge"  `Quick directed_edge;
        ];
        "weights", [
            test_case "persistent"     `Quick weights_persistent;
            test_case "transpose"      `Quick transpose_preserves;
        ];
        "algorithms", [
            test_case "toposort"       `Quick toposort_valid;
            test_case "acyclic"        `Quick acyclic;
            test_case "dijkstra"       `Quick dijkstra_shortest;
        ];
    ]
;;
