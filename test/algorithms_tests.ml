open Fungi
open Alcotest

module G = Graph.MakeGraph (struct
    type t      = string
    type edge   = float
    let compare = String.compare
end)

module P  = G.Path.Compute   (Graph.Adapt (Float))
module Fl = G.Flow           (Graph.Adapt (Float))
module M  = G.Matching.Compute (Graph.Adapt (Float))
module Sp = G.Span           (Graph.Adapt (Float))

let cost_of = function
    | `Val f  -> f
    | `Inf    -> infinity
    | `NegInf -> neg_infinity
    | `Nan    -> nan
;;

(* cumulative cost recorded for [node] in a returned path list *)
let cost_to node path = cost_of (List.assoc node path)

(* ---------------------------------------------------------------------- *)
(* Strongly Connected Components                                          *)
(* ---------------------------------------------------------------------- *)

(* Two cycles {A,B,C} and {D,E} joined by a one-way edge C->D. *)
let scc_graph =
    G.empty
    |> G.add "A" |> G.add "B" |> G.add "C" |> G.add "D" |> G.add "E"
    |> G.add_edge "A" "B" |> G.add_edge "B" "C" |> G.add_edge "C" "A"
    |> G.add_edge "C" "D"
    |> G.add_edge "D" "E" |> G.add_edge "E" "D"
;;

(* group the flat sccnode -> node table into a sorted list of components *)
let components (sccs : string G.Scc.SccTbl.t) =
    let tbl = Hashtbl.create 8 in
    G.Scc.SccTbl.iter (fun (k : G.Scc.sccnode) n ->
        let prev = try Hashtbl.find tbl k.link with Not_found -> [] in
        Hashtbl.replace tbl k.link (n :: prev)
    ) sccs;
    Hashtbl.fold (fun _ ns acc -> List.sort compare ns :: acc) tbl []
    |> List.sort compare
;;

let expected_components = [ ["A"; "B"; "C"]; ["D"; "E"] ]

let tarjan_components _cx =
    let sol = G.Scc.tarjan scc_graph in
    check (list (list string)) "tarjan finds the two SCCs"
        expected_components (components sol.G.Scc.sccs)
;;

let kosaraju_components _cx =
    let sol = G.Scc.kosaraju scc_graph in
    check (list (list string)) "kosaraju finds the two SCCs"
        expected_components (components sol.G.Scc.sccs)
;;

(* ---------------------------------------------------------------------- *)
(* Clusters (maximal cliques)                                            *)
(* ---------------------------------------------------------------------- *)

(* triangle A-B-C plus a pendant edge A-D (undirected via add_edge2) *)
let clique_graph =
    G.empty
    |> G.add "A" |> G.add "B" |> G.add "C" |> G.add "D"
    |> G.add_edge2 "A" "B" |> G.add_edge2 "B" "C" |> G.add_edge2 "A" "C"
    |> G.add_edge2 "A" "D"
;;

let normalize_cliques cqs =
    cqs
    |> List.map (fun s -> List.sort compare (G.AdjSet.to_list s))
    |> List.sort compare
;;

let expected_cliques = [ ["A"; "B"; "C"]; ["A"; "D"] ]

let bronkerbosch_cliques _cx =
    check (list (list string)) "bron-kerbosch maximal cliques"
        expected_cliques (normalize_cliques (G.Cluster.bronkerbosch clique_graph))
;;

let bronkerbosch2_cliques _cx =
    check (list (list string)) "bron-kerbosch (pivoting) maximal cliques"
        expected_cliques (normalize_cliques (G.Cluster.bronkerbosch2 clique_graph))
;;

(* ---------------------------------------------------------------------- *)
(* Flow                                                                  *)
(* ---------------------------------------------------------------------- *)

(* classic max-flow diamond, edge weights are capacities; max flow = 5 *)
let flow_graph =
    G.empty
    |> G.add "S" |> G.add "A" |> G.add "B" |> G.add "T"
    |> G.add_weight 3.0 "S" "A"
    |> G.add_weight 2.0 "S" "B"
    |> G.add_weight 1.0 "A" "B"
    |> G.add_weight 2.0 "A" "T"
    |> G.add_weight 3.0 "B" "T"
;;

let edmondskarp_maxflow _cx =
    let cap = Fl.Flowtbl.create 16 in
    let mf  = Fl.edmondskarp cap "S" "T" flow_graph in
    check (float 0.0001) "edmonds-karp max flow S->T is 5" 5.0 (cost_of mf)
;;

(* a simple line S->A->T terminates quickly for ford-fulkerson; flow = 2 *)
let fordfulkerson_maxflow _cx =
    let g =
        G.empty |> G.add "S" |> G.add "A" |> G.add "T"
        |> G.add_weight 3.0 "S" "A"
        |> G.add_weight 2.0 "A" "T"
    in
    let cap = Fl.Flowtbl.create 16 in
    let mf  = Fl.fordfulkerson cap "S" "T" g in
    check (float 0.0001) "ford-fulkerson max flow S->T is 2" 2.0 (cost_of mf)
;;

(* ---------------------------------------------------------------------- *)
(* Path                                                                  *)
(* ---------------------------------------------------------------------- *)

(* A->B (1), B->C (2), A->C (5): shortest A->C routes through B at cost 3 *)
let path_graph =
    G.empty |> G.add "A" |> G.add "B" |> G.add "C"
    |> G.add_weight 1.0 "A" "B"
    |> G.add_weight 2.0 "B" "C"
    |> G.add_weight 5.0 "A" "C"
;;

let astar_shortest _cx =
    let path = P.astar (fun _ -> `Val 0.0) "A" "C" path_graph in
    check (float 0.0001) "A* (zero heuristic) shortest cost to C" 3.0 (cost_to "C" path)
;;

let bellmanford_shortest _cx =
    let (path, _negcycles) = P.bellmanford "A" "C" path_graph in
    check (float 0.0001) "bellman-ford shortest cost to C" 3.0 (cost_to "C" path)
;;

let floyd_shortest _cx =
    let (dist, next, map) = P.floydwarshall path_graph in
    match P.floydwresolve "A" "C" dist next map with
    | None      -> fail "floyd-warshall found no path A->C"
    | Some path -> check (float 0.0001) "floyd-warshall shortest cost to C"
                       3.0 (cost_to "C" path)
;;

let johnsons_smoke _cx =
    let (reweighted, _restore) = P.johnsons "__tmp__" path_graph in
    check int "johnsons keeps all nodes (plus the temp source)"
        (G.cardinal path_graph + 1) (G.cardinal reweighted)
;;

(* ---------------------------------------------------------------------- *)
(* Matching                                                              *)
(* ---------------------------------------------------------------------- *)

let hall_holds _cx =
    (* P1-{X,Y}, P2-{X}: every subset has enough neighbours *)
    let g =
        G.empty
        |> G.add "P1" |> G.add "P2" |> G.add "X" |> G.add "Y"
        |> G.add_edge2 "P1" "X" |> G.add_edge2 "P1" "Y" |> G.add_edge2 "P2" "X"
    in
    check bool "Hall's condition holds" true
        (G.Matching.hall g (G.AdjSet.of_list ["P1"; "P2"]))
;;

let hall_fails _cx =
    (* P1 and P2 both only reach X: {P1,P2} has just one neighbour *)
    let g =
        G.empty
        |> G.add "P1" |> G.add "P2" |> G.add "X"
        |> G.add_edge2 "P1" "X" |> G.add_edge2 "P2" "X"
    in
    check bool "Hall's condition violated" false
        (G.Matching.hall g (G.AdjSet.of_list ["P1"; "P2"]))
;;

(* With these preferences the unique stable matching is (A1,P1),(A2,P2). *)
let galeshapely_stable _cx =
    (* directed rankings both ways; higher weight = more preferred *)
    let g =
        G.empty
        |> G.add "P1" |> G.add "P2" |> G.add "A1" |> G.add "A2"
        |> G.add_weight 2.0 "P1" "A1" |> G.add_weight 1.0 "P1" "A2"
        |> G.add_weight 2.0 "P2" "A1" |> G.add_weight 1.0 "P2" "A2"
        |> G.add_weight 2.0 "A1" "P1" |> G.add_weight 1.0 "A1" "P2"
        |> G.add_weight 1.0 "A2" "P1" |> G.add_weight 2.0 "A2" "P2"
    in
    let proposers = G.AdjSet.of_list ["P1"; "P2"] in
    let acceptors = G.AdjSet.of_list ["A1"; "A2"] in
    let matching  = M.galeshapely g proposers acceptors in
    let pairs = List.sort compare (G.EdgeSet.to_list matching) in
    check (list (pair string string)) "stable matching (acceptor, proposer)"
        [ ("A1", "P1"); ("A2", "P2") ] pairs
;;

(* ---------------------------------------------------------------------- *)
(* Span (minimum spanning tree)                                          *)
(* ---------------------------------------------------------------------- *)

(* Undirected weighted graph whose unique MST is A-B(1), B-C(2), C-D(4) = 7.
   The competing spanning trees both cost 8, so the total cost pins down the
   correct tree without depending on stored edge direction. *)
let mst_graph =
    G.empty
    |> G.add "A" |> G.add "B" |> G.add "C" |> G.add "D"
    |> G.add_weight2 1.0 "A" "B"
    |> G.add_weight2 2.0 "B" "C"
    |> G.add_weight2 3.0 "A" "C"
    |> G.add_weight2 4.0 "C" "D"
    |> G.add_weight2 5.0 "B" "D"
;;

let edge_count g = List.length (List.of_seq (G.edgewgtseq g))

let kruskal_mst _cx =
    let mst = Sp.kruskal mst_graph in
    check int    "MST spans all 4 nodes"       4   (G.cardinal mst);
    check int    "MST has n-1 edges"           3   (edge_count mst);
    check (float 0.0001) "kruskal MST cost"    7.0 (Sp.cost mst)
;;

let prim_mst _cx =
    let mst = Sp.prim "A" mst_graph in
    check int    "MST spans all 4 nodes"       4   (G.cardinal mst);
    check int    "MST has n-1 edges"           3   (edge_count mst);
    check (float 0.0001) "prim MST cost"       7.0 (Sp.cost mst)
;;

(* cost simply sums the (directed) edge weights it walks over *)
let span_cost _cx =
    let g =
        G.empty |> G.add "A" |> G.add "B" |> G.add "C"
        |> G.add_weight 2.0 "A" "B"
        |> G.add_weight 5.0 "B" "C"
    in
    check (float 0.0001) "cost sums edge weights" 7.0 (Sp.cost g)
;;

let () =
    run "Algorithms" [
        "scc", [
            test_case "tarjan"        `Quick tarjan_components;
            test_case "kosaraju"      `Quick kosaraju_components;
        ];
        "cluster", [
            test_case "bronkerbosch"  `Quick bronkerbosch_cliques;
            test_case "bronkerbosch2" `Quick bronkerbosch2_cliques;
        ];
        "flow", [
            test_case "edmondskarp"   `Quick edmondskarp_maxflow;
            test_case "fordfulkerson" `Quick fordfulkerson_maxflow;
        ];
        "path", [
            test_case "astar"         `Quick astar_shortest;
            test_case "bellmanford"   `Quick bellmanford_shortest;
            test_case "floydwarshall" `Quick floyd_shortest;
            test_case "johnsons"      `Quick johnsons_smoke;
        ];
        "matching", [
            test_case "hall holds"    `Quick hall_holds;
            test_case "hall fails"    `Quick hall_fails;
            test_case "galeshapely"   `Quick galeshapely_stable;
        ];
        "span", [
            test_case "kruskal"       `Quick kruskal_mst;
            test_case "prim"          `Quick prim_mst;
            test_case "cost"          `Quick span_cost;
        ];
    ]
;;
