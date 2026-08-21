open Fungi
open Alcotest

(* Verification suite for the (as-yet unimplemented) Graph.Planar module -
   Hopcroft-Tarjan articulation points / bridges / biconnected components and
   planarity testing.

   The real expected values are encoded below so the tests are ready the moment
   the algorithms land. Until then every Planar.* call raises Failure (the
   `todo` stub), so with [implemented = false] each case asserts that the stub
   raises and the suite stays green. Flip [implemented] to true once the module
   is written and the same cases become genuine correctness checks. *)
let implemented = true

module G = Graph.MakeGraph (struct
    type t      = string
    type edge   = float
    let compare = String.compare
end)

(* --------------------------------------------------------------------- *)
(* builders                                                              *)
(* --------------------------------------------------------------------- *)

let add_nodes ns g = List.fold_left (fun acc n -> G.add n acc) g ns
let add_edges es g = List.fold_left (fun acc (a, b) -> G.add_edge2 a b acc) g es

(* all unordered pairs of a list, in order *)
let rec pairs = function
    | []      -> []
    | x :: xs -> List.map (fun y -> (x, y)) xs @ pairs xs
;;

let complete ns = add_nodes ns G.empty |> add_edges (pairs ns)

(* --------------------------------------------------------------------- *)
(* fixtures                                                              *)
(* --------------------------------------------------------------------- *)

(* path A-B-C-D: internal nodes are cut vertices, every edge is a bridge *)
let chain =
    add_nodes ["A"; "B"; "C"; "D"] G.empty
    |> add_edges [("A", "B"); ("B", "C"); ("C", "D")]
;;

(* a single triangle: 2-connected, no cut vertices, no bridges *)
let triangle = complete ["A"; "B"; "C"]

(* two triangles sharing vertex C: C is the only cut vertex, no bridges *)
let hourglass =
    add_nodes ["A"; "B"; "C"; "D"; "E"] G.empty
    |> add_edges
        [ ("A", "B"); ("B", "C"); ("A", "C")     (* triangle A B C *)
        ; ("C", "D"); ("D", "E"); ("C", "E") ]   (* triangle C D E *)
;;

(* two triangles joined by a single bridge C-D: cut vertices C and D *)
let dumbbell =
    add_nodes ["A"; "B"; "C"; "D"; "E"; "F"] G.empty
    |> add_edges
        [ ("A", "B"); ("B", "C"); ("A", "C")     (* triangle A B C *)
        ; ("C", "D")                              (* bridge         *)
        ; ("D", "E"); ("E", "F"); ("D", "F") ]   (* triangle D E F *)
;;

let k4  = complete ["A"; "B"; "C"; "D"]           (* planar     *)
let k5  = complete ["A"; "B"; "C"; "D"; "E"]      (* NOT planar *)

let k33 =                                          (* NOT planar *)
    let l = ["A"; "B"; "C"] and r = ["X"; "Y"; "Z"] in
    add_nodes (l @ r) G.empty
    |> add_edges (List.concat_map (fun a -> List.map (fun b -> (a, b)) r) l)
;;

(* Petersen graph: NOT planar - contains a K3,3 subdivision but, being
   3-regular, is not itself a Kuratowski subgraph, so a correct certificate
   must be strictly smaller than the whole graph. *)
let petersen =
    add_nodes ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"] G.empty
    |> add_edges
        [ ("0","1"); ("1","2"); ("2","3"); ("3","4"); ("4","0")  (* outer cycle *)
        ; ("5","7"); ("7","9"); ("9","6"); ("6","8"); ("8","5")  (* inner star  *)
        ; ("0","5"); ("1","6"); ("2","7"); ("3","8"); ("4","9")  (* spokes      *)
        ]
;;

(* 3-cube Q3: planar (bipartite, 3-regular, 8 vertices) *)
let cube =
    add_nodes ["0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"] G.empty
    |> add_edges
        [ ("0","1"); ("1","3"); ("3","2"); ("2","0")
        ; ("4","5"); ("5","7"); ("7","6"); ("6","4")
        ; ("0","4"); ("1","5"); ("2","6"); ("3","7") ]
;;

(* --------------------------------------------------------------------- *)
(* normalisers - make results representation-agnostic                    *)
(* --------------------------------------------------------------------- *)

let norm (a, b) = if compare a b <= 0 then (a, b) else (b, a)
let edge_str e  = let (a, b) = norm e in a ^ "-" ^ b

let got_articulations g =
    G.Planar.articulations g |> G.AdjSet.to_list |> List.sort compare

let got_bridges g =
    G.Planar.bridges g
    |> G.EdgeSet.to_list |> List.map edge_str |> List.sort compare

let got_bicomps g =
    G.Planar.biconnected_components g
    |> List.map (fun es ->
        List.sort compare (List.map edge_str (G.EdgeSet.to_list es)))
    |> List.sort compare
;;

(* rebuild a graph from an edge list (add_edge2 does not resolve endpoints, so
   the nodes are added first). *)
let graph_of_edges es =
    add_nodes (List.concat_map (fun (a, b) -> [a; b]) es) G.empty
    |> add_edges es
;;

(* a Kuratowski certificate must be a genuine minimal obstruction: itself
   non-planar, yet planar after deleting ANY single edge. This is exactly what
   separates a real K5/K3,3 subdivision from an un-minimized biconnected block. *)
let is_minimal_obstruction g =
    match G.Planar.planarity g with
    | G.Planar.Planar _      -> false
    | G.Planar.Nonplanar es ->
        let edges = G.EdgeSet.to_list es in
        not (G.Planar.is_planar (graph_of_edges edges))
        && List.for_all (fun e ->
            G.Planar.is_planar
                (graph_of_edges (List.filter (fun x -> x <> e) edges))) edges
;;

(* the whole Planar module is a stub: assert the call raises the todo Failure *)
let expect_stub name thunk =
    match thunk () with
    | exception (Failure _) -> ()
    | exception e ->
        fail (Printf.sprintf "%s: unexpected exception %s"
                  name (Printexc.to_string e))
    | () ->
        fail (Printf.sprintf
            "%s: stub returned a value - set `implemented` to true" name)
;;

(* --------------------------------------------------------------------- *)
(* case wrappers: real check when implemented, stub-raises otherwise     *)
(* --------------------------------------------------------------------- *)

let articulations_case name g expected () =
    if implemented
    then check (list string) name (List.sort compare expected) (got_articulations g)
    else expect_stub name (fun () -> ignore (G.Planar.articulations g))
;;

let bridges_case name g expected () =
    if implemented
    then check (list string) name (List.sort compare expected) (got_bridges g)
    else expect_stub name (fun () -> ignore (G.Planar.bridges g))
;;

let bicomps_case name g expected () =
    if implemented
    then check (list (list string)) name (List.sort compare expected) (got_bicomps g)
    else expect_stub name (fun () -> ignore (G.Planar.biconnected_components g))
;;

let planar_case name g expected () =
    if implemented
    then check bool name expected (G.Planar.is_planar g)
    else expect_stub name (fun () -> ignore (G.Planar.is_planar g))
;;

let obstruction_case name g () =
    if implemented
    then check bool name true (is_minimal_obstruction g)
    else expect_stub name (fun () -> ignore (G.Planar.planarity g))
;;

(* --------------------------------------------------------------------- *)
(* expected results                                                      *)
(* --------------------------------------------------------------------- *)

let () =
    run "Planar" [
        "articulations", [
            test_case "chain: B,C"      `Quick (articulations_case "chain"     chain     ["B"; "C"]);
            test_case "triangle: none"  `Quick (articulations_case "triangle"  triangle  []);
            test_case "hourglass: C"    `Quick (articulations_case "hourglass" hourglass ["C"]);
            test_case "dumbbell: C,D"   `Quick (articulations_case "dumbbell"  dumbbell  ["C"; "D"]);
            test_case "K4: none"        `Quick (articulations_case "k4"        k4        []);
            test_case "K3,3: none"      `Quick (articulations_case "k33"       k33       []);
        ];
        "bridges", [
            test_case "chain: all 3"    `Quick (bridges_case "chain"     chain     ["A-B"; "B-C"; "C-D"]);
            test_case "triangle: none"  `Quick (bridges_case "triangle"  triangle  []);
            test_case "hourglass: none" `Quick (bridges_case "hourglass" hourglass []);
            test_case "dumbbell: C-D"   `Quick (bridges_case "dumbbell"  dumbbell  ["C-D"]);
            test_case "K5: none"        `Quick (bridges_case "k5"        k5        []);
        ];
        "biconnected", [
            test_case "chain: 3 edges"  `Quick (bicomps_case "chain"    chain
                [ ["A-B"]; ["B-C"]; ["C-D"] ]);
            test_case "triangle: 1"     `Quick (bicomps_case "triangle" triangle
                [ ["A-B"; "A-C"; "B-C"] ]);
            test_case "hourglass: 2"    `Quick (bicomps_case "hourglass" hourglass
                [ ["A-B"; "A-C"; "B-C"]; ["C-D"; "C-E"; "D-E"] ]);
            test_case "dumbbell: 3"     `Quick (bicomps_case "dumbbell" dumbbell
                [ ["A-B"; "A-C"; "B-C"]; ["C-D"]; ["D-E"; "D-F"; "E-F"] ]);
        ];
        "planarity", [
            test_case "chain planar"     `Quick (planar_case "chain"    chain    true);
            test_case "triangle planar"  `Quick (planar_case "triangle" triangle true);
            test_case "K4 planar"        `Quick (planar_case "k4"       k4       true);
            test_case "cube Q3 planar"   `Quick (planar_case "cube"     cube     true);
            test_case "K5 nonplanar"     `Quick (planar_case "k5"       k5       false);
            test_case "K3,3 nonplanar"   `Quick (planar_case "k33"      k33      false);
            test_case "petersen nonplanar" `Quick (planar_case "petersen" petersen false);
        ];
        (* the certificate really is a minimal K5/K3,3 subdivision *)
        "kuratowski", [
            test_case "K5 minimal"       `Quick (obstruction_case "k5"       k5);
            test_case "K3,3 minimal"     `Quick (obstruction_case "k33"      k33);
            test_case "petersen minimal" `Quick (obstruction_case "petersen" petersen);
        ];
    ]
;;
