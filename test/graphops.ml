open Fungi;;
open Alcotest;;

(* Create 2 graph models, one being weighted and the other unweighted *)
module FloatStrGraph = Graph.MakeGraph (struct 
    type t      = string
    type edge   = float
    let compare = String.compare
end)

module UnitStrGraph = Graph.MakeGraph (struct 
    type t      = string
    type edge   = float
    let compare = String.compare
end)

let graph_creation _cx = 

    let wgtconn = [
        ("A", [ ("B", 1.); ("C", 2.) ]);
        ("B", [ ("C", 4.) ]);
        ("C", [ ]);
    ] in

    let unwconn = [
        ("A", [ "B"; "C" ]);
        ("B", [ "C" ]);
        ("C", [ ]);
    ] in

    let nodeseq = (List.to_seq ["A"; "B"; "C"]) in
 
    (* unweighted directed and undirected *)
    let ug = UnitStrGraph.of_list     unwconn (UnitStrGraph.all nodeseq UnitStrGraph.empty) in 
    let ug'= UnitStrGraph.of_list2    unwconn (UnitStrGraph.all nodeseq UnitStrGraph.empty) in 

    (* weighted directed and undirected *)
    let wg = FloatStrGraph.of_weights  wgtconn (FloatStrGraph.all nodeseq FloatStrGraph.empty) in 
    let wg'= FloatStrGraph.of_weights2 wgtconn (FloatStrGraph.all nodeseq FloatStrGraph.empty) in 

    let unodes,  uedges,  _ = UnitStrGraph.edgesetsz ug  in
    let unodes', uedges', _ = UnitStrGraph.edgesetsz ug' in

    let wnodes,  wedges,  _ = FloatStrGraph.edgesetsz wg  in
    let wnodes', wedges', _ = FloatStrGraph.edgesetsz wg' in

    let _ = Alcotest.(check int)  "Unweighted directed   node count" 3 unodes  in 
    let _ = Alcotest.(check int)  "Unweighted undirected node count" 3 unodes' in 
    let _ = Alcotest.(check int)  "Weighted directed     node count" 3 wnodes  in 
    let _ = Alcotest.(check int)  "Weighted directed     node count" 3 wnodes' in 

    let _ = Alcotest.(check int)  "Unweighted directed   edge count" 3 uedges  in 
    let _ = Alcotest.(check int)  "Unweighted undirected edge count" 6 uedges' in 
    let _ = Alcotest.(check int)  "Weighted directed     edge count" 3 wedges  in 
            Alcotest.(check int)  "Weighted directed     edge count" 6 wedges' 
;;


let graph_edge_manipulation _cx = 

    let nodeseq = (List.to_seq ["A"; "B"; "C"]) in

    (* graph is not persistent  *)
    let ug = UnitStrGraph.add_edge "A" "B" (UnitStrGraph.all  nodeseq UnitStrGraph.empty) in
    let ug'= UnitStrGraph.remove_edge ug "A" "C" in

    let wg = FloatStrGraph.add_edge "A" "B" (FloatStrGraph.all nodeseq FloatStrGraph.empty) in
    let wg'= FloatStrGraph.remove_edge wg "A" "C" in

    let _ = Alcotest.(check bool)  "edge created"        true  (UnitStrGraph.has_edge  "A" "B" ug)  in 
    let _ = Alcotest.(check bool)  "edge doesn't exists" false (UnitStrGraph.has_edge  "A" "C" ug)  in 
    let _ = Alcotest.(check bool)  "edge removal"        false (UnitStrGraph.has_edge  "A" "C" ug') in 
    let _ = Alcotest.(check bool)  "edge created"        true  (FloatStrGraph.has_edge "A" "B" wg)  in 
    let _ = Alcotest.(check bool)  "edge removal"        false (FloatStrGraph.has_edge "A" "C" wg') in 
            Alcotest.(check bool)  "edge doesn't exists" false (FloatStrGraph.has_edge "A" "C" wg) 
;;




let () = 
    Alcotest.run "Fungi Graph" [
        "primitives", [
            test_case "creation"       `Quick graph_creation;
            test_case "edge operation" `Quick graph_edge_manipulation;
        ];
    ];
