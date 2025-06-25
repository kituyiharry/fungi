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


let graph_matrix_ops _cx = 

    let unwconn = [
        ("A", [ "B"; "C" ]);
        ("B", [ "C" ]);
        ("C", [ "D" ]);
        ("D", [ "C" ]);
    ] in

    let nodeseq = (List.to_seq ["A"; "B"; "C"; "D"; ]) in
 
    (* unweighted directed and undirected *)
    let dg = UnitStrGraph.of_list  unwconn (UnitStrGraph.all nodeseq UnitStrGraph.empty) in 
    let ug'= UnitStrGraph.of_list2 unwconn (UnitStrGraph.all nodeseq UnitStrGraph.empty) in 

    let (und_adjmat, und_ind) = UnitStrGraph.adjmatrix ug' in
    let (dir_adjmat, dir_ind) = UnitStrGraph.adjmatrix dg  in 

    let find_node_index_mat els node = fst @@ List.find (
        fun (_x, y) -> UnitStrGraph.equal y node
    ) els in

    let uaidx = find_node_index_mat und_ind "A" in 
    let ubidx = find_node_index_mat und_ind "B" in 
    let ucidx = find_node_index_mat und_ind "C" in 
    let udidx = find_node_index_mat und_ind "D" in

    let daidx = find_node_index_mat dir_ind "A" in 
    let dbidx = find_node_index_mat dir_ind "B" in 
    let dcidx = find_node_index_mat dir_ind "C" in 
    let ddidx = find_node_index_mat dir_ind "D" in

    let _ = Alcotest.(check int) "undirected_a_b" 1 (und_adjmat.(uaidx).(ubidx))  in 
    let _ = Alcotest.(check int) "undirected_b_a" 1 (und_adjmat.(ubidx).(uaidx))  in 

    let _ = Alcotest.(check int) "undirected_a_c" 1 (und_adjmat.(uaidx).(ucidx))  in 
    let _ = Alcotest.(check int) "undirected_c_a" 1 (und_adjmat.(ucidx).(uaidx))  in 

    let _ = Alcotest.(check int) "undirected_b_c" 1 (und_adjmat.(ubidx).(ucidx))  in 
    let _ = Alcotest.(check int) "undirected_c_b" 1 (und_adjmat.(ucidx).(ubidx))  in 

    let _ = Alcotest.(check int) "undirected_d_c" 1 (und_adjmat.(udidx).(ucidx))  in 
    let _ = Alcotest.(check int) "undirected_c_d" 1 (und_adjmat.(ucidx).(udidx))  in 

    let _ = Alcotest.(check int) "undirected_b_d" 0 (und_adjmat.(ubidx).(udidx))  in 
    let _ = Alcotest.(check int) "undirected_d_b" 0 (und_adjmat.(udidx).(ubidx))  in 

    let _ = Alcotest.(check int) "directed_a_b" 1 (dir_adjmat.(daidx).(dbidx))  in 
    let _ = Alcotest.(check int) "directed_b_a" 0 (dir_adjmat.(dbidx).(daidx))  in 

    let _ = Alcotest.(check int) "directed_a_c" 1 (dir_adjmat.(daidx).(dcidx))  in 
    let _ = Alcotest.(check int) "directed_c_a" 0 (dir_adjmat.(dcidx).(daidx))  in 

    let _ = Alcotest.(check int) "directed_b_c" 1 (dir_adjmat.(dbidx).(dcidx))  in 
    let _ = Alcotest.(check int) "directed_c_b" 0 (dir_adjmat.(dcidx).(dbidx))  in 

    let _ = Alcotest.(check int) "directed_d_c" 1 (dir_adjmat.(ddidx).(dcidx))  in 
    let _ = Alcotest.(check int) "directed_c_d" 1 (dir_adjmat.(dcidx).(ddidx))  in 

    let _ = Alcotest.(check int) "directed_b_d" 0 (dir_adjmat.(dbidx).(ddidx))  in 
    let _ = Alcotest.(check int) "directed_d_b" 0 (dir_adjmat.(ddidx).(dbidx))  in 

    ()

;;

let () = 
    Alcotest.run "Fungi Graph" [
        "matrix_ops", [
            test_case "adjacency matrix"  `Quick graph_matrix_ops;
        ];
    ];


