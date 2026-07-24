open Fungi
open Alcotest

module G = Graph.MakeGraph (struct
    type t      = string
    type edge   = float
    let compare = String.compare
end)

(* A dot serializer for string nodes and float edge weights. *)
module Ser = G.Serialize (struct
    let string_of_elt = Fun.id
    let string_of_wgt = Printf.sprintf "%g"
    let elt_of_string = Fun.id
    let wgt_of_string = float_of_string
end)

let no_style ()  = Ser.StyleTbl.create 0
let no_attrs ()  = Ser.AttrbTbl.create 0

(* ---- example 1: a directed graph keeps genuine mutual edges ------------- *)
let directed_mutual _cx =
    let g =
        G.empty |> G.add "A" |> G.add "B" |> G.add "C"
        |> G.add_edge "A" "B" |> G.add_edge "B" "A" |> G.add_edge "A" "C"
    in
    let dot = Ser.to_dot_string ~dir:true "G" (no_style ()) (no_attrs ()) (no_attrs ()) g in
    check string "both A->B and B->A survive"
        "digraph \"G\" {\n\t\"A\" -> \"B\";\n\t\"A\" -> \"C\";\n\t\"B\" -> \"A\";\n}\n"
        dot
;;

(* ---- example 2: undirected graph collapses reciprocal edges, keeps label - *)
let undirected_weighted _cx =
    let g = G.empty |> G.add "A" |> G.add "B" |> G.add_weight2 2.5 "A" "B" in
    let dot = Ser.to_dot_string "U" (no_style ()) (no_attrs ()) (no_attrs ()) g in
    check string "single edge with weight label"
        "graph \"U\" {\n\t\"A\" -- \"B\" [label=\"2.5\"];\n}\n"
        dot
;;

(* ---- example 3: identifiers with spaces / dashes are quoted -------------- *)
let quoting _cx =
    let g = G.empty |> G.add "New York" |> G.add "a-b" |> G.add_edge "New York" "a-b" in
    let dot = Ser.to_dot_string ~dir:true "Q" (no_style ()) (no_attrs ()) (no_attrs ()) g in
    check string "awkward names are quoted"
        "digraph \"Q\" {\n\t\"New York\" -> \"a-b\";\n}\n"
        dot
;;

(* ---- example 4: global, node and edge attributes + an isolated node ------ *)
let attributes _cx =
    let ga  = Ser.StyleTbl.create 1 in Ser.StyleTbl.add ga "rankdir" "LR";
    let na  = Ser.AttrbTbl.create 1 in
    let nA  = Ser.StyleTbl.create 1 in Ser.StyleTbl.add nA "color" "red";
    Ser.AttrbTbl.add na "A" nA;
    let ea  = Ser.AttrbTbl.create 1 in
    let eAB = Ser.StyleTbl.create 1 in Ser.StyleTbl.add eAB "style" "dashed";
    Ser.AttrbTbl.add ea "A-B" eAB;
    let g = G.empty |> G.add "A" |> G.add "B" |> G.add "Z" |> G.add_edge "A" "B" in
    let dot = Ser.to_dot_string ~dir:true "A" ga na ea g in
    check string "attributes and isolated node"
        "digraph \"A\" {\n\trankdir=\"LR\";\n\t\"A\" [color=\"red\"];\n\t\"A\" -> \"B\" [style=\"dashed\"];\n\t\"Z\";\n}\n"
        dot
;;

(* ---- example 5: escaping of embedded quotes ----------------------------- *)
let escaping _cx =
    let g = G.empty |> G.add "he\"llo" in
    let dot = Ser.to_dot_string "E" (no_style ()) (no_attrs ()) (no_attrs ()) g in
    check string "embedded quote is backslash-escaped"
        "graph \"E\" {\n\t\"he\\\"llo\";\n}\n"
        dot
;;

(* ---- the lazy Seq form is re-forceable and matches to_dot_string --------- *)
let seq_matches_string _cx =
    let g = G.empty |> G.add "A" |> G.add "B" |> G.add_edge "A" "B" in
    let force () =
        Ser.to_dot ~dir:true "G" (no_style ()) (no_attrs ()) (no_attrs ()) g
        |> Seq.concat
        |> Seq.fold_left (fun acc thunk -> acc ^ thunk ()) ""
    in
    let expected = Ser.to_dot_string ~dir:true "G" (no_style ()) (no_attrs ()) (no_attrs ()) g in
    check string "first force matches to_dot_string" expected (force ());
    (* forcing again must yield the same output (no leaked mutable state) *)
    check string "second force is identical"          expected (force ())
;;

(* ---- to_dot_channel writes the same bytes as to_dot_string --------------- *)
let read_file path =
    let ic = open_in_bin path in
    let s  = really_input_string ic (in_channel_length ic) in
    close_in ic; s
;;

let channel_matches_string _cx =
    let g = G.empty |> G.add "A" |> G.add "B" |> G.add_edge "A" "B" in
    let expected = Ser.to_dot_string ~dir:true "G" (no_style ()) (no_attrs ()) (no_attrs ()) g in
    let path = Filename.temp_file "fungi_dot" ".gv" in
    let oc   = open_out_bin path in
    Ser.to_dot_channel ~dir:true "G" (no_style ()) (no_attrs ()) (no_attrs ()) g oc;
    close_out oc;
    let got = read_file path in
    Sys.remove path;
    check string "channel output equals string output" expected got
;;

let () =
    run "Serialize" [
        "dot", [
            test_case "directed mutual" `Quick directed_mutual;
            test_case "undirected"      `Quick undirected_weighted;
            test_case "quoting"         `Quick quoting;
            test_case "attributes"      `Quick attributes;
            test_case "escaping"        `Quick escaping;
        ];
        "api", [
            test_case "seq re-forceable" `Quick seq_matches_string;
            test_case "channel"          `Quick channel_matches_string;
        ];
    ]
;;
