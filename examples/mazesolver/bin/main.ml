open Fungi;;

module SGraph = Graph.MakeGraph(struct 
    type t     = string
    type edge  = float
    let compare=  String.compare
end);;

let s = SGraph.empty;;

let s_a = "A";;
let s_b = "B";;
let s_c = "C";;
let s_d = "D";;
let s_e = "E";;
let s_f = "F";;
let s_g = "G";;
let s_h = "H";;
let s_i = "I";;
let s_j = "J";;
let s_k = "K";;
let s_l = "L";;
let s_s = "S";;

let nodes = [ s_a; s_b; s_c; s_d; s_e; s_f; s_g; s_h; s_i; s_j; s_k; s_l; s_s ];;

let s = List.fold_right (SGraph.add) nodes s;;

let s_adjlist = [
    (s_a, [(s_s, 7.); (s_b, 3.); (s_d, 4.)]);
    (s_s, [(s_c, 3.)]);
    (s_c, [(s_l, 2.)]);
    (s_l, [(s_i, 4.); (s_j, 4.)]);
    (s_i, [(s_j, 6.); (s_k, 4.)]);
    (s_j, [(s_k, 4.)]);
    (s_k, [(s_e, 5.)]);
    (s_d, [(s_b, 4.); (s_f, 5.)]);
    (s_f, [(s_h, 3.)]);
    (s_h, [(s_g, 2.)]);
    (s_g, [(s_e, 2.)]);
];;


module SPath = SGraph.Path.Compute(Graph.Biject(Float));;

let () = 
    let _ = SGraph.of_weights2 s_adjlist s in
    ()
