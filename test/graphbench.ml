open Fungi
(* watch for https://github.com/janestreet/core_unix/issues/14 *)
open Core_bench

(* Benchmarks for the flagship graph algorithms. The heap already has its own
   suite in treebench.ml; this file targets the operations most users actually
   pay for: construction, shortest paths, SCC, spanning trees and max-flow.

   All graphs are built once at module load so the benchmarked closures measure
   only the algorithm, not the setup (mirrors treebench's precomputed heap). *)

module G  = Graph.MakeGraph (struct
    type t      = int
    type edge   = int
    let compare = Int.compare
end)

module P  = G.Path.Compute (Graph.Adapt (Int))
module Sp = G.Span         (Graph.Adapt (Int))
module Fl = G.Flow         (Graph.Adapt (Int))

let () = Random.init 42

(* --------------------------------------------------------------------- *)
(* Generators                                                            *)
(* --------------------------------------------------------------------- *)

(* rows*cols 4-neighbour undirected grid with random 1..9 edge weights.
   Node ids encode coordinates as r*cols + c so heuristics can decode them. *)
let grid rows cols =
    let id r c = (r * cols) + c in
    let g = ref G.empty in
    for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
            g := G.add (id r c) !g
        done
    done;
    for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
            if c + 1 < cols then
                g := G.add_weight2 (1 + Random.int 9) (id r c) (id r (c + 1)) !g;
            if r + 1 < rows then
                g := G.add_weight2 (1 + Random.int 9) (id r c) (id (r + 1) c) !g
        done
    done;
    !g
;;

(* n nodes, m random directed edges — dense enough to grow real SCCs. *)
let randdigraph n m =
    let g = ref G.empty in
    for i = 0 to n - 1 do g := G.add i !g done;
    for _ = 1 to m do
        let a = Random.int n and b = Random.int n in
        if a <> b then g := G.add_edge a b !g
    done;
    !g
;;

(* layered directed acyclic graph, width nodes per layer, edges only forward. *)
let dag layers width =
    let id l w = (l * width) + w in
    let g = ref G.empty in
    for l = 0 to layers - 1 do
        for w = 0 to width - 1 do g := G.add (id l w) !g done
    done;
    for l = 0 to layers - 2 do
        for w = 0 to width - 1 do
            g := G.add_edge (id l w) (id (l + 1) w) !g;
            g := G.add_edge (id l w) (id (l + 1) ((w + 1) mod width)) !g
        done
    done;
    !g
;;

(* --------------------------------------------------------------------- *)
(* Fixtures                                                              *)
(* --------------------------------------------------------------------- *)

let big_cols   = 20
let big_grid   = grid big_cols big_cols          (* 400 nodes, ~760 edges *)
let src        = 0
let dst        = (big_cols * big_cols) - 1

let small_grid = grid 10 10                       (* 100 nodes: O(n^3) safe  *)
let scc_graph  = randdigraph 500 2000
let dag_graph  = dag 60 12                         (* 720 nodes DAG           *)
let flow_grid  = grid 12 12                        (* 144 nodes, src->sink    *)
let flow_sink  = (12 * 12) - 1
let johnsons_temp = max_int                        (* sentinel not in graph   *)

(* admissible manhattan heuristic for the big grid (min edge weight = 1). *)
let heuristic target n =
    let tr = target / big_cols and tc = target mod big_cols in
    let nr = n / big_cols      and nc = n mod big_cols in
    `Val (abs (tr - nr) + abs (tc - nc))
;;

(* --------------------------------------------------------------------- *)

let main () =
    Command_unix.run ~argv:[""; "-quota"; "1s"] (
        Bench.make_command [
            (* construction — the cost of persistence on every add *)
            Bench.Test.create ~name:"build_grid_20x20"
                (fun () -> ignore (grid big_cols big_cols));
            Bench.Test.create ~name:"build_randdigraph_500n_2000e"
                (fun () -> ignore (randdigraph 500 2000));

            (* shortest paths on the 400-node grid *)
            Bench.Test.create ~name:"dijkstra_grid_400"
                (fun () -> ignore (P.dijkstra src dst big_grid));
            Bench.Test.create ~name:"astar_grid_400"
                (fun () -> ignore (P.astar (heuristic dst) src dst big_grid));
            Bench.Test.create ~name:"bellmanford_grid_400"
                (fun () -> ignore (P.bellmanford src dst big_grid));

            (* all-pairs on the 100-node grid *)
            Bench.Test.create ~name:"floydwarshall_grid_100"
                (fun () -> ignore (P.floydwarshall small_grid));
            Bench.Test.create ~name:"johnsons_grid_100"
                (fun () -> ignore (P.johnsons johnsons_temp small_grid));

            (* strongly connected components *)
            Bench.Test.create ~name:"tarjan_scc_500"
                (fun () -> ignore (G.Scc.tarjan scc_graph));
            Bench.Test.create ~name:"kosaraju_scc_500"
                (fun () -> ignore (G.Scc.kosaraju scc_graph));

            (* minimum spanning trees on the 400-node grid *)
            Bench.Test.create ~name:"kruskal_grid_400"
                (fun () -> ignore (Sp.kruskal big_grid));
            Bench.Test.create ~name:"prim_grid_400"
                (fun () -> ignore (Sp.prim src big_grid));

            (* max-flow (fresh residual table per run) *)
            Bench.Test.create ~name:"edmondskarp_grid_144"
                (fun () ->
                    let cap = Fl.Flowtbl.create 256 in
                    ignore (Fl.edmondskarp cap src flow_sink flow_grid));

            (* ordering *)
            Bench.Test.create ~name:"toposort_dag_720"
                (fun () -> ignore (G.toposort dag_graph));
            (* weight-preserving transpose on the weighted grid *)
            Bench.Test.create ~name:"transpose_grid_400"
                (fun () -> ignore (G.transpose big_grid));
            (* unweighted structural transpose on the 500-node digraph *)
            Bench.Test.create ~name:"transpose2_scc_500"
                (fun () -> ignore (G.transpose2 scc_graph));
        ]
    )
;;

let () = main ()
;;
