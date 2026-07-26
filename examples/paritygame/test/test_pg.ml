open Paritygame
open Paritygame.Game
open Alcotest

module PG = ParityGame

let card  = PG.AdjSet.cardinal
let eqset a b = PG.AdjSet.subset a b && PG.AdjSet.subset b a

let contains hay needle =
    let hl = String.length hay and nl = String.length needle in
    let rec go i = i + nl <= hl && (String.sub hay i nl = needle || go (i + 1)) in
    go 0

(* --- independent winning-strategy checker --------------------------------- *)

(* [wins p region sigma game]: does p's positional strategy [sigma] win from
   every node of [region]? Build the strategy graph (p follows its single
   [sigma] move, the opponent moves freely, everything restricted to [region])
   and check two things:
     - it is closed: every p-node has exactly one sigma move that stays in
       [region], and every opponent node's moves all stay in [region];
     - no opponent-parity node lies on a cycle using only nodes of priority <=
       its own - equivalently, every cycle's maximum priority is p's parity, so
       p wins every infinite play.
   This is solver-independent, so it validates the derived strategies. *)
let par v = if PG.valueof v mod 2 = 0 then PG.Even else PG.Odd
let other = function PG.Even -> PG.Odd | PG.Odd -> PG.Even

let strat_succ p region sigma game v =
    if PG.playerof v = p then
        PG.StrSet.fold
            (fun (a, b) acc -> if PG.cmprands a v = 0 then PG.AdjSet.add b acc else acc)
            sigma PG.AdjSet.empty
    else
        PG.AdjSet.filter (fun x -> PG.AdjSet.mem x region) (PG.Graph.outgoingof v game)

let can_reach sc sub src dst =
    let vis = ref PG.AdjSet.empty in
    let rec dfs = function
        | [] -> false
        | v :: rest ->
            if PG.cmprands v dst = 0 then true
            else if PG.AdjSet.mem v !vis then dfs rest
            else begin
                vis := PG.AdjSet.add v !vis;
                dfs (PG.AdjSet.elements
                       (PG.AdjSet.filter (fun x -> PG.AdjSet.mem x sub) (sc v)) @ rest)
            end
    in dfs [src]

let wins p region sigma game =
    let sc = strat_succ p region sigma game in
    let closed = PG.AdjSet.for_all (fun v ->
        if PG.playerof v = p then
            PG.AdjSet.cardinal (sc v) = 1 && PG.AdjSet.subset (sc v) region
        else
            let o = PG.Graph.outgoingof v game in
            (not (PG.AdjSet.is_empty o)) && PG.AdjSet.subset o region) region in
    let opp = other p in
    let bad = PG.AdjSet.exists (fun m ->
        par m = opp &&
        (let sub = PG.AdjSet.filter (fun v -> PG.valueof v <= PG.valueof m) region in
         List.exists (fun s -> can_reach sc sub s m)
             (PG.AdjSet.elements (PG.AdjSet.filter (fun x -> PG.AdjSet.mem x sub) (sc m))))
    ) region in
    closed && not bad

(* --- games ---------------------------------------------------------------- *)

(* Even's e2<->o1 cycle (max prio 2) and e4 self-loop (prio 4): Even wins all. *)
let tiny () =
    let p = PG.empty in
    let (e2, p) = PG.add_node PG.Even 2 p in
    let (o1, p) = PG.add_node PG.Odd  1 p in
    let (e4, p) = PG.add_node PG.Even 4 p in
    PG.Graph.of_list [ (e2, [o1]); (o1, [e2; e4]); (e4, [e4]) ] p

(* a forced 2-cycle a<->b: the play is forced, so the higher priority decides *)
let cycle2 pa pra pb prb =
    let (a, g) = PG.add_node pa pra PG.empty in
    let (b, g) = PG.add_node pb prb g in
    PG.Graph.of_list [ (a, [b]); (b, [a]) ] g

(* [rings priorities per]: one disjoint forced ring per priority, [per] nodes
   each with that uniform priority. A forced ring cycles forever, so ring i is
   won entirely by the parity of [priorities.(i)] (even -> Even, else Odd) -
   a solver-independent ground truth on a large, multi-SCC game. *)
let rings priorities per =
    let arr = Array.of_list priorities in
    let n   = Array.length arr * per in
    let rec build i acc g =
        if i >= n then (List.rev acc, g)
        else
            let pl = if i mod 2 = 0 then PG.Even else PG.Odd in
            let (l, g') = PG.add_node pl arr.(i / per) g in
            build (i + 1) (l :: acc) g'
    in
    let (ll, g) = build 0 [] PG.empty in
    let a = Array.of_list ll in
    let adj = ref [] in
    for i = 0 to n - 1 do
        let ri = i / per and pos = i mod per in
        adj := (a.(i), [ a.(ri * per + ((pos + 1) mod per)) ]) :: !adj
    done;
    PG.Graph.of_list !adj g

(* the 13-node reference game; its winning split is Even=6, Odd=7 (verified by
   both zielonka and the independent scc_zielonka) *)
let sample () =
    let p = PG.empty in
    let (l_2, p)  = PG.add_node PG.Even 2  p in
    let (l_15,p)  = PG.add_node PG.Odd  15 p in
    let (l_4, p)  = PG.add_node PG.Even 4  p in
    let (l_6, p)  = PG.add_node PG.Even 6  p in
    let (l_8, p)  = PG.add_node PG.Even 8  p in
    let (l_10,p)  = PG.add_node PG.Even 10 p in
    let (l_3, p)  = PG.add_node PG.Odd  3  p in
    let (l_5, p)  = PG.add_node PG.Odd  5  p in
    let (l_7, p)  = PG.add_node PG.Odd  7  p in
    let (l_9, p)  = PG.add_node PG.Odd  9  p in
    let (l_11,p)  = PG.add_node PG.Odd  11 p in
    let (l_13,p)  = PG.add_node PG.Odd  13 p in
    let (l_99,p)  = PG.add_node PG.Odd  99 p in
    PG.Graph.of_list [
        (l_2,[l_4;l_11]); (l_4,[l_2;l_8;l_6]); (l_6,[l_3;l_5;l_7;l_9]);
        (l_8,[l_7;l_5;l_2]); (l_10,[l_13;l_15]); (l_3,[l_2;l_4]); (l_5,[l_7;l_9]);
        (l_7,[l_10]); (l_9,[l_3;l_5;l_10;l_99]); (l_11,[l_8]); (l_13,[l_15]);
        (l_99,[l_99]); (l_15,[l_13]);
    ] p

let big_priorities = List.init 40 (fun j -> 1 + (j mod 6))   (* 40 rings, prios 1..6 *)
let big_per        = 8                                        (* => 320 nodes *)
let big_expected_even =
    big_per * List.length (List.filter (fun p -> p mod 2 = 0) big_priorities)

(* --- main solver correctness (regression tests for the else-branch fix) ---- *)

let tiny_even_wins _cx =
    let (w0, w1) = (Solve.zielonka (tiny ())).PG.regions in
    check int "Even wins all three" 3 (card w0);
    check int "Odd wins none"       0 (card w1)
;;

(* forced cycles: the parity of the max priority is the winner (ground truth) *)
let forced_cycles _cx =
    let win name g exp_even exp_odd =
        let (w0, w1) = (Solve.zielonka g).PG.regions in
        check int (name ^ " Even") exp_even (card w0);
        check int (name ^ " Odd")  exp_odd  (card w1)
    in
    win "odd 3<->1"   (cycle2 PG.Odd 3 PG.Odd 1)   0 2;   (* max 3 odd  -> Odd  *)
    win "even 2<->4"  (cycle2 PG.Even 2 PG.Even 4) 2 0;   (* max 4 even -> Even *)
    win "mixed 5<->2" (cycle2 PG.Odd 5 PG.Even 2)  0 2    (* max 5 odd  -> Odd  *)
;;

(* the else branch fires here (opponent wins a sub-region); zielonka must match
   the constructed ground truth *)
let rings_ground_truth _cx =
    let g = rings big_priorities big_per in
    let n = big_per * List.length big_priorities in
    let (w0, w1) = (Solve.zielonka g).PG.regions in
    check int "Even region = ground truth" big_expected_even (card w0);
    check int "Odd region  = ground truth" (n - big_expected_even) (card w1);
    check int "regions partition"          n (card w0 + card w1);
    check int "regions disjoint"           0 (card (PG.AdjSet.inter w0 w1))
;;

let sample_split _cx =
    let (w0, w1) = (Solve.zielonka (sample ())).PG.regions in
    check int "Even wins 6" 6 (card w0);
    check int "Odd wins 7"  7 (card w1)
;;

(* lazy_zielonka shares the (fixed) region logic, so its regions must also match
   the ground truth. Its strategy is validated separately by [strategies_win]. *)
let lazy_rings_ground_truth _cx =
    let g = rings big_priorities big_per in
    let n = big_per * List.length big_priorities in
    let (w0, w1) = (Solve.lazy_zielonka g).PG.regions in
    check int "lazy Even region = ground truth" big_expected_even (card w0);
    check int "lazy Odd region  = ground truth" (n - big_expected_even) (card w1)
;;

(* the three solvers must agree on the winning regions *)
let solvers_agree _cx =
    let g = sample () in
    let (z0, z1) = (Solve.zielonka g).PG.regions in
    let (s0, s1) = (Solve.scc_zielonka g).PG.regions in
    let (l0, l1) = (Solve.lazy_zielonka g).PG.regions in
    check bool "zielonka = scc_zielonka"  true (eqset z0 s0 && eqset z1 s1);
    check bool "zielonka = lazy_zielonka" true (eqset z0 l0 && eqset z1 l1)
;;

(* every solver's derived strategy must actually win its region, for both
   players, on both a small irregular game and the large multi-SCC one *)
let strategies_win _cx =
    let on gname g =
        let each solver sname =
            let s = solver g in
            let (w0, w1) = s.PG.regions and (st0, st1) = s.PG.strategy in
            check bool (gname ^ "/" ^ sname ^ ": Even strategy wins W0") true
                (wins PG.Even w0 st0 g);
            check bool (gname ^ "/" ^ sname ^ ": Odd strategy wins W1") true
                (wins PG.Odd w1 st1 g)
        in
        each Solve.zielonka      "zielonka";
        each Solve.lazy_zielonka "lazy_zielonka";
        each Solve.scc_zielonka  "scc_zielonka"
    in
    on "sample" (sample ());
    on "rings"  (rings big_priorities big_per)
;;

(* --- SCC solver on a large multi-SCC game --------------------------------- *)

let big_is_multi_scc _cx =
    let g = rings big_priorities big_per in
    let module Scc = PG.Graph.Scc in
    let sol = Scc.tarjan g in
    let ids = Hashtbl.create 64 in
    Scc.SccTbl.iter (fun (k : Scc.sccnode) _ -> Hashtbl.replace ids k.Scc.link ())
        sol.Scc.sccs;
    check int "320 nodes"        320 (PG.Nodes.cardinal g);
    check int "one SCC per ring" 40  (Hashtbl.length ids)
;;

let scc_ground_truth _cx =
    let g = rings big_priorities big_per in
    let n = big_per * List.length big_priorities in
    let (w0, w1) = (Solve.scc_zielonka g).PG.regions in
    check int "Even region = ground truth" big_expected_even (card w0);
    check int "Odd region  = ground truth" (n - big_expected_even) (card w1)
;;

(* --- dot rendering -------------------------------------------------------- *)

let dot_render _cx =
    let g   = sample () in
    let sol = Solve.zielonka g in
    let dot = Dot.to_dot ~name:"t" ~solution:sol g in
    check bool "is a digraph"          true (contains dot "digraph \"t\" {");
    check bool "Even region fill"      true (contains dot "lightblue");
    check bool "Odd region fill"       true (contains dot "lightsalmon");
    check bool "owner shape (diamond)" true (contains dot "shape=\"diamond\"");
    check bool "closes the graph"      true (contains dot "}\n")
;;

let () =
    run "ParityGame" [
        "solve", [
            test_case "tiny even wins"   `Quick tiny_even_wins;
            test_case "forced cycles"    `Quick forced_cycles;
            test_case "rings ground truth" `Quick rings_ground_truth;
            test_case "lazy ground truth"  `Quick lazy_rings_ground_truth;
            test_case "sample split"     `Quick sample_split;
            test_case "solvers agree"    `Quick solvers_agree;
        ];
        "strategy", [
            test_case "strategies win"   `Quick strategies_win;
        ];
        "scc", [
            test_case "multi-scc graph"  `Quick big_is_multi_scc;
            test_case "ground truth"     `Quick scc_ground_truth;
        ];
        "dot", [
            test_case "render split"     `Quick dot_render;
        ];
    ]
