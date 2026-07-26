open Paritygame
open Paritygame.Game

module PG = ParityGame

let card   = PG.AdjSet.cardinal
let card_s = PG.StrSet.cardinal

(* the 13-node reference game (Even wins 6, Odd wins 7) *)
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

(* [rings priorities per]: one disjoint forced ring per priority, [per] nodes
   each. A forced ring is won entirely by the parity of its priority, so the
   winners are known by construction (ground truth). Multiple rings => multiple
   SCCs and a genuine Even/Odd split. *)
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

let timed f =
    let t0 = Unix.gettimeofday () in
    let r  = f () in
    (r, Unix.gettimeofday () -. t0)

(* solve the reference game and render it to dot *)
let show_dot () =
    let g   = sample () in
    let sol = Solve.zielonka g in
    let (w0, w1) = sol.PG.regions in
    Printf.printf "=== dot: sample (%d nodes) | Even wins %d, Odd wins %d ===\n"
        (PG.Nodes.cardinal g) (card w0) (card w1);
    print_endline (Dot.to_dot ~name:"sample" ~solution:sol g);
    print_newline ()

(* a large game whose winners are known by construction: compare the solvers on
   time and against the ground truth *)
let benchmark () =
    let priorities = List.init 40 (fun j -> 1 + (j mod 6)) in
    let per = 8 in
    let g   = rings priorities per in
    let n   = per * List.length priorities in
    let exp_even = per * List.length (List.filter (fun p -> p mod 2 = 0) priorities) in
    let tag c = if c = exp_even then "  [matches ground truth]" else "  (! disagrees)" in
    Printf.printf "=== benchmark: %d nodes across %d SCCs ===\n" n (List.length priorities);
    Printf.printf "  ground truth : Even=%d Odd=%d\n" exp_even (n - exp_even);
    let (z, tz) = timed (fun () -> Solve.zielonka g) in
    let (y, ty) = timed (fun () -> Solve.lazy_zielonka g) in
    let (s, ts) = timed (fun () -> Solve.scc_zielonka g) in
    let (zw0, zw1) = z.PG.regions and (sw0, sw1) = s.PG.regions in
    let (ss0, ss1) = s.PG.strategy in
    let (yw0, yw1) = y.PG.regions and (ys0, ys1) = y.PG.strategy in
    Printf.printf "  zielonka     : Even=%-4d Odd=%-4d  %.4f s%s\n"
        (card zw0) (card zw1) tz (tag (card zw0));
    Printf.printf "  lazy_zielonka: Even=%-4d Odd=%-4d  %.4f s%s\n"
        (card yw0) (card yw1) ty (tag (card yw0));
    (* strategy is derived from the regions (see winning_strategy), so both sides
       are populated and winning - verified by the strategies_win test *)
    Printf.printf "  lazy strategy: Even=%d edges, Odd=%d edges\n"
        (card_s ys0) (card_s ys1);
    Printf.printf "  scc_zielonka : Even=%-4d Odd=%-4d  %.4f s%s\n"
        (card sw0) (card sw1) ts (tag (card sw0));
    Printf.printf "  scc strategy : Even=%d edges, Odd=%d edges\n\n" (card_s ss0) (card_s ss1)

let () =
    show_dot ();
    benchmark ()
