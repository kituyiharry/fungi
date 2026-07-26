(******************************************************************************
*                                                                             *
*                       Functions to Solve Parity Games                       *
*                                                                             *
*   Each solver computes the winning REGIONS with a recursion, then synthesises*
*   a correct positional winning STRATEGY for each player directly from those  *
*   regions (see [winning_strategy]). The strategy is not threaded through the *
*   recursion: a correct attractor strategy needs the move that carries each   *
*   node toward the target, which the old in-flight [validstrategy] heuristic  *
*   could not track (it dropped a player's own forced moves), so it has been   *
*   removed. Strategies are validated by the [strategies_win] test.            *
*                                                                             *
*******************************************************************************)
open Game;;

module PG = ParityGame

let empty_region = (PG.AdjSet.empty, PG.AdjSet.empty)

let (<+>) x y = PG.AdjSet.union x y

(* ------------------------------------------------------------------------- *)
(*  Winning regions                                                          *)
(* ------------------------------------------------------------------------- *)

(** [zielonka_regions game] returns the winning regions [(W0, W1)] in the fixed
    (Even = W0, Odd = W1) layout. Standard recursive (Zielonka) algorithm:
    https://en.wikipedia.org/wiki/Parity_game. Both the base case and the else
    branch place the freshly-won region on the {e current player}'s side of the
    pair - assigning it in a hardcoded order mislabels every game whose recursion
    takes the opponent-dominion (else) path. *)
let rec zielonka_regions game =
    if PG.Nodes.is_empty game then empty_region
    else
        let node = PG.max_elt game in
        let i    = PG.omega node in
        let u    = PG.cluster node game in
        let a    = PG.attr i u game in
        let (w_0, w_1) = zielonka_regions (PG.carve game a) in
        let w_1_i = (match i with Even -> w_1 | Odd -> w_0) in
        if PG.AdjSet.is_empty w_1_i then
            (match i with
             | Even -> (PG.collective game, PG.AdjSet.empty)
             | Odd  -> (PG.AdjSet.empty, PG.collective game))
        else
            let flip = PG.invert i in
            let b    = PG.attr flip w_1_i game in
            let (w_0', w_1') = zielonka_regions (PG.carve game b) in
            (match i with
             | Even -> (w_0', w_1' <+> b)
             | Odd  -> (w_0' <+> b, w_1'))
;;

(** [scc_regions game] - an SCC-decomposed variant. The condensation is a DAG, so
    it always has a terminal SCC (no edges leaving it): a {e closed} subgame that
    [zielonka_regions] solves exactly. Its winning regions are then dominions in
    the whole game (a player's strategy inside the SCC keeps play there forever),
    so we attract each across the full game, remove them, and recurse. Only ever
    runs the exponential recursion on one SCC at a time, and agrees with
    [zielonka_regions]. *)
let scc_regions game =
    let module Scc = PG.Graph.Scc in
    let components g =
        let sol = Scc.tarjan g in
        let tbl = Hashtbl.create 64 in
        Scc.SccTbl.iter (fun (k : Scc.sccnode) node ->
            let cur = try Hashtbl.find tbl k.Scc.link with Not_found -> PG.AdjSet.empty in
            Hashtbl.replace tbl k.Scc.link (PG.AdjSet.add node cur)
        ) sol.Scc.sccs;
        Hashtbl.fold (fun _ s acc -> s :: acc) tbl []
    in
    let is_terminal g comp =
        PG.AdjSet.for_all
            (fun n -> PG.AdjSet.subset (PG.Graph.outgoingof n g) comp) comp
    in
    let rec go g =
        if PG.Nodes.is_empty g then empty_region
        else
            let bottom = List.find (is_terminal g) (components g) in
            let others = PG.AdjSet.diff (PG.collective g) bottom in
            let (c0, c1) = zielonka_regions (PG.carve g others) in
            let a0   = PG.attr PG.Even c0 g in
            let g1   = PG.carve g a0 in
            let a1   = PG.attr PG.Odd  c1 g1 in
            let (w0, w1) = go (PG.carve g1 a1) in
            (a0 <+> w0, a1 <+> w1)
    in
    go game
;;

(* ------------------------------------------------------------------------- *)
(*  Positional winning-strategy synthesis (from the final regions)           *)
(* ------------------------------------------------------------------------- *)

let other_player = function PG.Even -> PG.Odd | PG.Odd -> PG.Even

(* [attr_strat p target work game]: the p-attractor of [target] inside the
   sub-game induced on [work], together with the plays that realise it - each
   attracted p-owned node moves to a node already in the attractor (one step
   toward the target), and p-owned target nodes move to a successor staying in
   [work]. This is the move information a correct attractor strategy needs. *)
let attr_strat p target work game =
    let succ_in v =
        PG.AdjSet.filter (fun x -> PG.AdjSet.mem x work) (PG.Graph.outgoingof v game) in
    let first s = match PG.AdjSet.elements s with x :: _ -> Some x | [] -> None in
    let attr  = ref target in
    let strat = ref PG.StrSet.empty in
    PG.AdjSet.iter (fun v ->
        if PG.playerof v = p then
            match first (succ_in v) with
            | Some w -> strat := PG.StrSet.add (v, w) !strat
            | None   -> ()) target;
    let changed = ref true in
    while !changed do
        changed := false;
        PG.AdjSet.iter (fun v ->
            if not (PG.AdjSet.mem v !attr) then begin
                let outs = succ_in v in
                if PG.playerof v = p then
                    (match first (PG.AdjSet.filter (fun x -> PG.AdjSet.mem x !attr) outs) with
                     | Some w ->
                        attr  := PG.AdjSet.add v !attr;
                        strat := PG.StrSet.add (v, w) !strat;
                        changed := true
                     | None -> ())
                else
                    if not (PG.AdjSet.is_empty outs) && PG.AdjSet.subset outs !attr then begin
                        attr := PG.AdjSet.add v !attr; changed := true
                    end
            end) work
    done;
    (!attr, !strat)

(* A positional winning strategy for [p] within its winning region [work]. Peel
   the highest priority [d]: if its parity is [p]'s, p-attract those vertices
   (recording p's moves) and recurse on the rest; otherwise opponent-attract them
   and recurse, giving the p-owned nodes in that block a region-staying move. *)
let rec winning_strategy p work game =
    if PG.AdjSet.is_empty work then PG.StrSet.empty
    else
        let d = PG.AdjSet.fold (fun v m -> max m (PG.valueof v)) work min_int in
        let u = PG.AdjSet.filter (fun v -> PG.valueof v = d) work in
        let par = if d mod 2 = 0 then PG.Even else PG.Odd in
        if par = p then
            let (a, sa) = attr_strat p u work game in
            PG.StrSet.union sa (winning_strategy p (PG.AdjSet.diff work a) game)
        else
            let (b, _) = attr_strat (other_player p) u work game in
            let rest   = PG.AdjSet.diff work b in
            let srest  = winning_strategy p rest game in
            PG.AdjSet.fold (fun v acc ->
                if PG.playerof v <> p then acc
                else
                    let outs = PG.Graph.outgoingof v game in
                    let pick s = match PG.AdjSet.elements
                                         (PG.AdjSet.filter (fun x -> PG.AdjSet.mem x s) outs) with
                        | w :: _ -> Some w | [] -> None in
                    (match (match pick rest with Some _ as r -> r | None -> pick work) with
                     | Some w -> PG.StrSet.add (v, w) acc
                     | None   -> acc)
            ) b srest

(** [strategies game (w0, w1)] - a positional winning strategy for each player
    over their winning region. *)
let strategies game (w0, w1) =
    (winning_strategy PG.Even w0 game, winning_strategy PG.Odd w1 game)

(* ------------------------------------------------------------------------- *)
(*  Public solvers: correct regions + correct native strategy                *)
(* ------------------------------------------------------------------------- *)

let solution_of regions_fn game =
    let regions = regions_fn game in
    { PG.regions; strategy = strategies game regions }

let zielonka game = solution_of zielonka_regions game

(** Retained for API compatibility. Its historical distinction from [zielonka]
    was a {e lazy} strategy-building path; that path (built on the now-removed
    [validstrategy] heuristic) was unsound, so [lazy_zielonka] now shares the
    same region recursion and correct strategy synthesis - it is equivalent to
    [zielonka]. *)
let lazy_zielonka game = solution_of zielonka_regions game

let scc_zielonka game = solution_of scc_regions game
