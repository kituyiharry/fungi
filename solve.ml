open Paritygame

(* Largest priority node in the game *)
let max_priority_node = ParityGame.max_elt

let empty_strategy    = (ParityGame.StrSet.empty, ParityGame.StrSet.empty)
let empty_region      = (ParityGame.AdjSet.empty, ParityGame.AdjSet.empty)

(* Union shorthand *)
let (<->) x y = ParityGame.StrSet.union x y
let (<+>) x y = ParityGame.AdjSet.union x y

(** [zielonka PGame.t PGame.solution]
    Recursive algorithm which produces winning sets of the game
    https://oliverfriedmann.com/downloads/papers/recursive_lower_bound.pdf
*)
let rec zielonka:'a ParityGame.Nodes.t -> ParityGame.solution = fun game ->
    if ParityGame.Nodes.is_empty game then
        { regions=empty_region; strategy=empty_strategy; }
    else
        let node         = max_priority_node game in
        let i            = ParityGame.omega node in
        let u            = ParityGame.cluster node game in
        let tau          = ParityGame.strategy i u game ParityGame.StrSet.empty in
        let (a, tau')    = ParityGame.attr (i) u game in
        let g_a          = ParityGame.carve game a in
        let { ParityGame.regions=(w_0, w_1); strategy=(s_0, s_1); } = zielonka g_a in
        let (_wi, w_1_i) = (
            match i with
            | Even -> (w_0, w_1)
            | Odd  -> (w_1, w_0)
        ) in
        if ParityGame.AdjSet.is_empty w_1_i then
            let strat = match i with
                | Even -> ((s_0 <-> tau <-> tau'), ParityGame.StrSet.empty)
                | Odd  -> (ParityGame.StrSet.empty, (s_1 <-> tau <-> tau')) in
            { ParityGame.regions=((ParityGame.collective game), ParityGame.AdjSet.empty); strategy=strat }
        else
            let flip     = ParityGame.invert i in
            let (b, rho) = ParityGame.attr (flip) w_1_i game in
            let g_b      = ParityGame.carve game b in
            let { ParityGame.regions=(w_0', w_1'); strategy=(s_0', s_1') } = zielonka g_b in
            let strat' = match flip with
                | Even -> ((rho <-> s_0' <-> s_0), s_1')
                | Odd  -> (s_0', (rho <-> s_1' <-> s_1)) in
            { ParityGame.regions=(w_1' <+> b, w_0'); strategy=strat' }
;;
