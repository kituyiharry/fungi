open Paritygame.Game;;
open Paritygame.Solve;;
open Paritygame.Pickle;;

type edg = ParityGame.play ParityGame.StrSet.set;;

(* Printing functions *)

let ppluck fromset =
    match ParityGame.StrSet.max_elt_opt fromset with
    | Some(node) -> Some (node, (ParityGame.StrSet.remove node fromset))
    | _ -> None
;;

let pp_prio (ParityGame.Priority (value, pplayer)) = match pplayer with
    | ParityGame.Even -> Format.printf " (%02d) " value
    | ParityGame.Odd  -> Format.printf " <%02d> " value
;;

let pp_node (ParityGame.Label (prio, rand )) =
    let () =    Format.printf "%03d:" rand in
    pp_prio prio
;;

let rec pp_strat: edg -> unit = fun strat ->
    if ParityGame.StrSet.is_empty strat then
        Format.printf " ∅ "
    else
        match ppluck strat with
        | None -> ()
        | Some(((f, t), rest)) -> (
            pp_node f; Format.printf " -> "; pp_node t; Format.printf ",";
            pp_strat rest
        );
;;

(* Convenience functions for printing in the REPl *)
(** [asplayerprio ParityGame.t (node * priority)]
helper to show parity game as a graph without the random generated ids *)
let asplayerprio (ParityGame.Label (ParityGame.Priority (_value, player) as prio , _rand)) =
    (player, prio)
;;

let print_player_prio_dir (_, (ParityGame.Priority (value,pplayer)))  = match pplayer with
    | ParityGame.Even -> Format.printf " (%02d) " value
    | ParityGame.Odd  -> Format.printf " <%02d> " value
;;


let print_player_prio (ParityGame.Label(ParityGame.Priority (value,pplayer),_))  = match pplayer with
    | ParityGame.Even -> Format.printf " (%02d) " value
    | ParityGame.Odd  -> Format.printf " <%02d> " value
;;

let pp_adjset set =
    Format.printf "{$: {";
    if ParityGame.AdjSet.is_empty set then
        Format.printf " ∅ "
    else
        List.iter (print_player_prio_dir)
            (
                List.map (asplayerprio) @@ ParityGame.AdjSet.elements set
            );
    Format.printf "} $} "
;;


let pp_parity_game = fun game ->
    Format.printf "G: {";
    List.iter (print_player_prio)
        (
            ParityGame.bindings game
        );
    Format.printf "} "
;;

let pp_solution:(ParityGame.solution) -> unit = fun sol ->
    let ((w0, w1), (s0, s1)) = (sol.regions, sol.strategy) in
    Format.printf "\n\t+ ---  ---  --- --- --- --- --- --- --- --- --- --- ---";
    Format.printf "\n\t|\n\t| W0: ";  pp_adjset w0;
    Format.printf "\n\t| S0:    <";  pp_strat s0; Format.printf ">";
    Format.printf "\n\t|\n\t| ---  ---  --- --- --- --- --- --- --- --- --- --- ---";
    Format.printf "\n\t|\n\t| W1: ";  pp_adjset w1;
    Format.printf "\n\t| S1:    <";  pp_strat s1; Format.printf ">";
    Format.printf "\n\t|\n\t+ ---  ---  --- --- --- --- --- --- --- --- --- --- ---"
;;

open ParityGame.Graph;;
open ParitySerializer;;

let () =

    let _ = Format.printf "WARNING!: strategies may not be correct! Graph will
    be split into winning regions - inter-cluster edges will not be shown!\n" in

    if Array.length Sys.argv < 2 then
        Format.printf "missing file! \n run <exe> <file> where <file> contains
        the parity game data and <exe> is this executable"
    else
        let file = Sys.argv.(1) in

        let lines_from_file filename =
            let ic = open_in filename in
            Seq.unfold (fun () ->
              try Some (input_line ic, ())
              with End_of_file -> close_in ic; None
            ) () 
        in 

        let p = load (lines_from_file file) in

        let _ = pp_parity_game p in
        let sol = lazy_zielonka p in 
        let _ = pp_solution sol in

        let gs = ParityGame.Nodes.cardinal p in

        let gattrs  = ParitySerializer.StyleTbl.create  2 in 

        let clattrs = ParitySerializer.ClstrTbl.create  2 in

        let ostyls  = ParitySerializer.StyleTbl.create  (1) in 
        let estyls  = ParitySerializer.StyleTbl.create  (1) in 
        let nstyls  = ParitySerializer.StyleTbl.create  (1) in 

        (* odd and even node and cluster attributes (shared) *)
        let _ = StyleTbl.add ostyls "color" "red" in
        let _ = StyleTbl.add estyls "color" "green" in
        let _ = StyleTbl.add nstyls "color" "black" in

        (* odd and even node and cluster attributes (shared) *)
        let _ = ClstrTbl.add clattrs 0  estyls in
        let _ = ClstrTbl.add clattrs 1  ostyls in

        (* per node attributes *)
        let nattrs  = ParitySerializer.AttrbTbl.create (gs) in 
        let eattrs  = ParitySerializer.AttrbTbl.create (gs) in 

        let _ = StyleTbl.add gattrs "rankdir" "td" in

        let (w0, w1) = sol.regions  in
        let (s0, s1) = sol.strategy in
        
        (* update per node styles based on win sets *)
        let _ = ParityGame.Nodes.iter (
            fun n _ -> 
                if AdjSet.mem n w0 then 
                    AttrbTbl.add nattrs (ParitySer.string_of_elt n) estyls
                else if AdjSet.mem n w1 then
                    AttrbTbl.add nattrs (ParitySer.string_of_elt n) ostyls
                else ()
        ) p in

        (* update per edge styles based on win sets *)
        let _ = ParityGame.Graph.edgeseq p |> Seq.iter (fun ((f, t) as e) -> 
            let ts = ParitySer.string_of_elt t in
            let fs = ParitySer.string_of_elt f in
            let es = fs ^ "-" ^ ts in
            if ParityGame.StrSet.mem e s0 then
                AttrbTbl.add eattrs es estyls
            else if ParityGame.StrSet.mem e s1 then
                AttrbTbl.add eattrs es ostyls
            else
                AttrbTbl.add eattrs es nstyls
        ) in

        let clstrs = Scc.SccMap.empty in 
        let clstrs = Scc.SccMap.add 0 ([], ParityGame.carve p w1) clstrs in
        let clstrs = Scc.SccMap.add 1 ([], ParityGame.carve p w0) clstrs in

        let _ = Format.print_newline () in
        let _ = Format.print_newline () in

        (*()*)
        let ss = ParitySerializer.to_dot_cluster ?dir:(Some true) file
            (fun id _ -> if id = 0 then "Even" else "Odd")
            gattrs clattrs nattrs eattrs clstrs in
        ss |> Seq.concat |> Seq.iter (fun s -> Format.printf "%s" (s ()))

