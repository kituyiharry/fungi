open Paritygame.Game;;
open Paritygame.Solve;;

type edg = ParityGame.play ParityGame.StrSet.set;;

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

let () =
    let p = ParityGame.empty in

    (* <> is Odd, () is Even *)

    (* I also assume there are no same priority nodes to make life easy *)
    let (l_2, p)  = ParityGame.add_node  ParityGame.Even 2  p in
    let (l_15,p)  = ParityGame.add_node  ParityGame.Odd  15 p in
    let (l_4, p)  = ParityGame.add_node  ParityGame.Even 4  p in
    let (l_6, p)  = ParityGame.add_node  ParityGame.Even 6  p in
    let (l_8, p)  = ParityGame.add_node  ParityGame.Even 8  p in
    let (l_10,p)  = ParityGame.add_node  ParityGame.Even 10 p in
    let (l_3, p)  = ParityGame.add_node  ParityGame.Odd  3  p in
    let (l_5, p)  = ParityGame.add_node  ParityGame.Odd  5  p in
    let (l_7, p)  = ParityGame.add_node  ParityGame.Odd  7  p in
    let (l_9, p)  = ParityGame.add_node  ParityGame.Odd  9  p in
    let (l_11,p)  = ParityGame.add_node  ParityGame.Odd  11 p in
    let (l_13,p)  = ParityGame.add_node  ParityGame.Odd  13 p in
    let (l_99,p)  = ParityGame.add_node  ParityGame.Odd  99 p in

    let p_adjlist =
        [
            (l_2,  [l_4;  l_11]);
            (l_4,  [l_2;  l_8;  l_6]);
            (l_6,  [l_3;  l_5;  l_7; l_9]);
            (l_8,  [l_7;  l_5;  l_2]);
            (l_10, [l_13; l_15]);
            (l_3,  [l_2;  l_4]);
            (l_5,  [l_7;  l_9]);
            (l_7,  [l_10]);
            (l_9,  [l_3;  l_5;  l_10; l_99]);
            (l_11, [l_8]);
            (l_13, [l_15]);
            (l_99, [l_99]);
            (l_15, [l_13])
        ] in

    let p = ParityGame.Graph.of_list p_adjlist p  in
    let _ = pp_parity_game p in
    pp_solution @@ lazy_zielonka p
