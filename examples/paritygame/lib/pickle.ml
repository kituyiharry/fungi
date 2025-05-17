(******************************************************************************
*                                                                             *
*       Export | Import the game in a format compatible with                  *
*       https://github.com/tcsprojects/pgsolver/blob/master/doc/pgsolver.pdf  *
*                                                                             *
*       ⟨parity game⟩ ::= [parity ⟨identifier⟩;] ⟨node spec⟩+                 *
*       ⟨node spec⟩ ::= ⟨identifier⟩⟨priority⟩⟨owner⟩ ⟨successors⟩[⟨name⟩] ;  *
*       ⟨identifier⟩ ::= N                                                    *
*       ⟨priority⟩ ::= N                                                      *
*       ⟨owner⟩ ::= 0 |1                                                      *
*       ⟨successors⟩ ::= ⟨identifier⟩(, ⟨identifier⟩)∗                        *
*       ⟨name⟩ ::= \" ( any ASCII string not containing ‘\"’)"                *
*                                                                             *
*                                                                             *
*******************************************************************************)
open Game;;

let edges outgoing = 
    ParityGame.AdjSet.elements outgoing
    |> List.map (fun x ->  Int.to_string ((ParityGame.labelof x)))
    |> String.concat ", "
;;

let repr_prio (ParityGame.Priority (value, pplayer)) = match pplayer with
  | ParityGame.Even -> Printf.sprintf "(Even %d)"  value
  | ParityGame.Odd  -> Printf.sprintf "(Odd  %d)"  value
;;

let repr_node (ParityGame.Label (Priority (p, _pl), rand)) =
  (*Printf.sprintf "%d: %s" rand (repr_prio prio)*)
  Printf.sprintf "\"%d : %d\"" rand p 
;;

let export_player = function
    | ParityGame.Even -> 0
    | ParityGame.Odd  -> 1
;;

let monotonic = ref 0;;

module ParitySer = struct 
    let string_of_elt  = repr_node
    let string_of_wgt  = Fun.const ""
end

module ParitySerializer = ParityGame.Graph.Serialize (ParitySer);;

let dump game = 
    let total  = ParityGame.Graph.NodeMap.cardinal game in
    let () = Format.printf "parity %d;\n" total in
    let bnds = ParityGame.Graph.NodeMap.bindings game in
    List.iter (fun (b, {ParityGame.Graph.out;_})->  
        Format.printf "%d %d %d %s \"%s\";\n" 
            (ParityGame.labelof b)
            (ParityGame.valueof b)
            (export_player (ParityGame.playerof b))
            (edges out)
            (repr_node b)
    ) bnds
;;

open ParityGame;;

let defaultnode rand = 
    Label (Priority (1, Even), rand)
;;

(* Using String.map to remove a specific character by replacing it with empty string *)
let strip_char str char_to_remove =
  String.to_seq str
  |> Seq.filter (fun c -> c <> char_to_remove)
  |> String.of_seq
;;

let load (strseq: string Seq.t) = 
    let _ = Format.printf "Parsing game stream ... \n\n" in
    let game = ParityGame.empty in
    let game', _remnodes = strseq
    |> Seq.map (String.trim)
    |> Seq.filter_map (fun s -> 
        (*let _ = Format.printf "parse line %s \n" s in*)
        match s with
        | "" -> None
        | s' -> 
            let lead = String.get s' 0 in
            match int_of_string_opt (Format.sprintf "%c" lead) with
            | None -> None
            | _    -> Some s'
    )
    |> Seq.filter_map (fun s -> 
        let nodes = String.split_on_char ' ' s 
            |> List.filter (fun t -> match t with 
                | ""  -> false 
                | "," -> false 
                | _   -> true
            )
        in match nodes with 
            | id :: prio :: plyr :: rest -> (
                match int_of_string_opt prio with
                    | None ->  None
                    | Some prio' -> (
                        match (String.trim plyr) with
                            | "0" -> Some (
                                    (Label ((Priority (prio', Even)), int_of_string id))
                                    , rest
                                )
                            | "1" -> Some (
                                    (Label ((Priority (prio', Odd)),  int_of_string id))
                                    , rest
                                )
                            |  _  -> 
                                let _ = Format.printf "failed to parse node (%s %s for %s) \n" id prio plyr in
                                None
                        )
                )
            | _ -> 
                let _ = Format.printf "failed to parse nodes list \n" in
                None
    )
    |> Seq.fold_left (fun (game', fo) (n, r) ->
        let game'' = ParityGame.Graph.ensure n game' in
        (game'', (n, r) :: fo)
    ) (game, [])
    in 
    List.fold_left (fun game'' (n, c) -> 
        List.fold_left (fun game' rand -> 
            match int_of_string_opt (String.trim @@ strip_char rand ',') with
            | Some p ->
                ParityGame.connect game' n p
            | None -> 
                (*let _ = Format.printf "failed parsing: %s\n" rand in *)
                game'
        ) game'' c ) game' _remnodes
    (*game'*)
;;

