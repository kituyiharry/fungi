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
open Paritygame;;


let edges outgoing = 
    ParityGame.AdjSet.elements outgoing
    |> List.map (fun x ->  Int.to_string ((ParityGame.labelof x)))
    |> String.concat ", "
;;

let repr_prio (ParityGame.Priority (value, pplayer)) = match pplayer with
  | ParityGame.Even -> Printf.sprintf "(Even %d)" value
  | ParityGame.Odd  -> Printf.sprintf "(Odd  %d)" value
;;

let repr_node (ParityGame.Label (prio, _rand )) =
  repr_prio prio
;;

let dump game = 
    let total  = ParityGame.Graph.NodeMap.cardinal game in
    let () = Format.printf "parity %d;\n" total in
    let bnds = ParityGame.Graph.NodeMap.bindings game in
    List.iter (fun (b, (_in, out, _node))->  
        Format.printf "%d %d %d %s \"%s\";\n" 
            (ParityGame.labelof b)
            (ParityGame.valueof b)
            (ParityGame.export_player (ParityGame.playerof b))
            (edges out)
            (repr_node b)
    ) bnds
;;

