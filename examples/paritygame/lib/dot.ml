(******************************************************************************
*                                                                             *
*        Render a parity game (and optionally a Zielonka solution) to         *
*        graphviz dot for visual inspection.                                  *
*                                                                             *
*   Drawing conventions:                                                      *
*     - node SHAPE encodes the owner : Even = box, Odd = diamond              *
*     - node LABEL is "<priority>:<owner>"                                    *
*     - with a solution, node FILL encodes the winner:                        *
*         Even's region (W0) = lightblue, Odd's region (W1) = lightsalmon     *
*     - STRATEGY edges are highlighted: Even = blue, Odd = red, bold          *
*                                                                             *
*   The rendering goes through the library's Graph.Serialize functor, so all  *
*   ids / labels / attribute values are quoted and escaped.                   *
*                                                                             *
*******************************************************************************)
open Game

module PG = ParityGame

(* a stable, unique dot id per node - its internal label integer *)
let node_id n = Printf.sprintf "n%d" (PG.labelof n)

let player_name = function PG.Even -> "Even" | PG.Odd -> "Odd"

(* text displayed inside a node *)
let node_text n = Printf.sprintf "%d:%s" (PG.valueof n) (player_name (PG.playerof n))

module Ser = PG.Graph.Serialize (struct
    let string_of_elt    = node_id
    let string_of_wgt () = ""   (* parity edges carry [unit]: no weight labels *)
    let elt_of_string _  = failwith "Dot: elt_of_string is unused for rendering"
    let wgt_of_string _  = ()
end)

(** [to_dot ?name ?solution game] renders [game] to a dot string. When a
    [solution] is supplied the winning regions and strategy edges are
    highlighted; otherwise just the game structure is drawn. *)
let to_dot ?(name = "paritygame") ?solution game =
    let gattrs = Ser.StyleTbl.create 2 in
    Ser.StyleTbl.add gattrs "rankdir" "LR";
    let nattrs = Ser.AttrbTbl.create 32 in
    let eattrs = Ser.AttrbTbl.create 32 in
    let (w0, w1) = match solution with
        | Some s -> s.PG.regions
        | None   -> (PG.AdjSet.empty, PG.AdjSet.empty)
    in
    (* per-node styling: shape by owner, fill by winning region *)
    PG.Nodes.iter (fun n _ ->
        let a = Ser.StyleTbl.create 4 in
        Ser.StyleTbl.add a "label" (node_text n);
        Ser.StyleTbl.add a "shape"
            (match PG.playerof n with PG.Even -> "box" | PG.Odd -> "diamond");
        (match solution with
         | None   -> ()
         | Some _ ->
            Ser.StyleTbl.add a "style" "filled";
            if      PG.AdjSet.mem n w0 then Ser.StyleTbl.add a "fillcolor" "lightblue"
            else if PG.AdjSet.mem n w1 then Ser.StyleTbl.add a "fillcolor" "lightsalmon");
        Ser.AttrbTbl.replace nattrs (node_id n) a
    ) game;
    (* per-edge styling: highlight the chosen strategy moves *)
    (match solution with
     | None -> ()
     | Some s ->
        let (s0, s1) = s.PG.strategy in
        let mark color plays =
            PG.StrSet.iter (fun (from, dst) ->
                let a = Ser.StyleTbl.create 2 in
                Ser.StyleTbl.add a "color" color;
                Ser.StyleTbl.add a "penwidth" "2.0";
                Ser.AttrbTbl.replace eattrs (node_id from ^ "-" ^ node_id dst) a
            ) plays
        in
        mark "blue" s0;   (* Even's strategy *)
        mark "red"  s1);  (* Odd's strategy  *)
    Ser.to_dot_string ~dir:true name gattrs nattrs eattrs game
