module T = Set.Make(Int);;

let x = T.cardinal


(** Rendering function *)
let render_right = function
    | (depth, value) ->
        Format.print_string (String.make depth '\t');
        Format.printf "---( %s )\n" (value);
        Format.print_string (String.make depth '\t');
        Format.printf "|\n";
;;

(** Rendering function *)
let render_left = function
    | (depth, value) ->
        Format.print_string (String.make depth '\t');
        Format.printf("|\n");
        Format.print_string (String.make depth '\t');
        Format.printf "---( %s )\n" (value)
;;

(** Rendering function *)
let render_root = function
    | (depth, value) ->
        Format.print_string (String.make depth '\t');
        Format.printf "[( %s )]" (value)
;;

(** Rendering function *)
(*
 *let rec gorender wasRight depth rootval = function
 *    | Empty -> ()
 *    | Node(l, v, r) ->
 *        gorender true (depth+1) rootval r;
 *        (match v = rootval with
 *            | true ->
 *                render_root (depth, v)
 *            | false ->
 *                (if wasRight then
 *                    render_right (depth, v)
 *                    else
 *                        render_left (depth, v)
 *                )
 *        );
 *        gorender false (depth+1) rootval l
 *;;
 *
 *)
(** Rendering function *)
(*
 *let render = function
 *    | Empty -> ()
 *    | n ->
 *        match root n with
 *        | Some(p) -> gorender false 0 p n
 *        | None -> ()
 *;;
 *)

