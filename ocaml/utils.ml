(******************************************************************************
*                                                                             *
*            Utilities and Helpers for some repetitive tasks                  *
*                                                                             *
*******************************************************************************)
module Utils = struct


    (**
    Poor mans benchmarking
  *)
    let time f =
        let
        t = Unix.gettimeofday ()
        and
        res = f () in
        Printf.printf "Execution time: %f secondsn"
            (Unix.gettimeofday () -. t);
        res
    ;;

    (**
    Poor mans benchmarking
  *)
    let timeonly f =
        let t = Unix.gettimeofday () in
        let _res = f () in
        Printf.printf "Execution time: %f secondsn" (Unix.gettimeofday () -. t)

    let take n somelist =
        let rec takerec state count alist =
            if count >= n then
                state
            else
                match alist with
                | [] -> state
                | hd :: rest -> takerec (hd :: state) (count + 1) rest in
        takerec []  0 somelist

    let take_arr n somearray =
        if n > Array.length somearray then
            somearray
        else
            Array.sub somearray 0 n
    ;;

    let randnode max_prio idx =  
        if ((Random.int (idx+1)) mod 2) = 0 then
            (Paritygame.ParityGame.Even, Random.int max_prio)
        else
            (Paritygame.ParityGame.Odd, Random.int max_prio)
    ;;

    let add_node g (pl, pr) = Paritygame.ParityGame.add pl pr g

    let rec pick bindings total output count maxno = 
        if total <= 2 then
            output
        else if count >= maxno then
            output
        else
            let bound = (Random.int total) in
            match List.nth_opt bindings bound with
            | Some v -> v :: (pick bindings bound output (count+1) maxno)
            | None ->   output
    ;;

    let makerandomgame num_nodes max_prio maxno = 
        let game =
            Seq.fold_left (add_node) Paritygame.ParityGame.empty
            @@ Seq.init num_nodes (randnode max_prio) 
        in 
            let bindings = Paritygame.ParityGame.bindings game in
            let adj_list_desc =
                List.map (fun x -> (x, pick bindings num_nodes [] 0 maxno)) bindings
            in
                Paritygame.ParityGame.Graph.of_list adj_list_desc game
    ;;

    (* use seeded random *)
    let smakerandomgame seed num_nodes max_prio maxno =
        let _ = Random.init seed in
        let game =
            Seq.fold_left (add_node) Paritygame.ParityGame.empty
            @@ Seq.init num_nodes (randnode max_prio) 
        in 
            let bindings = Paritygame.ParityGame.bindings game in
            let adj_list_desc =
                List.map (fun x -> (x, pick bindings num_nodes [] 0 maxno)) bindings
            in
                Paritygame.ParityGame.Graph.of_list adj_list_desc game
    ;;

end
