(**
  Utilities and Helpers for some repetitive tasks
*)

module Utils = struct

  (**
    Generate an n sized list with random non-zero numbers
  *)
  let makerandlist size =
    let rec recmaker lst count =
      if count >= size then
        lst
      else
        recmaker (Random.bits() :: lst) (count + 1)
    in
      recmaker [] 0

  let rec makerandlistrec size =
    if size = 0 then
      []
    else
      Random.bits() :: (makerandlistrec (size - 1))

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

let print_ints x =
  Printf.sprintf  (format_of_string " %d ") x

end
