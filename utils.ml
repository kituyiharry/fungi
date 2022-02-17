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

  let rec makerandlisrec size =
    if size = 0 then
      []
    else
      Random.bits() :: (makerandlisrec (size - 1))

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
;;

end
