(***************************************************************
 *    Sorting Algorithm implemetations (Bubble Sort)           *
***************************************************************)
(************************
 * Author: Harry K
 *  Date : Aug 24, 2020 *
************************)
open Printf;;

(*********************************
*  Defined for questions below  *
*********************************)

let swap array x y =
  let l_temp = array.(x) in
  array.(x) <- array.(y);
  array.(y) <- l_temp;
  array
;;

(*********************************************
*  Bubble Sort implemented using Iteration  *
*********************************************)

let bubble_sort_iter array =
  (*Use refs for some mutability*)
  let mpass = ref 0 in
  let sorted = ref false in
  (* remember '!' is like 'deref-mut'*)
  while (not !sorted) do
    sorted := true;

    for i = 0 to ((Array.length(array) - !mpass) - 2) do
      (*Why doesn't this work*)
      (*(if array.(i) > array.(i+1) then swap array i i+1);*)
      if array.(i) > array.(i+1) then
        (*swap array i i+1*)
        (*
         * Why doesn't this work!!
         * Is it something to do with side effects
         *
         * *)
        let l_temp = array.(i) in
        array.(i) <- array.(i+1);
        array.(i+1) <- l_temp;
        sorted := false;
    done;
    mpass := !mpass+1;
  done
;;

(*****************************
 * Must recursive call be the last call??
 * Recursive bubble sort
 * https://www.geeksforgeeks.org/recursive-bubble-sort/
 * https://www.techiedelight.com/bubble-sort-iterative-recursive/
*****************************)

let rec bubble_sort_recur array n =
  match n with
  | 1 -> n
  | _ -> (
      for i = 0 to (n - 2) do
        (if array.(i) > array.(i+1) then
           let l_temp = array.(i) in
           array.(i) <- array.(i+1);
           array.(i+1) <- l_temp;
        )
      done;
      bubble_sort_recur array (n-1)
    )
;;


(**********************
 *  Simple factorial  *
 **********************)

let rec factorial n =
  match n with
  | 0 -> 1;
  | n -> n * factorial(n-1);
;;

(*************************
 *  Test all functions  *
 *************************)


let a = [|2; 4; -1; 90; -99; -08; 352; -2982; 221;|];;
bubble_sort_iter a;;

let b = [|23;1231;1;124;1214;1241;1;11435;5464;234;466;542;25;1212;|];;
let len = Array.length(b);;

bubble_sort_recur b len;;

(* Interesting!
 *bubble_sort_recur b (Array.length(b));;
 *bubble_sort_recur b Array.length(b);;
 *)

Array.iter (Printf.printf " %d ") a;;
Printf.printf("\n\n");;
Array.iter (Printf.printf " %d ") b;;
Printf.printf("\n\n");;

Printf.printf "factorial of 20 is %d \n" (factorial 20);;
