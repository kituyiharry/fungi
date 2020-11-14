(************************
 * Author: Harry K
 * Date  : Nov 14, 2020
 * Based on upenn lectures: (Interface only)
   https://www.seas.upenn.edu/~cis120/archive/16sp/lectures/lec10.pdf

  Usage: 
    let f = BSTSet.empty ;;  (* Empty Set *)
    let f = BSTSet.set_of_list [1;2;3;4;5;6;7;...;];;  (* Set from a List *)

************************)

module type SET = sig
  type 'a set
  val empty  : 'a set
  val add    : 'a -> 'a set -> 'a set
  val member : 'a -> 'a set -> bool
  val equals : 'a set -> 'a set -> bool
  val cardinality : 'a set -> int
  val set_of_list : 'a list -> 'a set
end

module BSTSet : SET = struct

  type 'a tree =
    | Empty
    | Node of 'a tree * 'a * 'a tree

  type 'a set = 'a tree

  let empty : 'a set = Empty

  let rec add aval = function 
    | Empty -> Node(Empty, aval, Empty)
    | Node(left, v, right) ->
        if aval < v then
          Node (add aval left, v, right)
        else if aval > v then
          Node (left, v, add aval right)
        else
          Node(left, v, right);; (* What do i do here, they are equal *)
  
  let rec member aval = function
    | Empty -> false                (* Maybe they searched for Empty ?*)
    | Node(left, v, right) ->       (* Recursively find the node *)
        if aval = v then
          true
        else if aval > v then
          member aval right
        else 
          member aval left;;

  (*
   * Recursive equals
   * FIXME: Not Balanced so in some cases this will be off (Root Node shifted)
   * FIXME: e.g BSTSet.set_of_list [1;2;3;4;5;6;] != BSTSet.set_of_list [6;5;4;3;2;1;]
   *)
  let rec equals oset = function
    | Empty -> oset = Empty         (* Equal if other set is Empty *)
    | Node(x, v, y) -> 
        match oset with
        | Node(a, b, c) ->          (* Match on the Root Node *)
            if v = b then           (* If Root nodes are equal *)
              if equals c y then    (* Recursively check right tree *)
                equals x a          (* Recursively check left node  *)
              else
                false               (* Fail otherwise with a False  *)
            else
              false
        | _ -> false;;              (* False when Root nodes not equal *)

  let rec set_of_list = function
    | [] -> Empty
    (*| hd :: tail -> add hd set_of_list tail;; --- Why doesn't this work ??*)
    | hd :: tail -> add hd (set_of_list tail);;

  let rec cardinality = function
  | Empty -> 0
  | Node(x,_,y) -> 1 + cardinality x + cardinality y;; (* Sum Left and Right subtrees *)
end
