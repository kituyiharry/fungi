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
  val tailcardinality : 'a set -> int
  val set_of_list : 'a list -> 'a set
  val root : 'a set -> 'a option
  val take_min: 'a set -> 'a option * 'a set
  val render_right: (int * int) -> unit
  val render_left:  (int * int) -> unit
  val render_root:  (int * int) -> unit
  val gorender : bool -> int -> int -> int set -> unit
  val render : int set -> unit
  val invert : 'a set-> 'a set
  val inorder: 'a list -> 'a set -> 'a list
  val preorder: 'a list -> 'a set -> 'a list
  val postorder: 'a list -> 'a set -> 'a list
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
          member aval left
  ;;

  (*
   * Thanks to Oliver Friedmann
   *)
  let rec take_min = function
    | Empty -> (None, Empty)
    | Node(Empty, v, r) -> (Some v, r)
    | Node(l, v, r) -> let (el, rest) = take_min l in
                       (el, Node(rest, v, r))
  ;;

  (*
   * Equals takes the minimum value of a BSTSet on each iteration and compares
   * the remaining subtrees
   *)
  let rec equals oset = function
    | Empty -> oset = Empty         (* Equal if other set is Empty *)
    | n -> match (take_min oset, take_min n) with
      | ((Some(v), o), (Some(x),y)) ->
          if v = x then
            equals o y
          else
            false
      | ((None, o), (None, v)) ->
          equals o v
      | _ -> false
  ;;

  let root = function
    | Empty ->  None
    | Node(_, v, _) -> Some(v)
  ;;

  let render_right = function
    | (depth, value) ->
        Format.print_string (String.make depth '\t');
        Format.printf "---( %d )\n" value;
        Format.print_string (String.make depth '\t');
        Format.printf "|\n";
  ;;

  let render_left = function
    | (depth, value) ->
        Format.print_string (String.make depth '\t');
        Format.printf("|\n");
        Format.print_string (String.make depth '\t');
        Format.printf "---( %d )\n" value
  ;;

  let render_root = function
    | (depth, value) ->
        Format.print_string (String.make depth '\t');
        Format.printf "[( %d )]" value
  ;;

  let rec gorender wasRight depth rootval = function
    | Empty -> ()
    | Node(l, v, r) ->
        gorender true (depth+1) rootval r;
        (match v = rootval with
          | true ->
              render_root (depth, v)
          | false ->
              (if wasRight then
                render_right (depth, v)
              else
                render_left (depth, v)
              )
        );
        gorender false (depth+1) rootval l
  ;;

  let render = function
    | Empty -> ()
    | n ->
        match root n with
          | Some(p) -> gorender false 0 p n
          | None -> ()
  ;;

  let rec set_of_list = function
    | [] -> Empty
    | hd :: tail -> add hd (set_of_list tail)
  ;;

  let rec cardinality = function
    | Empty -> 0
    | Node(x,_,y) -> 1 + cardinality x + cardinality y
  ;; (* Sum Left and Right subtrees *)

  (*((+) (cardinality x)) @@ ((+) (cardinality y)) @@ 1*)
  let tailcardinality = function
    | sometree ->
        let rec cardrec tree sum = match take_min tree with
          | (Some _value, more) -> cardrec more (sum + 1)
          | (None, _) -> sum
        in
          cardrec sometree 0
  ;; (* Tail recursive cardinality :-) but still slow :-( *)

  let rec invert = function
    | Node(x, a, y) -> Node(invert y, a, invert x)
    | e -> e
  ;;

  let rec inorder stack = function
    | Empty -> stack
    | Node (Empty, a, Empty) ->  stack @ [a]
    | Node (x, a, y) ->
        inorder ((inorder stack x) @ [a]) y
  ;; (* Inorder traversal - Left - Root - Right *)


  let rec preorder stack = function
    | Empty -> stack
    | Node (Empty, a, Empty) ->  stack @ [a]
    | Node (x, a, y) ->
       preorder (preorder (stack @ [a]) x) y
  ;; (* Preorder traversal - Root - left - Right*)

  let rec postorder stack = function
    | Empty -> stack
    | Node (Empty, a, Empty) ->  stack @ [a]
    | Node (x, a, y) ->
        (postorder (postorder stack x) y) @ [a]
  ;; (* Postorder traversal Left - Right - Root *)

end
