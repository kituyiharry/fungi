(* Unbalanced Binary Tree *)
(***********************************************************
 *  By  : owainlewis
 *  From: https://gist.github.com/owainlewis/3829544  *
***********************************************************)

(* Ordered binary search tree *)
module MyTree = struct

  type 'a tree =
    Node of 'a * 'a tree * 'a tree
    | Leaf;;

  (* Make ordered binary tree *)
  let rec insert_ordered x = function
    Leaf -> Node (x, Leaf, Leaf)
    | Node (y, left, right) as node ->
        if x < y then
          Node (y, insert_ordered x left, right)
        else if x > y then
          Node (y, left, insert_ordered x right)
        else
          node
  ;;

  (* Make a binary search tree aka ordered or sorted binary tree *)
  let rec make_ordered_binary_tree = function
    [] -> Leaf
    | x :: xs -> insert_ordered x (make_ordered_binary_tree xs);;

  (* Use recursion to find if x is an element of a binary tree *)
  let rec mem x = function
    Leaf -> false
    | Node (y, left, right) ->
        x = y || (x < y && mem x left) || (x > y && mem y right);;

  let rec inorder stack = function
    | Leaf -> stack
    | Node (a, Leaf, Leaf) ->  a :: stack
    | Node (_, x, y) ->
        inorder (inorder stack x) y

end
