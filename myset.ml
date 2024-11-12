(************************
 * Author: Harry K
 * Date  : Nov 14, 2020
 * Based on upenn lectures: (most of the Interface only)
   https://www.seas.upenn.edu/~cis120/archive/16sp/lectures/lec10.pdf

   The tree will remain mostly unbalanced!

  Usage:
    let f = TreeSet.empty ;;                            (* Empty Set *)
    let f = TreeSet.set_of_list [1;2;3;4;5;6;7;...;];;  (* Set from a List *)

************************)

module type TreeSet = functor(Ord: Set.OrderedType) -> sig
    type t
    type elt
    type 'a set
    val empty  : elt set
    val add    : elt -> elt set -> elt set
    val member : elt -> elt set -> bool
    val cardinality : elt set -> int
    val tailcardinality : elt set -> int
    val of_list : elt list -> elt set
    val root : elt set -> elt option
    val take_min: elt set -> elt option * elt set
    val invert : elt set-> elt set
    val inorder: elt list -> elt set -> elt list
    val iter_inorder: (elt -> unit) -> elt set -> unit
    val preorder: elt list -> elt set -> elt list
    val iter_preorder: (elt -> unit) -> elt set -> unit
    val postorder: elt list -> elt set -> elt list
    val iter_postorder: (elt -> unit) -> elt set ->  unit
    val fold: (elt -> 'b -> 'b) -> elt set -> 'b -> 'b
    val remove: elt -> elt set -> elt set
    val union: elt set -> elt set -> elt set
    val is_empty: elt set -> bool
    val elements: elt set -> elt list
    val filter: (elt -> bool) -> elt set -> elt set
    val for_all: (elt -> bool) -> elt set -> bool
    val subset: elt set -> elt set -> bool
    val diff: elt set -> elt set -> elt set
end

module TreeSet(Ord: Set.OrderedType) =
struct

    type elt = Ord.t

    type 'a set =
        | Empty
        | Node of 'a set * 'a * 'a set
    ;;

    type t = elt set

    let empty = Empty

    (** [add 'a 'a set] Adds a node 'a to a given set *)
    let rec add aval = function
        | Empty -> Node(Empty, aval, Empty)
        | Node(left, v, right) ->
            let p = Ord.compare aval v in
            if p < 0 then
                Node (add aval left, v, right)
            else if p > 0 then
                Node (left, v, add aval right)
            else
                Node(left, v, right)
    ;;

    (** [member 'a 'a set] Checks whether 'a is a member of the set *)
    let rec mem aval = function
        | Empty -> false
        | Node(left, v, right) ->
            let p = Ord.compare aval v in
            p = 0 || (if p > 0 then mem aval right else mem aval left)
    ;;

    (** [take_min 'a set]
      Returns a pair of some minimum element in the set and the remaining set
   *)
    let rec take_min = function
        | Empty -> (None, Empty)
        | Node(Empty, v, r) -> (Some v, r)
        | Node(l, v, r) -> let (el, rest) = take_min l in
            (el, Node(rest, v, r))
    ;;

    (** [take_max 'a set]
      Returns a pair of some maximum element in the set and the remaining set
   *)
    let rec take_max = function
        | Empty -> (None, Empty)
        | Node(l, v, Empty) -> (Some v, l)
        | Node(l, v, r) -> let (el, rest) = take_max r in
            (el, Node(l, v, rest))
    ;;

    (** [max_elt_opt 'a set]
      Returns some maximum element in the set and the remaining set
   *)
    let rec max_elt_opt = function
        | Empty -> None
        | Node(_l, v, Empty) -> Some v
        | Node(_l, _v, r) -> max_elt_opt r
    ;;

    (** [root 'a set] Root element of the Set *)
    let root = function
        | Empty ->  None
        | Node(_, v, _) -> Some(v)
    ;;

    (** [set_of_list 'a list] Build a Set from a list *)
    let rec of_list = function
        | [] -> Empty
        | hd :: tail -> add hd (of_list tail)
    ;;

    (** [cardinality 'a set] number of elements in the set (recursive) *)
    let rec cardinal = function
        | Empty -> 0
        | Node(x,_,y) -> cardinal x + 1 + cardinal y
    ;; (* Sum Left and Right subtrees *)

    (** [invert 'a set] Invert the BST holding the set *)
    let rec invert = function
        | Node(x, a, y) -> Node(invert y, a, invert x)
        | e -> e
    ;;

    (** [inorder 'a set] Inorder walk on the set *)
    let rec inorder stack = function
        | Empty -> stack
        | Node (Empty, a, Empty) ->  stack @ [a]
        | Node (x, a, y) ->
            inorder ((inorder stack x) @ [a]) y
    ;; (* Inorder traversal - Left - Root - Right *)

    let rec iter_inorder g = function
        | Empty -> ()
        | Node (Empty, a, Empty) ->  (g a)
        | Node (x, a, y) ->
            let _ = iter_inorder g x in
            let _ = g a in
            iter_inorder g y
    ;; (* Inorder traversal - Left - Root - Right *)

    (** [preorder 'a set] Preorder walk on the set *)
    let rec preorder stack = function
        | Empty -> stack
        | Node (Empty, a, Empty) ->  stack @ [a]
        | Node (x, a, y) ->
            preorder (preorder (stack @ [a]) x) y
    ;; (* Preorder traversal - Root - left - Right*)

    (** [preorder 'a set] Preorder walk on the set *)
    let rec iter_preorder g = function
        | Empty -> ()
        | Node (Empty, a, Empty) ->  (g a)
        | Node (x, a, y) ->
            let _ = g a in
            let _ = iter_preorder g x in
            iter_preorder g y
    ;; (* Preorder traversal - Root - left - Right*)

    (** [postorder 'a set] Postorder walk on the set *)
    let rec postorder stack = function
        | Empty -> stack
        | Node (Empty, a, Empty) ->  stack @ [a]
        | Node (x, a, y) ->
            (postorder (postorder stack x) y) @ [a]
    ;; (* Postorder traversal Left - Right - Root *)

    (** [postorder 'a set] Postorder walk on the set *)
    let rec iter_postorder g = function
        | Empty -> ()
        | Node (Empty, a, Empty) ->  (g a)
        | Node (x, a, y) ->
            let _ = iter_postorder g x in
            let _ =  iter_postorder g y in
            g a
    ;; (* Postorder traversal Left - Right - Root *)

    (*if the set is empty as in only contains `Empty*)
    let is_empty = function
        | Empty -> true
        | _ -> false
    ;;

    (** ... f set'' (fold f set' (fold f set acc)) ... *)
    let rec fold f set acc = match set with
        | Empty -> acc
        | Node (Empty, a, Empty) -> f a acc
        | Node (x, a, y) -> f a (fold f y (fold f x acc))
    ;;

    (** ... Remove an element from the set  ... *)
    let rec remove el = function
        | Empty -> Empty
        | Node (x, a, y) -> 
            let p = Ord.compare el a in
            if p = 0 then
                match (x, y) with
                | (Empty, Empty) -> Empty
                |  _ ->
                    let min, rest = take_min y in 
                    match min with
                    | Some v -> Node(x, v, rest)
                    | None -> x
            else if p > 0 then
                Node(x, a, remove el y)
            else
                Node(remove el x, a, y)
    ;;

    (** ... set union of 2 sets  ... *)
    let rec union other = function
        | Empty -> other
        | self ->
            match (take_max self, take_max other) with
            | ((Some a, rest), (Some b, more)) -> 
                (add a (add b (union rest more)))
            | ((None, _rest), (Some _b, _more)) -> 
                other
            | ((Some _a, _rest), (None, _more)) -> 
                self
            | _ -> Empty
    ;;

    (** ... list of elements in a set ... *)
    let elements = function
        | Empty -> []
        | nodes -> fold (fun elt acc -> (elt :: acc)) nodes [] 
    ;;

    (** ... test whether f is true for_all members of this set ... *)
    let rec for_all f = function
        | Empty -> true
        | nodes -> 
            let (max, rest) = take_max nodes in 
            match max with
            | Some v -> 
                if f v then
                    for_all f rest
                else
                    false
            | _ -> true
    ;;

    (* subset other self -> other is subset of self *)
    let subset other = function
        | Empty -> is_empty other
        | nodes ->
            for_all (fun x -> mem x nodes) other
    ;;

    (* Filter the elements of a set *)
    let filter f = function
        | Empty -> Empty
        | nodes ->
            fold (fun elt acc -> 
                if f elt then add elt acc else acc
            ) nodes empty
    ;;

    (* set difference *)
    let diff other = function
        | Empty -> other
        | nodes ->
            filter (fun x -> not (mem x nodes)) other
    ;;

end
