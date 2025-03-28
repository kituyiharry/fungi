(******************************************************************************
*                                                                             *
*                      A Tree Set implementation                              *
*                                                                             *
*       The tree will remain mostly unbalanced!                               *
*       Target is to add some 'laziness' with some functions                  *
*                                                                             *
*       Harry K W                                                             *
*                                                                             *
*******************************************************************************)

module type TSet = sig
    type t
    type 'a set
    val empty:          t set
    val add:            t -> t set -> t set
    val mem:            t -> t set -> bool
    val cardinal:       t set -> int
    val of_list:        t list -> t set
    val to_list:        t set  -> t list
    val root:           t set -> t option
    val choose:         t set -> t
    val take_min_opt:   t set -> t option * t set
    val take_min:       t set -> t * t set
    val take_max_opt:   t set -> t option * t set
    val invert:         t set-> t set
    val inorder:        t list -> t set -> t list
    val iter:           (t -> unit) -> t set -> unit
    val traverse:       (t -> 'b -> 'b) -> 'b -> t set -> 'b
    val preorder:       t list -> t set -> t list
    val iter_preorder:  (t -> unit) -> t set -> unit
    val postorder:      t list -> t set -> t list
    val iter_postorder: (t -> unit) -> t set ->  unit
    val fold:           (t -> 'b -> 'b) -> t set -> 'b -> 'b
    val remove:         t -> t set -> t set
    val union:          t set -> t set -> t set
    val is_empty:       t set -> bool
    val elements:       t set -> t list
    val filter:         (t -> bool) -> t set -> t set
    val for_all:        (t -> bool) -> t set -> bool
    val subset:         t set -> t set -> bool
    val subset_seq:     t set -> t set Seq.t
    val diff:           t set -> t set -> t set
    val to_seq:         t set -> t Seq.t
    val singleton:      t -> t set
    val min_elt_opt:    t set -> t option
    val max_elt_opt:    t set -> t option
    val of_seq:         t Seq.t -> t set
    val inter:          t set -> t set -> t set
    val exists:         (t -> bool) -> t set -> bool
    val find_first_opt: (t -> bool) -> t set -> t option
    val find_first:     (t -> bool) -> t set -> t
end

let (let*) = Option.bind

module TreeSet(Ord: Set.OrderedType): TSet with type t := Ord.t = struct

    type 'a set =
        | Empty
        | Node of 'a set * 'a * 'a set
    ;;

    let empty = Empty

    exception Not_found

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
                Node(left, aval, right)
    ;;

    (** [member 'a 'a set] Checks whether 'a is a member of the set *)
    let rec mem aval = function
        | Empty -> false
        | Node(left, v, right) ->
            let p = Ord.compare aval v in
            p = 0 || (if p > 0 then (mem[@tailcall]) aval right else (mem[@tailcall]) aval left)
    ;;

    (** [take_min 'a set]
      Returns a pair of some minimum element in the set and the remaining set
   *)
    let rec take_min = function
        | Empty -> raise Not_found
        | Node(Empty, v, r) -> (v, r)
        | Node(l, v, r) -> let (el, rest) = take_min l in
            (el, Node(rest, v, r))
    ;;

    (** [take_min 'a set]
      Returns a pair of some minimum element in the set and the remaining set
   *)
    let rec take_min_opt = function
        | Empty -> (None, Empty)
        | Node(Empty, v, r) -> (Some v, r)
        | Node(l, v, r) -> let (el, rest) = take_min_opt l in
            (el, Node(rest, v, r))
    ;;

    (** [choose 'a set]
      Returns a pair of some minimum element in the set
   *)
    let rec choose = function
        | Empty -> raise Not_found
        | Node(Empty, v, _r) -> v
        | Node(l, _v, _r) -> choose l
    ;;

    (** [choose 'a set]
      Returns a pair of some minimum element in the set and the remaining set
   *)
    let rec choose_rest = function
        | Empty -> raise Not_found
        | Node(Empty, v, r) -> (v, r)
        | Node(l, v, r) -> let (el, rest) =
            choose_rest l in (el, Node(rest, v, r))
    ;;

    (** [take_max 'a set]
      Returns a pair of some maximum element in the set and the remaining set
   *)
    let rec take_max_opt = function
        | Empty -> (None, Empty)
        | Node(l, v, Empty) -> (Some v, l)
        | Node(l, v, r) -> let (el, rest) = take_max_opt r in
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

    (** [min_elt_opt 'a set]
      Returns some maximum element in the set and the remaining set
   *)
    let rec min_elt_opt = function
        | Empty -> None
        | Node(Empty, v, _r) -> Some v
        | Node(l, _v, _r) -> min_elt_opt l
    ;;

    (** [root 'a set] Root element of the Set *)
    let root = function
        | Empty ->  None
        | Node(_, v, _) -> Some(v)
    ;;

    (** [root 'a set] Root element of the Set *)
    let take_root = function
        | Empty ->  (None, Empty)
        | Node (x, v, y) -> 
            let g = let min, rest = take_min_opt y in 
                match min with
                | Some next -> Node(x, next, rest)
                | None -> x
            in
                Some(v), g
    ;;

    (** [set_of_list 'a list] Build a Set from a list *)
    let of_list = function
        | []  -> Empty
        | rst -> List.fold_left (Fun.flip add) Empty rst
    ;;
    let of_list = of_list

    (** [set_of_seq 'a Seq] Build a Set from a lazy sequence *)
    let of_seq = Seq.fold_left (Fun.flip add) Empty;;

    (** [cardinality 'a set] number of elements in the set (recursive) *)
    let rec cardinal = function
        | Empty -> 0
        | Node(x,_,y) -> cardinal x + 1 + cardinal y
    ;; (* Sum Left and Right subtrees *)
    let cardinal = cardinal

    (** [invert 'a set] Invert the BST holding the set *)
    let rec invert = function
        | Node(x, a, y) -> Node(invert y, a, invert x)
        | e -> e
    ;;
    let invert = invert

    (** [inorder 'a set] Inorder walk on the set *)
    let rec inorder stack = function
        | Empty -> stack
        | Node (Empty, a, Empty) ->  a :: stack
        | Node (x, a, y) -> inorder (a :: (inorder stack x)) y
    ;; (* Inorder traversal - Left - Root - Right *)

    (** [to_list 'a list] Build a list from a set *)
    let to_list set = inorder [] set
    ;;

    let rec iter g = function
        | Empty -> ()
        | Node (Empty, a, Empty) ->  (g a)
        | Node (x, a, y) ->
            let _ = iter g x in
            let _ = g a in
            iter g y
    ;; (* Inorder traversal - Left - Root - Right *)

    (** [preorder 'a set] Preorder walk on the set *)
    let rec preorder stack = function
        | Empty -> stack
        | Node (Empty, a, Empty) ->  a :: stack
        | Node (x, a, y) -> preorder (preorder (a :: stack) x) y
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
        | Node (Empty, a, Empty) ->  a :: stack
        | Node (x, a, y) -> a :: (postorder (postorder stack x) y)
    ;; (* Postorder traversal Left - Right - Root *)

    (** [postorder 'a set] Postorder walk on the set *)
    let rec iter_postorder g = function
        | Empty -> ()
        | Node (Empty, a, Empty) ->  (g a)
        | Node (x, a, y) ->
            let _ = iter_postorder g x in
            let _ = iter_postorder g y in
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
                let min, rest = take_min_opt y in
                match min with
                | Some v -> Node(x, v, rest)
                | None -> x
            else if p > 0 then
                Node(x, a, remove el y)
            else
                Node(remove el x, a, y)
    ;;

    (** [travers 'a set] Inorder traversal on the set *)
    let rec traverse f acc = function
        | Empty -> acc
        | Node (Empty, a, Empty) ->  (f a acc)
        | Node (x, a, y) -> traverse f (traverse f (f a acc) x) y
    ;; (* Inorder traversal - Left - Root - Right *)

    (** ... set union of 2 sets  ... *)
    let union other = function
        | Empty -> other
        | self -> traverse (add) other self
    ;;

    (** ... list of elements in a set ... *)
    let elements = function
        | Empty -> []
        | self -> fold (fun elt acc -> (elt :: acc)) self [] 
    ;;

    (** ... sequence of elements in a set ... *)
    let to_seq = function
        | self ->
            let rec aux l () = match take_root l with
                | (None, _) -> Seq.Nil
                | (Some x, tail) -> Seq.Cons (x, (aux tail))
            in
                (aux self)
    ;;

    (** ... test whether f is true for_all members of this set ... *)
    let rec for_all f = function
        | Empty -> true
        | self -> 
            let (max, rest) = take_min_opt self in 
            match max with
            | Some v -> if f v then for_all f rest else false
            | _ -> true
    ;;

    (** Subset other self -> other is subset of self *)
    let subset other = function
        | Empty -> is_empty other
        | self -> for_all (Fun.flip mem self) other
    ;;

    (** Added: generate a sequence of subsets *)
    let rec subset_seq = function 
        | Empty -> Seq.return Empty 
        | rem   -> 
            let (x, rest) = take_min rem in  
            let rest' = subset_seq rest in
            let rem  =  Seq.map (fun y ->  add x y) rest' in
            Seq.append rest' rem
    ;;

    (** Filter the elements of a set *)
    let filter f = function
        | Empty -> Empty
        | self -> fold (fun elt acc -> if f elt then add elt acc else acc) self empty
    ;;

    (** set difference - filter all elements of other not in self *)
    let diff other = function
        | Empty -> other
        | self -> filter (fun x -> not (mem x self)) other
    ;;

    (** singleton *)
    let singleton v = Node(Empty, v, Empty)
    ;;

    (** set intersection *)
    let inter other = function
        | Empty -> Empty
        | self  -> filter (fun x -> mem x self) other
    ;;

    (** elt in set by function *)
    let rec exists f = function
        | Empty -> false
        | Node (l, v, r) -> (f v) || (exists f l) || (exists f r)
    ;;

    (** find first element matching predicate f *)
    let rec find_first_opt f = function
        | Empty -> None
        | nodes -> let (sel, rest) = take_min_opt nodes in
            let* el = sel in
            if f el then Some el else find_first_opt f rest
    ;;

    (** find first element matching predicate f *)
    let rec find_first f = function
        | Empty -> raise Not_found
        | nodes -> let (el, rest) = choose_rest nodes in
            if f el then el else find_first f rest
    ;;

end
