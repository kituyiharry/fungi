(******************************************************************************
*                                                                             *
*                      A Tree Set implementation                              *
*                                                                             *
*       The tree will remain mostly unbalanced!                               *
*       Target is to add some 'laziness' with some functions                  *
*                                                                             *
*       In the standard approach for ML-style sets, it's generally            *
*       more useful to have type-specific sets where the ordering relation is *
*       FIXED for that particular type.                                       *
*                                                                             *
*       The current non-polymorphic design is intentional because:            *
*       - Each type might need different comparison logic                     *
*       - It ensures type safety at compile time                              *
*       - It follows the principle of parametric modules in ML, where you     *
*       instantiate different set implementations for different types         *
*                                                                             *
*******************************************************************************)

signature OrderedType = sig 
  type t
  val compare: t -> t -> General.order
end

signature TSet = sig
    type t
    type 'a set
    exception Not_Found
    val empty: t set
    val add: t -> t set -> t set
    val mem: t -> t set -> bool
    val take_min_opt: t set -> t option * t set
    val take_max: t set -> t option * t set
    val cardinal: t set -> int
    val of_list: t list -> t set
    val root: t set -> t option
    val invert: t set-> t set
    val inorder: t list -> t set -> t list
    val iter_inorder: (t -> unit) -> t set -> unit
    val preorder: t list -> t set -> t list
    val iter_preorder: (t -> unit) -> t set -> unit
    val postorder: t list -> t set -> t list
    val iter_postorder: (t -> unit) -> t set ->  unit
    val fold: (t -> 'b -> 'b) -> t set -> 'b -> 'b
    val remove: t -> t set -> t set
    val union: t set -> t set -> t set
    val is_empty: t set -> bool
    val elements: t set -> t list
    val filter: (t -> bool) -> t set -> t set
    val for_all: (t -> bool) -> t set -> bool
    val subset: t set -> t set -> bool
    val diff: t set -> t set -> t set
    val to_seq: t set -> t Sequence.seq
    val singleton: t -> t set
    val min_elt_opt: t set -> t option
    val max_elt_opt: t set -> t option
    (*val of_seq: t Seq.t -> t t set*)
end

functor TreeSet(structure Ord: OrderedType) :> TSet where type t = Ord.t  = struct

    type t = Ord.t

    datatype 'a set =
      Empty
      | Node of 'a set * 'a * 'a set

    val empty = Empty

    exception Not_Found

    (* [add x s] Adds element x to set s *)
    fun add (x: Ord.t) (s: t set): t set =
        case s of
            Empty => Node(Empty, x, Empty)
          | Node(left, v, right) =>
                case Ord.compare x v of
                    LESS => Node(add x left, v, right)
                  | GREATER => Node(left, v, add x right)
                  | EQUAL => Node(left, x, right)

    (** [member 'a 'a set] Checks whether 'a is a member of the set *)
    fun mem (aval: Ord.t) (s: t set) = case s of
        Empty => false
        | Node(left, v, right) =>  case Ord.compare aval v of
          LESS => mem aval left
          | GREATER => mem aval right
          | EQUAL => true

      (** [take_min_opt 'a t set]
     Returns a pair of some minimum element in the t set and the remaining t set
      *)
   fun take_min_opt (s: t set) = case s of
         Empty => (NONE, Empty)
         | Node(Empty, v, r) => (SOME v, r)
         | Node(l, v, r) => let val (el, rest) = take_min_opt l in
             (el, Node(rest, v, r))
         end

      (** [take_max 'a t set]
        Returns a pair of some maximum element in the t set and the remaining t set
        *)
    fun take_max (s: t set) = case s of
         Empty => (NONE, Empty)
        | Node(l, v, Empty) => (SOME v, l)
        | Node(l, v, r) => let val (el, rest) = take_max r in
            (el, Node(l, v, rest))
        end
 
    (** [max_elt_opt 'a t set]
      Returns some maximum element in the t set and the remaining t set
      *)
    fun max_elt_opt (s: t set) = case s of
        Empty => NONE
        | Node(_, v, Empty) => SOME v
        | Node(_, _, r)     => max_elt_opt r
 
    (** [min_elt_opt 'a t set]
       Returns some maximum element in the t set and the remaining t set
    *)
     fun min_elt_opt (s: t set) = case s of
         Empty => NONE
         | Node(Empty, v, _) => SOME v
         | Node(l, _, _) => min_elt_opt l
 
     (** [root 'a t set] Root element of the t set *)
     fun root (s: t set) = case s of
         Empty =>  NONE
         | Node(_, v, _) => SOME v
 
     (** [root 'a t set] Root element of the t set *)
     fun take_root (s: t set): (t option * t set) = case s of
         Empty =>  (NONE, Empty)
         | Node (x, v, y) => 
             let val g = let val (min, rest) = take_min_opt y in 
                 case min of
                  SOME next => Node(x, next, rest)
                  | NONE => x
                 end
             in
                 (SOME v, g)
             end

    (** [t set_of_list 'a list] Build a t set from a list *)
    fun of_list (s: t list): t set = case s of
        [] => Empty
        | hd :: tail => add hd (of_list tail)

     (** [t set_of_seq 'a Seq] Build a t set from a lazy sequence *)
     fun of_seq (s: t Sequence.seq) = 
      Sequence.fold_left (fn x => fn a => add a x) Empty s

     (** [cardinality 'a t set] number of elements in the t set (recursive) *)
     fun cardinal (s: t set): int = case s of
         Empty => 0
         | Node(x,_,y) => cardinal x + 1 + cardinal y
     (* Sum Left and Right subtrees *)

     (** [invert 'a t set] Invert the BST holding the t set *)
     fun invert (s:t set) = case s of
         Node(x, a, y) => Node(invert y, a, invert x)
         | e => e

     (** [inorder 'a t set] Inorder walk on the t set *)
     fun inorder (stack: t list) (s: t set) = case s of
         Empty => stack
         | Node (Empty, a, Empty) =>  stack @ [a]
         | Node (x, a, y) => inorder ((inorder stack x) @ [a]) y
     (* Inorder traversal - Left - Root - Right *)

     fun iter_inorder (g: t -> unit) (s: t set)= case s of
         Empty => ()
         | Node (Empty, a, Empty) =>  (g a)
         | Node (x, a, y) =>
             let val _ = iter_inorder g x in
             let val _ = g a in
             iter_inorder g y
             end end 
     (* Inorder traversal - Left - Root - Right *)

     (** [preorder 'a t set] Preorder walk on the t set *)
     fun preorder (stack: t list) (s: t set) =  case s of
         Empty => stack
         | Node (Empty, a, Empty) =>  stack @ [a]
         | Node (x, a, y) => preorder (preorder (stack @ [a]) x) y
     (* Preorder traversal - Root - left - Right*)

     (** [preorder 'a t set] Preorder walk on the t set *)
     fun iter_preorder (g: t -> unit) (s: t set) = case s of
         Empty => ()
         | Node (Empty, a, Empty) =>  (g a)
         | Node (x, a, y) => 
             let val _ = g a in
             let val _ = iter_preorder g x in
             iter_preorder g y
             end end
     (* Preorder traversal - Root - left - Right*)

     (** [postorder 'a t set] Postorder walk on the t set *)
     fun postorder (stack: t list) (s: t set) = case s of
         Empty => stack
         | Node (Empty, a, Empty) =>  stack @ [a]
         | Node (x, a, y) => (postorder (postorder stack x) y) @ [a]
     (* Postorder traversal Left - Right - Root *)

     (** [postorder 'a t set] Postorder walk on the t set *)
     fun iter_postorder (g: t -> unit) (s: t set) = case s of
         Empty => ()
         | Node (Empty, a, Empty) =>  (g a)
         | Node (x, a, y) =>
             let val _ = iter_postorder g x in
             let val _ = iter_postorder g y in
             g a
             end end
     (* Postorder traversal Left - Right - Root *)

    (*if the t set is empty as in only contains `Empty*)
    fun is_empty (s: t set) = case s of
        Empty => true
        | _ => false

    (** ... f t set'' (fold f t set' (fold f t set acc)) ... *)
    fun fold (f: t -> 'a -> 'a) (set: t set) (acc: 'a) = case set of
        Empty => acc
        | Node (Empty, a, Empty) => f a acc
        | Node (x, a, y) => f a (fold f y (fold f x acc))

    (** ... Remove an element from the t set  ... *)
    fun remove (el: t) (s: t set) = case s of
        Empty => Empty
        | Node (x, a, y) => 
            case Ord.compare el a of
            EQUAL => let val (min, rest) = take_min_opt y in
                case min of
                  SOME v => Node(x, v, rest)
                  | NONE => x
                end
            | GREATER =>
                Node(x, a, remove el y)
            | LESS =>
                Node(remove el x, a, y)

    (** [travers 'a t set] Inorder traversal on the t set *)
    fun traverse (f: t -> 'a -> 'a) (acc: 'a) (s: t set) = case s of
        Empty => acc
        | Node (Empty, a, Empty) =>  (f a acc)
        | Node (x, a, y) => traverse f (traverse f (f a acc) x) y
    (* Inorder traversal - Left - Root - Right *)

    (** ... t set union of 2 t sets  ... *)
    fun union (other: t set) (self: t set) = case self of
        Empty => other
        | selfset => traverse add other selfset

    (** ... list of elements in a t set ... *)
    fun elements (s: t set): t list = 
      fold (fn elt => fn acc => elt :: acc) s []

    (** ... sequence of elements in a t set ... 
     * *)
    fun to_seq (self: t set) = case self of
        Empty => Sequence.empty
        | nodes =>
            let fun aux l = (case take_root l of
                (NONE, _) => Sequence.empty
                | (SOME x, tail) => Sequence.cons (x, (aux tail))
              )
            in
              (aux nodes)
    end

    (** ... test whether f is true for_all members of this t set ... *)
    fun for_all (f: t -> bool) (s: t set) = case s of
        Empty => true
        | nodes => 
            let val (max, rest) = take_min_opt nodes in 
              case max of
                SOME v => if (f v) then (for_all f rest) else false
                | _ => true
            end

    (* Subt set other self -> other is subt set of self *)
    fun subset (other: t set) (s: t set) = case s of
        Empty => is_empty other
        | nodes => for_all (fn x => mem x nodes) other

    (* Filter the elements of a t set *)
    fun filter (f: t -> bool) (s: t set) =
      fold (fn elt => fn acc => if f elt then (add elt acc) else acc) s Empty

    (* t set difference *)
    fun diff (other: t set) (self: t set) =  case self of
        Empty => other
        | nodes => filter (fn x => not (mem x nodes)) other

    (* singleton *)
    fun singleton v = Node(Empty, v, Empty)

    (** find first element matching predicate f *)
    fun find_first_opt (f: t -> bool) (self: t set) = case self of
        Empty => NONE
        | nodes => let val (sel, rest) = take_min_opt nodes in case sel of 
            NONE => NONE
            | SOME el => if f el then SOME el else find_first_opt f rest
            end

    (** [take 'a set]
      Returns a pair of some minimum element in the set and the remaining set
   *)
    fun take (self: t set) = case self of
        Empty => raise Not_Found
        | Node(Empty, v, r) => (v, r)
        | Node(l, v, r) => let val (el, rest) =
            take l in (el, Node(rest, v, r))
          end

    (** find first element matching predicate f *)
    fun find_first (f: t -> bool) (self: t set) = case self of
        Empty => raise Not_Found
        | nodes => let val (el, rest) = take nodes in
            if f el then el else find_first f rest
        end
end
