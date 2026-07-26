(******************************************************************************
*                                                                             *
*                      An ordered Set implementation                          *
*                                                                             *
*       This is a thin adapter over the Stdlib [Set]. The historical          *
*       hand-rolled (deliberately unbalanced) BST has been replaced by        *
*       [Stdlib.Set.Make], which gives balanced-tree performance for free.    *
*                                                                             *
*       The [TSet] interface is kept unchanged so every existing call site    *
*       (AdjSet, EdgeSet, SccSet, PathSet, ...) continues to work. A few of    *
*       the original tree-specific operations (inorder / preorder / postorder *
*       / traverse / invert / root / search / subset_seq) are provided as     *
*       compatibility shims expressed over the ordered set.                   *
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
    val search:         (t -> int)  -> t set ->  t
end

module TreeSet(Ord: Set.OrderedType): TSet with type t := Ord.t = struct

    module S = Set.Make(Ord)

    (* the historical interface carries a phantom type parameter; the set is
       always instantiated at the element type so [_ set] is just [S.t] *)
    type 'a set = S.t

    (* The historical TreeSet REPLACED an existing element on a compare-equal
       insert (its [add] stored the freshly supplied value). Stdlib's [Set.add]
       instead keeps the element already present and drops the new one. Some
       callers rely on replacement - e.g. Tarjan re-adds an sccnode with the same
       [node] key but an updated low-link, and Dijkstra re-adds a path entry with
       the same endpoints but a smaller cost - so force replacement to preserve
       the original semantics. *)
    let add x s      = S.add x (S.remove x s)

    (* --- straight pass-throughs to the Stdlib set --------------------------- *)
    let empty        = S.empty
    let mem          = S.mem
    let cardinal     = S.cardinal
    let is_empty     = S.is_empty
    let singleton    = S.singleton
    let remove       = S.remove
    (* built via the replacing [add] so, like the original, the last of any
       compare-equal elements wins *)
    let of_list l    = List.fold_left (fun s x -> add x s) S.empty l
    let of_seq sq    = Seq.fold_left  (fun s x -> add x s) S.empty sq
    let to_seq       = S.to_seq
    let elements     = S.elements
    let iter         = S.iter
    let fold         = S.fold
    let filter       = S.filter
    let for_all      = S.for_all
    let exists       = S.exists
    let min_elt_opt  = S.min_elt_opt
    let max_elt_opt  = S.max_elt_opt
    let union        = S.union
    let inter        = S.inter
    (* [diff a b], [subset a b] keep the same argument order/meaning as before
       ([diff other self] = other \ self, [subset other self] = other ⊆ self) *)
    let diff         = S.diff
    let subset       = S.subset

    (* choose historically returned the minimum element (raising Not_found on
       an empty set), so keep that rather than an arbitrary element *)
    let choose       = S.min_elt

    (* to_list historically returned elements in DESCENDING order (it was
       [inorder []], which reverses); callers rely on [List.rev (to_list s)]
       yielding ascending order, so preserve that shape. *)
    let to_list s    = List.rev (S.elements s)

    (* any element will do as a "root"; use the minimum *)
    let root         = S.min_elt_opt

    let take_min s     = let m = S.min_elt s in (m, S.remove m s)
    let take_min_opt s = match S.min_elt_opt s with
        | Some m -> (Some m, S.remove m s)
        | None   -> (None, s)
    let take_max_opt s = match S.max_elt_opt s with
        | Some m -> (Some m, S.remove m s)
        | None   -> (None, s)

    (* a set is unordered, so mirroring is a no-op; retained only because the
       interface exposes it (round-trip identity is all that was relied upon) *)
    let invert s       = s

    (* in-order accumulation prepends each element onto [acc] (descending) *)
    let inorder acc s  = S.fold (fun x a -> x :: a) s acc
    (* We have altered the tree structure and so these traversals don't make
       sense and are only here for historical purposes *)
    let preorder       = inorder
    let postorder      = inorder
    let iter_preorder  = S.iter
    let iter_postorder = S.iter

    let traverse f acc s = S.fold (f) s acc

    (* Find the least element satisfying an arbitrary predicate. NB: unlike
       Stdlib's [Set.find_first] (which requires a monotone predicate) this
       scans in order, matching the original linear-search semantics. *)
    let find_first_opt p s = Seq.find p (S.to_seq s)
    let find_first p s = match Seq.find p (S.to_seq s) with
        | Some x -> x
        | None   -> raise Not_found

    (* binary-search style lookup: [c v = 0] marks the target. Implemented as an
       ordered scan since the Stdlib set hides its tree structure. *)
    let search c s = match Seq.find (fun v -> c v = 0) (S.to_seq s) with
        | Some x -> x
        | None   -> raise Not_found

    (* the powerset as a lazy sequence of subsets *)
    let rec subset_seq s = match S.min_elt_opt s with
        | None   -> Seq.return S.empty
        | Some x ->
            let subs = subset_seq (S.remove x s) in
            Seq.append subs (Seq.map (S.add x) subs)

end
