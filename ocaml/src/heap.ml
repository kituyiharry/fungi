(******************************************************************************
*                                                                             *
*   Simplest Fibonacci (d-ary)  heap                                          *
*     - Every node's key is less(default cmp) than or equal to its children   * 
*       keys as given by the Entry.compare function                           *
*     - The minimum|maximum element is always in the root list depending      *
*       on your compare function                                              *
*     - Unlike binary heaps, there's no enforced structure                    *
*     - Duplicates are tolerated as they can be introduced by decrease and    *
*       increase operations                                                   *
*     - A node can have any number of children                                *
*     - Children can be added or removed freely                               *
*     - no redundancy (if 2 nodes are the same their trees are assumed to     *
*       be the same)                                                          *
*     - For simplicity we don't have a min pointer at the root list but it may*
*       be added in the future                                                *
*                                                                             *
*******************************************************************************)

module type Ordinal = sig 
    (* The full type *)
    type t
    (* How we order in the heap *)
    type order
    (* How we get the order from a node *)
    val  bind:     t  -> order
    (* How we compare different elements *)
    (* total order *)
    val  compare:  t     -> t ->     int
    (* How we compare different orders *)
    val  ocompare: order -> order -> int
end

(* immediately binds a value as its own order *)
module Immediate(Inner: Set.OrderedType): Ordinal with type t = Inner.t and type order = Inner.t = struct 
    type t         = Inner.t
    type order     = Inner.t
    let  bind    e = e
    let  ocompare  = Inner.compare
    let  compare   = Inner.compare
end

module type FibHeap = sig
    type node
    type order
    type elts        = { data: node; mutable churn: int; succ: elts list }
    type t           = elts list
    val empty:       t
    val minify:      node -> node -> bool
    val maxify:      node -> node -> bool
    val degree:      elts -> int
    val cardinal:    t -> int
    val collapse:    t -> node list
    val singleton:   node -> t
    val extract_til: ?cmp:(node -> node -> bool) -> (node -> bool) -> t -> node list
    val to_seq:      ?cmp:(node -> node -> bool) -> t -> node Seq.t
    val of_list:     ?cmp:(node -> node -> bool) -> node list -> t
    val join:        ?cmp:(node -> node -> bool) -> t -> t
    val mem:         ?cmp:(node -> node -> bool) -> node -> t -> bool
    val insert:      ?cmp:(node -> node -> bool) -> node -> t -> t
    val peek:        ?cmp:(node -> node -> bool) -> t -> elts
    val peek_opt:    ?cmp:(node -> node -> bool) -> t -> elts option
    val merge:       ?cmp:(node -> node -> bool) -> elts -> elts -> elts
    val extract:     ?cmp:(node -> node -> bool) -> t -> (node * t)
    val extract_opt: ?cmp:(node -> node -> bool) -> t -> (node * t) option
    val extract_all: ?cmp:(node -> node -> bool) -> t -> node list
    val increase:    ?cmp:(node -> node -> bool) -> node -> node -> t -> t
    val decrease:    ?cmp:(node -> node -> bool) -> node -> node -> t -> t
end

module MakeFibHeap(Entry: Ordinal): FibHeap with type node = Entry.t and type order = Entry.order = struct

    type node  = Entry.t
    ;;

    type order = Entry.order 
    ;;

    (* churn tracks the grand-children that have been removed *)
    type elts = { data: node; mutable churn: int; succ: elts list }
    ;;

    type t    = elts list
    ;;

    let empty = []
    ;;

    (* internal joining tbl which stores int (degree) -> elts *)
    let tbl = Hashtbl.create 0
    ;;

    let equal  l r = (Entry.compare l r) =  0
    ;;

    (* comparator that determines if the root is the smallest or the largest *)

    (* max-heapify, root elts are the most of their successors *)
    let maxify l r = (Entry.ocompare (Entry.bind l) (Entry.bind r)) =  1
    ;;

    (* churn threshold *)
    let churn_threshold = ref 2
    ;;

    (* min-heapify, root elts are the least of their successors *)
    let minify l r = (Entry.ocompare (Entry.bind l) (Entry.bind r)) = -1
    ;;

    let mem ?(cmp=minify) pleaf ptree = 
        let rec fmem tree rem = match tree with
        | [] -> (match rem with
                | [] -> false
                | v  -> fmem v [])
        | hd :: tail ->
            (equal hd.data pleaf) || 
               (if cmp hd.data pleaf then fmem hd.succ (tail @ rem) else fmem tail rem)
        in fmem ptree []
    ;;

    let rec cardinal = function 
        | [] -> 0
        | hd :: tail ->
            (cardinal hd.succ) + 1 + (cardinal tail)
    ;;

    let rec insert ?(cmp=minify) p = function
        | [] -> [ { data=p; churn=0; succ=[] } ]
        | (hd :: tail) as s ->
            if equal hd.data p then
                { data=p; churn=0; succ=[] } :: s
            else if cmp hd.data p then
                { hd with succ=(insert p ~cmp:cmp hd.succ) } :: tail
            else
                hd :: (insert p ~cmp:cmp tail)
    ;;

    let singleton p = 
        [ { data=p; churn=0; succ=[] } ]
    ;;

    (* insert like merge into an existing tree *)
    let rec merge_node ?(cmp=minify) p = function
        | [] -> [ p ]
        | (hd :: tail) as s ->
            if equal hd.data p.data then
                p :: s
            else if cmp hd.data p.data then
                { hd with succ=(merge_node p ~cmp:cmp hd.succ) } :: tail
            else
                hd :: (merge_node p ~cmp:cmp tail)
    ;;

    (* merge two tree elements *)
    let merge ?(cmp=minify) tree trunk = 
        (* duplicate allowed to exit in successor *)
        if equal tree.data trunk.data then
            { tree with succ = trunk :: tree.succ }
        (* bubbling elements to maintain heap properties  *)
        else if cmp tree.data trunk.data then
            (* bubble down the tree *)
            { tree with succ=(merge_node ~cmp:cmp trunk tree.succ) }
        else
            (* bubble up the trunk *)
            { trunk with succ=(merge_node ~cmp:cmp tree trunk.succ) }
    ;;

    exception Empty

    let degree t = List.length t.succ
    ;;

    (** inorder traverse the heap, elements will likely be out of order *)
    let rec collapse = function 
        | [] -> [] 
        | { succ=s; data=p;_ } :: tail ->
            collapse s @ [ p ] @ collapse tail
    ;;

    (* straddles the root elts for the min *)
    let rec peek ?(cmp=minify) = function 
        | [] -> raise Empty 
        | hd :: tail ->
            match tail with 
            | [] -> hd 
            | fllw :: rest -> 
                if cmp hd.data fllw.data then
                    peek ~cmp:cmp (hd   :: rest)
                else
                    peek ~cmp:cmp (fllw :: rest)
    ;;
 
    let rec peek_opt ?(cmp=minify) = function 
        | [] -> None
        | hd :: tail ->
            match tail with 
            | [] -> Some hd 
            | fllw :: rest -> 
                if cmp hd.data fllw.data then
                    peek_opt ~cmp:cmp (hd   :: rest)
                else
                    peek_opt ~cmp:cmp (fllw :: rest)
    ;;

    (* joins subtrees of equal degrees 
       The ntree is added back into the hashtable to be
       rejoined with the one there if it existed.
       this creates the binomial tree situation which sees the
       degree
    *)
    let join ?(cmp=minify) trees  =
        let rec cascade rejoin =  
            let leftover = List.fold_left (fun acc el ->
                let degs = degree el in
                match Hashtbl.find_opt tbl degs with
                | Some tree ->
                    (* merge and rejoin *)
                    let ntree = merge ~cmp:cmp tree el  in
                    let _     = Hashtbl.remove tbl degs in
                    ntree :: acc
                | None ->
                    let _ = Hashtbl.add tbl degs el in
                    acc
            ) [] rejoin in if List.is_empty leftover then () else cascade leftover
        in
        let _   = cascade trees in
        let fin = List.of_seq @@ Hashtbl.to_seq_values tbl in
        let _   = Hashtbl.clear tbl in
        fin
    ;;

    (*
    TODO: Because we are traversing the root elements instead of memoizing the
    min|max, this is actuall o(len(root elts)) and not o1, fix it!
    *)

    let extract ?(cmp=minify) = function 
        | [] -> raise Empty
        | hd :: tail -> 
            (* straddle the root elts for the elt most true for cmp *)
            let rec split hd tl acc =
                match tl with 
                | [] -> (hd, [], acc) 
                | fllw :: rest -> 
                    if cmp hd.data fllw.data then
                        split hd   rest (fllw :: acc)
                    else
                        split fllw rest (hd   :: acc)
            in 
            (* consolidate all its successors to the root list *)
            let (it, _, rem) = split hd tail [] in 
            (it.data, join ~cmp:cmp (rem @ it.succ))
    ;;

    let extract_opt ?(cmp=minify) = function 
        | [] -> None
        | head :: tail -> 
            (* straddle the root elts for the elt most true for cmp *)
            let rec split hd tl acc =
                match tl with 
                | [] -> (hd, [], acc) 
                | fllw :: rest -> 
                    if cmp hd.data fllw.data then
                        split hd   rest (fllw :: acc)
                    else
                        split fllw rest (hd   :: acc)
            in 
            (* add all its successors to the root list 
               try and keep the number of root trees to a minimum 
               by joining same degreee nodes (consolidate) *)
            let  (it, _, rem) = split head tail [] in 
            Some (it.data, join ~cmp:cmp (rem @ it.succ))
    ;;

    (* extract_all should yield a sorted list *)
    let rec extract_all ?(cmp=minify) tree = 
        let c , y =  extract ~cmp:cmp tree in 
        match y with
        | []   -> [ c ]
        | rest ->   c :: extract_all ~cmp:cmp rest
    ;;

    let of_list ?(cmp=minify) els = 
        List.fold_right (insert ~cmp:cmp) els empty
    ;;

    let to_seq ?(cmp=minify) tree = 
            let rec aux l () = match extract_opt ~cmp:cmp l with
                | None -> Seq.Nil
                | Some (hd, tail) -> Seq.Cons (hd, (aux tail))
            in
                (aux tree)
    ;;

    (* extract until a condition is true should yield a sorted list *)
    let extract_til ?(cmp=minify) f tree = 
        let c , y =  extract ~cmp:cmp tree in 
        if f c then 
            [ c ]
        else
            match y with
            | []   -> [ c ]
            | rest ->   c :: extract_all ~cmp:cmp rest
    ;;

    (* basically search until we find the old-entry and replace with a new one 
       we have to confirm with the parent whether the main property holds and
       change otherwise by "cutting it out" to the root

       As this can slow extract by making us end up with trees of large degree
       but few successors we mark parents to have them also be cut off once a
       threshold of succesors have been "updated" by decrease or incr to sort-of 
       balance out the nodes

       This operation may end up introducing duplicates from the new entry as a
       replacement - which creates a new challenge 

       In the presence of duplicates, we decrease|increase only the first item to be
       found (local only approach) if you want to update all, you would need to
       either call it multiple times until it raises empty, but likely you need
       to choose a better structure and comparator for your Entry.t to better
       disambiguatae entries

       decrease-key does NOT trigger consolidation of the root list. 
       This is actually crucial for maintaining the O(1) amortized time complexity of decrease-key.

    *)

    let update ?(cmp=minify) hd tl newent parent leftover = 
        let maxbelopt = (peek_opt ~cmp:cmp hd.succ) in
        match maxbelopt with
        | Some maxbel ->
            let par, son = (cmp parent.data newent), (cmp maxbel.data newent) in
            if par && not son then
                (* true, false -> ok and consistent *)
                ({ hd with data=newent }  :: tl, leftover, true)
            else
                (* true, true  -> hill inconsistency *)
                let _ = (parent.churn <- parent.churn + 1) in
                (tl, { data=newent; churn=0; succ=[] } :: hd.succ, true)
        | None -> 
            let par = (cmp parent.data newent) in
            if par then
                (* true, false  -> ok *)
                ({ hd with data=newent } :: tl, leftover, true)
            else
                (* true, true   -> hill  *)
                let _ = (parent.churn <- parent.churn + 1) in
                (tl, { data=newent; churn=0; succ=[] } :: hd.succ, true)
    ;;

    (* local only increase, duplicates not updated *)
    let increase ?(cmp=minify) oldent newent tree = 
        if (Entry.ocompare (Entry.bind oldent) (Entry.bind newent)) =  1 then
            failwith "new value must be larger"
        else if List.is_empty tree then
            raise Empty
        else
            let rec atparent parent tree leftover found = match tree with
                | [] ->
                    tree, leftover, found
                | (hd :: tl) -> 
                    if (equal hd.data oldent) then 
                        update hd tl newent parent leftover
                    else if cmp hd.data oldent then
                        let nt, lf, wasfound = atparent hd hd.succ leftover found in
                        if wasfound then
                            if hd.churn > (!churn_threshold) then
                                (* many children have died, we die as well :-( *)
                                let _ = (parent.churn <- parent.churn + 1) in
                                (tl, { hd with succ=nt } :: lf, wasfound)
                            else
                                ({ hd with succ=nt } :: tl, lf, wasfound)
                        else
                            (* the node may have moved to the root, straddle
                               the tail *)
                            let nt, lf, rem = atparent parent tl leftover wasfound in
                            (hd :: nt, lf, rem)
                    else
                        let nt, lf, rem = atparent parent tl leftover found in
                        (hd :: nt, lf, rem)
            in let ntree, left, found = atparent { data=newent; churn=0; succ=[] } tree [] false in 
            if found then
                (* reset root churn values *)
                let _ = (List.iter (fun n -> n.churn <- 0) left) in
                ntree @ left
            else
                failwith "value not in heap"
    ;;

    (* local only decrease, duplicates not updated *)
    let decrease ?(cmp=minify) oldent newent tree = 
        if (Entry.ocompare (Entry.bind oldent) (Entry.bind newent)) = -1 then
            failwith "new value must be smaller"
        else if List.is_empty tree then
            raise Empty
        else
            let rec atparent parent tree leftover found = match tree with
                | [] ->
                    (tree, leftover, found)
                | (hd :: tl) -> 
                    (* found the element *)
                    if (equal hd.data oldent) then 
                        update hd tl newent parent leftover
                    else if cmp hd.data oldent then
                        let nt, lf, wasfound = (atparent hd hd.succ leftover found) in
                        (* backtrack, check if we hit a churn threshold *)
                        if wasfound then
                            if hd.churn > (!churn_threshold) then
                                (* many children have died, we die as well :-( *)
                                let _ = (parent.churn <- parent.churn + 1) in
                                (tl, { hd with succ=nt } :: lf, wasfound)
                            else
                                ({ hd with succ=nt } :: tl, lf, wasfound)
                        else 
                            (* the node may have moved to the root, straddle
                               the tail *)
                            let nt, lf, rem = atparent parent tl leftover wasfound in
                            (hd :: nt, lf, rem)
                    else
                        (* straddle as the former doesn't need checking *)
                        let nt, lf, rem = atparent parent tl leftover found in
                        (hd :: nt, lf, rem)
            in 
            (* start with self as its own parent *)
            let ntree, left, wasfound = atparent { data=newent; churn=0; succ=[] } tree [] false in 
            if wasfound then
                (* reset churn values *)
                let _ = (List.iter (fun n -> n.churn <- 0) left) in
                ntree @ left
            else
                failwith "value not in heap"
    ;;

end
