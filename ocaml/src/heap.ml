(******************************************************************************
*                                                                             *
*                             Fungi Heap                                      *
*                      Functional Fibonacci Heap                              *
*                         harryk@harryk.dev                                   *
*                                                                             *
*******************************************************************************)

(**

   {1:fibheap Fibonacci Heap Implementation }

    Simplest Indexed Fibonacci (d-ary) heap                                    
      - Every node's key is less(default cmp) than or equal to its children     
        keys as given by the Entry.compare function                            
      - The minimum|maximum element is always in the root list depending       
        on your compare function                                               
      - Unlike binary heaps, there's no enforced structure                     
      - Duplicates are tolerated as they can be introduced by decrease and     
        increase operations                                                    
      - A node can have any number of children                                 
      - Children can be added or removed freely                                
      - no redundancy (if 2 nodes are the same their trees are assumed to      
        be the same)                                                           
      - For simplicity we don't have a min pointer at the root list but it may 
        be added in the future, we so have to walk the root list to get the    
        min                                                                    

*)
 
module type Ordinal = sig 
    (* The full type *)
    type t
    (* How we order in the heap *)
    type order
    (* How we get the order from a node *)
    val  bind:     t  -> order
    (* How we compare different elements *)
    (* total order *)
    val  compare:  t  -> t -> int
    (* How we compare different orders *)
    val  order: order -> order -> int
end

(* immediately binds a value as its own order *)
module Surject(Inner: Set.OrderedType): Ordinal with type t = Inner.t and type order = Inner.t = struct 
    type t          = Inner.t
    type order      = Inner.t
    let  bind    e  = e
    let  order      = Inner.compare
    let  compare    = Inner.compare
end

module type FibHeap = sig
    type node
    type order
    type elts        = { data: node; mutable churn: int; index: int; succ: elts list }
    type t           = elts list
    val empty:       t
    val is_empty:    t -> bool
    val equal:       node -> node -> bool
    val minify:      node -> node -> bool
    val maxify:      node -> node -> bool
    val oequal:      node -> node -> int
    val degree:      elts -> int
    val cardinal:    t -> int
    val collapse:    t -> node list
    val instance:    node -> int -> elts
    val singleton:   node -> t
    val dedup:       node -> elts list -> (elts list * node list)
    val dedup_idx:   node -> elts list -> elts list * (node * int) list
    val dupcount:    node -> t -> int
    val extract_til: ?cmp:(node -> node -> bool) -> (node -> bool) -> t -> node list
    val to_seq:      ?cmp:(node -> node -> bool) -> t -> node Seq.t
    val of_seq:      ?cmp:(node -> node -> bool) -> node Seq.t -> elts list
    val of_list:     ?cmp:(node -> node -> bool) -> node list -> t
    val consolidate: ?cmp:(node -> node -> bool) -> t -> t
    val mem:         ?cmp:(node -> node -> bool) -> node -> t -> bool
    val insert:      ?cmp:(node -> node -> bool) -> node -> t -> t
    val dupinsert:   ?cmp:(node -> node -> bool) -> node -> t -> t
    val peek:        ?cmp:(node -> node -> bool) -> t -> elts
    val peek_opt:    ?cmp:(node -> node -> bool) -> t -> elts option
    val merge:       ?cmp:(node -> node -> bool) -> elts -> elts -> elts
    val extract:     ?cmp:(node -> node -> bool) -> t -> (node * t)
    val extract_opt: ?cmp:(node -> node -> bool) -> t -> (node * t) option
    val extract_all: ?cmp:(node -> node -> bool) -> t -> node list
    val update:      ?cmp:(node -> node -> bool) -> node -> elts -> elts list -> elts -> elts list -> elts list * elts list * bool
    val increase:    ?cmp:(node -> node -> bool) -> node -> node -> t -> t
    val decrease:    ?cmp:(node -> node -> bool) -> node -> node -> t -> t
    val find:        (node -> bool) -> elts list -> node
    val find_opt:    (node -> bool) -> elts list -> node option
end

(** 
    {2:create Create a Fibonacci Heap}

    {@ocaml[ 
        module F  = Heap.MakeFibHeap (struct
            (* the main type *)
            type t         = int
            (* how to determine order *)
            type order     = int
            let  bind    e = e
            (* compare to orders *)
            let  order     = Int.compare
            (* compare to nodes *)
            let  compare   = Int.compare
        end);; 
    ]}


*)
module MakeFibHeap(Entry: Ordinal): FibHeap with type node = Entry.t and type order = Entry.order = struct

    type node  = Entry.t
    ;;

    type order = Entry.order 
    ;;

    (* churn tracks the grand-children that have been removed *)
    type elts = { data: node; mutable churn: int; index: int; succ: elts list }
    ;;

    type t    = elts list
    ;;

    let empty = []
    ;;

    let is_empty = List.is_empty
    ;;

    (* internal joining tbl which stores int (degree) -> elts *)
    let tbl = Hashtbl.create 0
    ;;

    let equal  l r = (Entry.compare l r) =  0
    ;;

    (* comparator that determines if the root is the smallest or the largest *)
    let oequal l r = (Entry.order (Entry.bind l) (Entry.bind r))
    ;;

    (* max-heapify, root elts are the most of their successors *)
    let maxify l r = oequal l r =  1
    ;;

    (* min-heapify, root elts are the least of their successors *)
    let minify l r = oequal l r = -1
    ;;

    (* churn threshold *)
    let churn_threshold = ref 2
    ;;

    let instance pleaf idx = { data=pleaf; churn=0; index=idx; succ=[] }
    ;;

    let mem ?(cmp=minify) pleaf ptree = 
        let rec fmem tree rem = match tree with
        | [] -> (match rem with
                | [] -> false
                | branch :: left  ->  
                    (match branch with
                        | [] ->
                            (match left with 
                                | [] -> false
                                | l :: r ->  fmem l r)
                        | hd :: rest ->
                            (* reduce the number of `List.concat` operations by
                               'batching' lists - this works really good for dense heaps
                            *)
                            (equal hd.data pleaf) 
                                || 
                            (if cmp hd.data pleaf then 
                                fmem hd.succ (rest :: left) 
                            else 
                                fmem rest (hd.succ :: left))
                    )
                )
        | hd :: tail ->
            (equal hd.data pleaf) 
                ||
            (if cmp hd.data pleaf then 
                fmem hd.succ (tail :: rem) 
            else 
                fmem tail (hd.succ :: rem))
        in fmem ptree []
    ;;

    let rec cardinal = function 
        | [] -> 0
        | hd :: tail ->
            (cardinal hd.succ) + 1 + (cardinal tail)
    ;;

    (* linear time find *)
    let find f = function 
        | [] -> raise Not_found
        | hd :: tail ->
            if f hd.data then 
                hd.data 
            else
                let rec mergefind tree rem = match tree with 
                    | [] -> 
                        (match rem with
                            | [] -> raise Not_found
                            | hd :: rest -> 
                                if f hd.data then 
                                    hd.data
                                else
                                    (mergefind rest hd.succ)
                        )
                    | hd :: tail ->
                        if f hd.data then 
                            hd.data 
                        else
                            (mergefind tail (hd.succ @ rem))
                in mergefind hd.succ tail
    ;;

    (* linear time find *)
    let find_opt f = function 
        | [] -> None
        | hd :: tail ->
            if f hd.data then 
                Some hd.data 
            else
                let rec mergefind tree rem = match tree with 
                    | [] -> 
                        (match rem with
                            | [] -> None
                            | hd :: rest -> 
                                if f hd.data then 
                                    Some hd.data
                                else
                                    (mergefind rest hd.succ)
                        )
                    | hd :: tail ->
                        if f hd.data then 
                            Some hd.data 
                        else
                            (mergefind tail (hd.succ @ rem))
                in mergefind hd.succ tail
    ;;

    (* count all duplicate occurences of pleaf *)
    let dupcount pleaf tree = 
        let rec fdup stree acc = match stree with 
            | [] -> acc 
            | hd :: tl ->
                if equal hd.data pleaf then 
                    fdup tl @@ fdup hd.succ (acc + 1)
                else
                    (fdup[@tailcall]) tl acc
        in fdup tree 0
    ;;

    (* insert with partial tolerance for duplicates, insertion order may not hold
       when extracting. Especially if you insert AFTER an extract_min operation
       however it should be fine if inserts happen at once before extracts 
       or in the case that updates are consistent with older inserted heap
       entries and not creating Heap violations e.g. hill | basin inconsistency 

       TODO: Make list head the min pointer (along with consolidate) ??
    *)
    let insert ?(cmp=minify) pleaf tree = 
        let rec pinsert pleaf dupc = function
            | [] -> [ (instance pleaf dupc) ]
            | (hd :: tail) ->
                if equal hd.data pleaf then
                   let dupc' = dupc + 1 in  
                   { hd with succ= (pinsert pleaf (dupc') hd.succ) } :: tail
                else if cmp hd.data pleaf then
                    { hd with succ=(pinsert pleaf dupc hd.succ) } :: tail
                else
                    hd :: (pinsert pleaf dupc tail)
        in pinsert pleaf 0 tree
    ;;

    (* insert with high tolerance for duplicates and try to preserve the insertion order
       as much as possible - requires counting all duplicates ahead *)
    let dupinsert ?(cmp=minify) pleaf tree = 
        (* TODO: By using a mutable ref, we can perhaps reference the duplicate
           value and emplace it in new duplicate instances *)
        let dc = (dupcount pleaf tree) + 1 in
        let rec dupinsert pleaf dupc = function
            | [] -> [ (instance pleaf dupc) ]
            | (hd :: tail) ->
                if equal hd.data pleaf then
                    (* we could have duplicates with different priorities!
                       so watch out for hill or basin inconsistency *)
                    if cmp hd.data pleaf then
                        { hd with succ=(dupinsert pleaf (dupc) hd.succ) } :: tail
                    else
                        (* pleaf is fresh so we bubble down hd.data with its index *)
                        {   data=pleaf; churn=0; index=dupc; 
                            succ=(dupinsert hd.data (hd.index)  hd.succ)
                        } :: (tail)
                else if cmp hd.data pleaf then
                    { hd with succ=(dupinsert pleaf dupc hd.succ) } :: tail
                else
                    hd :: (dupinsert pleaf dupc tail)
        in dupinsert pleaf dc tree
    ;;

    let singleton pleaf = 
        [ { data=pleaf; churn=0; index=0; succ=[] } ]
    ;;

    (* insert like merge into an existing tree *)
    let rec merge_node ?(cmp=minify) pleaf = function
        | [] -> [ pleaf ]
        | (hd :: tail) as s ->
            if equal hd.data pleaf.data then
                if hd.index < pleaf.index then
                    { hd with succ=pleaf :: (hd.succ @ pleaf.succ) } :: s
                else
                    { pleaf with succ=hd :: (hd.succ @ pleaf.succ) } :: s
            else if cmp hd.data pleaf.data then
                { hd with succ=(merge_node pleaf ~cmp:cmp hd.succ) } :: tail
            else
                hd :: (merge_node pleaf ~cmp:cmp tail)
    ;;

    (* remove all duplicate occurences of pleaf *)
    let dedup pleaf tree = 
        let rec fdup stree ntree acc = match stree with 
            | [] -> (ntree, acc) 
            | hd :: tl ->
                if equal hd.data pleaf then 
                    let (ntree', acc') = fdup hd.succ ntree (hd.data :: acc) in 
                    fdup tl ntree' acc'
                else
                    fdup tl (hd :: ntree) acc
        in fdup tree [] []
    ;;

    (* remove all duplicate occurences of pleaf in reverse insertion order *)
    let dedup_idx pleaf tree = 
        let rec fdup stree ntree acc = match stree with 
            | [] -> (ntree, acc) 
            | hd :: tl ->
                if equal hd.data pleaf then 
                    let (ntree', acc') = fdup hd.succ ntree ((hd.data, hd.index) :: acc) in 
                    fdup tl ntree' acc'
                else
                    fdup tl (hd :: ntree) acc
        in fdup tree [] []
    ;;

    (* merge two tree elements *)
    let merge ?(cmp=minify) tree trunk = 
        (* duplicate allowed to exit in successor *)
        if equal tree.data trunk.data then
            (* try and preserve the insertion order *)
            if tree.index > trunk.index then
                { trunk with succ = tree  :: trunk.succ }
            else
                { tree with  succ = trunk :: tree.succ }
        (* bubbling elements to maintain heap properties  *)
        else if cmp tree.data trunk.data then
            (* bubble down the tree *)
            { tree  with succ=(merge_node ~cmp:cmp trunk tree.succ) }
        else
            (* bubble up the trunk *)
            { trunk with succ=(merge_node ~cmp:cmp tree trunk.succ) }
    ;;

    exception Empty

    let degree tree = List.length tree.succ
    ;;

    (** inorder traverse the heap, elements will likely be out of order 
        NB: In the rem, we use list of list rather than flat plain list to
        speed up the process
    *)
    let collapse = function 
        | [] -> [] 
        | { succ=child; data=pleaf;_ } :: tail ->
            let rec fcollapse tree rem acc = 
                match tree with
                | [] -> 
                    (* reduce the number of `List.concat` operations by
                       'batching' lists - this works really good for dense heaps
                    *)
                    (match rem with 
                        | [] -> acc
                        | branch :: left -> 
                            (match branch with
                                | [] -> 
                                    (match left with 
                                        | [] -> acc
                                        | l :: r ->  fcollapse l r acc)
                                | { succ=child''; data=pleaf'';_ } :: tl'' -> 
                                    fcollapse child'' ([  tl'' ] @ left) (pleaf'' :: acc)
                            )
                    )
                | { succ=child'; data=pleaf';_ } :: tl' ->
                    fcollapse child' (tl' :: rem) (pleaf' :: acc)
            in fcollapse child ([ tail ]) ([ pleaf ])
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
       this creates the binomial tree situation 
    *)
    let consolidate ?(cmp=minify) trees  =
        (* push all nodes by degree *)
        let rec cascade rejoin =  
            let leftover = List.fold_left (fun acc eltree ->
                let deg = degree eltree in
                match Hashtbl.find_opt tbl deg with
                | Some tree ->
                    (* merge and rejoin *)
                    let ntree = merge ~cmp:cmp tree eltree  in
                    let _     = Hashtbl.remove tbl deg in
                    ntree :: acc
                | None ->
                    let _ = Hashtbl.add tbl deg eltree in
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
                | [] -> (hd, acc) 
                | fllw :: rest -> 
                    if cmp hd.data fllw.data then
                        split hd   rest (fllw :: acc)
                    else
                        split fllw rest (hd   :: acc)
            in 
            (* consolidate all its successors to the root list *)
            let (it, rem) = split hd tail [] in 
            (it.data, consolidate ~cmp:cmp (rem @ it.succ))
    ;;

    let extract_opt ?(cmp=minify) = function 
        | [] -> None
        | head :: tail -> 
            (* straddle the root elts for the elt most true for cmp *)
            let rec split hd tl acc =
                match tl with 
                | [] -> (hd, acc) 
                | fllw :: rest -> 
                    if cmp hd.data fllw.data then
                        split hd   rest (fllw :: acc)
                    else
                        split fllw rest (hd   :: acc)
            in 
            (* add all its successors to the root list 
               try and keep the number of root trees to a minimum 
               by joining same degreee nodes (consolidate) *)
            let  (it, rem) = split head tail [] in 
            Some (it.data, consolidate ~cmp:cmp (rem @ it.succ))
    ;;

    (* extract_all should yield a sorted list *)
    let rec extract_all ?(cmp=minify) tree = 
        let sml, rem =  extract ~cmp:cmp tree in 
        match rem with
        | []   -> [ sml ]
        | rest ->   sml :: extract_all ~cmp:cmp rest
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

    let rec of_seq ?(cmp=minify) tseq = 
        match tseq () with
        | Seq.Nil -> []
        | Seq.Cons (x1, seq) ->
            begin match seq () with
                | Seq.Nil -> [(instance x1 0)]
                | Seq.Cons (x2, seq) -> 
                    insert ~cmp:cmp (x1) @@ insert ~cmp:cmp (x2) @@ of_seq ~cmp:cmp seq
                end
    ;;

    (* extract until a condition is true should yield a sorted list *)
    let extract_til ?(cmp=minify) f tree = 
        let sml , rem =  extract ~cmp:cmp tree in 
        if f sml then 
            [ sml ]
        else
            match rem with
            | []   -> [ sml ]
            | rest ->   sml :: extract_all ~cmp:cmp rest
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

    let update ?(cmp=minify) newent hd tl parent leftover = 
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
                (tl, { data=newent; churn=0; succ=[]; index=hd.index; } :: hd.succ, true)
        | None -> 
            let par = (cmp parent.data newent) in
            if par then
                (* true, false  -> ok *)
                ({ hd with data=newent } :: tl, leftover, true)
            else
                (* true, true   -> hill  *)
                let _ = (parent.churn <- parent.churn + 1) in
                (tl, { data=newent; churn=0; succ=[]; index=hd.index } :: hd.succ, true)
    ;;

    (* local only increase, duplicates not updated 
    *)
    let increase ?(cmp=minify) old bgger tree = 
        if List.is_empty tree then
            raise Empty
        else
            let rec atparent parent tree leftover found = match tree with
                | [] ->
                    tree, leftover, found
                | (hd :: tl) -> 
                    if (equal hd.data old) then 
                        if (oequal hd.data bgger) =  1 then
                            failwith "new value must be larger"
                        else
                            update ~cmp:cmp bgger hd tl parent leftover
                    else
                        let nt, lf, wasfound = atparent hd hd.succ leftover found in
                        if wasfound then
                            if hd.churn > (!churn_threshold) then
                                (* many children have died, we die as well :-( *)
                                let _ = (parent.churn <- (parent.churn + 1)) in
                                (tl, { hd with succ=nt } :: lf, wasfound)
                            else
                                ({ hd with succ=nt } :: tl, lf, wasfound)
                        else
                            (* the node may have moved to the root, straddle
                               the tail *)
                            let nt, lf, rem = atparent parent tl leftover found in
                            (hd :: nt, lf, rem)
            in 
            let self = { data=bgger; churn=0; succ=[]; index=0; } in
            let ntree, left, found = atparent self tree [] false in 
            if found then
                (* reset root churn values *)
                let _ = (List.iter (fun n -> n.churn <- 0) left) in
                ntree @ left
            else
                failwith "value not in heap"
    ;;

    (* local only priority decrease, duplicates not updated 
        this operation is uninformed of the former nodes priority and thus searches
        the whole list 
    *)
    let decrease ?(cmp=minify) old smller tree = 
        if List.is_empty tree then
            raise Empty
        else
            let rec atparent parent tree leftover found = match tree with
                | [] ->
                    (tree, leftover, found)
                | (hd :: tl) -> 
                    (* found the element *)
                    if (equal hd.data old) then 
                        (*if (Entry.ocompare (Entry.bind hd.data) (newent)) = -1 then*)
                        if (oequal hd.data smller) = -1 then
                            failwith "new value must be smaller"
                        else
                            update ~cmp:cmp smller hd tl parent leftover
                    else
                        let nt, lf, wasfound = (atparent hd hd.succ leftover found) in
                        (* backtrack, check if we hit a churn threshold *)
                        if wasfound then
                            if hd.churn > (!churn_threshold) then
                                (* many children have died, we die as well :-( *)
                                let _ = (parent.churn <- (parent.churn + 1)) in
                                (tl, { hd with succ=nt } :: lf, wasfound)
                            else
                                ({ hd with succ=nt } :: tl, lf, wasfound)
                        else
                            (* the node may have moved to the root, straddle
                               the tail *)
                            let nt, lf, rem = atparent parent tl leftover found in
                            (hd :: nt, lf, rem)
            in 
            (* start with self as its own parent *)
            let self = { data=smller; churn=0; succ=[]; index=0; } in
            let ntree, left, wasfound = atparent self tree [] false in 
            if wasfound then
                (* reset churn values *)
                let _ = (List.iter (fun n -> n.churn <- 0) left) in
                ntree @ left
            else
                failwith "value not in heap"
    ;;

end
