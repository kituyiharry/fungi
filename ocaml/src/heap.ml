(**

    Simplest fibonacci heap
        - Every node's key is less than or equal to its children's keys
          as given by the Entry.compare function
        - The minimum element is always in the root list
        - Unlike binary heaps, there's no enforced structure
        - A node can have any number of children
        - Children can be added or removed freely
    
*)
module MakeFibHeap(Entry: Set.OrderedType) = struct

    type node = Entry.t
    ;;

    type elts = { data: node; succ: elts list }
    ;;

    type t = elts list
    ;;

    let empty: t = []
    ;;

    let equal  l r = (Entry.compare l r) = 0
    ;;

    (* comparator that determines if the root is the smallest or the largest *)

    (* max-heapify, root elts are the most of their successors *)
    let maxify l r = (Entry.compare l r) =  1
    ;;

    (* min-heapify, root elts are the least of their successors *)
    let minify l r = (Entry.compare l r) = -1
    ;;

    let rec mem p = function 
        | [] -> 
            false
        | hd :: tail ->
            (equal hd.data p) || (mem p hd.succ) || (mem p tail)
    ;;

    let rec cardinal = function 
        | [] -> 0
        | hd :: tail ->
            1 + (cardinal hd.succ) + (cardinal tail)
    ;;

    let rec insert ?(cmp=minify) p = function
        | [] as a -> { data=p; succ=[] } :: a
        | (hd :: tail) as s ->
            if equal hd.data p then
                s
            else if cmp hd.data p then
                { data=hd.data; succ=(insert p ~cmp:cmp hd.succ) } :: tail
            else
                hd :: (insert p ~cmp:cmp tail)
    ;;

    exception Empty

    let rec find ?(cmp=minify) p = function 
        | [] -> raise Empty
        | hd :: tail ->
            if (equal hd.data p) then 
                hd
            else if cmp hd.data p then
                find p hd.succ
            else
                find p tail
    ;;

    let rec find_opt ?(cmp=minify) p = function 
        | [] -> None
        | hd :: tail ->
            if (equal hd.data p) then 
                Some hd
            else if cmp hd.data p then
                find_opt p hd.succ
            else
                find_opt p tail
    ;;

    let degree p h =
        match find_opt p h with
        | None -> 0
        | Some v -> List.length v.succ
    ;;

end
