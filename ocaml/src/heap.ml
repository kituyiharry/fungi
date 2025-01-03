module MakeFibHeap(Entry: Set.OrderedType) = struct

    type node = Entry.t
    ;;

    type elts = { data: node; succ: elts list }
    ;;

    type t = elts list
    ;;

    let empty: t = []
    ;;

    let equal l r = (Entry.compare l r) = 0
    ;;

    let rec insert p = function
        | [] as a -> { data=p; succ=[] } :: a
        | hd :: tail -> 
            if equal hd.data p then
                hd :: tail 
            else if hd.data > p then
                { data=hd.data; succ=(insert p hd.succ) } :: tail
            else
                hd :: (insert p tail)
    ;;

    let rec mem p = function 
        | [] -> false 
        | hd :: tail -> 
            (equal hd.data p) || (mem p hd.succ) || (mem p tail)
    ;;

    let rec cardinal = function 
        | [] -> 0 
        | hd :: tail ->
            1 + (cardinal hd.succ) + cardinal tail
    ;;

end
