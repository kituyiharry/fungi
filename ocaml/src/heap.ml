module Make_heap(Cmp : Set.OrderedType) = struct

    type elt = Cmp.t

    exception Empty

    type t = 
        | Empty
        | Node of { value: elt; rank : int; left : t; right: t; }
    ;;

    let rank = function
        | Empty -> 0
        | Node {rank; _} -> rank
    ;;

    let make_node value left right =
        let left, right = 
            if rank left >= rank right
            then left, right
            else right, left
        in
        Node {
            value;
            rank = rank right + 1;
            left ;
            right;
        }
    ;;

    (* Merge two trees assuming all elements in t1 and t2 are >= than value *)
    let rec merge_with_min value t1 t2 =
        match t1, t2 with
        | Empty, t | t, Empty -> t
        | Node {value=v1; left=l1; right=r1; _},
            Node {value=v2; left=l2; right=r2; _} ->
            if Cmp.compare v1 v2 <= 0 then
                if Cmp.compare value v1 <= 0 then
                    make_node v1 l1 (merge_with_min value r1 t2)
                else failwith "Heap property violation"
            else
            if Cmp.compare value v2 <= 0 then
                make_node v2 l2 (merge_with_min value t1 r2)
            else failwith "Heap property violation"
    ;;

    (* Main merge function that starts the merge process *)
    let merge t1 t2 =
        match t1, t2 with
        | Empty, t | t, Empty -> t
        | Node {value=v1; left=l1; right=r1; _},
            Node {value=v2; left=l2; right=r2; _} ->
            if Cmp.compare v1 v2 <= 0 then
                make_node v1 l1 (merge_with_min v1 r1 t2)
            else
                make_node v2 l2 (merge_with_min v2 t1 r2)
    ;;

    let empty = Empty

    let is_empty = function
        | Empty -> true
        | Node _ -> false
    ;;

    let insert t x = 
        merge t (Node {value=x; rank=1; left=Empty; right=Empty})

    let peek_top = function
        | Empty -> raise Empty
        | Node {value; _} -> value
    ;;

    let extract_top = function
        | Empty -> raise Empty
        | Node {value; left; right; _} -> value, merge left right
    ;;

    (* Build heap bottom-up to maintain invariants *)
    let of_list xs =
        let sorted = List.sort Cmp.compare xs in
        let rec build = function
            | [] -> empty
            | x :: xs ->
                let heap = build xs in
                insert heap x
        in
        build (List.rev sorted)

    let to_list t =
        let rec aux acc = function
            | Empty -> acc
            | t -> 
                let min, rest = extract_top t in
                aux (min :: acc) rest
        in
        List.rev (aux [] t)

    let size t =
        let rec aux = function
            | Empty -> 0
            | Node {left; right; _} -> 1 + aux left + aux right
        in
        aux t

    let merge = merge

    (* Helper to verify heap property - useful for testing *)
    let rec verify = function
        | Empty -> true
        | Node {value; left; right; _} ->
            let check_subtree = function
                | Empty -> true
                | Node {value=child_value; _} ->
                    Cmp.compare value child_value <= 0
            in
            check_subtree left && check_subtree right &&
            verify left && verify right
end
