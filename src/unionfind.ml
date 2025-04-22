(**
    Disjoint Set data structure
*)
module type UnionFind = sig 
    type elt

    module BijectTbl: Hashtbl.S with type key = elt
    module ResolvTbl: Hashtbl.S with type key = int

    type t = { 
        (* mapping from each element to an index *)
        map: int BijectTbl.t;
        (* each group index *)
        arr: int array;
        (* how large each group is *)
        sze: int array;
        (* number of components *)
        mutable count: int
    }

    val create:   int -> elt Seq.t -> t
    val find:     elt -> t -> int
    val union:    t -> elt -> elt -> unit
end

module MakeDisjointSet(Ord: Hashtbl.HashedType): UnionFind with type elt = Ord.t = struct 

    module BijectTbl = Hashtbl.Make (Ord)
    module ResolvTbl = Hashtbl.Make (struct 
        type t    = int 
        let equal = Int.equal
        let hash  = Hashtbl.hash
    end)

    type t = { 
        map: int BijectTbl.t;
        arr: int array;
        sze: int array;
        mutable count: int
    }
    type elt = Ord.t

    let create size eltseq  = 
        let acc = {
            map = BijectTbl.create size;
            arr = Array.init size (Fun.id);
            sze = Array.init size (Fun.const 1);
            count=size 
        } in
        let _ = Seq.iteri (fun idx elt ->
            BijectTbl.add acc.map elt idx
        ) eltseq in 
        acc
    ;;

    (* find the root index of an element *)
    let find bijelt dj = 
        let bijidx = BijectTbl.find dj.map bijelt in
        let rec iter bijidx cmprss =
            let bijval = dj.arr.(bijidx) in
            if bijval = bijidx then
                (* path compression to shorten future queries *)
                let _ = List.iter (fun idx -> dj.arr.(idx) <- bijidx) cmprss in
                bijidx
            else
                iter bijval (bijidx :: cmprss)
        in iter bijidx []
    ;;

    (* unify elements *)
    let union disj elt elt' = 
        (* merge elements into the bigger set - loops are considered as their
           own group - at the beginning all elements are their own groups *)
        let eltroot  = find elt  disj in
        let eltroot' = find elt' disj in

        let grpsze   = disj.sze.(eltroot)  in
        let grpsze'  = disj.sze.(eltroot') in

        (* check if already in the same group *)
        if not (eltroot = eltroot') then
            if grpsze' > grpsze then
                let _ = disj.sze.(eltroot') <- grpsze' + disj.sze.(eltroot)  in
                let _ = disj.count <- disj.count - 1 in
                disj.arr.(eltroot) <- disj.arr.(eltroot')
            else
                let _ = disj.sze.(eltroot) <- grpsze   + disj.sze.(eltroot') in
                let _ = disj.count <- disj.count - 1 in
                disj.arr.(eltroot') <- disj.arr.(eltroot)
    ;;

end
