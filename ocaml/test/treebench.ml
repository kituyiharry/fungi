open Fungi
open Core_bench


let main () =

    let module IntFibHeap =  Heap.MakeFibHeap(Heap.Surject(Int)) in
    let randInts = List.init 1500 (fun _idx -> Random.int 500) in
    let theap    = List.fold_right (IntFibHeap.insert) randInts IntFibHeap.empty in

    (* TODO: do a csv export, bench only relevant operations *)
    Command_unix.run ~argv:[""; "-quota"; "1s"] (
        Bench.make_command [
            Bench.Test.create ~name:"fibHeap_insert"
                (fun () -> List.fold_right (IntFibHeap.insert) randInts IntFibHeap.empty);
            Bench.Test.create ~name:"fibHeap_extract_all"
                (fun () -> ignore (IntFibHeap.extract_all theap));
            Bench.Test.create ~name:"fibHeap_collapse"
                (fun () -> ignore (IntFibHeap.collapse theap))
        ]
    )
;;

let () = main ()
;;
