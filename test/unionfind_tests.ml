open Fungi
open Alcotest

module U = Unionfind.MakeDisjointSet (struct
    type t    = int
    let equal = Int.equal
    let hash  = Hashtbl.hash
end)

let mk n = U.create n (Seq.init n Fun.id)

(* Each element starts in its own component *)
let initial_singletons _cx =
    let dj = mk 5 in
    check int "5 components initially" 5 dj.U.count;
    check bool "0 and 1 are in different sets initially"
        false (U.find 0 dj = U.find 1 dj)
;;

(* union links two elements and shrinks the component count *)
let simple_union _cx =
    let dj = mk 5 in
    U.union dj 0 1;
    check bool "0 and 1 share a root after union" true (U.find 0 dj = U.find 1 dj);
    check int  "component count decremented" 4 dj.U.count
;;

(* union is transitive across a chain of merges *)
let transitive_union _cx =
    let dj = mk 6 in
    U.union dj 0 1;
    U.union dj 2 3;
    U.union dj 1 3;
    check bool "0 and 2 connected transitively" true (U.find 0 dj = U.find 2 dj);
    check bool "1 and 3 connected"              true (U.find 1 dj = U.find 3 dj);
    check bool "4 remains separate from 0"      false (U.find 4 dj = U.find 0 dj);
    check int  "three components remain"        3 dj.U.count
;;

(* unioning already-connected elements is a no-op on the count *)
let idempotent_union _cx =
    let dj = mk 4 in
    U.union dj 0 1;
    let c = dj.U.count in
    U.union dj 0 1;
    check int "count unchanged when already merged" c dj.U.count
;;

(* a sequence longer than the declared size is rejected instead of crashing
   with an opaque out-of-bounds error later on *)
let create_bounds_guard _cx =
    check_raises "over-long sequence rejected"
        (Invalid_argument "MakeDisjointSet.create: element sequence is longer than size")
        (fun () -> ignore (U.create 2 (Seq.init 3 Fun.id)))
;;

(* looking up an element that was never inserted raises Not_found *)
let find_absent _cx =
    let dj = mk 3 in
    check_raises "absent element" Not_found (fun () -> ignore (U.find 99 dj))
;;

let () =
    run "UnionFind" [
        "components", [
            test_case "singletons"  `Quick initial_singletons;
            test_case "union"       `Quick simple_union;
            test_case "transitive"  `Quick transitive_union;
            test_case "idempotent"  `Quick idempotent_union;
        ];
        "robustness", [
            test_case "bounds guard" `Quick create_bounds_guard;
            test_case "find absent"  `Quick find_absent;
        ];
    ]
;;
