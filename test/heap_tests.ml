open Fungi
open Alcotest

module H = Heap.MakeFibHeap (Heap.Adapt (Int))

(* default comparator is minify, so the heap behaves as a min-heap *)

let extract_all_sorted _cx =
    let h = H.of_list [5; 3; 8; 1; 9; 2; 7] in
    check (list int) "extract_all yields ascending order"
        [1; 2; 3; 5; 7; 8; 9] (H.extract_all h)
;;

let cardinal_counts _cx =
    let h = H.of_list [4; 4; 1; 3; 1] in
    check int "cardinal counts every (duplicate included) node" 5 (H.cardinal h)
;;

let duplicates_preserved _cx =
    let h = H.of_list [2; 2; 1; 3; 1] in
    check (list int) "duplicates are all extracted, sorted"
        [1; 1; 2; 2; 3] (List.sort compare (H.extract_all h))
;;

let peek_is_min _cx =
    let h = H.of_list [5; 3; 8; 1; 9] in
    let top = H.peek h in
    check int "peek returns the minimum" 1 top.H.data
;;

let membership _cx =
    let h = H.of_list [5; 3; 8] in
    check bool "present element found"     true  (H.mem 3 h);
    check bool "absent element not found"  false (H.mem 42 h)
;;

let seq_roundtrip _cx =
    let h = H.of_list [5; 3; 8; 1] in
    check (list int) "to_seq drains in sorted order"
        [1; 3; 5; 8] (List.of_seq (H.to_seq h))
;;

let empty_behaviour _cx =
    check bool "empty is empty" true (H.is_empty H.empty);
    (* the [Empty] exception is not exported by the FibHeap signature, so we
       only assert that extraction on an empty heap does raise *)
    let raised = try ignore (H.extract H.empty); false with _ -> true in
    check bool "extract on empty raises" true raised
;;

(* decrease-key lowers a node's priority; it should surface earlier *)
let decrease_key _cx =
    let h = H.of_list [5; 10; 15] in
    let h' = H.decrease 10 3 h in
    check (list int) "decreased key becomes new minimum"
        [3; 5; 15] (H.extract_all h')
;;

let () =
    run "FibHeap" [
        "ordering", [
            test_case "extract_all sorted" `Quick extract_all_sorted;
            test_case "peek is min"        `Quick peek_is_min;
            test_case "to_seq sorted"      `Quick seq_roundtrip;
        ];
        "contents", [
            test_case "cardinal"     `Quick cardinal_counts;
            test_case "duplicates"   `Quick duplicates_preserved;
            test_case "membership"   `Quick membership;
        ];
        "operations", [
            test_case "empty"        `Quick empty_behaviour;
            test_case "decrease key" `Quick decrease_key;
        ];
    ]
;;
