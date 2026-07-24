open Fungi ;;
open Alcotest;;

module IntTree = Treeset.TreeSet(Int);;

let simple_tree_add _cx =
    Alcotest.(check int) "must account for all inserted elements" 
        1 ((IntTree.cardinal (IntTree.singleton 0)))
;;

let simple_tree_remove _cx = 
    Alcotest.(check bool) "must be empty after removal" 
        true (IntTree.is_empty @@ IntTree.remove 0 @@ IntTree.singleton 0)
;;

let simple_tree_member _cx = 
    Alcotest.(check bool) "element in the set" 
        true (IntTree.mem 0 @@ IntTree.singleton 0)
;;

let simple_min_elt _cx = 
    Alcotest.(check (option int)) "minimum element" 
        (Some 1) (IntTree.min_elt_opt @@ IntTree.of_list [10;5;8;6;3;2;9;1])
;;

let simple_max_elt _cx = 
    Alcotest.(check (option int)) "maximum element" 
        (Some 10) (IntTree.max_elt_opt @@ IntTree.of_list [10;5;8;6;3;2;9;1])
;;

let simple_inorder_traversal _cx = 
    Alcotest.(check (list int)) "Inorder traversal" 
        [1;2;3;5;6;8;9;10] (List.rev @@ IntTree.inorder [] @@ IntTree.of_list [10;5;8;6;3;2;9;1])
;;

let subset_test _cx = 
    let s1 = IntTree.of_list [1;2;3;4;5] in 
    let s2 = IntTree.singleton 1 in
    let s3 = IntTree.singleton 9 in
    let _ = Alcotest.(check bool) "simple subset relation"   true (IntTree.subset s2 s1) in 
    let _ = Alcotest.(check bool) "empty set must be subset" true (IntTree.subset IntTree.empty s1) in
    let _ = Alcotest.(check bool) "proper subset "           true (IntTree.subset s1 s1) in
            Alcotest.(check bool) "not a subset "            true (not @@ IntTree.subset s3 s1) 
;;

let set_intersection _cx = 
    let s1 = IntTree.of_list [1;2;3;4;5] in 
    let s2 = IntTree.singleton 1 in
    let s3 = IntTree.singleton 9 in
    let _ =  Alcotest.(check (list int)) "overlapping" [1] (IntTree.to_list @@ IntTree.inter s2 s1) in
    let _ =  Alcotest.(check (list int)) "empty"       []  (IntTree.to_list @@ IntTree.inter IntTree.empty s1) in
             Alcotest.(check (list int)) "disjoint"    []  (IntTree.to_list @@ IntTree.inter s3 s1)
;;


let set_difference _cx = 
    let s1 = IntTree.of_list [1;2;3;4;5] in 
    let s2 = IntTree.singleton 1 in
    let s3 = IntTree.singleton 9 in
    let _  = Alcotest.(check (list int)) "overlapping"   [2;3;4;5]   (List.rev @@ IntTree.to_list @@ IntTree.diff s1 s2) in
    let _  = Alcotest.(check (list int)) "left empty"    [1;2;3;4;5] (List.rev @@ IntTree.to_list @@ IntTree.diff s1 IntTree.empty) in
    let _  = Alcotest.(check (list int)) "right empty"   []          (List.rev @@ IntTree.to_list @@ IntTree.diff IntTree.empty s1) in
    let _  = Alcotest.(check (list int)) "left disjoint" [9]         (List.rev @@ IntTree.to_list @@ IntTree.diff s3 s1) in
             Alcotest.(check (list int)) "right disjoint"[1;2;3;4;5] (List.rev @@ IntTree.to_list @@ IntTree.diff s1 s3)
;;

let set_union _cx = 
    let s1 = IntTree.of_list [1;2;3;4;5] in 
    let s2 = IntTree.singleton 1 in
    let s3 = IntTree.singleton 9 in
    let _  = Alcotest.(check (list int)) "overlapping" [1;2;3;4;5]   (List.rev @@ IntTree.to_list @@ IntTree.union s1 s2) in
    let _  = Alcotest.(check (list int)) "left empty"  [1;2;3;4;5]   (List.rev @@ IntTree.to_list @@ IntTree.union s1 IntTree.empty) in
    let _  = Alcotest.(check (list int)) "right empty" [1;2;3;4;5]   (List.rev @@ IntTree.to_list @@ IntTree.union IntTree.empty s1) in
    let _  = Alcotest.(check (list int)) "left apply"  [1;2;3;4;5;9] (List.rev @@ IntTree.to_list @@ IntTree.union s3 s1) in
    Alcotest.(check (list int))          "right apply" [1;2;3;4;5;9] (List.rev @@ IntTree.to_list @@ IntTree.union s1 s3)
;;



let fold_sum _cx =
    Alcotest.(check int) "fold accumulates every element"
        15 (IntTree.fold (+) (IntTree.of_list [1;2;3;4;5]) 0)
;;

let filter_test _cx =
    let evens = IntTree.filter (fun x -> x mod 2 = 0) (IntTree.of_list [1;2;3;4;5;6]) in
    Alcotest.(check (list int)) "only even elements retained"
        [2;4;6] (List.rev @@ IntTree.to_list evens)
;;

let exists_test _cx =
    let s = IntTree.of_list [1;2;3;4;5] in
    let _ = Alcotest.(check bool) "matching element exists"      true  (IntTree.exists (fun x -> x = 3) s) in
            Alcotest.(check bool) "no matching element"          false (IntTree.exists (fun x -> x = 99) s)
;;

let for_all_test _cx =
    let s = IntTree.of_list [2;4;6;8] in
    let _ = Alcotest.(check bool) "all satisfy predicate"  true  (IntTree.for_all (fun x -> x mod 2 = 0) s) in
            Alcotest.(check bool) "not all satisfy"        false (IntTree.for_all (fun x -> x > 4) s)
;;

let search_test _cx =
    let s = IntTree.of_list [10;5;8;6;3;2;9;1] in
    let _ = Alcotest.(check int) "search finds present element"
        7 (IntTree.search (fun v -> Int.compare 7 v) (IntTree.add 7 s)) in
    Alcotest.check_raises "search raises on absent element" Not_found
        (fun () -> ignore (IntTree.search (fun v -> Int.compare 42 v) s))
;;

let find_first_test _cx =
    let s = IntTree.of_list [1;2;3;4;5] in
    Alcotest.(check int) "first element above 3" 4 (IntTree.find_first (fun x -> x > 3) s)
;;

let take_min_max _cx =
    let s = IntTree.of_list [10;5;8;6;3;2;9;1] in
    let (mn, rest) = IntTree.take_min s in
    let _ = Alcotest.(check int)  "take_min returns the smallest" 1 mn in
    let _ = Alcotest.(check bool) "min no longer present"  false (IntTree.mem 1 rest) in
    let (mx, _)    = IntTree.take_max_opt s in
    Alcotest.(check (option int)) "take_max_opt returns the largest" (Some 10) mx
;;

let invert_roundtrip _cx =
    let s = IntTree.of_list [10;5;8;6;3;2;9;1] in
    Alcotest.(check (list int)) "double inversion is identity"
        (List.rev @@ IntTree.to_list s)
        (List.rev @@ IntTree.to_list (IntTree.invert (IntTree.invert s)))
;;

let seq_permutation _cx =
    let s = IntTree.of_list [10;5;8;6;3;2;9;1] in
    Alcotest.(check (list int)) "to_seq yields all elements"
        [1;2;3;5;6;8;9;10] (List.sort compare (List.of_seq (IntTree.to_seq s)))
;;

let of_seq_roundtrip _cx =
    let s = IntTree.of_seq (List.to_seq [3;1;2;5;4]) in
    Alcotest.(check (list int)) "of_seq builds the same set"
        [1;2;3;4;5] (List.rev @@ IntTree.to_list s)
;;

let remove_absent _cx =
    let s = IntTree.of_list [1;2;3] in
    Alcotest.(check (list int)) "removing an absent element is a no-op"
        [1;2;3] (List.rev @@ IntTree.to_list (IntTree.remove 99 s))
;;

let duplicate_insert _cx =
    Alcotest.(check int) "duplicates collapse to one element"
        3 (IntTree.cardinal (IntTree.of_list [1;2;3;2;1;3;1]))
;;

let preservation =
  QCheck.Test.make ~count:1000 ~name:"unique_element_membership"
    QCheck.(list nat_small)
    (fun l -> 
        let intset = IntTree.of_list l in
        List.for_all (fun x -> IntTree.mem x intset) l )
;;

let rec ascending = function
  | x::y::l -> x <= y && ascending (y::l)
  | _ -> true
;;

let sorted =
  QCheck.Test.make ~count:1000 ~name:"inorder_traversal_sorted"
    QCheck.(list nat_small)
    (fun l -> 
        (* Preconditions edge
        QCheck.assume (l <> []);*)
        let intset = List.rev @@ IntTree.to_list @@ IntTree.of_list l in
        ascending intset)
;;

let suite =
    List.map QCheck_alcotest.to_alcotest
      [ preservation; sorted ]
;;

let () =
    (* For json output 
       Alcotest.run ~argv:[|"ignored"; "--json"; "--verbose"|] "Treeset" [
    *)
    Alcotest.run "Treeset" [
        "structural", [
          test_case "addition"       `Quick simple_tree_add;
          test_case "removal"        `Quick simple_tree_remove;
          test_case "membership"     `Quick simple_tree_member;
          test_case "minimum"        `Quick simple_min_elt;
          test_case "maximum"        `Quick simple_max_elt;
        ];
        "traversal", [
            test_case "traversal"    `Quick simple_inorder_traversal
        ];
        "set operations", [
            test_case "subset"       `Quick subset_test;
            test_case "intersection" `Quick set_intersection;
            test_case "difference"   `Quick set_difference;
            test_case "union"        `Quick set_union;
        ];
        "queries", [
            test_case "fold"           `Quick fold_sum;
            test_case "filter"         `Quick filter_test;
            test_case "exists"         `Quick exists_test;
            test_case "for_all"        `Quick for_all_test;
            test_case "search"         `Quick search_test;
            test_case "find_first"     `Quick find_first_test;
        ];
        "manipulation", [
            test_case "take_min_max"   `Quick take_min_max;
            test_case "invert"         `Quick invert_roundtrip;
            test_case "to_seq"         `Quick seq_permutation;
            test_case "of_seq"         `Quick of_seq_roundtrip;
            test_case "remove_absent"  `Quick remove_absent;
            test_case "duplicates"     `Quick duplicate_insert;
        ];
        "set properties", suite
    ]


(* ============================ ounit style ================================ *)
(*let tests = "test suite for treeset " >::: [*)
  (*"addition"     >:: (simple_tree_add);*)
  (*"removal "     >:: (simple_tree_remove);*)
  (*"membership"   >:: (simple_tree_member);*)
  (*"minimum"      >:: (simple_min_elt);*)
  (*"maximum"      >:: (simple_max_elt);*)
  (*"inorder"      >:: (simple_inorder_traversal);*)
  (*"subset ops"   >:: (subset_test);*)
  (*"intersection" >:: (set_intersection);*)
  (*"difference"   >:: (set_difference);*)
(*]*)
(*let props = List.map QCheck_ounit.to_ounit_test [passing; failing]*)
(*let _ = run_test_tt_main (tests @ props)*)
