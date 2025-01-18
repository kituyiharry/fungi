open Fungi ;;
open Alcotest;;

module IntTree = Treeset.TreeSet(Int);;

let simple_tree_add _cx =
    Alcotest.(check bool) "must account for all inserted elements" 
        true ((IntTree.cardinal (IntTree.singleton 0)) = 1)
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
        [1;2;3;5;6;8;9;10] (IntTree.inorder [] @@ IntTree.of_list [10;5;8;6;3;2;9;1])
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
    let _  = Alcotest.(check (list int)) "overlapping"   [2;3;4;5]   (IntTree.to_list @@ IntTree.diff s1 s2) in
    let _  = Alcotest.(check (list int)) "left empty"    [1;2;3;4;5] (IntTree.to_list @@ IntTree.diff s1 IntTree.empty) in
    let _  = Alcotest.(check (list int)) "right empty"   []          (IntTree.to_list @@ IntTree.diff IntTree.empty s1) in
    let _  = Alcotest.(check (list int)) "left disjoint" [9]         (IntTree.to_list @@ IntTree.diff s3 s1) in
             Alcotest.(check (list int)) "right disjoint"[1;2;3;4;5] (IntTree.to_list @@ IntTree.diff s1 s3)
;;


let passing =
  QCheck.Test.make ~count:1000
    ~name:"list_rev_is_involutive"
    QCheck.(list small_nat)
    (fun l -> List.rev (List.rev l) = l);;

let suite =
    List.map QCheck_alcotest.to_alcotest
      [ passing;]
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
