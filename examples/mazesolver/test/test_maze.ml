open Mazesolver
open Alcotest

(* a hand-verified maze whose shortest S->E path is 12 steps *)
let solvable = [
    "#########";
    "#S#     #";
    "# # ### #";
    "#   #   #";
    "### ### #";
    "#     # #";
    "# ### # #";
    "#   #  E#";
    "#########";
]

(* E is walled off from S *)
let unsolvable = [
    "#####";
    "#S# #";
    "# #E#";
    "#####";
]

let adjacent (r1, c1) (r2, c2) = abs (r1 - r2) + abs (c1 - c2) = 1

let valid_path _cx =
    let m = Maze.parse solvable in
    match Maze.solve m with
    | None -> fail "expected a path"
    | Some path ->
        let arr = Array.of_list path in
        check (pair int int) "starts at S" m.Maze.start arr.(0);
        check (pair int int) "ends at E"   m.Maze.goal  arr.(Array.length arr - 1);
        check bool "every cell is open" true (Array.for_all (Maze.is_open m) arr);
        let steps_ok = ref true in
        for i = 0 to Array.length arr - 2 do
            if not (adjacent arr.(i) arr.(i + 1)) then steps_ok := false
        done;
        check bool "consecutive cells are adjacent" true !steps_ok
;;

let shortest_length _cx =
    let m = Maze.parse solvable in
    match Maze.solve m with
    | None      -> fail "expected a path"
    | Some path -> check int "shortest path is 12 steps" 12 (Maze.steps path)
;;

let no_path _cx =
    let m = Maze.parse unsolvable in
    check (option (list (pair int int))) "unreachable goal -> None" None (Maze.solve m)
;;

let () =
    run "Maze" [
        "solve", [
            test_case "valid path"      `Quick valid_path;
            test_case "shortest length" `Quick shortest_length;
            test_case "no path"         `Quick no_path;
        ];
    ]
