open Mazesolver

(* '#' walls, 'S' start, 'E' goal, spaces are open floor *)
let sample = [
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

let () =
    let maze = Maze.parse sample in
    print_endline "Maze:";
    List.iter print_endline sample;
    match Maze.solve maze with
    | None ->
        print_endline "\nNo path from S to E."
    | Some path ->
        Printf.printf "\nShortest path (%d steps) via Dijkstra:\n" (Maze.steps path);
        print_endline (Maze.render maze path)
