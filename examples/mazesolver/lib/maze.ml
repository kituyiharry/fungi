(******************************************************************************
*                                                                             *
*                     Maze solving on the fungi graph library                 *
*                                                                             *
*   A maze is a character grid:                                               *
*     '#'  wall            'S'  start                                         *
*     'E'  end / goal      any other char = open floor                        *
*                                                                             *
*   Each open cell becomes a graph node keyed by its (row, col) coordinate,   *
*   and every passable 4-neighbour adjacency becomes a unit-weight            *
*   undirected edge. Shortest paths are then just Dijkstra / A* on that       *
*   graph.                                                                     *
*                                                                             *
*******************************************************************************)

open Fungi

(* graph nodes are (row, col) coordinates, edges carry an int step cost *)
module Cell = struct
    type t      = int * int
    type edge   = int
    let compare (a, b) (c, d) = match Int.compare a c with
        | 0 -> Int.compare b d
        | n -> n
end

module G = Graph.MakeGraph (Cell)
module P = G.Path.Compute (Graph.Biject (Int))

type t = {
    rows  : string array;  (* the raw grid *)
    rowsn : int;           (* number of rows *)
    start : int * int;     (* location of 'S' *)
    goal  : int * int;     (* location of 'E' *)
}

let is_wall = function '#' -> true | _ -> false

(* char at (r, c), treating anything off-grid as a wall *)
let char_at rows r c =
    if r < 0 || r >= Array.length rows then '#'
    else if c < 0 || c >= String.length rows.(r) then '#'
    else rows.(r).[c]

let is_open t (r, c) = not (is_wall (char_at t.rows r c))

(** [parse lines] builds a maze, locating the single 'S' and 'E' markers.
    @raise Invalid_argument if either marker is missing. *)
let parse lines =
    let rows = Array.of_list lines in
    let find ch =
        let res = ref None in
        Array.iteri (fun r s ->
            String.iteri (fun c x -> if x = ch then res := Some (r, c)) s) rows;
        !res
    in
    match find 'S', find 'E' with
    | Some start, Some goal -> { rows; rowsn = Array.length rows; start; goal }
    | None, _ -> invalid_arg "Maze.parse: no start marker 'S'"
    | _, None -> invalid_arg "Maze.parse: no goal marker 'E'"

(* the four orthogonal neighbours of a cell *)
let neighbours (r, c) = [ (r - 1, c); (r + 1, c); (r, c - 1); (r, c + 1) ]

(** build the grid graph: every open cell is a node linked to each open
    4-neighbour by a unit-weight undirected edge *)
let to_graph t =
    let g = ref G.empty in
    let each_open f =
        for r = 0 to t.rowsn - 1 do
            for c = 0 to String.length t.rows.(r) - 1 do
                if is_open t (r, c) then f (r, c)
            done
        done
    in
    each_open (fun cell -> g := G.add cell !g);
    each_open (fun cell ->
        List.iter (fun n -> if is_open t n then g := G.add_weight2 1 cell n !g)
            (neighbours cell));
    !g

(** [solve t] returns the shortest path from 'S' to 'E' as an ordered list of
    cells (inclusive), or [None] if the goal is unreachable.

    Dijkstra on a unit-cost grid gives an optimal path (equivalent to BFS here).

    NB: the library also exposes [P.astar], but it currently stores [g + h] in a
    single path field and reuses it as [g] when relaxing, so with a non-trivial
    heuristic it can return non-optimal / broken paths. Until that is fixed
    (separating the path cost from the ordering key) we use Dijkstra, which is
    plenty fast for mazes. *)
let solve t =
    let g = to_graph t in
    let path = P.dijkstra t.start t.goal g in
    (* dijkstra reports the goal even when it is unreachable, but with an
       infinite cost; only a finite cost means a real path exists *)
    match List.assoc_opt t.goal path with
    | Some (`Val _) -> Some (List.map fst path)
    | _             -> None

(** number of steps in a path (edges traversed) *)
let steps = function [] -> 0 | path -> List.length path - 1

(** overlay a path onto the grid with '*', preserving the 'S' and 'E' markers *)
let render t path =
    let grid = Array.map Bytes.of_string t.rows in
    List.iter (fun (r, c) ->
        if (r, c) <> t.start && (r, c) <> t.goal
        && r >= 0 && r < Array.length grid && c >= 0 && c < Bytes.length grid.(r)
        then Bytes.set grid.(r) c '*'
    ) path;
    Array.to_list grid |> List.map Bytes.to_string |> String.concat "\n"
