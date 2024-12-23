(******************************************************************************
*     Simplest Functional Directed Graph Implementation (Adjacency list)      *
*                                                                             *
* https://www.lri.fr/~filliatr/ftp/publis/ocamlgraph-tfp-8.pdf                *
* https://bryangarza.github.io/blog/basic-graph-traversal-in-ocaml            *
* https://gist.github.com/theawesomestllama/7d1c0961a2c4446ef40b              *
* https://github.com/backtracking/ocamlgraph                                  *
*                                                                             *
* TODO: use Weak module instead of label-key duplication                      *
*                                                                             *
*******************************************************************************)
open Myset;;

let (let*) = Option.bind

module type SccImpl = sig 

    type elt
    type adj

    type sccnode = {
        link: int;
        node: elt;
        indx: int
    };;

    module NodeMap: Map.S    with type key := elt
    module AdjSet: TSet      with type t   := elt

    module SccTbl: Hashtbl.S with type key := sccnode
    module SccSet: TSet      with type t   := sccnode
    module SccMap: Map.S     with type key := int

    type solution =  {
        disc: sccnode SccSet.set;
        onst: elt     AdjSet.set;
        stck: sccnode list;
        sccs: elt     SccTbl.t;
        time: int;
    }

    type sccedge = (int list * adj NodeMap.t)

    val subgraphs: adj NodeMap.t -> elt SccTbl.t -> (sccedge) SccMap.t
    val tarjan:    adj NodeMap.t -> solution
    val kosaraju:  adj NodeMap.t -> solution
end

module type ClusterImpl = sig 
    type elt
    type adj
    module NodeMap: Map.S    with type key := elt
    module AdjSet: TSet      with type t   := elt

    val bronkerbosch: adj NodeMap.t -> elt AdjSet.set list
end

module type PathImpl = sig
    type elt
    type adj
    module NodeMap: Map.S    with type key := elt
    module AdjSet: TSet      with type t   := elt
end

module type SpanImpl = sig 
    type elt
    type adj
    module NodeMap: Map.S    with type key := elt
    module AdjSet: TSet      with type t   := elt
end


module type Graph = sig

    type +'a t
    type elt

    module AdjSet:  TSet with type t := elt
    type adj     := (elt AdjSet.set * elt AdjSet.set * elt)

    module Vertex:  Set.OrderedType with type t   := adj
    module NodeMap: Map.S           with type key := elt

    module Scc: SccImpl with
        type       elt     := elt
        and type   adj     := adj
        and module NodeMap := NodeMap
        and module AdjSet  := AdjSet

    module Cluster: ClusterImpl with
        type       elt     := elt
        and type   adj     := adj
        and module NodeMap := NodeMap
        and module AdjSet  := AdjSet

    module Path: PathImpl with
        type       elt     := elt
        and type   adj     := adj
        and module NodeMap := NodeMap
        and module AdjSet  := AdjSet

    module Span: SpanImpl with
        type       elt     := elt
        and type   adj     := adj
        and module NodeMap := NodeMap
        and module AdjSet  := AdjSet

    val empty:       adj NodeMap.t
    val equal:       elt -> elt -> bool
    val add:         elt -> adj NodeMap.t -> adj NodeMap.t
    val add_edge:    elt -> elt -> adj NodeMap.t -> adj NodeMap.t
    val add_all:     elt -> elt list -> adj NodeMap.t -> adj NodeMap.t
    val of_list:     (elt * elt list) list -> adj NodeMap.t -> adj NodeMap.t
    val incomingof:  elt -> adj NodeMap.t -> (elt AdjSet.set)
    val outgoingof:  elt -> adj NodeMap.t -> (elt AdjSet.set)
    val neighbours:  elt -> adj NodeMap.t -> elt AdjSet.set 
    val xorneighbors:elt -> adj NodeMap.t -> elt AdjSet.set 
    val mutuals:     elt -> adj NodeMap.t -> elt AdjSet.set 
    val xormutuals:  elt -> adj NodeMap.t -> elt AdjSet.set 
    val degree:      elt -> adj NodeMap.t -> int
    val remove:      elt -> adj NodeMap.t -> adj NodeMap.t
    val cull:        adj NodeMap.t -> adj NodeMap.t
    val bfs:         (elt -> elt AdjSet.set -> 'a -> bool * 'a) -> (elt -> elt AdjSet.set -> 'a -> 'a) -> adj NodeMap.t -> elt -> 'a -> 'a
    val dfs:         (elt -> elt AdjSet.set -> 'a -> bool * 'a) -> (elt -> elt AdjSet.set -> 'a -> 'a) -> adj NodeMap.t -> elt -> 'a -> 'a
    val adj_list_of: elt -> adj NodeMap.t -> elt list
    val transpose:   adj NodeMap.t -> adj NodeMap.t
end

(* TODO: To accept weighted edges, use a data record `Graph with type data :='a ` *)
module MakeGraph(Unique: Set.OrderedType): Graph with type elt := Unique.t = struct

    type elt = Unique.t

    (** Module for manipulating the Set structure holding the Adjacency list *)
    module AdjSet  = TreeSet(Unique)
    type   adj     = (elt AdjSet.set * elt AdjSet.set * elt)

    (*  Incoming nodes  Outgoing nodes data *)
    (* TODO: To accept weighted edges, use a data record Vertex{data:'a;elt:elt} *)
    module Vertex  = struct 
        type t      = adj
        let compare = fun (_, _, lnode) (_, _, rnode) -> Unique.compare lnode rnode
        let empty lbl = (AdjSet.empty, AdjSet.empty, lbl)
    end

    (** compare 2 nodes in the graph *)
    let equal lnode rnode = (Unique.compare lnode rnode) = 0

    (** Adjacency list graph definition **)
    (* Map from NodeType.t to (incoming outgoing label) *)
    type 'a t      = (Vertex.t) Map.Make(Unique).t

    (** Module for manipulating the Map (Node -> (set , set , label)) *)
    module NodeMap = Map.Make(Unique) 

    (** An empty graph **)
    let empty      = NodeMap.empty

    (** Add a new node with its label -> ( ... , nodedata) *)
    let add nodekey nodeMap =
        NodeMap.add nodekey (Vertex.empty nodekey) nodeMap
    ;;

    (**
    Add a directed edge [(tail)] --> [(head)] such that the tails outgoing
    set points to the heads incoming set.

    (*Find the tail of the directed edge*)
      (*Update with outgoing*)
        (*Find the head of the directed edge*)
          (*Update with incoming*)
    *)
    let add_edge nodeFrom nodeTo nodeMap =
        (NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label) = x in 
            Some (fromIncoming, (AdjSet.add nodeTo fromOutgoing), label)) nodeMap)
        |>  NodeMap.update nodeTo (fun x -> let* (toIncoming, toOutgoing, tolabel) = x in
            Some ((AdjSet.add nodeFrom toIncoming), (toOutgoing), tolabel))
    ;;

    let rec add_all nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | nodeTo :: rest -> add_edge nodeFrom nodeTo (add_all nodeFrom rest nodeMap)
    ;;

    (* Creates a graph given a node and outgoing edge, incoming edges will be
       resolved naturally. All nodes should already be available in the map  *)
    let rec of_list adjList nodeMap = match adjList with
        | [] -> nodeMap
        | (nodeFrom, nodeJoinList) :: rest ->
            add_all nodeFrom nodeJoinList (of_list rest nodeMap)
    ;;

    (** [ incomingof identity (Graph.t) AdjSet.t]
    Incoming set of nodes *)
    let incomingof node game = let (inc, _, _) = NodeMap.find node game in inc

    (** [ incomingof identity (Graph.t) AdjSet.t]
    Outgoing set of nodes *)
    let outgoingof node game = let (_, out, _) = NodeMap.find node game in out

    (* both incoming and outgoing edges of a graph - self edges included *)
    let neighbours node game = let (inc, out, _) = NodeMap.find node game in (AdjSet.union inc out)

    (* both incoming and outgoing edges of a graph - self edges NOT included *)
    let xorneighbors node game = let (inc, out, _) = NodeMap.find node game in 
        AdjSet.remove node (AdjSet.union inc out)

    (* items in both incoming and outgoing edges of a graph - self edges included *)
    let mutuals node game = let (inc, out, _) = NodeMap.find node game in
        (AdjSet.inter inc out)
    ;;

    (* items in both incoming and outgoing edges of a graph - self edges NOT included *)
    let xormutuals node game = let (inc, out, _) = NodeMap.find node game in
        AdjSet.remove node (AdjSet.inter inc out)
    ;;

    (*  *)
    let degree node nodeMap = 
        AdjSet.cardinal @@ neighbours node nodeMap
    ;;

    (** Removes a node from the graph *)
    let remove delnode nodeMap =
        let (incoming, outgoing, _label) = (NodeMap.find delnode nodeMap) in
        NodeMap.remove delnode (AdjSet.fold (fun nodelabel updatemap ->
            NodeMap.update nodelabel (fun x -> 
                let* (deepinc, deepout, deeplabel) = x in
                Some (AdjSet.remove delnode deepinc, AdjSet.remove delnode deepout, deeplabel)
            ) updatemap
        ) (AdjSet.union incoming outgoing) nodeMap)
    ;;

    (* Remove self edges from a graph - TODO: could be more efficient *)
    let cull graph =
        graph
        |> NodeMap.to_seq
        |> Seq.filter (fun (elt, (_, out, _)) -> AdjSet.mem elt out)
        |> Seq.fold_left (fun g (elt, (inc, out, lbl)) -> 
            NodeMap.update elt (fun _ ->
                Some (AdjSet.remove elt inc, AdjSet.remove elt out, lbl)
            ) g
        ) graph
    ;;

    (*breadth first search starting from start node applying f until returns true*)
    let bfs f b game start init =
        let que     = Queue.create () in
        let _       = Queue.add start que in
        let visited = AdjSet.empty in
        let rec iter vis nxt acc =
            if Queue.is_empty nxt then
                (vis, acc)
            else
                let label = Queue.take nxt in
                let (stop, acc') = f label vis acc in
                let (vis', acc'') = if stop then
                    (vis, acc')
                else
                    let out =  outgoingof label game in
                    let diff = AdjSet.diff out vis in
                    let _   = AdjSet.iter_inorder (fun x -> Queue.add x nxt) (diff) in
                    iter (AdjSet.union diff vis) nxt acc'
                in
                (vis', b label vis' acc'')
        in let (_, acc) = iter visited que init in 
        acc
    ;;

    (*depth first search starting from start node applying f until returns true*)
    let dfs f b game start init =
        let stck    = Stack.create () in
        let _       = Stack.push start stck in
        let visited = AdjSet.empty in
        let rec iter vis nxt acc =
            if Stack.is_empty stck then
                (vis, acc)
            else
                let label = Stack.pop nxt in
                let (stop, acc') = f label vis acc in
                let (vis', acc'') = (
                    if stop then
                        (vis, acc')
                    else
                    if AdjSet.mem label vis then
                        iter (vis) nxt acc'
                    else
                        let out = outgoingof label game in
                        let _   = AdjSet.iter_inorder (fun x -> Stack.push x nxt) (out) in
                        iter (AdjSet.add label vis) nxt acc'
                ) in (vis', b label vis' acc'')
        in
        let (_, acc) = iter visited stck init in
        acc
    ;;

    (* Get adjacency list of a node *)
    let adj_list_of node nodeMap =
        let (incoming, outgoing, _label) = NodeMap.find node nodeMap in
        AdjSet.fold (
            fun anode alist -> anode :: alist
        ) (AdjSet.union incoming outgoing) []
    ;;

    let transpose nodeMap =
        NodeMap.map (fun (inc, out, label) -> (out, inc, label)) nodeMap
    ;;

    (*************************************************************************
    *                    Strongly connected Components                       *
    *                  Every vertex is reachable in a SCC                    *
    **************************************************************************)

    module Scc = struct

        type sccnode = { link: int; node: elt; indx: int };;

        module SccNode = struct
            type t      = sccnode
            let compare = fun x y -> (Unique.compare x.node y.node)
            let equal   = fun {link=left;_} {link=right;_} -> (Int.compare left right) = 0
            let hash    = fun x   -> x.link
        end

        module SccTbl: Hashtbl.S with type key := sccnode = Hashtbl.Make(SccNode)
        module SccSet: TSet      with type t   := sccnode = TreeSet(SccNode)
        module SccMap: Map.S     with type key := int     = Map.Make(Int)

        let hasnode e x = equal x.node e
        ;;

        (* creates a Map of ints -> ([], Graph.t) where the int is the link value.
           it computes its neighbours into the list section of the Map value
        *)
        let subgraphs nodeMap sccs =
            SccTbl.fold (fun {link=lowlink;_} elt acc -> 
                let edges = NodeMap.find elt nodeMap in
                let (_, out, _) = edges in
                let keyseq = SccTbl.to_seq_keys sccs in
                let sccedg = (AdjSet.fold (fun e ac ->
                    match (keyseq) |> Seq.find (hasnode e) with
                    | Some v -> if v.link != lowlink then  v.link :: ac else ac
                    | None   -> ac
                ) out []) in
                SccMap.update lowlink (fun nodeEl -> match nodeEl with
                    | Some (l, v) -> Some ((l @ sccedg), (NodeMap.add elt (edges) v))
                    | None        -> Some (sccedg,   NodeMap.singleton elt (edges))
                ) acc) sccs SccMap.empty 
            |> SccMap.map (fun (v, m) -> (List.sort_uniq (Int.compare) v, m))
        ;;

        (* save some memory reserves when creating the SccState *)
        let buckets size =
            int_of_float (ceil (float_of_int(size) /. 2.))
        ;;

        type solution =
            {
                disc: sccnode SccSet.set;
                onst: elt     AdjSet.set;
                stck: sccnode list;
                sccs: elt     SccTbl.t;
                time: int;
            }
        ;;

        type sccedge = (int list * adj NodeMap.t)
        ;;

        let empty (size: int) =
            {
                disc = SccSet.empty;
                onst = AdjSet.empty;
                stck = [];
                sccs = SccTbl.create (buckets (size));
                time = 0
            }
        ;;

        let whereis edg tarj = SccSet.find_first (hasnode edg) tarj.disc
        ;;

        let findby pred dset = Option.is_some (SccSet.find_first_opt (pred) dset)
        ;;

        (**  visit function graph  root-node  neighbour-node solution*)
        let visit apply g root visited tarj =
            if not (findby (hasnode visited) tarj.disc) then
                (* unvisited edge - recurse and update *)
                let tarj' = apply   visited g tarj  in
                let uss'  = whereis visited   tarj' in
                let ngbr  = whereis root      tarj' in
                { tarj'
                    with disc = (SccSet.add ({
                        ngbr with link = (min ngbr.link uss'.link)
                    }) tarj'.disc)
                }
            else if (AdjSet.mem visited tarj.onst) then
                (* the visited is on the stack *)
                (* update roots lowlink to visiteds discovery time index *)
                let ngbr = whereis (root)    tarj in
                let usss = whereis (visited) tarj in
                { tarj
                    with disc = (SccSet.add ({
                        ngbr with link = (min ngbr.link usss.indx)
                    }) tarj.disc);
                }
            else
                tarj
        ;;

        (* How Tarjan builds an SCC until pred is met *)
        let rec popscc pred tarj id =
            match tarj.stck with
            | sccel :: rest ->
                let tarj' = {
                    tarj with stck = rest;
                    onst = AdjSet.remove (sccel.node) tarj.onst;
                } in
                    let x = { sccel with link=id } in
                    let _ = SccTbl.add tarj'.sccs x x.node in
                    if pred sccel then
                        tarj'
                    else
                        popscc pred tarj' id
            | [] ->
                tarj
        ;;

        let mksccnode n time = {node=n;link=time;indx=time}
        ;;


        (* how tarjan creates a link for later use *)
        let tarjanadd n s =
            {
                s with
                disc = (SccSet.add n      s.disc);
                onst = (AdjSet.add n.node s.onst);
                stck = (n :: s.stck);
                time = s.time + 1
            }
        ;;

        (** Tarjans SCC algorithm 
        basically we should pop until we find our own
        low-link when we started (lowlink = index) - this shows that it
        is an SCC on backtrack - otherwise we keep pushing on to the stack
        The stack is there to maintain an invariance over the visited nodes
        during Depth first search

        In the end remaining elements in the Stack are their own SCCs this way
        All elements always belong to an SCC
        *)
        let tarjan graph =
            let count = ref 0 in
            let rec strongconnect n g s =
                (* mark as discovered and add onto the stack *)
                let r     = (tarjanadd (mksccnode n s.time) s) in
                let out   = (outgoingof     n  g) in
                (* recurse while adding *)
                let s'    = AdjSet.fold (visit (strongconnect) g n) (out) r in
                (* get the low link of this node and compare to discovery time *)
                let ngbr' = (whereis (n) s') in
                if  ngbr'.link = ngbr'.indx then
                    let _ = incr count in
                    (* pop elements into an scc until the elt is the same as ours *)
                    popscc (hasnode n) s' !count
                else
                    s'
            in
            NodeMap.fold (fun elt _ acc ->
                if findby (hasnode elt) acc.disc then acc else (strongconnect elt graph acc)
            ) graph (empty (NodeMap.cardinal graph))
        ;;

        (* How Kosaraju builds an scc within the f (el) constraint *)
        let rec popwhile f t id =
            match t.stck with
            | el :: rest ->
                if f el then
                    let _ = SccTbl.add t.sccs {el with link=id} el.node in
                    let t' = { t with
                        onst = AdjSet.add el.node t.onst; 
                        stck = rest;
                    } in
                    popwhile f t' id
                else
                    t
            | [] -> t
        ;;

        (* how kosaraju composes a solution for later use *)
        let kosaradd elt scc scc' = 
            (* if already on the stack then already discovered again *)
            if AdjSet.mem elt scc'.onst then
                scc'
            else
                (* not on the stack after recursion, stack and mark discovered *)
                {
                    scc'  with
                    stck = (mksccnode elt scc.time) :: scc.stck;
                    onst = AdjSet.add elt scc.onst;
                }
        ;;

        (* Kosaraju visits all nodes until the first nodes with all edges going
           back into the discovered or has none and builds a stack. After that 
           we transpose the graph and pop from the stack, seeing which elements
           in the stack are still reachable on pop *)
        let kosaraju graph =
            let count  = ref 0 in
            let rec iter node (_, out, _) scc =
                let scc'' = 
                    (* If already on the stack it has been discovered *)
                    if AdjSet.mem node scc.onst then scc else
                        (* If all out edges are a subset of the discovered elements *)
                        if AdjSet.for_all (fun el -> findby (hasnode el) scc.disc) (out) then
                            (* stack it and move on *)
                            {
                                scc with
                                stck = (mksccnode node scc.time) :: scc.stck;
                                onst = AdjSet.add node scc.onst;
                            }
                        else
                            (* Mark this node as discovered at this time *)
                            let nvst = { scc with disc = SccSet.add (mksccnode node scc.time) scc.disc } in
                            (* Get the subset of undiscovered elements for recurse *)
                            let diff = AdjSet.filter (fun oe -> not (findby (hasnode oe) nvst.disc)) out in
                            AdjSet.fold (fun elt state -> 
                                iter elt (NodeMap.find elt graph) state
                                |> kosaradd elt scc
                            ) (diff) (nvst)
                in
                (* If already on the stack after first recursion then ok *)
                kosaradd node scc'' scc''
            in
            let fscc = NodeMap.fold (iter) graph (empty (NodeMap.cardinal graph)) in 
            (* transpose the graph *)
            let tgraph = transpose graph in
            (* pop elements while a condition is true *)
            let iter2 scc sccnode =
                (* if onst the we consider it already a part of an scc *)
                if AdjSet.mem sccnode.node scc.onst then
                    scc
                else
                    let _ = incr count in
                    (* find all reachable nodes *)
                    let vstd = dfs
                        (fun _ _ _ -> (false, AdjSet.empty)) 
                        (fun _ vis _ -> vis)
                        tgraph sccnode.node (AdjSet.empty) in
                    (* popelements into an scc while they are visited *)
                    popwhile (fun e -> AdjSet.mem e.node vstd) scc (!count)
            in List.fold_left iter2 {fscc with onst = AdjSet.empty} (fscc.stck)
        ;;

    end

    (*************************************************************************
    *                           Clusters                                     *
    * Clique: Induced subgraph is fully connected every pair of distinct     *
    * vertices is* adjacent with distance 1                                  *
    *                                                                        *
    * Club: A generalization of a clique, where a set of vertices induces a  *
    * subgraph with a diameter at most (s)                                   *
    *                                                                        *
    * Clan: A subclass of an (n)-clique with diameter (n), which makes       *
    * it connected                                                           *
    **************************************************************************)
    module Cluster = struct 
        (*
            Bron–Kerbosch algorithm  (Maximal Cliques)
            Warning: Self edges will cause p to run into an infinite loop so we
            have to filter them out with `xormutuals`
        *)
        let bronkerbosch graph =
            (*
                r: clique being built
                p: candidate vertices
                x: exclusion set (already processed) - ensures r is maximal
            *)
            let rec bk r p x cqs =
                if AdjSet.is_empty p && AdjSet.is_empty x then
                    (r :: cqs)
                else
                    let _, _, ncqs =
                        AdjSet.fold (fun v (np, nx, cqs') -> 
                            let ngb = (xormutuals v graph) in
                            (* r + {v} *)
                            let br  = (AdjSet.add v r) in
                            (* p intersect ngb *)
                            let bp  = (AdjSet.inter ngb np) in
                            (* x intersect ngb *)
                            let bx  = (AdjSet.inter ngb nx) in
                            (* any new cliques *)
                            let cqs'' = bk (br) (bp) (bx) cqs' in
                            ((AdjSet.remove v np), (AdjSet.add v nx), cqs'')
                        ) (p) (p, x, cqs)
                    in ncqs
            in
            (* collect all nodes as candidates *)
            let keys = NodeMap.to_seq graph |> Seq.map (fst) |> AdjSet.of_seq in
            bk (AdjSet.empty) (keys) (AdjSet.empty) []
        ;;
    end

    (*************************************************************************
    *                             Path Algos                                 *
    **************************************************************************)
    module Path = struct 
    (*
        Floyd warshall
        Bellman ford
        Djikstra
        Astar
    *)
    end

    (* Ford-Fulkerson (flow) *)

    (*************************************************************************
    *                           Spanning Trees                               *
    **************************************************************************)
    module Span = struct
    (**
      kruskal  
    *)
    end

end;;
