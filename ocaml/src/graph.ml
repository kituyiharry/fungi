(******************************************************************************
*                                                                             *
*     Simplest Functional Directed Graph Implementation (Adjacency list)      *
*                                                                             *
*******************************************************************************)
open Treeset;;
open Heap;;

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
    module AdjSet : TSet     with type t   := elt

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
    module AdjSet:  TSet     with type t   := elt

    val bronkerbosch:  adj NodeMap.t -> elt AdjSet.set list
    val bronkerbosch2: adj NodeMap.t -> elt AdjSet.set list
end

module type PathImpl = sig
    type elt
    type adj
    type 'b ctx
    type path := (elt * elt * float) list

    module NodeMap: Map.S    with type key := elt
    module AdjSet:  TSet     with type t   := elt
    module NodeHeap:TreeHeap with type elt := elt

    val walkdepthuntil:   (path ctx -> bool) -> elt -> adj NodeMap.t -> path
    val walkbreadthuntil: (path ctx -> bool) -> elt -> adj NodeMap.t -> path
    val djikstra:         elt -> elt -> adj NodeMap.t -> path
end

module type SpanImpl = sig 
    type elt
    type adj

    module NodeMap: Map.S    with type key := elt
    module AdjSet:  TSet     with type t   := elt
end

module type VertexImpl = sig 
    type elt
    type adj
    type weights

    include Set.OrderedType  with type t   := adj
    module  NodeMap: Map.S   with type key := elt

    val empty     : elt -> adj
    val weights   : elt -> adj NodeMap.t -> weights
    val edge      : elt -> elt -> adj NodeMap.t -> float
end

module type Graph = sig

    type +'a t
    type elt

    module AdjSet:  TSet      with type t   := elt
    module Weights: Hashtbl.S with type key := elt

    type 'b srchctx = { prev: elt option; elt: elt;  vis: elt AdjSet.set; acc: 'b }
    type adj := (elt AdjSet.set * elt AdjSet.set * elt * float Weights.t)

    module NodeMap: Map.S     with type key := elt

    module Vertex: VertexImpl with 
        type       elt     := elt
        and type   adj     := adj
        and type   weights := float Weights.t
        and module NodeMap := NodeMap

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

    module Span: SpanImpl with
        type       elt     := elt
        and type   adj     := adj
        and module NodeMap := NodeMap
        and module AdjSet  := AdjSet

    module Path: PathImpl with
        type       elt     := elt
        and type   adj     := adj
        and type   'b ctx  := 'b srchctx
        and module NodeMap := NodeMap
        and module AdjSet  := AdjSet

    (*TODO: sparse graphs: module Sparse*)

    val empty:       adj NodeMap.t
    val equal:       elt -> elt -> bool
    val add:         elt -> adj NodeMap.t -> adj NodeMap.t
    val add_edge:    elt -> elt -> adj NodeMap.t -> adj NodeMap.t
    val add_all:     elt -> elt list -> adj NodeMap.t -> adj NodeMap.t
    val allweighted: elt -> (elt * float) list -> adj NodeMap.t -> adj NodeMap.t
    val allweighted2:elt -> (elt * float) list -> adj NodeMap.t -> adj NodeMap.t
    val add_weight:  elt -> elt -> float -> adj NodeMap.t -> adj NodeMap.t
    val add_weight2: elt -> elt -> float -> adj NodeMap.t -> adj NodeMap.t
    val of_list:     (elt * elt list) list -> adj NodeMap.t -> adj NodeMap.t
    val of_weights:  (elt * (elt * float) list) list -> adj NodeMap.t -> adj NodeMap.t
    val of_weights2: (elt * (elt * float) list) list -> adj NodeMap.t -> adj NodeMap.t
    val incomingof:  elt -> adj NodeMap.t -> (elt AdjSet.set)
    val outgoingof:  elt -> adj NodeMap.t -> (elt AdjSet.set)
    val neighbours:  elt -> adj NodeMap.t -> elt AdjSet.set 
    val xorneighbors:elt -> adj NodeMap.t -> elt AdjSet.set 
    val mutuals:     elt -> adj NodeMap.t -> elt AdjSet.set 
    val xormutuals:  elt -> adj NodeMap.t -> elt AdjSet.set 
    val degree:      elt -> adj NodeMap.t -> int
    val remove:      elt -> adj NodeMap.t -> adj NodeMap.t
    val cull:        adj NodeMap.t -> adj NodeMap.t

    val toposort:    ('a * elt AdjSet.set * 'b * 'c) NodeMap.t -> elt list
    val bfs:         ('b srchctx -> bool * 'b) -> ('b srchctx -> 'b) -> adj NodeMap.t -> elt -> 'b -> 'b
    val dfs:         ('b srchctx -> bool * 'b) -> ('b srchctx -> 'b) -> adj NodeMap.t -> elt -> 'b -> 'b

    val adj_list_of: elt -> adj NodeMap.t -> elt list
    val transpose:   adj NodeMap.t -> adj NodeMap.t
end

module MakeGraph(Unique: Set.OrderedType): Graph with type elt := Unique.t = struct

    type elt = Unique.t

    (** compare 2 nodes in the graph *)
    let equal lnode rnode = (Unique.compare lnode rnode) = 0

    (** Weights hold edge values should they exist. elt is the `to` direction of 
        the weight, on undirected graphs it may be duplicated on both nodes
        vertex values as a to in one and a from in another *)
    module WeightNode = struct
        type t      = elt 
        let equal   = fun (left) (right) -> (equal left right)
        let hash    = fun x -> (Obj.magic x)
    end
    module Weights = Hashtbl.Make (WeightNode)

    (** Module for manipulating the Set structure holding the Adjacency list *)
    module AdjSet  = TreeSet(Unique)
    type   adj     = (elt AdjSet.set * elt AdjSet.set * elt * float Weights.t)

    (** Module for manipulating the Map (Node -> (set , set , label)) *)
    module NodeMap = Map.Make(Unique)

    (*  Incoming nodes  Outgoing nodes data *)
    module Vertex  = struct 
        type t          = adj
        let compare     = fun (_, _, lnode, _) (_, _, rnode, _) -> Unique.compare lnode rnode
        let empty lbl   = (AdjSet.empty, AdjSet.empty, lbl, Weights.create 1)
        let weights n g = let (_, _, _, w) = NodeMap.find n g in w
        let edge  f t g = Weights.find (weights f g) t
    end

    (** Adjacency list graph definition **)
    (* Map from NodeType.t to (incoming outgoing label) *)
    type +'a t     = (Vertex.t) Map.Make(Unique).t

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
        (NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label, wgts) = x in 
            Some (fromIncoming, (AdjSet.add nodeTo fromOutgoing), label, wgts)) nodeMap)
        |>  NodeMap.update nodeTo (fun x -> let* (toIncoming, toOutgoing,
            tolabel, wgts) = x in
            Some ((AdjSet.add nodeFrom toIncoming), (toOutgoing), tolabel, wgts))
    ;;

    (*Find the tail of the directed edge*)
      (*Update with outgoing*)
        (*Find the head of the directed edge*)
          (*Update with incoming*)
    (** Add nodeFrom nodeTo with weight values on from end  *)
    let add_weight nodeFrom nodeTo weightValue nodeMap =
        (NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label, wgts) = x in 
            let _  = Weights.add wgts nodeTo weightValue in
            Some (fromIncoming, (AdjSet.add nodeTo fromOutgoing), label, wgts)) nodeMap)
        |>  NodeMap.update nodeTo (fun x -> let* (toIncoming, toOutgoing,
            tolabel, wgts) = x in
            Some ((AdjSet.add nodeFrom toIncoming), (toOutgoing), tolabel, wgts))
    ;;

    (*Find the tail of the directed edge*)
      (*Update with outgoing*)
        (*Find the head of the directed edge*)
          (*Update with incoming*)
    (** Add bidirectional nodeFrom nodeTo with weight values on both ends  *)
    let add_weight2 nodeFrom nodeTo weightValue nodeMap =
        (NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label, wgts) = x in 
            let _  = Weights.add wgts nodeTo weightValue in
            Some (fromIncoming, (AdjSet.add nodeTo fromOutgoing), label, wgts)) nodeMap)
        |>  NodeMap.update nodeTo (fun x -> let* (toIncoming, toOutgoing, tolabel, wgts) = x in
            let _  = Weights.add wgts nodeFrom weightValue in
            Some ((AdjSet.add nodeFrom toIncoming), (toOutgoing), tolabel, wgts))
    ;;

    let rec add_all nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | nodeTo :: rest -> add_edge nodeFrom nodeTo (add_all nodeFrom rest nodeMap)
    ;;

    let rec allweighted nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | (nodeTo, nodeVal) :: rest -> add_weight nodeFrom nodeTo nodeVal
            (allweighted nodeFrom rest nodeMap)
    ;;

    let rec allweighted2 nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | (nodeTo, nodeVal) :: rest -> add_weight2 nodeFrom nodeTo nodeVal
            (allweighted2 nodeFrom rest nodeMap)
    ;;

    (** Creates a graph given a node and outgoing edge, incoming edges will be
       resolved naturally. All nodes should already be available in the map  *)
    let rec of_list adjList nodeMap = match adjList with
        | [] -> nodeMap
        | (nodeFrom, nodeJoinList) :: rest ->
            add_all nodeFrom nodeJoinList (of_list rest nodeMap)
    ;;

    (** Creates a graph given a node and outgoing edge, incoming edges will be
       resolved naturally. All nodes should already be available in the map  *)
    let rec of_weights adjList nodeMap = match adjList with
        | [] -> nodeMap
        | (nodeFrom, nodeJoinList) :: rest ->
            allweighted nodeFrom nodeJoinList (of_weights rest nodeMap)
    ;;

    (** Creates a graph given a node and outgoing edge, incoming edges will be
       resolved naturally. All nodes should already be available in the map  *)
    let rec of_weights2 adjList nodeMap = match adjList with
        | [] -> nodeMap
        | (nodeFrom, nodeJoinList) :: rest ->
            allweighted2 nodeFrom nodeJoinList (of_weights2 rest nodeMap)
    ;;

    (** [ incomingof identity (Graph.t) AdjSet.t]
    Incoming set of nodes *)
    let incomingof node game = let (inc, _, _, _) = NodeMap.find node game in inc

    (** [ incomingof identity (Graph.t) AdjSet.t]
    Outgoing set of nodes *)
    let outgoingof node game = let (_, out, _, _) = NodeMap.find node game in out

    (** both incoming and outgoing edges of a graph - self edges included *)
    let neighbours node game = let (inc, out, _, _) = NodeMap.find node game in (AdjSet.union inc out)

    (** both incoming and outgoing edges of a graph - self edges NOT included *)
    let xorneighbors node game = let (inc, out, _, _) = NodeMap.find node game in 
        AdjSet.remove node (AdjSet.union inc out)

    (** items in both incoming and outgoing edges of a graph - self edges included *)
    let mutuals node game = let (inc, out, _, _) = NodeMap.find node game in
        (AdjSet.inter inc out)
    ;;

    (** items in both incoming and outgoing edges of a graph - self edges NOT included *)
    let xormutuals node game = let (inc, out, _, _) = NodeMap.find node game in
        AdjSet.remove node (AdjSet.inter inc out)
    ;;

    (** number of incoming and outgoing edges  *)
    let degree node nodeMap = 
        AdjSet.cardinal @@ neighbours node nodeMap
    ;;

    (** Removes a node from the graph - weights aren't altered and may still be
    available from an opposite end of the edge depending oon how the graph is
    structured *)
    let remove delnode nodeMap =
        let (incoming, outgoing, _label, _) = (NodeMap.find delnode nodeMap) in
        NodeMap.remove delnode (AdjSet.fold (fun nodelabel updatemap ->
            NodeMap.update nodelabel (fun x -> 
                let* (deepinc, deepout, deeplabel, wgts) = x in
                Some (AdjSet.remove delnode deepinc, AdjSet.remove delnode
                    deepout, deeplabel, wgts)
            ) updatemap
        ) (AdjSet.union incoming outgoing) nodeMap)
    ;;

    (** Remove self edges from a graph - TODO: could be more efficient - perhaps *)
    let cull graph =
        graph
        |> NodeMap.to_seq
        |> Seq.filter (fun (elt, (_, out, _, _)) -> AdjSet.mem elt out)
        |> Seq.fold_left (fun g (elt, (inc, out, lbl, wgts)) -> 
            NodeMap.update elt (fun _ ->
                Some (AdjSet.remove elt inc, AdjSet.remove elt out, lbl, wgts)
            ) g
        ) graph
    ;;

    type 'b srchctx = { prev: elt option; elt: elt;  vis: elt AdjSet.set; acc: 'b }

    (** breadth first search starting from start node applying f until returns
        true or queue is empty applying f on each node and b on backtrack *)
    let bfs f b game start init =
        let que     = Queue.create () in
        let _       = Queue.add (None, start) que in
        let visited = AdjSet.empty in
        let rec iter vis nxt acc =
            if Queue.is_empty nxt then
                (vis, acc)
            else
                let (prev, label) = Queue.take nxt in
                let (stop, acc') = f {prev=prev;elt=label; vis=vis; acc=acc} in
                let (vis', acc'') = if stop then
                    (vis, acc')
                else
                    let out =  outgoingof label game in
                    let diff = AdjSet.diff out vis in
                    let _   = AdjSet.iter (fun x -> Queue.add (Some label, x) nxt) (diff) in
                    iter (AdjSet.union diff vis) nxt acc'
                in
                (vis', b {prev=prev;elt=label; vis=vis'; acc=(acc'');})
        in let (_, acc) = iter visited que init in acc
    ;;

    (** depth first search starting from start node applying f until returns
        true applying f until returns
        true or stack is empty applying f on each node and b on backtrack *)
    let dfs f b game start init =
        let stck    = Stack.create () in
        let _       = Stack.push (None, start) stck in
        let visited = AdjSet.empty in
        let rec iter vis nxt acc =
            if Stack.is_empty stck then
                (vis, acc)
            else
                let (prev, label) = Stack.pop nxt in
                let (stop, acc') = f {prev=prev;elt=label; vis=vis; acc=acc;} in
                let (vis', acc'') = (
                    if stop then
                        (vis, acc')
                    else
                    if AdjSet.mem label vis then
                        iter (vis) nxt acc'
                    else
                        let out = outgoingof label game in
                        let _   = AdjSet.iter (fun x -> Stack.push (Some label, x) nxt) (out) in
                        iter (AdjSet.add label vis) nxt acc'
                ) in (vis', b {prev=prev; elt=label; vis=vis'; acc=acc''})
        in let (_, acc) = iter visited stck init in acc
    ;;

    (** Get adjacency list of a node *)
    let adj_list_of node nodeMap =
        let (incoming, outgoing, _label, _wgts) = NodeMap.find node nodeMap in
        AdjSet.fold (
            fun anode alist -> anode :: alist
        ) (AdjSet.union incoming outgoing) []
    ;;

    (** swap the incoming and outgoing edge direction *)
    let transpose nodeMap =
        NodeMap.map (fun (inc, out, label, wgts) -> (out, inc, label, wgts)) nodeMap
    ;;

    (** toposort *)
    let toposort nodeMap =
        snd @@ NodeMap.fold (fun x _y (v, a) -> 
            if AdjSet.mem x v then
                (v, a)
            else
                dfs (fun s -> (false, s.acc)) (fun s -> 
                    if AdjSet.mem s.elt (fst s.acc) then
                        s.acc
                    else
                        let ov, os = s.acc in
                        (AdjSet.add s.elt ov, s.elt :: os)
                ) nodeMap x (v, a) 
        ) nodeMap (AdjSet.empty, [])
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
           it computes its neighbours into the list section of the Map value. 
           This makes more idiomatic to the outer graph structure which is also
           a map but with different values
        *)
        let subgraphs nodeMap sccs =
            SccTbl.fold (fun {link=lowlink;_} elt acc -> 
                let edges = NodeMap.find elt nodeMap in
                let (_, out, _, _) = edges in
                let keyseq = SccTbl.to_seq_keys sccs in
                let sccedg = (AdjSet.fold (fun e ac ->
                    match Seq.find (hasnode e) keyseq with
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
                    tarj with 
                    stck = rest;
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
                    scc' with
                    stck = (mksccnode elt scc.time) :: scc.stck;
                    onst = AdjSet.add elt scc.onst;
                }
        ;;

        (* Kosaraju visits all nodes until the first nodes with all edges going
           back into the discovered or has none and builds a stack. After that 
           we transpose the graph and pop from the stack, seeing which elements
           in the stack are still reachable on pop 
           time values are not really as relevant for this algorithm *)
        let kosaraju graph =
            let count  = ref 0 in
            let rec iter node (_, out, _, _) scc =
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
                    let vstd = bfs
                        (fun _ -> (false, AdjSet.empty)) 
                        (fun {vis;_} -> vis) tgraph sccnode.node (AdjSet.empty)
                    in
                    (* popelements into an scc while they are visited *)
                    popwhile (fun e -> AdjSet.mem e.node vstd) scc (!count)
            in List.fold_left iter2 {fscc with onst = AdjSet.empty} fscc.stck
        ;;

    end

    (*************************************************************************
    *                           Clusters                                     *
    * Clique: Induced subgraph is fully connected every pair of distinct     *
    * vertices is* adjacent with distance 1 or k - paths outside the clique  *
    * seem to be allowed                                                     *
    *                                                                        *
    * Club: A generalization of a clique, where a set of vertices induces a  *
    * subgraph with a diameter at most (s) - the diameter must remain within *
    * the club!                                                              *
    *                                                                        *
    * Clan: A subclass of an (n)-clique with diameter (n), which makes       *
    * it connected within the clan - part of the point is whether a clique is*
    * restricted to be maximal. k-clans have to be k-cliques                 *
    **************************************************************************)
    module Cluster = struct 
        (**
            Bron–Kerbosch algorithm  (Maximal Cliques)
            Warning: Self edges can cause p to run into an infinite loop so we
            have to filter them out with `xormutuals`
            This implementation does not use pivoting, use bronkerbosch2 for
            that depending on your graph (it does extra computations)
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

        (** Same as bronkerbosch but with pivoting - can be usefull in some
           cases with a tradeoff in extra computation! *)
        let bronkerbosch2 graph =
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
                        let u = AdjSet.choose (AdjSet.union p x) in
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
                        ) (AdjSet.diff p (xormutuals u graph)) (p, x, cqs)
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
        module NodeHeap = Make_heap (Unique)

        (* All pairs depth first walk until (f ctx -> bool) is true or all nodes
           are exhausted. Requires the node edges to be weighted  Uses simple
           dfs to append edges and their corresponding weights to a list*)
        let walkdepthuntil f start graph = 
            List.rev @@ dfs (fun s -> (
                match s.prev with
                | Some prev ->
                    (f s), ((prev, s.elt, (Vertex.edge prev s.elt graph)) :: s.acc)
                | None ->
                    (f s), ((s.elt, s.elt, (0.)) :: s.acc)
            )) (fun s -> s.acc) graph start []
        ;;

        (* All pairs breadth first walk until (f ctx -> bool) is true or all nodes
           are exhausted. Requires the node edges to be weighted. Uses simple
           bfs to append edges and their corresponding weights to a list *)
        let walkbreadthuntil f start graph = 
            List.rev @@ bfs (fun s -> (
                match s.prev with
                | Some prev ->
                    (f s), ((prev, s.elt, (Vertex.edge prev s.elt graph)) :: s.acc)
                | None -> 
                    (f s), ((s.elt, s.elt, (0.)) :: s.acc)
            )) (fun s -> s.acc) graph start []
        ;;

        let djikstra _start _target _graph = 
            []
        ;;

        (*
        Floyd warshall
        Bellman ford
        Djikstra
        Astar
    *)
    end

    module Flow = struct 
        (* Ford-Fulkerson (flow) *)
    end

    (*************************************************************************
    *                           Spanning Trees                               *
    **************************************************************************)
    module Span = struct
        (**
      kruskal  
    *)
    end

end;;
