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

    val induced:  adj NodeMap.t -> elt SccTbl.t -> sccedge SccMap.t
    val tarjan:   adj NodeMap.t -> solution
    val kosaraju: adj NodeMap.t -> solution
end

module type ClusterImpl = sig
    type elt
    type adj

    module NodeMap: Map.S    with type key := elt
    module AdjSet:  TSet     with type t   := elt

    val bronkerbosch:  adj NodeMap.t -> elt AdjSet.set list
    val bronkerbosch2: adj NodeMap.t -> elt AdjSet.set list
end

(* Axioms *)
(* we have to use a wrap type to denote extremes, ideally this is just a float
   but the challenge is we don't know they final type ahead of time so we
   prefigure it as we need it for some algorithms e.g djikstra *)
type +!'a wrap = [`Inf | `NegInf | `Nan | `Value of 'a]

let wcompare f l r = match (l, r) with
    | (`Value l',`Value r' ) ->  f l' r'
    | (`Inf,    `Inf)     ->  0
    | (`NegInf, `NegInf)  ->  0
    | (`Nan,    `Nan)     ->  0
    | (`NegInf, `Inf)     -> -1
    | (`NegInf, `Value _) -> -1
    | (`Value _,`Inf)     -> -1
    | (`Nan,    `Inf)     -> -1
    | (`Nan,    `NegInf)  -> -1
    | (`Nan,    `Value _) -> -1
    | (`Inf,    `NegInf)  ->  1
    | (`Value _,`NegInf)  ->  1
    | (`Inf,    `Value _) ->  1
    | (`Inf,    `Nan)     ->  1
    | (`NegInf, `Nan)     ->  1
    | (`Value _,`Nan)     ->  1
;;

(** apply f with the values in both l and r *)
let wbind f l r = match (l, r) with
    | (`Value l', `Value r') -> (`Value (f l' r'))
    | (x, `Value _) -> x
    | (`Value _, y) -> y
    (* TODO:maybe fallback to polymorphic compare and takes least if not directly comparable *)
    | (x',      _y') -> x'
;;

let string_of_wrap f v = match v with
    | `Inf     ->  "`Inf"
    | `NegInf  ->  "`NegInf"
    | `Nan     ->  "`Nan"
    | `Value x -> Printf.sprintf (format_of_string "`Value %s") (f x)
;;

let wmin f l r = if (wcompare f l r) = -1 then l else r
;;

let wmax f l r = if (wcompare f l r) =  1 then l else r
;;

(** Our space can be any ast you want, so far we mostly use zero and add for
    dijkstra, astar and bellman-ford *)
module type Space = sig

  type t
  include Set.OrderedType with type t := t

  val zero : t
  val one  : t

  val neg  : t -> t
  val add  : t -> t -> t
  val sub  : t -> t -> t
  val mul  : t -> t -> t
  val div  : t -> t -> t
  val pow  : t -> t -> t
  val log  : t -> t
  val exp  : t -> t
  val sin  : t -> t
  val cos  : t -> t
  val tan  : t -> t
  val sinh : t -> t
  val cosh : t -> t
  val tanh : t -> t

end

module type Measurable = sig
    include Space
    type edge
    val measure: edge -> t wrap
end

(* simple adapter for some types to save some boilerplate in some cases *)
module Biject(T: Space): Measurable with type edge = T.t and type t = T.t = struct

    include T

    type t         = T.t
    type edge      = t

    let  compare   = T.compare
    let  measure e = `Value e
end

(* simple adapter for some types to save some boilerplate in some cases *)
module type PathImpl = sig
    type elt
    type adj
    type edge
    type weights
    type 'b ctx

    module NodeMap:  Map.S with type key  := elt
    module AdjSet:   TSet  with type t    := elt
    module EdgeSet:  TSet  with type t    := (elt * elt)

    type 'c pathelt = { from: elt; next: elt; via: elt; value: 'c; }

    val mkpath: elt -> elt -> 'a -> 'a pathelt

    module Compute(Measure: Measurable with type t = edge and type edge = edge): sig
        type measure = Measure.t wrap

        module PathList: Ordinal with type order = measure     and type t = measure pathelt
        module PathHeap: FibHeap with type node  = measure pathelt and type order = measure
        module PathSet : TSet    with type t    := measure pathelt

        val dijkstra:    elt -> elt -> adj NodeMap.t -> ((elt * measure) list)
        val astar:       (elt -> measure) -> elt -> elt -> adj NodeMap.t -> (elt * measure) list
        val bellmanford: elt -> elt -> adj NodeMap.t -> ((elt * measure) list * (elt * elt) EdgeSet.set)
    end

    type path     := (edge pathelt) list
    val hierholzer: ?endpoints:(adj NodeMap.t -> (elt * elt * int) option) -> adj NodeMap.t -> (elt list) option
    val naivedfs:   adj NodeMap.t -> (path ctx -> bool) -> elt -> path
    val naivebfs:   adj NodeMap.t -> (path ctx -> bool) -> elt -> path
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
    type edge
    type weights

    include Set.OrderedType  with type t   := adj
    module  NodeMap: Map.S   with type key := elt

    val empty     : elt -> adj
    val weights   : elt -> adj NodeMap.t -> weights
    val edge      : elt -> elt -> adj NodeMap.t -> edge
    val edge2     : elt -> weights -> edge
end

module type GraphElt = sig
    type edge
    include Set.OrderedType
end

module type Graph = sig

    type +'a t
    type elt
    type edge

    module AdjSet:  TSet      with type t   := elt
    module Weights: Hashtbl.S with type key := elt

    (* TODO: make edge Weights.t an option type *)
    type adj    = (elt AdjSet.set * elt AdjSet.set * elt * edge Weights.t)
    type 'b ctx = { 
        stop: bool; prev: elt option; elt: elt; 
        vis: elt AdjSet.set; acc: 'b; vtx: adj 
    }

    module NodeMap: Map.S with type key := elt
    module EdgeSet: TSet  with type t   := (elt * elt)

    module Vertex: VertexImpl with
        type       elt     := elt
        and type   adj     := adj
        and type   weights := edge Weights.t
        and type   edge    := edge
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
        and type   edge    := edge
        and type   weights := edge Weights.t
        and type   'b ctx  := 'b ctx
        and module NodeMap := NodeMap
        and module AdjSet  := AdjSet
        and module EdgeSet := EdgeSet

    val empty:       adj NodeMap.t
    val equal:       elt -> elt -> bool
    val add:         elt -> adj NodeMap.t -> adj NodeMap.t
    val ensure:      elt -> adj -> adj NodeMap.t -> adj NodeMap.t
    val add_edge:    elt -> elt -> adj NodeMap.t -> adj NodeMap.t
    val add_all:     elt -> elt list -> adj NodeMap.t -> adj NodeMap.t
    val allweighted: elt -> (elt * edge) list -> adj NodeMap.t -> adj NodeMap.t
    val allweighted2:elt -> (elt * edge) list -> adj NodeMap.t -> adj NodeMap.t
    val add_weight:  edge -> elt -> elt -> adj NodeMap.t -> adj NodeMap.t
    val add_weight2: edge -> elt -> elt -> adj NodeMap.t -> adj NodeMap.t
    val of_list:     (elt * elt list) list -> adj NodeMap.t -> adj NodeMap.t
    val of_weights:  (elt * (elt * edge) list) list -> adj NodeMap.t -> adj NodeMap.t
    val of_weights2: (elt * (elt * edge) list) list -> adj NodeMap.t -> adj NodeMap.t
    val cardinal:    adj NodeMap.t -> int
    val vertexof:    elt -> adj NodeMap.t -> adj
    val incomingof:  elt -> adj NodeMap.t -> elt AdjSet.set
    val incweights:  elt -> adj NodeMap.t -> elt AdjSet.set * edge Weights.t
    val outgoingof:  elt -> adj NodeMap.t -> elt AdjSet.set
    val outweights:  elt -> adj NodeMap.t -> elt AdjSet.set * edge Weights.t
    val neighbours:  elt -> adj NodeMap.t -> elt AdjSet.set
    val xorneighbors:elt -> adj NodeMap.t -> elt AdjSet.set
    val mutuals:     elt -> adj NodeMap.t -> elt AdjSet.set
    val xormutuals:  elt -> adj NodeMap.t -> elt AdjSet.set
    val degree:      elt -> adj NodeMap.t -> int
    val iodeg:       elt -> adj NodeMap.t -> (int * int)
    val undcircuit:  adj NodeMap.t -> bool
    val dircircuit:  adj NodeMap.t -> bool
    val undeulpath:  adj NodeMap.t -> (elt * elt * int) option
    val direulpath:  adj NodeMap.t -> (elt * elt * int) option
    val incdeg:      elt -> adj NodeMap.t -> int
    val outdeg:      elt -> adj NodeMap.t -> int
    val allpairs:    adj NodeMap.t -> (elt * elt) list
    val allweights:  adj NodeMap.t -> (elt * elt * edge) list
    val remove:      elt -> adj NodeMap.t -> adj NodeMap.t
    val remove_edge: adj NodeMap.t -> elt -> elt -> adj NodeMap.t
    val cull:        adj NodeMap.t -> adj NodeMap.t
    val toposort:    adj NodeMap.t -> elt list
    val bfs:         ('b ctx -> 'b ctx) -> ('b ctx -> 'b ctx) -> adj NodeMap.t -> elt -> 'b -> 'b
    val dfs:         ('b ctx -> 'b ctx) -> ('b ctx -> 'b ctx) -> adj NodeMap.t -> elt -> 'b -> 'b
    val adj_list_of: elt -> adj NodeMap.t -> elt list
    val transpose:   adj NodeMap.t -> adj NodeMap.t
    val transpose2:  adj NodeMap.t -> adj NodeMap.t
    val outlist:     adj NodeMap.t -> (elt * elt AdjSet.set) list
    val of_matrix:   int array array -> (int * elt) list -> adj NodeMap.t
    val adjmatrix:   adj NodeMap.t -> int array array * (int * elt) list
    val wgtmatrix:   edge -> adj NodeMap.t -> (edge array array) * ((int * elt) list)
    val degmatrix:   adj NodeMap.t -> int array array * (int * elt) list
    val incmatrix:   adj NodeMap.t -> int array array * elt array * (elt * elt) array
    val degtable:    (elt, int * int) Hashtbl.t -> adj NodeMap.t -> unit
    val edgeset:     adj NodeMap.t -> (elt * elt) EdgeSet.set
    val bfstree:     bool -> elt -> adj NodeMap.t -> adj NodeMap.t
end

(* simple adapter for some types to save some boilerplate in some cases *)
module Plain(T: Set.OrderedType): GraphElt with type edge = unit = struct
    type t       = T.t
    let  compare = T.compare
    type edge    = unit
end

(* NB:
        It is not clear how to represent multiple edges between nodes
        but for now you can either introduce a new 'intermediary' node.
        this has the consequence of raising the number of nodes overall in the
        graph which may not be desired - another approach will be considered

        Most algorithms should remain the same after that transformation !!
*)

module MakeGraph(Unique: GraphElt): Graph with type elt := Unique.t and type edge := Unique.edge = struct

    type elt  = Unique.t
    type edge = Unique.edge

    (** compare 2 nodes in the graph *)
    let equal lnode rnode = (Unique.compare lnode rnode) = 0

    (** Weights hold edge values should they exist. elt is the `to` direction of
        the weight, on undirected graphs it may be duplicated on both nodes
        vertex values as a to in one and a from in another *)
    module WeightNode = struct
        type t      = elt
        let  equal  = equal
        let  hash   = Hashtbl.hash
    end
    module Weights = Hashtbl.Make (WeightNode)

    (** Module for manipulating the Set structure holding the Adjacency list *)
    module AdjSet  = TreeSet(Unique)
    type   adj     = (elt AdjSet.set * elt AdjSet.set * elt * edge Weights.t)

    (** Module for manipulating the Map (Node -> (set , set , label)) *)
    module NodeMap = Map.Make(Unique)
    module EdgeSet = TreeSet (struct
        type t = (elt * elt)
        let compare (x, y) (x', y') = match Unique.compare x x' with
            | 0 -> Unique.compare y y'
            | z -> z
    end)

    (*  Incoming nodes  Outgoing nodes data *)
    module Vertex  = struct
        type t          = adj
        let compare     = fun (_, _, lnode, _) (_, _, rnode, _) -> Unique.compare lnode rnode
        let empty   lbl = (AdjSet.empty, AdjSet.empty, lbl, Weights.create 1)
        let weights n g = let (_, _, _, w) = NodeMap.find n g in w
        let edge  f t g = Weights.find (weights f g) t
        let edge2   t o = Weights.find o t
    end

    (** Adjacency list graph definition **)
    (* Map from NodeType.t to (incoming outgoing label) *)
    type +'a t     = (Vertex.t) Map.Make(Unique).t

    (** An empty graph **)
    let empty      = NodeMap.empty

    (** Add a new node with its label -> ( ... , nodedata), this will replace
        existing entries *)
    let add nodekey nodeMap =
        NodeMap.add nodekey (Vertex.empty nodekey) nodeMap
    ;;

    (** only adds and updates if the value was not already present, otherwise
        leaves as is *)
    let ensure nodeKey nodeValue nodeMap = 
        NodeMap.update nodeKey (fun v -> match v with 
            | None -> Some nodeValue
            | v'   -> v'
        ) nodeMap
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
        |>  NodeMap.update nodeTo (fun x -> let* (toIncoming, toOutgoing, tolabel, wgts) = x in
            Some ((AdjSet.add nodeFrom toIncoming), (toOutgoing), tolabel, wgts))
    ;;

    (*Find the tail of the directed edge*)
    (*Update with outgoing*)
    (*Find the head of the directed edge*)
    (*Update with incoming*)
    (** Add nodeFrom nodeTo with weight values on from end  *)
    let add_weight weightValue nodeFrom nodeTo nodeMap =
        (NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label, wgts) = x in
            let _  = Weights.add wgts nodeTo weightValue in
            Some (fromIncoming, (AdjSet.add nodeTo fromOutgoing), label, wgts)) nodeMap)
        |>  NodeMap.update nodeTo (fun x -> let* (toIncoming, toOutgoing, tolabel, wgts) = x in
            Some ((AdjSet.add nodeFrom toIncoming), (toOutgoing), tolabel, wgts))
    ;;

    (*Find the tail of the directed edge*)
    (*Update with outgoing*)
    (*Find the head of the directed edge*)
    (*Update with incoming*)
    (** Add bidirectional nodeFrom nodeTo with weight values on both ends  *)
    let add_weight2 weightValue nodeFrom nodeTo nodeMap =
        (* TODO: is it better to duplicate the edge weights on both node values ??*)
        (NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label, wgts) = x in
            let _  = Weights.add wgts nodeTo weightValue in
            Some (AdjSet.add nodeTo fromIncoming, (AdjSet.add nodeTo fromOutgoing), label, wgts)) nodeMap)
        |>  NodeMap.update nodeTo (fun x -> let* (toIncoming, toOutgoing, tolabel, wgts) = x in
            let _  = Weights.add wgts nodeFrom weightValue in
            Some ((AdjSet.add nodeFrom toIncoming), (AdjSet.add nodeFrom toOutgoing), tolabel, wgts))
    ;;

    let rec add_all nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | nodeTo :: rest -> add_edge nodeFrom nodeTo (add_all nodeFrom rest nodeMap)
    ;;

    let rec allweighted nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | (nodeTo, nodeVal) :: rest -> add_weight nodeVal nodeFrom nodeTo 
            (allweighted nodeFrom rest nodeMap)
    ;;

    let rec allweighted2 nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | (nodeTo, nodeVal) :: rest -> add_weight2 nodeVal nodeFrom nodeTo 
            (allweighted2 nodeFrom rest nodeMap)
    ;;

    let cardinal = NodeMap.cardinal;;

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

    (** vertex of an element *)
    let vertexof = NodeMap.find 
    ;;

    (** [ incomingof identity (Graph.t) AdjSet.t]
    Incoming set of nodes *)
    let incomingof node game = let (inc, _, _, _) = NodeMap.find node game in inc
    let incweights node game = let (inc, _, _, w) = NodeMap.find node game in (inc, w)

    (** [ incomingof identity (Graph.t) AdjSet.t]
    Outgoing set of nodes *)
    let outgoingof node game = let (_, out, _, _) = NodeMap.find node game in out
    let outweights node game = let (_, out, _, w) = NodeMap.find node game in (out, w)

    (** both incoming and outgoing edges of a graph - self edges included *)
    let neighbours node game = let (inc, out, _, _) = NodeMap.find node game in (AdjSet.union inc out)

    (** both incoming and outgoing edges of a graph - self edges NOT included *)
    let xorneighbors node game = let (inc, out, _, _) = NodeMap.find node game in
        AdjSet.remove node (AdjSet.union inc out)
    ;;

    (** items in both incoming and outgoing edges of a graph - self edges included *)
    let mutuals node game = let (inc, out, _, _) = NodeMap.find node game in
        (AdjSet.inter inc out)
    ;;

    (** items in both incoming and outgoing edges of a graph - self edges NOT included *)
    let xormutuals node game = let (inc, out, _, _) = NodeMap.find node game in
        AdjSet.remove node (AdjSet.inter inc out)
    ;;

    let incdeg node nodeMap =
        AdjSet.cardinal @@ incomingof node nodeMap
    ;;

    let outdeg node nodeMap =
        AdjSet.cardinal @@ outgoingof node nodeMap
    ;;

    (** number of incoming and outgoing edges  *)
    let degree node nodeMap =
        incdeg node nodeMap + outdeg node nodeMap
    ;;

    (** incoming and outgoing degree *)
    let iodeg node nodeMap =
        (incdeg node nodeMap, outdeg node nodeMap)
    ;;

    (** eulerian circuit test for undirected graphs (all edges visited exactly
        once) *)
    let undcircuit nodeMap =
        NodeMap.for_all (fun _ (i,o,_,_) ->
            (AdjSet.cardinal o + AdjSet.cardinal i) mod 2 == 0) nodeMap
    ;;

    (** eulerian circuit test for directed graphs (all edges visited exactly once) *)
    let dircircuit nodeMap =
        NodeMap.for_all (fun _ (i,o,_,_) ->
            (AdjSet.cardinal o) == (AdjSet.cardinal i)) nodeMap
    ;;

    (** undirected eulerian path for undirected graph return start and end points *)
    let undeulpath nodeMap =
        let (even, odd, sz, pnts) = NodeMap.fold (
            fun  k (i,o,_,_) (even, odd, sz', (l, r)) ->
            let deg = (AdjSet.cardinal o + AdjSet.cardinal i) in
            if deg mod 2 == 0  then
                (even + 1, odd, sz' + 1, (l, r))
            else
            if odd = 0 then
                (even, odd + 1, sz' + 1, (l, Some k))
            else
                (even, odd + 1, sz' + 1, (Some k, l))
        ) nodeMap (0, 0, 0, (None, None)) in
        (* either all vertices are even or exactly 2 have odd degree *)
        if even = sz then
            (* all vertices are even - pick any 2 nodes - or a circuit *)
            let  (k, _)  = NodeMap.choose nodeMap in
            Some (k, k, sz)
        else if odd = 2 then
            match pnts with
            | Some l, Some r -> Some (l, r, sz)
            (* ideally unreachable when odd = 2 *)
            | _  -> None
        else
            None
    ;;

    (** directed eulerian path for directed graph return start and end points
        any circuit is an eulerian path
    *)
    let direulpath nodeMap =
        (* in = 1,  out = 1, eq, count *)
        let (din, dou, deq, sze, pnts) = NodeMap.fold
            (fun  k (i,o,_,_) (din, dout, deq, cn, (start, fin)) ->
                let indeg = AdjSet.cardinal i  in
                let oudeg = AdjSet.cardinal o  in
                let io = ((indeg - oudeg) = 1) in
                let ou = ((oudeg - indeg) = 1) in
                let eq = ((indeg = oudeg)) in
                (
                    din  +  (Bool.to_int io),
                    dout +  (Bool.to_int ou),
                    deq  +  (Bool.to_int eq),
                    cn   +  1,
                    (
                        (* articulate the start and end *)
                        (if ou then Some k else start),
                        (if io then Some k else fin  )
                    )
                )
            ) nodeMap (0, 0, 0, 0, (None, None)) in

        (* degree check for euler path *)
        let check =
            (* all vertices have equal in and out degree *)
            (deq = sze)
            ||
            (* at most 1 has each of "greater by 1" degree *)
            (din = 1) && (dou = 1) && (deq = sze - 2)
        in

        match (check, pnts) with
        | (false, _) ->
            None
        (* Even case: The circuit is the euler path *)
        | (true, (None, None)) ->
            let  (k', _)  = NodeMap.choose nodeMap in
            Some (k', k', sze)
        (* Odd case: Verify there must be at least 2 points for start and end *)
        | (_, (Some x, Some y)) -> Some (x, y, sze)
        (* Ideally this case shouldn't be reached but it should mean no path *)
        | _ -> None
    ;;

    let allpairs nodeMap =
        NodeMap.fold (fun k (_, out, _, _) ac ->
            AdjSet.fold (fun el ac' -> (k, el) :: ac') out ac)  nodeMap []
    ;;

    let allweights nodeMap =
        NodeMap.fold (fun k (_, out, _, wg) ac ->
            AdjSet.fold (fun el ac' -> (k, el, Weights.find wg el) :: ac') out ac)  nodeMap []
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

    (* edge removal, call twice flipped if undirected graph *)
    let remove_edge nodeMap nodeFrom nodeTo =
        (NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label, wgts) = x in
            Some (fromIncoming, (AdjSet.remove nodeTo fromOutgoing), label, wgts)) nodeMap)
        |>
        (NodeMap.update nodeTo (fun x -> let* (toIncoming, toOutgoing, tolabel, wgts) = x in
            Some ((AdjSet.remove nodeFrom toIncoming), (toOutgoing), tolabel, wgts)))
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

    type 'b ctx = {
        stop: bool;           (* whether to stop a recurse *)
        prev: elt option;     (* the previous element, None if start *)
        elt:  elt;            (* the current node *)
        vis:  elt AdjSet.set; (* the visited nodes *)
        acc:  'b;             (* the accumulator *)
        vtx:  adj; (* the outgoing node *)
        (*out:  Vertex.t; (* the outgoing node *)*)
    }

    (** breadth first search starting from start node applying f until returns
        true or queue is empty applying f on each node and b on backtrack

        filters already visited nodes, in bidirectional cases,
        backedges will not be visited twice
    *)

    let mkctx p l v a s o =  {prev=p;elt=l; vis=v; acc=a;stop=s; vtx=o;} 
    ;;

    let bfs f b graph start init =
        let que     = Queue.create () in
        let _       = Queue.add (None, start) que in
        let visited = AdjSet.empty in
        let rec iter vis nxt acc =
            if Queue.is_empty nxt then
                (vis, acc)
            else
                let (prev, label) = Queue.take nxt in
                let vtx = vertexof label graph in
                let (_,out,_,_) = vtx in
                let {stop;acc=acc';_} = f (mkctx prev label vis acc false vtx) in
                let (vis', acc'') = if stop then
                    (vis, acc')
                    else
                        let diff= AdjSet.diff out vis in
                        let _   = AdjSet.iter (fun x -> Queue.add (Some label, x) nxt) (diff) in
                        iter (AdjSet.union diff vis) nxt acc'
                in (vis', (b (mkctx prev label vis' acc'' false vtx)).acc)
        in let (_, acc) = iter visited que init in acc
    ;;

    (** depth first search starting from start node applying f until returns
        true applying f until returns
        true or stack is empty applying f on each node and b on backtrack *)
    let dfs f b graph start init =
        let stck    = Stack.create () in
        let _       = Stack.push (None, start) stck in
        let visited = AdjSet.empty in
        let rec iter vis nxt acc =
            if Stack.is_empty stck then
                (vis, acc)
            else
                let (prev, label) = Stack.pop nxt in
                let vtx = vertexof label graph in
                let (_,out,_,_) = vtx in
                let {stop;acc=acc';_} = f  (mkctx prev label vis acc false vtx) in
                let (vis', acc'') = (
                    if stop then
                        (vis, acc')
                    else
                    if AdjSet.mem label vis then
                        iter (vis) nxt acc'
                    else
                        let _   = AdjSet.iter (fun x -> Stack.push (Some label, x) nxt) (out) in
                        iter (AdjSet.add label vis) nxt acc'
                ) in (vis', (b (mkctx prev label vis' acc'' false vtx)).acc)
        in let (_, acc) = iter visited stck init in acc
    ;;

    (** Get adjacency list of a node *)
    let adj_list_of node nodeMap =
        let (incoming, outgoing, _label, _wgts) = NodeMap.find node nodeMap in
        AdjSet.to_list (AdjSet.union incoming outgoing)
    ;;

    (* collect graph into a simple [(key,  adj list), ...] *)
    let outlist nodeMap =
        NodeMap.fold (fun elt (_, out, _, _) acc ->
            (elt, out) :: acc
        ) nodeMap []
    ;;

    (* graph to 2d array and a key list defining the elt for each array index *)
    let adjmatrix nodeMap =
        let sz   = NodeMap.cardinal nodeMap in
        let keys = NodeMap.to_list  nodeMap in
        (* map outgoing edges to indices, return each elt and its index along
           with the indexes of outgoing edges

            {
                1: [2,3]
                2: [1]
                3: []
                ....
            }

            becomes

                idx node out
            [
                ((0, 1), [1,2]),
                ((1, 2), [0]),
                ((2, 3), []),
            ]

           *)
        let adjs = List.mapi (fun i (k, (_, o, _, _)) ->
            ((i, k), AdjSet.fold (fun elt acc ->
                (* return the index of the outgoing edge *)
                Option.get (
                    List.find_index (fun (b, _) -> equal b elt) keys
                ) :: acc
            ) o [])
        ) keys in
        let b = Array.init_matrix sz sz (fun row col ->
            (* bool value of whether j is in the outgoing index *)
            Bool.to_int @@ List.mem col (snd @@ List.nth adjs row)
        ) in
        (* return matrix and keys showing which elt is which index *)
        (b, List.map (fst) adjs)
    ;;

    (* graph to 2d weights and a key list defining the elt for each array index *)
    let wgtmatrix defval nodeMap =
        let sz   = NodeMap.cardinal nodeMap in
        let keys = NodeMap.to_list  nodeMap in
        let adjs = List.mapi (fun i (k, (_, o, _, wgt)) ->
            (* any outgoing node has to be in the keys list *)
            ((i, k), AdjSet.fold (fun elt acc ->
                (* return the index of the outgoing edge *)
                (Option.get (
                    (List.find_index (fun (b, _) -> equal b elt) keys)
                ), Vertex.edge2 elt wgt) :: acc
            ) o [])
        ) keys in
        let b = Array.init_matrix sz sz (fun i j ->
            (* bool value of whether j is in the outgoing index *)
            let v = List.find_opt (fun (idx, _) -> idx = j) @@ (snd @@ List.nth adjs i)
            in match v with
                | Some (_, msr) -> msr
                | _             -> defval
        ) in
        (* return matrix and keys showing which elt is which index *)
        (b, List.map (fst) adjs)
    ;;



    (* graph from 2d adjacency matrix with keys defining which elt maps to what index *)
    let of_matrix nodeMatrix keys =
        let g = List.fold_right (fun (_, e) a  -> add e a) keys empty  in
        let index = 0 in
        let adj_list = snd @@ Array.fold_left (fun (idx, acc) iarr ->
            let k = (snd (List.find (fun  (id, _el) -> id = idx) keys )) in
            let index' = 0 in
            (idx+1, (k, snd @@ (Array.fold_left (fun (idx', acc') v ->
                if v = 1 then
                    (idx'+1, (snd (List.find (fun  (id, _el) -> id = idx') keys )) :: acc')
                else
                    (idx'+1, acc')
            ) (index', []) iarr)) :: acc)
        ) (index, []) nodeMatrix in
        of_list adj_list g
    ;;

    (* inout degree matrix along the diagonal *)
    let degmatrix nodeMap =
        let sz, adjs = NodeMap.fold (fun key (inc, out, _, _) (idx, acc) ->
            (idx + 1), (((idx, key), (AdjSet.cardinal inc + AdjSet.cardinal out)) :: acc)
        ) nodeMap (0, []) in
        let b = Array.init_matrix sz sz (fun i j ->
            if i == j then
                snd @@ List.nth adjs i
            else
                0
        ) in
        (* return matrix and keys showing which elt is which index *)
        (b, List.map (fst) adjs)
    ;;

    let degtable tbl nodeMap =
        NodeMap.iter (fun k (inc, out, _, _) ->
            Hashtbl.add tbl k (AdjSet.cardinal inc, AdjSet.cardinal out)
        ) nodeMap
    ;;

    let edgeset nodeMap =
        NodeMap.fold (fun el (_, ou, _, _) a ->
            AdjSet.fold (fun nx a' -> EdgeSet.add (el, nx) a') ou a
        ) nodeMap EdgeSet.empty
    ;;

    (* like edgeset but returns the size to avoid recomputing if needed *)
    let edgesetsz nodeMap =
        NodeMap.fold (fun el (_, ou, _, _) (g, e, a) ->
        let e', a' =  AdjSet.fold (fun nx (e', a') -> e'+1, EdgeSet.add (el, nx) a') ou (e, a) in
            g+1, e', a'
        ) nodeMap (0, 0, EdgeSet.empty)
    ;;

    (* graph -> incidence matrix, node array, elt array
       useful in spectral clustering: https://math.stackexchange.com/a/3726603 *)
    let incmatrix nodeMap =
        let r,c, edgeset = (edgesetsz nodeMap) in
        let (t,_) = NodeMap.choose nodeMap in
        let idx   = ref 0 in
        let nodes = Array.make r t in
        let edges = Array.make c (t,t) in
        let belongs i (f, t) = equal i f || equal i t in
        let _ = NodeMap.iter (fun n _ -> let _ = nodes.(!idx) <- n in incr idx) nodeMap in
        let _ = idx := 0 in
        let _ = EdgeSet.iter (fun e   -> let _ = edges.(!idx) <- e in incr idx) edgeset in
        Array.init_matrix r c (fun r' c' ->
            if belongs nodes.(r') edges.(c') then
                1
            else
                0
        ), nodes, edges
    ;;

    (** swap the incoming and outgoing edge direction - preserving edge weights *)
    let transpose (nodeMap: adj NodeMap.t) =
        NodeMap.map (
            fun (inc, out, label, wgts) ->
            let wgts' = Weights.create (Weights.length wgts) in
            let _     = AdjSet.iter (fun x ->
                match Weights.find_opt (Vertex.weights x nodeMap) label with
                | Some edge -> Weights.add wgts' x edge
                | None -> ()
            ) inc in
            (out, inc, label, wgts')
        ) nodeMap
    ;;

    (** swap the incoming and outgoing edge direction - WITHOUT preserving edge weights *)
    let transpose2 (nodeMap: adj NodeMap.t) =
        NodeMap.map (
            fun (inc, out, label, wgts) -> (out, inc, label, wgts)
        ) nodeMap
    ;;

    (** toposort (happens-before) *)
    let toposort nodeMap =
        snd @@ NodeMap.fold (fun x _y (v, a) ->
            if AdjSet.mem x v then
                (v, a)
            else
                dfs (fun s -> { s with stop=false }) (fun s ->
                    if AdjSet.mem s.elt (fst s.acc) then
                        s
                    else
                        { s with 
                            acc = AdjSet.add s.elt (fst s.acc), s.elt :: (snd s.acc)
                        }
                ) nodeMap x (v, a)
        ) nodeMap (AdjSet.empty, [])
    ;;

    let wedge directed (_,_,_,wgt) f t g =
        if directed then
            add_edge f t g
        else
            add_weight (Vertex.edge2 t wgt) f t g
    ;;

    (** build a tree like acyclic graph from a bfs traversal, with edges
        emanating from the start *)
    let bfstree isdir start (graph: adj NodeMap.t) = 
        snd @@ bfs 
            (fun ctx -> 
                let acc' = (
                    let v, g = ctx.acc in
                    let (_,out,_,_) = ctx.vtx in
                    let o'  = AdjSet.diff out v in
                    let g'  = AdjSet.fold (fun el acc -> ensure el (Vertex.empty el) acc) o' g in
                    let g'' = AdjSet.fold (wedge isdir ctx.vtx ctx.elt) o' (ensure ctx.elt (Vertex.empty ctx.elt) g') in 
                        (AdjSet.add ctx.elt (AdjSet.union o' v), g'')
                ) in { ctx with acc = acc' }
            ) (Fun.id) graph start ((AdjSet.empty), (NodeMap.empty))
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

        (* creates a Map of (ints -> ([idx...], Graph.t)) where the int is the link value.
           it computes its neighbours into the list section of the Map value.
           This makes more idiomatic to the outer graph structure which is also
           a map albeit with different values
        *)
        let induced nodeMap sccs =
            SccTbl.fold (fun {link=lowlink;_} elt acc ->
                let edges = NodeMap.find elt nodeMap in
                let (_, out, _, _) = edges in
                let keyseq = SccTbl.to_seq_keys sccs in
                let sccedg = (AdjSet.fold (fun e ac ->
                    match Seq.find (hasnode e) keyseq with
                    | Some v -> if v.link != lowlink then v.link :: ac else ac
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

        (** tarjans visit args: function graph  root-node  neighbour-node solution*)
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
            let tgraph = transpose2 graph in
            let iter2 scc sccnode =
                (* if onst the we consider it already a part of an scc *)
                if AdjSet.mem sccnode.node scc.onst then
                    scc
                else
                    let _ = incr count in
                    (* find all reachable nodes *)
                    let vstd = dfs (Fun.id)
                        (fun s' -> { s' with acc=s'.vis }) tgraph sccnode.node (AdjSet.empty)
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


        module Compute(Measure: Measurable with type t = edge and type edge = edge) = struct

            (* Edge betweenness of an element *)
            let _edgebtn _node _nodeMap =
              ()
            ;;

        end

        (*let girvannewman nodeMap = ()*)
        (*;;*)

    end

    (*************************************************************************
    *                           Spanning Trees                               *
    **************************************************************************)
    module Span = struct
        (**
            kruskal and prim
        *)
    end

    (*************************************************************************
    *                             Path Algos                                 *
    **************************************************************************)
    module Path = struct

        type 'c pathelt = { from: elt; next: elt; via: elt; value: 'c; }

        let mkpath f t cost =
            { from=f; next=t; via=f; value=cost; }
        ;;

        let viapath f v t cost =
            { from=f; next=t; via=v; value=cost; }
        ;;

        module Compute(Measure: Measurable with type t = edge and type edge = edge) = struct

            type measure = Measure.t wrap

            (* implement an ordinal interface with the measure compare for the
               heap order of paths (edges in the graph) *)
            module PathList: Ordinal with type t = measure pathelt and type order = measure = struct
                type t          = measure pathelt
                type order      = measure

                let  bind t     = t.value
                (* next node is enough to differentiate between 2 nodes *)
                let  compare l r=
                    match  Unique.compare l.next r.next with
                    | 0 -> Unique.compare l.from r.from
                    | x ->  x
                ;;
                let  order l r = wcompare (Measure.compare) l r
            end

            module PathHeap = MakeFibHeap (PathList)
            module PathSet  = TreeSet     (PathList)

            let shorterpathto e p =  PathSet.find_first_opt (fun e' -> equal e'.next e) p

            (*
               resolve djikstra path by skipping over non-interlinked nodes
               e.g. for a sample output path

               [("G", "E", `Value 7.); ("S", "A", `Value 7.); ("B", "D", `Value 6.);
                ("H", "F", `Value 6.); ("B", "A", `Value 5.); ("H", "G", `Value 5.);
                ("C", "L", `Value 5.); ("S", "C", `Value 3.); ("B", "H", `Value 3.);
                ("S", "B", `Value 2.); ("S", "S", `Value 0.)]

                we interlink on edges and reverse the path

               [("S", "S", `Value 0.); ("S", "B", `Value 2.); ("B", "H", `Value 3.);
                ("H", "G", `Value 5.); ("G", "E", `Value 7.)]

                due to commonality of the from origin
            *)
            let rec dijkstraresolve target acc = function
                | [] -> acc
                | (from, next, value) :: rest ->
                    if equal target next then
                        (dijkstraresolve[@tailcall]) from ((next, value) :: acc) rest
                    else
                        (dijkstraresolve[@tailcall]) target acc rest
            ;;

            (* Single source shortest path

                we choose between a log n insert or
                linear decrease like below
                Since we can tolerate duplicates we
                use a log-n insert as it gives us the
                option of structurally adding new
                info that doesn't necessarily
                violate the `compare` result and also saves a bit on time
            *)
            let dijkstra start target graph =
                (* we take the path to ourselves as 0 *)
                let startp = (viapath start start start (`Value Measure.zero)) in
                (*
                Set all start to out edges to infinity except the pseudo edge to
                self to denote the start point of dijkstra

                caveat: this may take longer but does not duplicate nodes hence
                save some memory. Tradeoff for dense graphs
                *)
                let init =
                    NodeMap.fold (fun k _ acc ->
                        (PathHeap.insert (viapath start start k `Inf) acc)
                    ) graph (PathHeap.singleton startp)
                in
                let rec iter ps heap elp =
                    if PathHeap.is_empty heap then
                        elp
                    else
                        let (u, rest) = PathHeap.extract heap in
                        if equal u.next target  then
                            ((u.via, u.next, u.value) :: elp)
                        else
                            let (out,   w) = outweights u.next graph in
                            let (ps'', h') = AdjSet.fold (fun e (p, a) ->
                                (* get distance from u to e *)
                                let d   = Measure.measure (Vertex.edge2 e w) in
                                (* add to distance from start to u *)
                                let alt = wbind (Measure.add) u.value d in
                                (* demarkate edge with distance from start *)
                                let pe  = viapath start u.next e alt in
                                (* Relaxation *)
                                match shorterpathto e ps with
                                | Some v ->
                                    (* If alternative path is shorter than
                                       previous we 'override it' *)
                                    if wcompare (Measure.compare) alt v.value = -1 then
                                        (PathSet.add pe p, PathHeap.decrease pe pe a)
                                    else
                                        (p, a)
                                | None   ->
                                    (* First sighting *)
                                    (PathSet.add pe p, PathHeap.decrease pe pe a)
                            ) out (ps, rest)
                            in
                            iter ps'' h' ((u.via, u.next, u.value) :: elp)
                in
                dijkstraresolve target [] @@ iter (PathSet.singleton startp) init []
            ;;

            let astar heuristic start target graph =
                (* we take the path to ourselves as 0 *)
                let startp = (mkpath start start (`Value Measure.zero)) in
                (*
                Set all start to out edges to infinity except the pseudo edge to
                self to denote the start point of dijkstra

                omitted as can be implied when we use insert instead of decrease operation

                init |> NodeMap.fold (fun k _v acc ->
                    (PathHeap.insert (mkpath start k `Inf) acc)
                ) graph

                ideally the heuristic reduces the number of duplicates so we can
                use a second (log n) insert
                *)
                let init =  (PathHeap.singleton startp) in
                let rec iter ps heap elp =
                    if PathHeap.is_empty heap then
                        elp
                    else
                        let (u, rest) = PathHeap.extract heap in
                        if equal u.next target  then
                            ((u.from, u.next, u.value) :: elp)
                        else
                            let (out,   w) = outweights u.next graph in
                            let (ps'', h') = AdjSet.fold (fun e (p, a) ->
                                (* get distance from u to e *)
                                let d   = Measure.measure (Vertex.edge2 e w) in
                                (* add to distance from start to u *)
                                let alt  = wbind (Measure.add) u.value d in
                                let alt' = wbind (Measure.add) alt (heuristic e) in
                                (* demarkate edge with distance from start
                                   accounting for the heuristic cost *)
                                let pe  = mkpath u.next e alt' in
                                match shorterpathto e ps with
                                | Some v ->
                                    (* If alternative path is shorter than
                                       previous we 'override it' *)
                                    if wcompare (Measure.compare) alt' v.value = -1 then
                                        (PathSet.add pe p, PathHeap.decrease pe pe a)
                                    else
                                        (p, a)
                                | None   ->
                                    (* First sighting *)
                                    (PathSet.add pe p, PathHeap.insert pe a)
                            ) out (ps, rest)
                            in
                            iter ps'' h' ((u.from, u.next, u.value) :: elp)
                in
                dijkstraresolve target [] @@ iter (PathSet.singleton startp) init []
            ;;

            (* similar idea to dijkstraresolve *)
            let bellresolve start target nodes =
                if PathSet.is_empty nodes then [] else
                    let rec iter target acc =
                        let {from; next=nxt; value;_} = PathSet.find_first (fun {from;_} -> equal from target) nodes in
                        if equal start from then
                            (from, value) :: acc
                        else
                        if equal start nxt then
                            let  {from=from'; value=value';_} = PathSet.find_first (fun {from;_} -> equal from start) nodes in
                            (from', value') :: ((from, value) :: acc)
                        else
                            iter nxt ((from, value) :: acc)
                    in  iter target []
            ;;

            (*
               the Bellman-Ford algorithm can handle directed and undirected graphs with non-negative weights
               it can only handle directed graphs with negative weights, as long as we don't have negative cycles
               If there is a negative cycle we return it as part of the EdgeSet
            *)
            let bellmanford start target (graph: adj NodeMap.t) =
                (* Set initial distance at `Inf except start *)
                let (sz, lst) = (NodeMap.fold (fun k (_, _,_,w) (acc, lst) ->
                    if equal k start then
                        acc + 1,
                        (k, (None, `Value Measure.zero, w)) :: lst
                    else
                        acc + 1,
                        (k, (None, `Inf, w)) :: lst
                ) graph (0, [])) in
                (* Hashtbl for tracking predecessors *)
                let belltable = Hashtbl.create sz in
                let _ = List.iter (fun (k, v) -> Hashtbl.add belltable k v) lst in
                (* upper bound of n - 1 sweeps for convergence *)
                let maxsz = sz - 1 in
                let rec iter sweep =
                    if sweep = maxsz then () else
                        let _ = NodeMap.iter (
                            fun elt (inc, out, _, wgts) ->
                            let (_, sofar, _) =  Hashtbl.find belltable elt in
                            (* All incoming edges *)
                            let sofar'' = AdjSet.fold (fun prv sofar' ->
                                let (_, value, wgts') = Hashtbl.find belltable prv in
                                let cost   = Vertex.edge2 elt wgts' |> Measure.measure  in
                                let value' = wbind (Measure.add) cost value in
                                (* relaxation *)
                                if wcompare (Measure.compare)  value' sofar' = -1 then
                                    let _ = Hashtbl.replace belltable elt (Some prv, value', wgts) in
                                    value'
                                else
                                    sofar'
                            ) inc sofar in
                            (* All outgoing edges *)
                            let _ = AdjSet.fold (fun nxt sofar'' ->
                                let (_, value, wgts') = Hashtbl.find belltable nxt in
                                let cost    = Vertex.edge2 nxt wgts |> Measure.measure in
                                let value'  = wbind (Measure.add) cost sofar'' in
                                (* relaxation *)
                                if wcompare (Measure.compare) value' value = -1 then
                                    let _ = Hashtbl.replace belltable nxt (Some elt, value', wgts') in
                                    sofar''
                                else
                                    sofar''
                            ) out sofar'' in ()
                        ) graph in iter (sweep + 1)
                in
                let _  = iter 0 in
                (* Do an extra iteration to detect a negative cycle by marking
                   edges that relax as part of a negative cycle
                   *)
                let negcycles =  NodeMap.fold (
                    fun elt (inc, out, _, wgts) acc ->
                    let (_, sofar, _) =  Hashtbl.find belltable elt in
                    let (sofar'', acc') = AdjSet.fold (fun prv (sofar', acc') ->
                        let (_, value, wgts') = Hashtbl.find belltable prv in
                        let cost   = Vertex.edge2 elt wgts' |> Measure.measure  in
                        let value' = wbind (Measure.add) cost value in
                        (* relaxation *)
                        if wcompare (Measure.compare)  value' sofar' = -1 then
                            let _ = Hashtbl.replace belltable elt (Some prv, value', wgts) in
                            (value', EdgeSet.add (prv, elt) acc')
                        else
                            (sofar', acc')
                    ) inc (sofar, acc) in
                    (* All outgoing edges *)
                    let (_, acc'') = AdjSet.fold (fun nxt (sofar'', acc'') ->
                        let (_, value, wgts') = Hashtbl.find belltable nxt in
                        let cost    = Vertex.edge2 nxt wgts |> Measure.measure in
                        let value'  = wbind (Measure.add) cost sofar'' in
                        (* relaxation *)
                        if wcompare (Measure.compare) value' value = -1 then
                            let _ = Hashtbl.replace belltable nxt (Some elt, value', wgts') in
                            (sofar'', EdgeSet.add (elt, nxt) acc'')
                        else
                            (sofar'', acc'')
                    ) out (sofar'', acc') in acc''
                ) graph EdgeSet.empty in
                let pl = PathSet.of_seq @@ Seq.map (fun (k, (p,v,_)) ->
                    let p' = Option.value ~default:k p in mkpath k p' v
                ) @@ Hashtbl.to_seq belltable in
                (bellresolve start target pl, negcycles)
            ;;

        end

        (* TODO: property test on random graphs and check if the output path can
           be followed along the edges *)
        (* construct an eulerian path *)
        let hierholzer ?(endpoints=direulpath) graph =
            let* start, fin, size = endpoints graph in
            let tbl = Hashtbl.create size in
            let _   = degtable tbl graph in
            let rec iter node vis path =
                let (ileft, oleft) = Hashtbl.find tbl node in
                if  oleft = 0 then
                    (vis, path)
                else
                    let out  = outgoingof  node graph in
                    let _    = Hashtbl.replace tbl node (ileft, oleft - 1) in
                    let (vis', path') = AdjSet.fold (fun elt (vis', path') ->
                        if EdgeSet.mem (node, elt) vis then
                            (vis', path')
                        else
                            iter elt (EdgeSet.add (node, elt) vis') path'
                    ) out (vis, path) in (vis', node :: path')
            in Some (snd @@ iter start EdgeSet.empty [fin])
        ;;

        (* All pairs depth first walk until (f ctx -> bool) is true or all nodes
           are exhausted. Requires the node edges to be weighted  Uses simple
           dfs to append edges and their corresponding weights to a list *)
        let naivedfs graph f start =
            List.rev @@ dfs (fun s -> (
                match s.prev with
                | Some prev ->
                    ({
                        s with stop=f s;
                        acc={from=prev; next=s.elt; via=prev;
                            value=(Vertex.edge prev s.elt graph);} :: s.acc
                    })
                | None ->
                    { s with stop = f s}
            )) (Fun.id) graph start []
        ;;

        (* All pairs breadth first walk until (f ctx -> bool) is true or all nodes
           are exhausted. Requires the node edges to be weighted. Uses simple
           bfs to append edges and their corresponding weights to a list *)
        let naivebfs graph f start =
            List.rev @@ bfs (fun s -> (
                match s.prev with
                | Some prev ->
                    {  s with stop=f s; acc = (
                        {
                            from=prev; next=s.elt; via=prev;
                            value=(Vertex.edge prev s.elt graph);
                        } :: s.acc
                    )
                    }
                | None ->
                    { s with stop = f s; }
            )) (Fun.id) graph start []
        ;;

        (*
            Floyd warshall
        *)
    end

    module Flow = struct
        (* Ford-Fulkerson (flow) *)
    end

    (* TODO:
        Efficient elt hash (bring own hash) ??

        Rodl nibble
        Dot (https://graphviz.org/doc/info/lang.html)
        IO
        Compressed (Path compression) ??
        DeBruijn

        Properties (Isomorphism: ullman, planar, acyclic)
        https://en.wikipedia.org/wiki/Sethi%E2%80%93Ullman_algorithm
        https://en.wikipedia.org/wiki/Glossary_of_graph_theory
        https://en.wikipedia.org/wiki/Component_(graph_theory)

        https://math.stackexchange.com/questions/277045/easiest-way-to-determine-all-disconnected-sets-from-a-graph
        modularity and louvain: https://www.youtube.com/watch?v=Xt0vBtBY2BU
    *)

end;;
