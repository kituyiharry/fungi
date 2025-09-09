(******************************************************************************
 *                                                                            *
 *                                Fungi Graph                                 *
 *                        Functional Graph algorithms                         *
 *                             harryk@harryk.dev                              *
 *                                                                            *
 ******************************************************************************)
open Treeset;;
open Heap;;
open Unionfind;;
open Axiom;;

(**
    {1:graph Fungi Graph Library}


    The graph is held as an adjacency list representation with an optional
    Hashtbl to hold edge weights. Self edges are supported by their presence in
    both incoming and outgoing sets

    {v
    Map
        elt =>  inc       out       adj(Hashtbl)
    {
        "A" => ("B")     ("C")     <["C" -> 10.]>
        "B" => ("C","B") ("A","B") <["A" -> 20., "B" -> 30.]>
        "C" => ("A")     ("B")     <["B" -> 40.]>
    }
    v}

    {2:build building the graph}
    we can construct the graph above by defining our GraphElt with a string
    element and float edge like so.

    {@ocaml[
        module SGraph = Graph.MakeGraph(struct
            type t      = string
            type edge   = float
            let  compare= String.compare
        end);;
    ]}

    then we can add our elements into the graph.

    {@ocaml[
        let sg = SGraph.empty
            |> SGraph.add "A"
            |> SGraph.add "B"
            |> SGraph.add "C"
        ;;
    ]}

    then we can connect edges between elements. in our case we want a directed
    graph with float weights like so. (For unweighted graphs a separate {i
    SGraph.add_edge } to connect edges)

    {@ocaml[
        let sg = sg
            |> SGraph.add_weight 10. "A" "C"
            |> SGraph.add_weight 20. "B" "A"
            |> SGraph.add_weight 30. "B" "B"
            |> SGraph.add_weight 40. "C" "A"
        ;;
    ]}

    we could also construct an adjacency list representation ahead and add all
    at once. This may still need the elements to have already been added in the
    graph.

    {@ocaml[
        [
            ("A", [("C", 10.);]);
            ("B", [("A", 20.);("B", 30.)]);
            ("C", [("A", 40.);]);
        ] |> SGraph.of_weights adjlist
    ]}

    {3:undirected Undirected graphs }

    For undirected graphs, the edges representation will be such that the
    element will be mirrored in both incoming of the head and outgoing of the
    tail. Here we use {i SGraph.of_weights2 } which creates a bidirectional edge
    which is structurally an undirected graph (we use {i SGraph.ensure } to
    ensure the elements are already in the graph!).

    {@ocaml[
        [
            ("A", [("C", 10.);]);
            ("B", [("A", 20.);("B", 30.)]);
            ("C", [("A", 40.);]);
        ] |> SGraph.of_weights2 adjlist
          (List.fold_left
            (Fun.flip (SGraph.ensure)) SGraph.empty ["A";"B";"C";])
    ]}

    the graph will structurally look like so:

    {v
    Map
        elt =>  inc       out       adj(Hashtbl)
    {
        "A" => ("B")     ("C","B")  <["C" -> 10., "B" -> 20.]>
        "B" => ("C","B") ("A","B")  <["A" -> 20., "B" -> 30., "C" -> 40.]>
        "C" => ("A","B") ("B","A")  <["B" -> 40., "A" -> 10.]>
    }
    v}
*)

(** Implementation signature for algorithms for working with strongly connected
    components *)
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

(** Implementation signature for generating Cliques  *)
module type ClusterImpl = sig
    type elt
    type adj

    module NodeMap: Map.S    with type key := elt
    module AdjSet:  TSet     with type t   := elt

    val bronkerbosch:  adj NodeMap.t -> elt AdjSet.set list
    val bronkerbosch2: adj NodeMap.t -> elt AdjSet.set list
end


(** Signature abstracting edge values from implementation. Measurable could be int
    or float or any ast implementing space. concepts such as infinity are
    handled separately from the final implementation so they can be used in
    places like Path finding or Flow algorithms `*)
module type Measurable = sig
    include Space

    type edge

    val measure: edge -> t wrap
end

(** Simple adapter for some types to save some boilerplate in some cases
    example: Building a Compute module to compute the shortest path using dijkstra

    {@ocaml [

      (* create a graph type using string nodes and float edges *)
      module SGraph = MakeGraph(struct
        type t      = string
        type edge   = float
        let compare = String.compare
      end)

      (* ... add nodes and edges ... *)

      (* Use compute to build a Path module - use biject to simplify the code *)
      module SPath = SGraph.Path.Compute(Biject(Float));;

      (* perform the computation *)
      let _ = SPath.dijkstra (* arguments... *)
    ]}*)
module Biject(T: Space): Measurable with type edge = T.t and type t = T.t = struct

    include T

    type t         = T.t
    type edge      = t

    let  compare   = T.compare
    let  measure e = `Val e
end

(** Algorithms for path finding *)
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
    type path      := (edge option pathelt) list

    val mkpath: elt -> elt -> 'a -> 'a pathelt

    module Compute(Measure: Measurable with type t = edge and type edge = edge): sig
        type measure = Measure.t wrap

        module PathList: Ordinal with type order = measure         and type t     = measure pathelt
        module PathHeap: FibHeap with type node  = measure pathelt and type order = measure
        module PathSet : TSet    with type t    := measure pathelt

        val dijkresolve:   elt -> (elt * measure) list -> (elt * elt * measure) list -> (elt * measure) list
        val dijkstra:      elt -> elt -> adj NodeMap.t -> ((elt * measure) list)
        val astar:         (elt -> measure) -> elt -> elt -> adj NodeMap.t -> (elt * measure) list
        val bellresolve:   elt -> elt -> PathList.t PathSet.set -> (elt * measure) list
        val bellmanford:   elt -> elt -> adj NodeMap.t -> ((elt * measure) list * (elt * elt) EdgeSet.set)
        val johnsons:      elt -> adj NodeMap.t -> (adj NodeMap.t * (elt -> elt -> edge -> edge))
        val floydwresolve: elt -> elt -> measure array array -> int wrap array array -> (int * elt) list -> (elt * measure) list option
        val floydwarshall: ?negcycles:(bool) -> adj NodeMap.t -> (measure array array * int wrap array array * (int * elt) list)
    end

    val hierholzer: ?endpoints:(adj NodeMap.t -> (elt * elt * int) option) -> adj NodeMap.t -> (elt list) option
    val naivedfs:   adj NodeMap.t -> (path ctx -> bool) -> elt -> path
    val naivebfs:   adj NodeMap.t -> (path ctx -> bool) -> elt -> path
end

(** Vertex carries node information  *)
module type VertexImpl = sig
    type elt
    type adj
    type edge
    type weights

    include Set.OrderedType  with type t   := adj
    module  NodeMap: Map.S   with type key := elt

    val empty  : elt -> adj
    val weights: elt -> adj NodeMap.t -> weights
    val edge   : elt -> elt -> adj NodeMap.t -> edge
    val edge2  : elt -> weights -> edge
    val edgeo  : elt -> elt -> adj NodeMap.t -> edge option
    val update : elt -> weights -> edge -> unit
    val ensure : elt -> weights -> edge -> unit
end

(** Routines for dumping out graphs *)
module type SerDe = sig
    type elt
    type edge

    val string_of_elt: elt  -> string
    val string_of_wgt: edge -> string

    val elt_of_string: string -> elt
    val wgt_of_string: string -> edge
end

(** the graph node (GraphElt.t) and edge types *)
module type GraphElt = sig
    type edge
    include Set.OrderedType
end

(** Graph Signature *)
module type Graph = sig

    type 'a t
    type elt    (* Main graph node *)
    type edge   (* Type of edge *)

    (** Adjacency set hold either incoming or outgoing elements *)
    module AdjSet:  TSet      with type t   := elt

    (** Weights hold a mapping of a Vertex node to outgoing edges in the
        adjacency list with the weight values of type edge *)
    module Weights: Hashtbl.S with type key := elt

    (** Vertex adjacency type  *)
    type adj = {
        inc: elt AdjSet.set; (* incoming set *)
        out: elt AdjSet.set; (* outgoing set *)
        lab: elt;            (* label *)
        edg: edge Weights.t; (* Weights edges *)
    }

    (** traversal context for breadth and depth first search *)
    type 'b ctx = {
        stop: bool;           (* whether to stop a recurse *)
        prev: (elt * adj) option; (* the previous element, None if start *)
        elt:  elt;            (* the current node *)
        vis:  elt AdjSet.set; (* the visited nodes *)
        acc:  'b;             (* the accumulator *)
        vtx:  adj;            (* the node vertex information *)
    }

    (** Mapping of nodes to their adjacency sets and Vertex information *)
    module NodeMap: Map.S with type key := elt

    (** Set for holding edges for use in some algorithms *)
    module EdgeSet: TSet  with type t   := (elt * elt)

    (** Vertex implementation holding adjacency information *)
    module Vertex: VertexImpl with
        type       elt     := elt             (* The node element *)
        and type   adj     := adj             (* The adjacency information *)
        and type   weights := edge Weights.t  (* Hashtbl holding edge weights *)
        and type   edge    := edge            (* The edge type *)
        and module NodeMap := NodeMap         (* Module for manipulating the map *)

    module Scc: SccImpl with
        type       elt     := elt      (* The node element *)
        and type   adj     := adj      (* The adjacency information *)
        and module NodeMap := NodeMap  (* Graph map manipulation *)
        and module AdjSet  := AdjSet   (* Adjacency set manipulation *)

    module Cluster: ClusterImpl with
        type       elt     := elt      (* The node element *)
        and type   adj     := adj      (* The adjacency information *)
        and module NodeMap := NodeMap  (* Graph map manipulation *)
        and module AdjSet  := AdjSet   (* Adjacency set manipulation *)

    module Path: PathImpl with
        type       elt     := elt            (* The node element *)
        and type   adj     := adj            (* The adjacency information *)
        and type   edge    := edge           (* The edge type *)
        and type   weights := edge Weights.t (* Hashtbl for holding weights *)
        and type   'b ctx  := 'b ctx         (* traversal context *)
        and module NodeMap := NodeMap        (* Graph map manipulation *)
        and module AdjSet  := AdjSet         (* Adjacency set manipulation *)
        and module EdgeSet := EdgeSet        (* Edge Set manipulation *)

    (** converting the graph into serializable formats *)
    module Serialize(_:SerDe with type elt := elt and type edge := edge): sig

        module StyleTbl: Hashtbl.S with type key = string
        module AttrbTbl: Hashtbl.S with type key = string
        module ClstrTbl: Hashtbl.S with type key = int

        type attrs   := string StyleTbl.t
        type attrmap := (string StyleTbl.t) AttrbTbl.t
        type clstmap := (string StyleTbl.t) ClstrTbl.t

        val to_csv: adj NodeMap.t -> ((unit -> string) Seq.t) Seq.t
        val to_dot: ?dir:bool -> ?sub:bool -> string -> attrs -> attrmap -> attrmap -> adj NodeMap.t -> (unit -> string) Seq.t Seq.t
        val to_dot_cluster: ?dir:bool -> string -> (int -> int list -> string) -> attrs -> clstmap  -> attrmap -> attrmap -> (int list * adj NodeMap.t) Scc.SccMap.t -> (unit -> string) Seq.t Seq.t

    end

    (** Graph Flow algorithms *)
    module Flow(Measure: Measurable with type t = edge and type edge = edge): sig
        type measure = Measure.t wrap
        type state = {
            mutable flow: edge
        }
        module Flowtbl: Hashtbl.S with type key = (elt * elt)

        val fordfulkerson:  ?maxit:int -> state Flowtbl.t -> elt -> elt -> adj NodeMap.t -> measure
        val edmondskarp:    ?maxit:int -> state Flowtbl.t -> elt -> elt -> adj NodeMap.t -> measure
    end

    (** Matching algorithms on graphs  e.g gale-shapely *)
    module Matching:  sig
        val hall: adj NodeMap.t -> elt AdjSet.set -> bool
        module Compute(_: Measurable with type t = edge and type edge = edge): sig
            val galeshapely: adj NodeMap.t -> elt AdjSet.set -> elt AdjSet.set -> (elt * elt) EdgeSet.set
        end
    end

    (** Minimum Spanning Trees algorithms on graphs i.e kruskal and prims *)
    module Span: functor(Measure: Measurable with type edge = edge) -> sig
        module EdgeDisj: UnionFind with type elt = elt
        val cost:    adj NodeMap.t -> Measure.t
        val prim:    elt -> adj NodeMap.t -> adj NodeMap.t
        val kruskal: ?connect:(Measure.edge -> elt -> elt -> adj NodeMap.t -> adj NodeMap.t) -> adj NodeMap.t -> adj NodeMap.t
    end

    (** An empty graph *)
    val empty:       adj NodeMap.t

    val equal:       elt -> elt -> bool

    (** Add a new node with its label -> ( ... , nodedata), this will replace
        existing entries *)
    val add:         elt -> adj NodeMap.t -> adj NodeMap.t

    (** only adds and updates if the value was not already present, otherwise
        leaves as is to avoid overwriting *)
    val ensure:      elt -> adj NodeMap.t -> adj NodeMap.t

    (** same as ensure but verifies a sequence of elements *)
    val ensureall:   adj NodeMap.t -> elt Seq.t -> adj NodeMap.t

    (** only adds and updates if the value was not already present, otherwise
        leaves as is but adds edge from nodeKey as a contingency *)
    val ensureof:    elt -> elt -> adj NodeMap.t -> adj NodeMap.t

    (** directly place elements into the graph - does not check if all edges
       are resolvable *)
    val emplace:     adj -> elt -> adj NodeMap.t -> adj NodeMap.t

    (** directly place elements into the graph - ensures edges exist and
       adds them otherwise *)
    val emplaceadj:  adj -> elt -> adj NodeMap.t -> adj NodeMap.t

    (**
    Add a directed edge [(tail)] --> [(head)] such that the tails outgoing
    set points to the heads incoming set. nonexistent nodes don't resolve
    and no error is raised in that case - it is up to the user to ensure the nodes already exist in
    the graph e.g using `Graph.ensure` function
    *)
    val add_edge:    elt -> elt -> adj NodeMap.t -> adj NodeMap.t

    (** Bidirectionally add an edge - if they nodes exist *)
    val add_edge2:   elt -> elt -> adj NodeMap.t -> adj NodeMap.t

    (** add all elements *)
    val add_all:     elt -> elt list -> adj NodeMap.t -> adj NodeMap.t

    (** add all elements (bidirectional) *)
    val add_all2:    elt -> elt list -> adj NodeMap.t -> adj NodeMap.t

    (** add elements with respective weights *)
    val allweighted: elt -> (elt * edge) list -> adj NodeMap.t -> adj NodeMap.t

    (** bidirectionally add all elements with respective weights - if they exist *)
    val allweighted2:elt -> (elt * edge) list -> adj NodeMap.t -> adj NodeMap.t

    (** add edge with a weight if the nodes exist *)
    val add_weight:  edge -> elt -> elt -> adj NodeMap.t -> adj NodeMap.t

    (** bidirectional add edge with a weight if the nodes exist *)
    val add_weight2: edge -> elt -> elt -> adj NodeMap.t -> adj NodeMap.t

    (** build graph from list - nodes should have already been added *)
    val of_list:     (elt * elt list) list -> adj NodeMap.t -> adj NodeMap.t

    (** build a bidirectional (undirected) graph *)
    val of_list2:    (elt * elt list) list -> adj NodeMap.t -> adj NodeMap.t

    (** build a directed graph with weight values -> edges already existing *)
    val of_weights:  (elt * (elt * edge) list) list -> adj NodeMap.t -> adj NodeMap.t

    (** build an undirected (bidirecitional) graph with weights  *)
    val of_weights2: (elt * (elt * edge) list) list -> adj NodeMap.t -> adj NodeMap.t

    (** number of nodes in the graph *)
    val cardinal:    adj NodeMap.t -> int

    (** Connect all nodes in the graph to form a complete graph *)
    val complete:    adj NodeMap.t -> adj NodeMap.t

    (** Get the vertex information of a node in the graph  *)
    val vertexof:    elt -> adj NodeMap.t -> adj

    (** Set of nodes forming the incoming set  *)
    val incomingof:  elt -> adj NodeMap.t -> elt AdjSet.set

    (** Incoming nodes and respective weights on the edges *)
    val incweights:  elt -> adj NodeMap.t -> elt AdjSet.set * edge Weights.t

    (** Set of nodes in the outgoing edges *)
    val outgoingof:  elt -> adj NodeMap.t -> elt AdjSet.set

    (** Outgoing nodes and respective weights on the edges *)
    val outweights:  elt -> adj NodeMap.t -> elt AdjSet.set * edge Weights.t

    (** both incoming and outgoing edges of a graph - self edges included *)
    val neighbours:  elt -> adj NodeMap.t -> elt AdjSet.set

    (** both incoming and outgoing edges of a graph - self edges NOT included *)
    val xorneighbors:elt -> adj NodeMap.t -> elt AdjSet.set

    (** items in both incoming and outgoing edges of a graph - self edges included *)
    val mutuals:     elt -> adj NodeMap.t -> elt AdjSet.set

    (** items in both incoming and outgoing edges of a graph - self edges NOT included *)
    val xormutuals:  elt -> adj NodeMap.t -> elt AdjSet.set

    (** degree of a single node *)
    val degree:      elt -> adj NodeMap.t -> int

    (** incoming + outgoing degree *)
    val iodeg:       elt -> adj NodeMap.t -> (int * int)

    (** eulerian circuit test for undirected graphs (all edges visited exactly
        once) *)
    val undcircuit:  adj NodeMap.t -> bool

    (** eulerian circuit test for directed graphs (all edges visited exactly once) *)
    val dircircuit:  adj NodeMap.t -> bool

    (** undirected eulerian path for undirected graph return start and end points *)
    val undeulpath:  adj NodeMap.t -> (elt * elt * int) option

    (** directed eulerian path for directed graph return start and end points any circuit is an eulerian path *)
    val direulpath:  adj NodeMap.t -> (elt * elt * int) option

    (** degree of incoming nodes *)
    val incdeg:      elt -> adj NodeMap.t -> int

    (** degree of outgoing nodes *)
    val outdeg:      elt -> adj NodeMap.t -> int

    (** all edges as pairs in the form (from, to) for edge from -> to *)
    val allpairs:    adj NodeMap.t -> (elt * elt) list

    (** all edges and respective weights *)
    val allweights:  adj NodeMap.t -> (elt * elt * edge) list

    (** Removes a node from the graph - weights aren't altered and may still be
    available from an opposite end of the edge depending oon how the graph is
    structured *)
    val remove:      elt -> adj NodeMap.t -> adj NodeMap.t

    (** Removes all nodes in the list from the graph - weights aren't altered and may still be
    available from an opposite end of the edge depending oon how the graph is
    structured *)
    val removeall:   adj NodeMap.t -> elt list -> adj NodeMap.t

    (** edge removal, call twice flipped if undirected graph *)
    val remove_edge: adj NodeMap.t -> elt -> elt -> adj NodeMap.t

    (** Remove self edges from a graph *)
    val cull:        adj NodeMap.t -> adj NodeMap.t

    (** Remove "free" vertices from a graph (no inc or out) *)
    val prune:       adj NodeMap.t -> adj NodeMap.t

    (** find "dangling" nodes in the graph with no incoming or outgoing edges *)
    val allfree:     adj NodeMap.t -> elt AdjSet.set

    (** toposort (happens-before) - assumes the graph is acyclic (DAG) otherwise the
        result is wrong. *)
    val toposort:    adj NodeMap.t -> elt list

    (* traversal state *)
    type state := ((elt * adj) option * elt)

    (* traversal callback *)
    type ('a, 'b) cback := 'a -> 'b ctx -> 'b ctx

    (** breadth first search starting from start node applying f until stop is
        true or queue is empty applying f on each node and b on backtrack
        filters already visited nodes, in bidirectional cases,
        backedges will not be visited twice
    *)
    val bfs:         (state Queue.t, 'b) cback -> (state Queue.t, 'b) cback -> adj NodeMap.t -> elt -> 'b -> 'b

    (** depth first search starting from start node applying f until returns
        true applying f until returns
        true or stack is empty applying f on each node and b on backtrack *)
    val dfs:         (state Stack.t, 'b) cback -> (state Stack.t, 'b) cback -> adj NodeMap.t -> elt -> 'b -> 'b

    (** Get adjacency list of a node *)
    val adj_list_of: elt -> adj NodeMap.t -> elt list

    (** Get adjacency list of a node *)
    val adj_seq_of:  elt -> adj NodeMap.t -> elt Seq.t

    (** swap the incoming and outgoing edge direction - preserving edge weights
        use transpose2 if you do not use edge weights! *)
    val transpose:   adj NodeMap.t -> adj NodeMap.t

    (** swap the incoming and outgoing edge direction - WITHOUT preserving edge weights *)
    val transpose2:  adj NodeMap.t -> adj NodeMap.t

    (** collect graph into a simple [(key,  adj list), ...] *)
    val outlist:     adj NodeMap.t -> (elt * elt AdjSet.set) list

    (** create a graph from 2d adjacency matrix with keys defining which elt maps to what index *)
    val of_matrix:   int array array -> (int * elt) list -> adj NodeMap.t

    (** convert a graph to a 2d array and a key list defining the elt for each array index *)
    val adjmatrix:   adj NodeMap.t -> int array array * (int * elt) list

    (** convert a graph to 2d weights (edge values) and a key list defining the elt for each array index *)
    val wgtmatrix:   (edge -> 'b) -> 'b -> adj NodeMap.t -> ('b array array) * ((int * elt) list)

    (** inout degree matrix along the diagonal - can be used to construct a laplacian *)
    val degmatrix:   adj NodeMap.t -> int array array * (int * elt) list

    (** graph -> incidence matrix, node array, elt array
       useful in spectral clustering: https://math.stackexchange.com/a/3726603 *)
    val incmatrix:   adj NodeMap.t -> int array array * elt array * (elt * elt) array

    (** in and out degree for each elt *)
    val degtable:    (elt, int * int) Hashtbl.t -> adj NodeMap.t -> unit

    (** create a set of all edges *)
    val edgeset:     adj NodeMap.t -> (elt * elt) EdgeSet.set

    (** create a sequence of edges *)
    val edgeseq:     adj NodeMap.t -> (elt * elt) Seq.t

    (** create a sequence of edges and weight values *)
    val edgewgtseq:  adj NodeMap.t -> (elt * elt * edge) Seq.t

    (** like edgeset but returns the size to avoid recomputing if needed. returns
       the node count, edge count and edge set in that order *)
    val edgesetsz:   adj NodeMap.t -> int * int * (elt * elt) EdgeSet.set

    (** whether edge from f to t exists in nodeMap *)
    val has_edge:    elt -> elt -> adj NodeMap.t -> bool

    (** acyclic if i cant form a cycle from each element *)
    val is_acyclic : adj NodeMap.t -> bool
end

(** simple adapter for some types to save some boilerplate in some cases - in this case making edges unitary *)
module Plain(T: Set.OrderedType): GraphElt with type edge = unit = struct
    type t       = T.t
    let  compare = T.compare
    type edge    = unit
end

(** Functor to create a graph using an adjacency set representation.

    example: To create a graph of String nodes and float edge weights

    {@ocaml[
        module SGraph = Graph.MakeGraph(struct
            type t     = string
            type edge  = float
            let compare= String.compare
        end);;
        ...
    ]}

    The structure of the graph is roughly as follows:
    For a graph consisting of nodes A,B,C with a self edge on A

    {[
        {
         "A"  => { inc:("A","B"); out:("C","A"); <{"C"-> 1.0,"A" -> 0.}>};;
         "B"  => { inc:("C");     out:("A");     <{"A"-> 1.0}>};;
         "C"  => { inc:("A");     out:("B");     <{"B"-> 2.0}>};;
         ...
        }
    ]}

    This is a simple map from each element to the `adj` structure holding the
    incoming and outgoing edges in separate sets and a Hashtbl of edge values

*)
module MakeGraph(Unique: GraphElt): Graph with type elt := Unique.t and type edge := Unique.edge = struct

    let (let*) = Option.bind

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

    (** The adjacency representation *)
    type adj    = {
        inc: elt AdjSet.set; (* incoming set *)
        out: elt AdjSet.set; (* outgoing set *)
        lab: elt;            (* label *)
        edg: edge Weights.t; (* Weights edges *)
    }

    (** Module for manipulating the Map (Node -> (set , set , label)) *)
    module NodeMap = Map.Make(Unique)

    (** Module for manipulating pairs of elements used mainly to represent edges *)
    module EdgeSet = TreeSet (struct
        type t = (elt * elt)
        let compare (x, y) (x', y') = match Unique.compare x x' with
            | 0 -> Unique.compare y y'
            | z -> z
    end)

    (** Nodes forming the graph *)
    module Vertex  = struct
        type t          = adj
        let compare     = fun {lab=lnode;_} {lab=rnode;_} -> Unique.compare lnode rnode
        let empty   lbl = {inc=AdjSet.empty; out=AdjSet.empty; lab=lbl; edg=(Weights.create 1)}
        let weights n g = let {edg;_} = NodeMap.find n g in edg
        let edge  f t g = Weights.find (weights f g) t
        let edge2   t o = Weights.find o t
        let edgeo  f t g= Weights.find_opt (weights f g) t
        let update t o v= Weights.replace o t v
        let ensure t o v= if Weights.mem o t then () else Weights.add o t v
    end

    (** Adjacency list graph definition **)
    type +'a t     = (Vertex.t) NodeMap.t

    (** An empty graph *)
    let empty      = NodeMap.empty

    (** Add a new node with its label -> ( ... , nodedata), this will replace
        existing entries *)
    let add nodekey nodeMap =
        NodeMap.add nodekey (Vertex.empty nodekey) nodeMap
    ;;

    (** only adds and updates if the value was not already present, otherwise
        leaves as is to avoid overwriting *)
    let ensure nodeKey nodeMap =
        NodeMap.update nodeKey (fun v -> match v with
            | None -> Some (Vertex.empty nodeKey)
            | v'   -> v'
        ) nodeMap
    ;;

    (** same as ensure but verifies a sequence of elements *)
    let ensureall nodeseq nodeMap =
        Seq.fold_left (Fun.flip ensure) nodeseq nodeMap
    ;;

    (** only adds and updates if the value was not already present, otherwise
        leaves as is but adds edge from nodeKey as a contingency *)
    let ensureof nodeFrom nodeKey nodeMap =
        NodeMap.update nodeKey (fun v -> match v with
            | None    ->
                let v' = Vertex.empty nodeKey in
                Some { v' with inc=(AdjSet.add nodeFrom v'.inc) }
            | Some v' ->
                Some { v' with inc=(AdjSet.add nodeFrom v'.inc) }
        ) (add nodeFrom nodeMap)
    ;;

    (** directly place elements into the graph - does not check if all edges
       are resolvable *)
    let emplace {inc;out;edg;_} nodekey nodeMap =
        NodeMap.add nodekey {inc;out;edg;lab=nodekey} nodeMap
    ;;

    (** directly place elements into the graph - ensures edges exist and
       adds them otherwise *)
    let emplaceadj {inc;out;edg;_} nodekey nodeMap =
        nodeMap
        |> (AdjSet.fold (fun el ac -> ensureof nodekey el ac) inc)
        |> (AdjSet.fold (fun el ac -> ensureof el nodekey ac) out)
        |> NodeMap.add nodekey {inc;out;edg;lab=nodekey}
    ;;

    (**
    Add a directed edge [(tail)] --> [(head)] such that the tails outgoing
    set points to the heads incoming set. nonexistent nodes don't resolve
    and no error is raised in that case - it is up to the user to ensure the nodes already exist in
    the graph e.g using `Graph.ensure` function

    Find the tail of the directed edge
      Update with outgoing
        Find the head of the directed edge
          Update with incoming
    *)
    let add_edge nodeFrom nodeTo nodeMap =
        (*(NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label, wgts) = x in*)
        (NodeMap.update nodeFrom (fun x -> let* { out=frOutgoing; _ } as a = x in
            Some { a with out=(AdjSet.add nodeTo frOutgoing) }) nodeMap)
        |> NodeMap.update nodeTo (fun x -> let* { inc=toIncoming; _ } as b = x in
            Some { b with inc=(AdjSet.add nodeFrom toIncoming) })
    ;;

    (** Bidirectionally Find the tail of the directed edge
        Update with outgoing and incoming
        Find the head of the directed edge
        Update with incoming and outgoing*)
    let add_edge2 nodeFrom nodeTo nodeMap =
        (*(NodeMap.update nodeFrom (fun x -> let* (fromIncoming, fromOutgoing, label, wgts) = x in*)
        (NodeMap.update nodeFrom (fun x -> let* { out=frOutgoing;inc=frIncoming;_ } as a = x in
            Some { a with out=(AdjSet.add nodeTo frOutgoing);   inc=(AdjSet.add nodeTo frIncoming) }) nodeMap)
        |> NodeMap.update nodeTo (fun x -> let* { inc=toIncoming;out=toOutgoing;_ } as b = x in
            Some { b with inc=(AdjSet.add nodeFrom toIncoming); out=(AdjSet.add nodeFrom toOutgoing) })
    ;;

    (** Find the tail of the directed edge
        Update with outgoing
        Find the head of the directed edge
        Update with incoming
        Add nodeFrom nodeTo with weight values on from end  *)
    let add_weight weightValue nodeFrom nodeTo nodeMap =
        (NodeMap.update nodeFrom (fun x -> let* { out=frOutgoing; edg=wgts; _ } as a = x in
            let _  = Weights.add wgts nodeTo weightValue in
            Some { a with out=(AdjSet.add nodeTo frOutgoing) }) nodeMap)
        |> NodeMap.update nodeTo (fun x -> let* { inc=toIncoming; _ } as b = x in
            Some { b with inc=(AdjSet.add nodeFrom toIncoming) })
    ;;

    (** Find the tail of the directed edge
        Update with outgoing
        Find the head of the directed edge
        Update with incoming
        Add bidirectional nodeFrom nodeTo with weight values on both ends  *)
    let add_weight2 weightValue nodeFrom nodeTo nodeMap =
        (NodeMap.update nodeFrom (fun x -> let* { inc=frIncoming; out=frOutgoing; edg=wgts; _ } as a = x in
            let _  = Weights.add wgts nodeTo weightValue in
            Some { a with inc=(AdjSet.add nodeTo frIncoming); out=(AdjSet.add nodeTo frOutgoing) }) nodeMap)
        |> NodeMap.update nodeTo (fun x -> let* { inc=toIncoming; out=toOutgoing; edg=wgts; _ } as b = x in
            let _  = Weights.add wgts nodeFrom weightValue in
            Some { b with inc=(AdjSet.add nodeFrom toIncoming); out=(AdjSet.add nodeFrom toOutgoing); })
    ;;

    (** create edges from nodeFrom to all nodeTo in nodeToList *)
    let rec add_all nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | nodeTo :: rest -> add_edge nodeFrom nodeTo (add_all nodeFrom rest nodeMap)
    ;;

    (** same as add_all except it add bidirectional edges which can be
        interpreted as undirected edges too *)
    let rec add_all2 nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | nodeTo :: rest -> add_edge2 nodeFrom nodeTo (add_all2 nodeFrom rest nodeMap)
    ;;

    (** creates a complete graph without erasing existing edges *)
    let complete nodeMap =
        NodeMap.fold (fun el {out;_} ac ->
            NodeMap.fold (fun el' _ ac' ->
                if AdjSet.mem el' out then
                    ac'
                else
                    add_edge2 el el' ac'
            ) nodeMap ac
        ) nodeMap nodeMap
    ;;

    (** creates edges from nodeFrom to all nodeTo in the list with their edge
        values alongside *)
    let rec allweighted nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | (nodeTo, nodeVal) :: rest -> add_weight nodeVal nodeFrom nodeTo
            (allweighted nodeFrom rest nodeMap)
    ;;

    (** same as allweighted but for bidirectional or undirected graphs *)
    let rec allweighted2 nodeFrom nodeToList nodeMap = match nodeToList with
        | [] -> nodeMap
        | (nodeTo, nodeVal) :: rest -> add_weight2 nodeVal nodeFrom nodeTo
            (allweighted2 nodeFrom rest nodeMap)
    ;;

    (** number of nodes in the graph *)
    let cardinal = NodeMap.cardinal;;

    (** Creates a graph given a node and outgoing edge, incoming edges will be
       resolved naturally. All nodes should already be available in the map  *)
    let rec of_list adjList nodeMap = match adjList with
        | [] -> nodeMap
        | (nodeFrom, nodeJoinList) :: rest ->
            add_all nodeFrom nodeJoinList (of_list rest nodeMap)
    ;;

    (** Creates a graph given a node and outgoing edge (bidir), incoming edges will be
       resolved naturally. All nodes should already be available in the map  *)
    let rec of_list2 adjList nodeMap = match adjList with
        | [] -> nodeMap
        | (nodeFrom, nodeJoinList) :: rest ->
            add_all2 nodeFrom nodeJoinList (of_list2 rest nodeMap)
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
    let incomingof node game = let {inc;_} = NodeMap.find node game in inc

    (** incoming edges with respective weights *)
    let incweights node game = let {inc;edg;_} = NodeMap.find node game in (inc, edg)

    (** [ incomingof identity (Graph.t) AdjSet.t]
    Outgoing set of nodes *)
    let outgoingof node game = let {out;_} = NodeMap.find node game in out

    (** outgoing edges with respective weights *)
    let outweights node game = let {out;edg;_} = NodeMap.find node game in (out, edg)

    (** both incoming and outgoing edges of a graph - self edges included *)
    let neighbours node game = let {inc;out;_} = NodeMap.find node game in (AdjSet.union inc out)

    (** both incoming and outgoing edges of a graph - self edges NOT included *)
    let xorneighbors node game = let {inc;out;_} = NodeMap.find node game in
        AdjSet.remove node (AdjSet.union inc out)
    ;;

    (** items in both incoming and outgoing edges of a graph - self edges included *)
    let mutuals node game = let {inc;out;_} = NodeMap.find node game in
        (AdjSet.inter inc out)
    ;;

    (** items in both incoming and outgoing edges of a graph - self edges NOT included *)
    let xormutuals node game = let {inc;out;_} = NodeMap.find node game in
        AdjSet.remove node (AdjSet.inter inc out)
    ;;

    (** degree of incoming nodes *)
    let incdeg node nodeMap =
        AdjSet.cardinal @@ incomingof node nodeMap
    ;;

    (** degree of outgoing nodes *)
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
        NodeMap.for_all (fun _ {inc;out;_} ->
            (AdjSet.cardinal out + AdjSet.cardinal inc) mod 2 == 0) nodeMap
    ;;

    (** eulerian circuit test for directed graphs (all edges visited exactly once) *)
    let dircircuit nodeMap =
        NodeMap.for_all (fun _ {inc;out;_} ->
            (AdjSet.cardinal out) == (AdjSet.cardinal inc)) nodeMap
    ;;

    (** undirected eulerian path for undirected graph return start and end points *)
    let undeulpath nodeMap =
        let (even, odd, sz, pnts) = NodeMap.fold (
            fun  k {inc;out;_} (even, odd, sz', (l, r)) ->
            let deg = (AdjSet.cardinal out + AdjSet.cardinal inc) in
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
            (fun  k {inc;out;_} (din, dout, deq, cn, (start, fin)) ->
                let indeg = AdjSet.cardinal inc in
                let oudeg = AdjSet.cardinal out in
                let io = ((indeg - oudeg) = 1)  in
                let ou = ((oudeg - indeg) = 1)  in
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

    (** all edges as pairs in the form (from, to) for edge from -> to *)
    let allpairs nodeMap =
        NodeMap.fold (fun k {out;_} ac ->
            AdjSet.fold (fun el ac' -> (k, el) :: ac') out ac)  nodeMap []
    ;;

    (** all edges and respective weights *)
    let allweights nodeMap =
        NodeMap.fold (fun k {out;edg;_} ac ->
            AdjSet.fold (fun el ac' -> (k, el, Weights.find edg el) :: ac') out ac)  nodeMap []
    ;;

    (** Removes a node from the graph - weights aren't altered and may still be
    available from an opposite end of the edge depending oon how the graph is
    structured *)
    let remove delnode nodeMap =
        let {inc=incoming;out=outgoing;_} = (NodeMap.find delnode nodeMap) in
        NodeMap.remove delnode (AdjSet.fold (fun nodelabel updatemap ->
            NodeMap.update nodelabel (fun x ->
                let* {inc=deepinc;out=deepout;_} as a = x in
                Some {
                        a with
                        inc=AdjSet.remove delnode deepinc;
                        out=AdjSet.remove delnode deepout;
                }
            ) updatemap
        ) (AdjSet.union incoming outgoing) nodeMap)
    ;;

    (** Removes all nodes from the graph - weights aren't altered and may still be
    available from an opposite end of the edge depending oon how the graph is
    structured *)
    let removeall delnodelist nodeMap =
        List.fold_left (fun acc delnode -> remove delnode acc) delnodelist nodeMap
    ;;

    (** edge removal, call twice flipped if undirected graph *)
    let remove_edge nodeMap nodeFrom nodeTo =
        (NodeMap.update nodeFrom (fun x -> let* {out=fromOutgoing;_} as a = x in
            Some { a with out=(AdjSet.remove nodeTo fromOutgoing) }) nodeMap)
        |>
        (NodeMap.update nodeTo (fun x -> let* {inc=toIncoming;_} as b = x in
            Some { b with inc=(AdjSet.remove nodeFrom toIncoming) }))
    ;;

    (** Remove self edges from a graph - TODO: could be more efficient - perhaps *)
    let cull graph =
        graph
        |> NodeMap.to_seq
        |> Seq.filter (fun (elt, {out; _}) -> AdjSet.mem elt out)
        |> Seq.fold_left (fun g (elt, ({inc; out; _} as a)) ->
            NodeMap.update elt (fun _ ->
                Some { a with inc=(AdjSet.remove elt inc); out=(AdjSet.remove elt out); }
            ) g
        ) graph
    ;;

    (** find "dangling" nodes in the graph with no incoming or outgoing edges *)
    let allfree graph =
        graph
        |> NodeMap.to_seq
        |> Seq.filter_map (fun (elt, {out;inc;_}) ->
            if AdjSet.is_empty inc && AdjSet.is_empty out then
                Some elt
            else
                None
        )
        |> Seq.fold_left (Fun.flip AdjSet.add) AdjSet.empty
    ;;

    (** Remove "free" vertices from a graph (no inc or out) *)
    let prune graph =
        graph
        |> NodeMap.to_seq
        |> Seq.filter (fun (_, {out;inc;_}) -> AdjSet.is_empty inc && AdjSet.is_empty out)
        |> Seq.fold_left (fun g (elt, _) -> NodeMap.remove elt g) graph
    ;;

    (** a graph traversal context *)
    type 'b ctx = {
        (* whether to stop a recurse or traversal *)
        stop: bool;
        (* the previous node vertex data *)
        prev: (elt * adj) option;
        (* the current node *)
        elt:  elt;
        (* the visited nodes so far in the traversal *)
        vis:  elt AdjSet.set;
        (* the accumulator *)
        acc:  'b;
        (* the node vertex information *)
        vtx:  adj;
    }

    let mkctx p l v a s o =  {prev=p; elt=l; vis=v; acc=a;stop=s; vtx=o;}
    ;;

    (** breadth first search starting from start node applying f until stop is
        true or queue is empty applying f on each node and b on backtrack
        filters already visited nodes, in bidirectional cases,
        backedges will not be visited twice
    *)
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
                let {out;_} = vtx in
                let {stop;vis=vis';acc=acc';_} = f que (mkctx (prev) label vis acc false vtx) in
                let (vis'', acc'') = if stop then (vis', acc') else
                    let diff= AdjSet.diff out vis' in
                    let _   = AdjSet.iter (fun x -> Queue.add ((Some (label, vtx)), x) nxt) (diff) in
                    iter (AdjSet.union diff vis') nxt acc'
                in (vis'', (b que (mkctx prev label vis'' acc'' stop vtx)).acc)
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
            if Stack.is_empty nxt then
                (vis, acc)
            else
                let (prev, label) = Stack.pop nxt in
                if AdjSet.mem label vis then
                    iter (vis) nxt acc
                else
                    let vtx = vertexof label graph in
                    let {out;_} = vtx in
                    (* callback can functionally rewrite these values *)
                    let {stop;acc=acc';vis=vis';_} = f nxt (mkctx (prev) label vis acc false vtx) in
                    let (vis'', acc'') = (
                        if stop then
                            (vis', acc')
                        else
                            let _   = AdjSet.iter (fun x -> Stack.push ((Some (label, vtx)), x) nxt) (out) in
                            iter (AdjSet.add label vis') nxt acc'
                    ) in
                    let { acc=acc'''; vis=vis'''; _ } = (b nxt (mkctx (prev) label vis'' acc'' stop vtx)) in
                    (vis''', acc''')
        in let (_, acc) = iter visited stck init in acc
    ;;

    (** Get adjacency list of a node *)
    let adj_list_of node nodeMap =
        let {inc=incoming;out=outgoing; _}= NodeMap.find node nodeMap in
        AdjSet.to_list (AdjSet.union incoming outgoing)
    ;;

    (** Get adjacency list of a node *)
    let adj_seq_of node nodeMap =
        let {inc=incoming;out=outgoing; _}= NodeMap.find node nodeMap in
        AdjSet.to_seq (AdjSet.union incoming outgoing)
    ;;

    (** collect graph into a simple [(key,  adj list), ...] *)
    let outlist nodeMap =
        NodeMap.fold (fun elt {out;_} acc ->
            (elt, out) :: acc
        ) nodeMap []
    ;;

    (* rev compat *)
    let _find_index p =
      let rec aux i = function
        [] -> None
        | a::l -> if p a then Some i else aux (i+1) l in
      aux 0
    ;;

    let _init_matrix sx sy f =
      let res = Array.make sx [||] in
      (* We must not evaluate [f x 0] when [sy <= 0]: *)
      if sy > 0 then begin
        for x = 0 to pred sx do
          let row = Array.make sy (f x 0) in
          for y = 1 to pred sy do
            Array.unsafe_set row y (f x y)
          done;
          Array.unsafe_set res x row
        done;
      end;
      res
    ;;

    (** convert a graph to a 2d array and a key list defining the elt for each array index *)
    let adjmatrix nodeMap =
        let sz   = NodeMap.cardinal nodeMap in
        let keys = NodeMap.bindings nodeMap in
        (* map outgoing edges to indices, return each elt and its index along
           with the indexes of outgoing edges

            {
               "1":["2","3"]
               "2":["1"]
               "3":[]
                ....
            }

            becomes

                idx node out
            [
                ((0, "1"), [1,2]),
                ((1, "2"), [0]),
                ((2, "3"), []),
            ]

        *)
        let adjs = List.mapi (fun i (k, {out=o;_}) ->
            ((i, k), AdjSet.fold (fun elt acc ->
                (* return the index of the outgoing edge *)
                Option.get (
                    _find_index (fun (b, _) -> equal b elt) keys
                ) :: acc
            ) o [])
        ) keys in
        let b = _init_matrix sz sz (fun row col ->
            (* bool value of whether j is in the outgoing index *)
            Bool.to_int @@ List.mem col (snd @@ List.nth adjs row)
        ) in
        (* return matrix and keys showing which elt is which index *)
        (b, List.map (fst) adjs)
    ;;

    (** convert a graph to 2d weights (edge values) and a key list defining the elt for each array index *)
    let wgtmatrix transform defval nodeMap =
        let sz   = NodeMap.cardinal nodeMap in
        let keys = NodeMap.bindings nodeMap in
        (* map each key to its index and list of outgoing indices *)
        let (adjs) = List.mapi (fun i (k, {out=o;edg=wgt;_}) ->
            (* any outgoing node has to be in the keys list *)
            ((i, k), AdjSet.fold (fun elt acc ->
                (* return the index of the outgoing edge *)
                (Option.get (
                    (_find_index (fun (b, _) -> equal b elt) keys)
                ), Vertex.edge2 elt wgt) :: acc
            ) o [])
        ) keys in
        let b = _init_matrix sz sz (fun i j ->
            (* bool value of whether j is in the outgoing index *)
            let v = List.find_opt (fun (idx, _) -> idx = j) @@ (snd @@ List.nth adjs i)
            in match v with
                | Some (_, msr) -> transform msr
                | _             -> defval
        ) in
        (* return matrix and keys showing which elt is which index *)
        (b, List.map (fst) adjs)
    ;;

    (** create a graph from 2d adjacency matrix with keys defining which elt maps to what index *)
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
        ) (index, []) nodeMatrix in of_list adj_list g
    ;;

    (** inout degree matrix along the diagonal - can be used to construct a
       laplacian *)
    let degmatrix nodeMap =
        let sz, adjs = NodeMap.fold (fun key {inc;out;_} (idx, acc) ->
            (idx + 1), (((idx, key), (AdjSet.cardinal inc + AdjSet.cardinal out)) :: acc)
        ) nodeMap (0, []) in
        let b = _init_matrix sz sz (fun i j ->
            if i == j then
                snd @@ List.nth adjs i
            else
                0
        ) in
        (* return matrix and keys showing which elt is which index *)
        (b, List.map (fst) adjs)
    ;;

    (** in and out degree for each elt *)
    let degtable tbl nodeMap =
        NodeMap.iter (fun k {inc;out;_} ->
            Hashtbl.add tbl k (AdjSet.cardinal inc, AdjSet.cardinal out)
        ) nodeMap
    ;;

    (** create a set of all edges *)
    let edgeset nodeMap =
        NodeMap.fold (fun el {out=ou;_} a ->
            AdjSet.fold (fun nx a' -> EdgeSet.add (el, nx) a') ou a
        ) nodeMap EdgeSet.empty
    ;;

    (** create a sequence of edges *)
    let edgeseq nodeMap =
        (NodeMap.to_seq nodeMap)
        |> Seq.map (fun (el, {out=ou;_}) ->
            AdjSet.to_seq ou |> Seq.map (fun nx -> (el, nx))
        ) |> Seq.concat
    ;;

    (** create a sequence of edges and weight values *)
    let edgewgtseq nodeMap =
        (NodeMap.to_seq nodeMap)
        |> Seq.map (fun (el, {out=ou;edg;_}) ->
            AdjSet.to_seq ou |> Seq.map (fun nx -> (el, nx, Vertex.edge2 nx edg))
        ) |> Seq.concat
    ;;

    (** like edgeset but returns the size to avoid recomputing if needed. returns
       the node count, edge count and edge set in that order *)
    let edgesetsz nodeMap =
        NodeMap.fold (fun el {out=ou;_} (g, e, a) ->
        let e', a' =  AdjSet.fold (fun nx (e', a') -> e'+1, EdgeSet.add (el, nx) a') ou (e, a) in
            g+1, e', a'
        ) nodeMap (0, 0, EdgeSet.empty)
    ;;

    (** graph -> incidence matrix, node array, elt array
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
        _init_matrix r c (fun r' c' ->
            if belongs nodes.(r') edges.(c') then
                1
            else
                0
        ), nodes, edges
    ;;

    (** swap the incoming and outgoing edge direction - preserving edge weights
        use transpose2 if you do not use edge weights! *)
    let transpose (nodeMap: adj NodeMap.t) =
        NodeMap.map (fun {inc; out; lab; edg=wgts} ->
            let wgts' = Weights.create (Weights.length wgts) in
            let _     = AdjSet.iter (fun x ->
                Weights.add wgts' x
                @@ Weights.find (Vertex.weights x nodeMap) lab
            ) inc in {inc=out; out=inc; lab; edg=wgts'}
        ) nodeMap
    ;;

    (** swap the incoming and outgoing edge direction - WITHOUT preserving edge weights *)
    let transpose2 (nodeMap: adj NodeMap.t) =
        NodeMap.map (
            fun {inc; out; lab; edg=wgts} -> {inc=out; out=inc; lab; edg=wgts}
        ) nodeMap
    ;;

    (** toposort (happens-before) - assumes the graph is acyclic (DAG) otherwise the
        result is wrong. *)
    let toposort graph =
        snd @@ NodeMap.fold (fun x _y (v, a) ->
            if AdjSet.mem x v then
                (v, a)
            else
                dfs (fun _ s -> { s with stop=false }) (fun _ s ->
                    if AdjSet.mem s.elt (fst s.acc) then
                        s
                    else
                        { s with
                            acc = AdjSet.add s.elt (fst s.acc), s.elt :: (snd s.acc)
                        }
                ) graph x (v, a)
        ) graph (AdjSet.empty, [])
    ;;

    (** whether edge from f to t exists in nodeMap *)
    let has_edge f t nodeMap =
        AdjSet.mem t (outgoingof f nodeMap)
    ;;

    (** acyclic if i cant form a cycle from each element *)
    let is_acyclic graph =
        NodeMap.for_all (fun x _y ->
            not @@ dfs (fun _ s ->
                let cyc = (AdjSet.mem x s.vtx.out) in
                { s with acc=cyc; stop=cyc }
            ) (fun _ -> Fun.id) graph x false
        ) graph
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

        (** creates a Map of (ints -> ([idx...], Graph.t)) where the int is the link value.
           it computes its neighbours into the list section of the Map value.
           This makes more idiomatic to the outer graph structure which is also
           a map albeit with different values
        *)
        let induced nodeMap sccs =
            SccTbl.fold (fun {link=lowlink;_} elt acc ->
                let edges = NodeMap.find elt nodeMap in
                let {out; _} = edges in
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

        (** data holding the solution for strongly connected components *)
        type solution =
            {
                (* discovered nodes *)
                disc: sccnode SccSet.set;
                (* visited nodes or nodes on the 'stck' of this solution *)
                onst: elt     AdjSet.set;
                (* stacked nodes *)
                stck: sccnode list;
                (* mapping from link values to nodes in an SCC *)
                sccs: elt     SccTbl.t;
                (* discovery time *)
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
                sccs = SccTbl.create size;
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
            ) graph (empty 8)
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
            let rec iter node {out;_} scc =
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
            let fscc = NodeMap.fold (iter) graph (empty 8) in
            (* transpose the graph *)
            let tgraph = transpose2 graph in
            let iter2 scc sccnode =
                (* if onst the we consider it already a part of an scc *)
                if AdjSet.mem sccnode.node scc.onst then
                    scc
                else
                    let _ = incr count in
                    (* find all reachable nodes *)
                    let vstd = dfs (fun _ -> Fun.id)
                        (fun _ s' -> { s' with acc=s'.vis }) tgraph sccnode.node (AdjSet.empty)
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
            let keys = NodeMap.fold (fun el _ acc -> AdjSet.add el acc) graph AdjSet.empty in
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
            let keys = NodeMap.fold (fun el _ acc -> AdjSet.add el acc) graph AdjSet.empty in
            bk (AdjSet.empty) (keys) (AdjSet.empty) []
        ;;

    end

    (*************************************************************************
    *                           Spanning Trees                               *
    **************************************************************************)
    (** Minimum Spanning Tree algorithms *)
    module Span(Measure: Measurable with type edge = edge) = struct

        let ecompare e e' = wcompare (Measure.compare) (Measure.measure e) (Measure.measure e')

        module EdgeDisj = MakeDisjointSet (struct
            type t    = elt
            let equal = equal
            let hash  = Hashtbl.hash
        end)

        module EdgeHeap = MakeFibHeap (struct
            type t = (elt * elt * edge)
            type order  = edge

            let bind (_, _, e) = e
            let compare (x, y, _) (x', y', _) =
                let xc = Unique.compare x x' in
                let yc = Unique.compare y y' in
                match  xc with
                | 0 ->  yc
                | x ->  x
            ;;
            let order =  ecompare
        end)

        (** Total cost from summing all edges in the minimum spanning tree  *)
        let cost mst =
            edgewgtseq mst
            |> Seq.fold_left (fun acc (_, _, ed) ->
                wapply (Measure.add acc) (Measure.measure ed)
            ) (Measure.zero)
        ;;

        (** Prims minimum spanning tree algo
           only works for undirected graphs!! *)
        let prim start graph =
            let vis = AdjSet.empty in
            let rec iter node vis pheap mst =
                let out, wgts = outweights node graph in
                let pheap'    = AdjSet.fold (fun el ac ->
                    EdgeHeap.insert (node, el, (Vertex.edge2 el wgts)) ac
                ) (AdjSet.diff out vis) pheap in
                match EdgeHeap.extract_opt pheap' with
                | Some ((c, n, e), pheap'') ->
                    if AdjSet.mem n vis then
                        iter n (AdjSet.add node vis) pheap'' mst
                    else
                        let mst' = add_weight e c n (ensure c @@ ensure n mst) in
                        iter n (AdjSet.add node @@ AdjSet.add n vis) pheap'' mst'
                | _ ->
                    mst
            in iter start vis EdgeHeap.empty empty
        ;;

        (** kruskals minimum spanning tree using disjoint set: connect describes
            how you add edges into the new graph. assumes the graph has no
            disconnected nodes *)
        let kruskal ?(connect=add_weight) graph =
            let nodeseq = (Seq.map (fst) @@ NodeMap.to_seq graph) in
            let wgts = EdgeDisj.create (cardinal graph) nodeseq in
            edgewgtseq graph
            |> List.of_seq
            |> List.fast_sort (fun (_,_,x) (_,_,y) ->
                ecompare x y
            )
            |> List.fold_left (fun acc (cur, nxt, edj) ->
                if EdgeDisj.find cur wgts = EdgeDisj.find nxt wgts then
                    acc
                else
                    let _ = EdgeDisj.union wgts cur nxt in
                    connect edj cur nxt (ensure cur @@ ensure nxt acc)
            ) empty
        ;;

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
                let  compare l r= match  Unique.compare l.next r.next with
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

               [("G", "E", `Val 7.); ("S", "A", `Val 7.); ("B", "D", `Val 6.);
                ("H", "F", `Val 6.); ("B", "A", `Val 5.); ("H", "G", `Val 5.);
                ("C", "L", `Val 5.); ("S", "C", `Val 3.); ("B", "H", `Val 3.);
                ("S", "B", `Val 2.); ("S", "S", `Val 0.)]

                we interlink on edges and reverse the path

               [("S", "S", `Val 0.); ("S", "B", `Val 2.); ("B", "H", `Val 3.);
                ("H", "G", `Val 5.); ("G", "E", `Val 7.)]

                due to commonality of the from origin
            *)
            let rec dijkresolve target acc = function
                | [] -> acc
                | (from, next, value) :: rest ->
                    if equal target next then
                        (dijkresolve[@tailcall]) from ((next, value) :: acc) rest
                    else
                        (dijkresolve[@tailcall]) target acc rest
            ;;

            (** Single source shortest path implementation of dijkstra *)
            let dijkstra start target graph =
                (* we take the path to ourselves as 0 *)
                let startp = (viapath start start start (`Val Measure.zero)) in
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
                                       previous we 'override it' in the set and
                                       decrease the value in the heap *)
                                    if wcompare (Measure.compare) alt v.value = -1 then
                                        (PathSet.add pe p, PathHeap.decrease pe pe a)
                                    else
                                        (p, a)
                                | None   ->
                                    (* First sighting *)
                                    (PathSet.add pe p, PathHeap.decrease pe pe a)
                            ) out (ps, rest)
                            in iter ps'' h' ((u.via, u.next, u.value) :: elp)
                in dijkresolve target [] @@ iter (PathSet.singleton startp) init []
            ;;

            (** shortest path with heuristics called on a node to align dijkstra outputs *)
            let astar heuristic start target graph =
                (* we take the path to ourselves as 0 *)
                let startp = (mkpath start start (`Val Measure.zero)) in
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
                                    (* First sighting - insert instead of
                                       decrease with dup  *)
                                    (PathSet.add pe p, PathHeap.insert pe a)
                            ) out (ps, rest)
                            in iter ps'' h' ((u.from, u.next, u.value) :: elp)
                in dijkresolve target [] @@ iter (PathSet.singleton startp) init []
            ;;

            (* similar idea to dijkstraresolve *)
            let bellresolve start target nodes =
                if PathSet.is_empty nodes then [] else
                    let rec iter target cycle acc =
                        let {from; next=nxt; value;_} = PathSet.find_first (fun {from;_} -> equal from target) nodes in
                        if equal start from then
                            (from, value) :: acc
                        else
                        if equal start nxt then
                            let  {from=from'; value=value';_} = PathSet.find_first (fun {from;_} -> equal from start) nodes in
                            (from', value') :: ((from, value) :: acc)
                        else
                        if EdgeSet.mem (from, nxt) cycle then
                            failwith "cycle in resolution"
                        else
                            iter nxt (EdgeSet.add (from, nxt) cycle) ((from, value) :: acc)
                    in  iter target EdgeSet.empty []
            ;;

            (*  how bellman ford updates values *)
            let belliter belltable maxsz graph =
                let rec iter sweep =
                    if sweep = maxsz then () else
                        let _ = NodeMap.iter (
                            fun elt {inc; out;edg=wgts;_} ->
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
                in let _  = iter 0 in ()
            ;;

            (**
               the Bellman-Ford algorithm can handle directed and undirected graphs with non-negative weights
               it can only handle directed graphs with negative weights, as long as we don't have negative cycles
               If there is a negative cycle we return it as part of the EdgeSet
            *)
            let bellmanford start target (graph: adj NodeMap.t) =
                (* Set initial distance at `Inf except start *)
                let (sz, lst) = (NodeMap.fold (fun k {edg=w;_} (acc, lst) ->
                    if equal k start then
                        acc + 1,
                        (k, (None, `Val Measure.zero, w)) :: lst
                    else
                        acc + 1,
                        (k, (None, `Inf, w)) :: lst
                ) graph (0, [])) in
                (* Hashtbl for tracking predecessors *)
                let belltable = Hashtbl.create sz in
                let _ = List.iter (fun (k, v) -> Hashtbl.add belltable k v) lst in
                (* upper bound of n - 1 sweeps for convergence *)
                let maxsz = sz - 1 in
                let _  = belliter belltable maxsz graph in
                (* Do an extra iteration to detect a negative cycle by marking
                   edges that relax as part of a negative cycle
                   *)
                let negcycles =  NodeMap.fold (
                    fun elt {inc;out;edg=wgts;_} acc ->
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

            let floydwresolve start target dist pred map =
                (* decode node indices *)
                let  decode idx = snd @@ List.nth map idx in
                let* sidx = List.find_opt (fun (_i, n) -> equal n start)  map in
                let* tidx = List.find_opt (fun (_i, n) -> equal n target) map in
                let  s,e = fst sidx, fst tidx in
                let rec iter at path  =
                    if not (at = s) then
                        let d   = dist.(s).(at) in
                        let at' = pred.(s).(at) in
                        if wcompare (Int.compare) at' `Nan = 0  then
                            (* denote a negative cycle! *)
                            Some ((target, `Nan) :: path)
                        else
                            let v  = decode at in
                            let c' = wapply (Fun.id) at' in
                            iter c' ((v, d) :: path)
                    else
                        Some ((start, `Val Measure.zero) :: path)
                in iter e []
            ;;

            (** All pairs shortest path via Floyd Warshall using a matrix, negative cycles are not detected by
                default  *)
            let floydwarshall ?(negcycles=false) (graph: adj NodeMap.t) =
                (* get distance matrix with non-existent edges at infinity *)
                let dist, map = wgtmatrix (fun x -> `Val x) `Inf graph in

                let sz = List.length map in
                (* reconstruction matrix should point to i by default or null *)
                let next = _init_matrix sz sz (fun i j ->
                    if wcompare (Measure.compare) dist.(i).(j) `Inf != 0 then
                        `Val i
                    else
                        `Nan
                ) in

                let idx = sz - 1 in
                let _ =

                    (for k = 0 to idx do
                        for i = 0 to idx do
                            for j = 0 to idx do
                                let ij    = dist.(i).(j) in
                                let ik    = dist.(i).(k) in
                                let kj    = dist.(k).(j) in
                                let ik_kj = wbind (Measure.add) ik kj in
                                (* min (i->j), ((i->k at k') + (k->j at k')) *)
                                if wcompare (Measure.compare) ik_kj ij = -1 then
                                    let _ = next.(i).(j) <- next.(k).(j) in
                                    dist.(i).(j) <- ik_kj
                            done
                        done
                    done) in

                let _ = if not negcycles then () else
                    (* extra iterations which check if a path is improved to
                       detect a negative cycle similar to bellmanford *)
                    (for k = 0 to idx do
                        for i = 0 to idx do
                            for j = 0 to idx do
                                let ik     = dist.(i).(k) in
                                let kj     = dist.(k).(j) in
                                let ik_kj  = wbind (Measure.add) ik kj in
                                let ik_kj' = dist.(i).(j) in
                                (* compare widist.th current calculation *)
                                if wcompare (Measure.compare) ik_kj ik_kj' = -1 then
                                    let _  = next.(i).(j) <- `NegInf in
                                    dist.(i).(j) <- `NegInf
                            done
                        done
                    done) in
                dist, next, map
            ;;

            (** allpairs shortest paths best suited for sparse weighted directed graphs
                if temp is equal to any value in the graph it will overwrite it
                so it needs to be chosen carefully - we don't check for negative
                cycles here!

                If the graph had negative weights, it will be "reweighted" to
                positive weights such that dijkstra can be applied!. This also*)
            let johnsons temp graph =
                (* temporary external source node *)
                let g', sz = NodeMap.fold (fun e _ (g, sz') ->
                    (add_weight (Measure.zero) temp e g, sz' + 1)
                ) graph (add temp graph, 0) in
                (* also includes the temp vertex *)
                let ht = Hashtbl.create sz in
                (* assume all nodes start at infinity for bellman ford relaxation except temp *)
                let _  = NodeMap.iter (fun k {edg=w;_} ->
                    if equal k temp then
                        Hashtbl.add ht k (None, `Val Measure.zero, w)
                    else
                        Hashtbl.add ht k (None, `Inf, w)
                ) g' in
                (* reweighting with bellman ford *)
                let _  = belliter ht (sz-1) g' in
                (* reweight the graph *)
                let g'' = NodeMap.map (
                    fun {inc; out; lab=u; edg=wgt} ->
                    (* we need to copy the weights as they are globally mutable *)
                    let wgt' = Weights.copy wgt in
                    let (_, d_u, _) = Hashtbl.find ht u in
                    let _ = AdjSet.iter (fun v ->
                        let (_,d_v,_) = Hashtbl.find ht v in
                        let w_uv  = wbind (Measure.add) d_u (`Val (Vertex.edge2 v wgt)) in
                        let w_uv' = wbind (Measure.sub) w_uv d_v in
                        wapply (Vertex.update v wgt') w_uv'
                    ) out in {inc; out; lab=u; edg=wgt'}
                ) g' in
                (* A way to restore weights from f -> t *)
                let restore f t edgev =
                    let (_,h_v,_) = Hashtbl.find ht t in
                    let (_,h_u,_) = Hashtbl.find ht f in
                    wapply (Measure.add edgev) (wbind (Measure.sub) h_v h_u) in
                (g'', restore)
            ;;

        end

        (** construct an eulerian path *)
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

        (** single source depth first walk until (f ctx -> bool) is true or all nodes
           are exhausted. Requires the node edges to be weighted  Uses simple
           dfs to append edges and their corresponding weights to a list *)
        let naivedfs graph f start =
            List.rev @@ dfs (fun _ s -> (
                match s.prev with
                | Some (prev, _adj) ->
                    ({
                        s with stop=f s;
                        acc={from=prev; next=s.elt; via=prev;
                            value=(Vertex.edgeo prev s.elt graph);} :: s.acc
                    })
                | None ->
                    { s with stop = f s; }
            )) (Fun.const Fun.id) graph start []
        ;;

        (** single source breadth first walk until (f ctx -> bool) is true or all nodes
           are exhausted. Requires the node edges to be weighted. Uses simple
           bfs to append edges and their corresponding weights to a list *)
        let naivebfs graph f start =
            List.rev @@ bfs (fun _q s -> (
                match s.prev with
                | Some (prev, _adj) ->
                    {  s with stop=f s; acc = (
                        {
                            from=prev; next=s.elt; via=prev;
                            value=(Vertex.edgeo prev s.elt graph);
                        } :: s.acc
                    )}
                | None ->
                    { s with stop = f s; }
            )) (Fun.const Fun.id) graph start []
        ;;
    end

    (** Graph flow algorithms *)
    module Flow(Measure: Measurable with type t = edge and type edge = edge) = struct
        (* NB: Capacity is always non negative *)
        type measure = Measure.t wrap

        type state = {
            mutable flow: edge
        }

        let mkstate f = { flow=f }
        let zero = Measure.zero

        (* measure Captbl.t - holds current filled capacity *)
        module Flowtbl = Hashtbl.Make (struct
            type t = (elt * elt)
            let equal (x, y) (x', y') = match Unique.compare x x' with
                | 0 -> (Unique.compare y y') = 0
                | _ -> false
            let hash = Hashtbl.hash
        end)

        (** dfs ford fulkerson - exists to grok the idea behind flow
            pushes flow to the sink if there is any available capacity along a
            chosen path until no more flow can be pushed. residual edges are
            added so flow can be `pushed back` if there is capacity towards the
            opposite direction. Path finding is the main addition here to ford
            fulkerson

           TODO: Capacity scaling or Dinics level graph heuristic ???
           NB: This algo does not always terminate
        *)
        let fordfulkerson ?(maxit=Int.max_int) cap source sink graph =
            (* if there are flow values already in the tbl we keep it - it can
               reduce the total number of iterations since this algo depends on
               Flow capacity *)
            let _ = edgeseq graph |> Seq.iter (fun (k, v) ->
                match Flowtbl.mem cap (k,v), Flowtbl.mem cap (v, k) with
                | (true, true)  ->
                    ()
                | (true, false) ->
                    Flowtbl.replace cap (v,k) (mkstate zero)
                | (false, true) ->
                    Flowtbl.replace cap (k,v) (mkstate zero)
                | _             ->
                    let _ = Flowtbl.replace cap (k,v) (mkstate zero) in
                    Flowtbl.replace cap (v,k) (mkstate zero)
            ) in

            (* back edges need to be available *)
            let graph' = NodeMap.fold (fun elt {out;inc;_} acc ->
                AdjSet.fold (fun elt' acc' ->
                    (* if on incoming then assume edge already exists *)
                    if AdjSet.mem elt' inc then
                        acc'
                    else
                        add_weight Measure.zero elt' elt acc'
                ) out acc
            ) graph graph in

            let break = ref false in

            let rec iter prev node flow vis path =
                if equal node sink then
                    (flow, AdjSet.add node vis, Seq.cons (prev, sink) path)
                else if !break then
                    (flow, AdjSet.add node vis, path)
                else
                    let {out;edg;_} = NodeMap.find node graph' in
                    (* TODO: refactor to favor 'take_while' style *)
                    let (flow', vis', path') = AdjSet.fold (fun el (flow', vis', path') ->
                        if !break || AdjSet.mem el vis' then
                            (flow', vis', path')
                        else
                            let mcap = Vertex.edge2 el edg in
                            let cflw = Flowtbl.find cap (node, el) in
                            let favl = Measure.sub mcap cflw.flow in
                            if Measure.compare favl Measure.zero = 1 then
                                let (flow'', vis'', path'') = iter (Some (node, mcap)) el (`Val favl) vis' path' in
                                (* is there an augmenting path *)
                                if wcompare (Measure.compare) flow'' (`Val Measure.zero) = 1 then
                                    let _ = break := true in
                                    (flow'', vis'', path'')
                                else
                                    (flow', vis'',  path')
                            else
                                (flow', vis', path')
                    ) out (flow, (AdjSet.add node vis), path) in

                    if !break then
                        let minflow = (wmin (Measure.compare) flow flow') in
                        (minflow, vis', Seq.cons (prev, node) path')
                    else
                        (`Val Measure.zero, vis', path')
            in
            let maxiter = ref 0 in
            (* setup a loop *)
            let rec terminal maxf =
                if !maxiter = maxit then
                    raise Not_found
                else
                    (* reset flow control *)
                    let _ = break := false in
                    let (x, _y, z) = (iter None source `Inf AdjSet.empty Seq.empty) in
                    let _ = Seq.iter (fun (f, t) ->
                        match f with
                        | Some (f',_) ->
                            let fwd  = Flowtbl.find cap (f', t)  in
                            let bwd  = Flowtbl.find cap (t, f')  in
                            let _    = fwd.flow <- wapply (Measure.add fwd.flow) x in
                            let _    = bwd.flow <- wapply (Measure.sub bwd.flow) x in
                            ()
                        | _ -> ()
                    ) z in
                    if wcompare (Measure.compare) x (`Val Measure.zero) = 0 then
                        maxf
                    else
                        (terminal[@tailcall]) @@ wbind (Measure.add) maxf x
            in terminal (`Val Measure.zero)
        ;;

        (** Edmund karp - depends more on Edges and Vertices and not the
           capacity values itself, mostyly like ford fulkerson.
           tends to find shorter paths with bfs whereas dfs (in ford fulkerson above) can zigzag through
           small capacity weights
        *)
        let edmondskarp ?(maxit=Int.max_int) cap source sink graph =

            let _ = edgeseq graph |> Seq.iter (fun (k, v) ->
                (* preserve flow value if they exist already *)
                match Flowtbl.mem cap (k,v), Flowtbl.mem cap (v, k) with
                | (true, true)  ->
                    ()
                | (true, false) ->
                    Flowtbl.replace cap (v,k) (mkstate zero)
                | (false, true) ->
                    Flowtbl.replace cap (k,v) (mkstate zero)
                | _             ->
                    let _ = Flowtbl.replace cap (k,v) (mkstate zero) in
                    Flowtbl.replace cap (v,k) (mkstate zero)
            ) in

            let que = Queue.create () in

            (* back edges (residual) need to be available *)
            let graph' = NodeMap.fold (fun elt {out;inc;_} acc ->
                AdjSet.fold (fun elt' acc' ->
                    if AdjSet.mem elt' inc then
                        acc'
                    else
                        add_weight Measure.zero elt' elt acc'
                ) out acc
            ) graph graph in

            let break = ref false in

            let fmin  = wmin     (Measure.compare) in
            let fcmp  = wcompare (Measure.compare) in

            let rec iter flow vis path =
                if Queue.is_empty que then
                    (`Val Measure.zero, vis, [])
                else
                    let prev, cur  = Queue.pop que in
                    let {out;edg;_} = vertexof cur graph' in
                    (* be careful not to add all paths as only some need to be considered *)
                    let diff = AdjSet.fold (fun nxt vis ->
                        if !break || AdjSet.mem nxt vis then
                            vis
                        else
                            let mcap = Vertex.edge2 nxt edg in
                            let cflw = Flowtbl.find cap (cur, nxt) in
                            let favl = Measure.sub mcap cflw.flow in
                            if Measure.compare favl Measure.zero = 1 then
                                if equal nxt sink then
                                    let _ = break := true in
                                    (AdjSet.add nxt vis)
                                else
                                    let _ = Queue.add (Some (cur, favl), nxt) que in
                                    (AdjSet.add nxt vis)
                            else
                                vis
                    ) out vis in
                    if !break then
                        let mcap = Vertex.edge2 sink edg in
                        let cflw = Flowtbl.find cap (cur, sink) in
                        let favl = Measure.sub  mcap cflw.flow in
                        let btnk = fmin (`Val favl) flow in
                        let cpth = (Some (cur, favl)) in
                        let path'= (prev, cur) :: (cpth, sink) :: path in
                        match prev with
                        | Some (_pelt, pcap) ->
                            (fmin (`Val pcap) btnk), (AdjSet.union diff vis), path'
                        | None ->
                            btnk, (AdjSet.union diff vis), path'
                    else
                        let f, v, p = iter flow (AdjSet.union diff vis) path
                        in if !break then
                            match prev with
                            | Some (_pelt, pcap) ->
                                (fmin (`Val pcap) (fmin f flow)), v, (prev, cur) :: p
                            | None ->
                                (fmin f flow), v, (prev, cur) :: p
                        else
                            `Val Measure.zero, v, p
            in

            let maxiter = ref 0 in

            (* setup a loop *)
            let rec loop maxf =
                if !maxiter = maxit then
                    raise Not_found
                else
                    (* reset flow control *)
                    let _ = break := false in
                    let _ = incr maxiter in
                    let _ = Queue.clear que in
                    let _ = Queue.push (None, source) que in
                    let (x, _, z) = (iter `Inf (AdjSet.singleton source) []) in
                    if fcmp x (`Val Measure.zero) = 0 then
                        maxf
                    else
                        let (_, _z') = List.fold_left (fun (sink', p) (f, t) ->
                            match f with
                            | Some (f',_) ->
                                if equal sink' t then
                                    let fwd  = Flowtbl.find cap (f', t)  in
                                    let bwd  = Flowtbl.find cap (t, f')  in
                                    let _    = fwd.flow <- wapply (Measure.add fwd.flow) x in
                                    let _    = bwd.flow <- wapply (Measure.sub bwd.flow) x in
                                    (f', sink' :: p)
                                else
                                    (sink', p)
                            | _ ->
                                (sink', sink' :: p)
                        ) (sink, []) (List.rev z) in
                        let maxf' = wbind (Measure.add) maxf x in
                        (loop[@tailcall]) maxf'
            in loop (`Val Measure.zero)
        ;;

    end

    module Matching = struct

        (** halls theorem:
           checks if all edges can be matched in a bipartite graph. pass in the
           graph and one half of the bipartite graph
           only really useful for small sets :-) as it generates
           many subsets

            you can use flow algorithms do find this match by adding dummy nodes
            to the graph and adding "binary (0,1)" weights. weights that are 1
            at the end of the flow will give the matching
           *)
        let hall graph tset  =
            let break = ref true in
            let _ = Seq.take_while(fun tset' ->
                let (crd, nbr) = (AdjSet.fold (fun el (sm, ac) ->
                    sm + 1, AdjSet.union (neighbours el graph) ac
                ) tset' (0, AdjSet.empty) )
                in
                if (crd <= AdjSet.cardinal nbr) then true else
                    let _ = break := false in
                    false
            ) (AdjSet.subset_seq tset) |> Seq.iter (ignore) in
            !break
        ;;

        module Compute(Measure: Measurable with type t = edge and type edge = edge) = struct

            type rank = (elt * Measure.t)
            module RankElt: Ordinal with type t = rank and type order = Measure.t = struct
                type t          = rank
                type order      = Measure.t
                let bind t      = snd t
                let compare l r = Unique.compare (fst l) (fst r)
                let order   l r = Measure.compare l r
            end
            module RankHeap = MakeFibHeap (RankElt)

            (* Graph is modeled as a complete graph with edges representing
               the rankings - higher values mean better rankings *)
            let galeshapely graph proposers acceptors =
                (* matching from acceptors to proposers *)
                let matching = EdgeSet.empty in

                (* max heap - higher ranked first meaning more preffered *)
                let cmp = RankHeap.maxify in
                let _   = RankHeap.churn_threshold := 16 in

                let proprank = AdjSet.fold (fun el ac ->
                    let ranks  = RankHeap.empty in
                    let wgts   = Vertex.weights el graph in
                    let ranks' = AdjSet.fold (fun el' ac' ->
                        let rate = Vertex.edge2 el' wgts in
                        RankHeap.insert ~cmp:cmp (el', rate) ac'
                    ) acceptors ranks in
                    (el, ranks') :: ac
                ) proposers  [] in

                (* closure over proprank for 'recalling' proposers for
                   rematching *)
                let recall elt = List.find (fun (x,_) ->  equal elt x) proprank in

                let rec iter matches props complete =
                    match props with
                    | []  ->
                        matches
                    | ((prps, prnk) :: rest) ->
                        (* our next choice
                           TODO: what if we run out of choices ?? *)
                        let ((acp, _), rem) = RankHeap.extract ~cmp:cmp prnk in

                        if AdjSet.mem acp complete then

                            (* Find the partner *)
                            let (_, curp) = EdgeSet.find_first (
                                fun (a, _) -> equal a acp
                            ) matches in

                            (* how do they rank us against their current partner *)
                            let wgt = Vertex.weights acp graph in

                            let rnk = Vertex.edge2 prps wgt in
                            let cur = Vertex.edge2 curp wgt in

                            (* are we better than the proposer *)
                            if Measure.compare cur rnk = 1 then
                                (* prefers us *)
                                let newmtch = EdgeSet.add (acp, prps) (EdgeSet.remove (acp, curp) matches) in
                                (* curp is now unmatched *)
                                let np = recall curp in
                                let sz = RankHeap.cardinal (snd np) in
                                let _ = Format.printf "recall capacity: %d\n" sz in
                                iter newmtch (np :: rest) complete
                            else
                                (* check remaining unmatched *)
                                iter matches ((prps, rem) :: rest) complete
                        else
                            (* match them and move on *)
                            let  matches' = EdgeSet.add (acp, prps) matches in
                            (* cross them off our lists *)
                            iter matches' rest (AdjSet.add acp complete)
                in
                iter matching proprank AdjSet.empty
            ;;
        end
    end

    module Serialize(Serde: SerDe with type edge := edge and type elt := elt) = struct

        (* e.g. color = red ... *)
        module StyleTbl: Hashtbl.S with type key = string = Hashtbl.Make (String)
        (* (string_of_elt elt) -> StylTbl *)
        module AttrbTbl: Hashtbl.S with type key = string = Hashtbl.Make (String)
        (* (cluster_index) -> AttrbTbl *)
        module ClstrTbl: Hashtbl.S with type key = int    = Hashtbl.Make (struct
            type t    = int
            let equal = Int.equal
            let hash  = Hashtbl.hash
        end)

        (** export to csv *)
        let to_csv graph =
            NodeMap.to_seq graph
            |> Seq.map (fun (elt, {out;edg=wgt;_}) ->
                if Weights.length wgt > 0 then
                    Seq.cons (fun () ->
                        Format.sprintf ("\n%s,") (Serde.string_of_elt elt))
                        (AdjSet.to_seq out |> Seq.map (fun el ->
                            fun () -> Format.sprintf ("%s %s,")
                                (Serde.string_of_elt el) (Serde.string_of_wgt (Vertex.edge2 el wgt))
                        ))
                else
                    Seq.cons (fun () ->
                        Format.sprintf ("\n%s,") (Serde.string_of_elt elt))
                        (AdjSet.to_seq out |> Seq.map (fun el ->
                            fun () -> Format.sprintf ("%s,")
                                (Serde.string_of_elt el)
                        ))
            )
        ;;

        (** export to dot *)
        let to_dot ?(dir=false) ?(sub=false) name gattrs nattrs eattrs graph =
            let header = if dir then "digraph" else "graph" in
            let edglnk = if dir then "->" else "--" in
            (* can be reused in a subgraph *)
            let header = if sub then "subgraph" else header in
            let visedg = ref EdgeSet.empty in
            Seq.cons (
                Seq.cons (fun () -> Format.sprintf ("%s %s {\n") header name)
                    (* global attributes *)
                    (StyleTbl.to_seq gattrs |> Seq.map (fun (k, v) ->
                        fun () -> Format.sprintf ("\t%s=\"%s\";\n") k v))
            )  (Seq.append (NodeMap.to_seq graph
                    |> Seq.map (fun (elt, {out;edg=wgt;_}) ->
                        (* no neighbours - check only for attributes *)
                        let eltkey = (Serde.string_of_elt elt) in
                        let weighted = Weights.length wgt > 0 in
                        if AdjSet.is_empty out then
                            match AttrbTbl.find_opt nattrs eltkey with
                            (* Some node attributes *)
                            | Some attrs ->
                                Seq.cons (fun () ->
                                    Format.sprintf "\t%s\t[" eltkey
                                ) (StyleTbl.to_seq attrs
                                        |> Seq.map (fun (k,v) ->
                                            fun () -> Format.sprintf "%s=%s, " k v
                                        ) |> (Fun.flip Seq.append (Seq.return (fun () -> "];\n")))
                                    )
                            (* no node attributes or neighbours *)
                            | _ ->
                                Seq.cons (fun () -> Format.sprintf "\t%s;\n" (Serde.string_of_elt elt)) Seq.empty
                        (* handle neighbours and attributes *)
                        else
                            AdjSet.to_seq out
                            (* handle edge attributes, edge key follows 'f-t' *)
                            |> Seq.map (fun x ->
                                let xs     = (Serde.string_of_elt x) in
                                let ek, ev = (eltkey ^ "-" ^ xs, xs) in
                                let dne = let vis = !visedg in
                                    if EdgeSet.mem (x, elt) vis then
                                        true
                                    else
                                        let _  = visedg := (EdgeSet.add (elt, x) vis) in
                                        false
                                in if dne then (fun () -> "") else
                                    let label  =
                                        if not weighted then "" else
                                            Format.sprintf "label=\"%s\"" (Serde.string_of_wgt @@ Vertex.edge2 x wgt)
                                    in
                                    match AttrbTbl.find_opt eattrs ek with
                                    | Some iattr ->
                                        fun () -> let attrs =
                                            if StyleTbl.length iattr = 0 then
                                                (if weighted then
                                                    "\t[ " ^ label ^ " ]"
                                                    else
                                                        label
                                                )
                                            else
                                                (* all attributes *)
                                                (Seq.fold_left (^) "\t[ "
                                                    (
                                                        (StyleTbl.to_seq iattr |>
                                                            Seq.map (fun (k,v) ->
                                                                Format.sprintf "%s=\"%s\", " k v
                                                            )
                                                        ) |> (Fun.flip Seq.append (Seq.return (Format.sprintf "%s ]" label)))
                                                    )
                                                )
                                        in Format.sprintf "\t%s %s %s %s;\n" eltkey edglnk ev attrs
                                    | None ->
                                        fun () ->
                                        Format.sprintf "\t%s %s %s\t[ %s ];\n" eltkey edglnk ev label
                            ) |> (Seq.append (Seq.return (fun () ->
                                match AttrbTbl.find_opt nattrs eltkey with
                                (* Some node attributes *)
                                | Some attrs -> Seq.fold_left (^) (Format.sprintf "\t%s\t[ " eltkey)
                                    (StyleTbl.to_seq attrs
                                        |> Seq.map (fun (k,v) ->
                                            Format.sprintf "%s=%s, " k v
                                        ) |> (Fun.flip Seq.append (Seq.return " ];\n"))
                                    )
                                (* no node attributes or neighbours *)
                                | _ ->
                                    Format.sprintf "\t%s;\n" (Serde.string_of_elt elt)
                            )))
                    )
                ) (Seq.return (Seq.return (fun () -> "}\n"))))
        ;;

        (** export a graph with subgraph clusters into dot format *)
        let to_dot_cluster ?(dir=false) name onname gattrs clattrs nattrs eattrs graphs =
            let header = if dir then "digraph" else "graph" in
            let tmpclstr = StyleTbl.create 0 in
            Seq.cons (
                Seq.cons (fun () -> Format.sprintf ("%s %s {\n") header name)
                    (* global attributes *)
                    (StyleTbl.to_seq gattrs |> Seq.map (fun (k, v) ->
                        fun () -> Format.sprintf ("\t%s=\"%s\";\n") k v)
                    )
            )  (Seq.append (Scc.SccMap.to_seq graphs
                    |> Seq.map (fun (idx, (ngbrs, cluster)) ->
                        let name = (onname idx ngbrs) in
                        match ClstrTbl.find_opt clattrs idx with
                        | Some clattr ->
                            (to_dot ~dir:dir ~sub:true name clattr nattrs eattrs cluster)
                            |> Seq.concat
                        | _ ->
                            (to_dot ~dir:dir ~sub:true name tmpclstr nattrs eattrs cluster)
                            |> Seq.concat
                    )
                ) (Seq.return (Seq.return (fun () -> "}\n"))))
        ;;

    end

    (* TODO:
        Random:
            Rodl nibble and Erdos-renyi
            Blossom
        Flow:
            Goldberg Rao (with link cut tree)
            Min Cost Flow || Min weight bipartite matching
        Matching (Unweighted + Weigted)
            Blossom algo (non bipartite)
            Hopcroft Kraft (unweighted)
            LP Network simplex
            Vertex cover and dual problems
        Planar:
            Boyer-Myrovold
            Euler formula: V - E + F = 2
        Extra:
            Ramsey and Moore graphs
            Tree linearization and LCA | RMQ
            Unordered Set and Splay Tree | Splay Maps as alternatives
            Segment trees
            Printers??
    *)

end;;
