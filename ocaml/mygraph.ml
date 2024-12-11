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

module type Graph = sig 
    type +'a t
    type elt
    module AdjSet: TSet with type t := elt
    type adj := (elt AdjSet.set * elt AdjSet.set * elt)
    module Vertex: Set.OrderedType with type t := adj
    module NodeMap: Map.S with type key := elt
    type sccNode = { mutable link: int; node: elt };;
    module SccTbl: Hashtbl.S with type key := sccNode
    module SccSet: TSet with type t := sccNode
    module SccMap: Map.S with type key := int
    val empty: adj NodeMap.t
    val equal: elt -> elt -> bool
    val add: elt -> adj NodeMap.t -> adj NodeMap.t
    val add_edge: elt -> elt -> adj NodeMap.t -> adj NodeMap.t
    val add_all: elt -> elt list -> adj NodeMap.t -> adj NodeMap.t
    val of_list: (elt * elt list) list -> adj NodeMap.t -> adj NodeMap.t
    val incomingof: elt -> adj NodeMap.t -> (elt AdjSet.set)
    val outgoingof: elt -> adj NodeMap.t -> (elt AdjSet.set)
    val remove: elt -> adj NodeMap.t -> adj NodeMap.t
    val bfs: (elt -> elt AdjSet.set -> bool) -> elt -> adj NodeMap.t -> bool option
    val dfs: (elt -> elt AdjSet.set -> bool) -> elt -> adj NodeMap.t -> bool option
    val adj_list_of: elt -> adj NodeMap.t -> elt list
    val transpose: adj NodeMap.t -> adj NodeMap.t
    val to_scc_set: elt SccTbl.t -> sccNode SccSet.set
    val to_induced_graphs: adj NodeMap.t -> elt SccTbl.t -> (elt AdjSet.set * elt AdjSet.set * elt) NodeMap.t SccMap.t
    val tarjan: ('a * elt AdjSet.set * 'b) NodeMap.t ->  elt SccTbl.t
end

module MakeGraph(Unique: Set.OrderedType): Graph with type elt := Unique.t = struct

    type elt = Unique.t

    (** Module for manipulating the Set structure holding the Adjacency list *)
    module AdjSet  = TreeSet(Unique)
    type   adj     = (elt AdjSet.set * elt AdjSet.set * elt)

    (*  Incoming nodes  Outgoing nodes data *)
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

    (*breadth first search starting from start node applying f until returns true*)
    let bfs f start game = 
        let que     = Queue.create () in
        let _       = Queue.add start que in
        let visited = AdjSet.empty in
        let rec iter vis nxt =
            let* label = Queue.take_opt nxt in
            if f label vis then
                Some true
            else
                let out =  outgoingof label game in 
                let _   = AdjSet.iter_inorder (fun x -> Queue.add x nxt) (AdjSet.diff out vis) in
                iter (AdjSet.union out vis) nxt
        in iter visited que
    ;;

    (*depth first search starting from start node applying f until returns true*)
    let dfs f start game =
        let stck    = Stack.create () in
        let _       = Stack.push start stck in
        let visited = AdjSet.empty in
        let rec iter vis nxt =
            let* label = Stack.pop_opt nxt in
            if f label vis then
                Some true
            else
                let out = outgoingof label game in 
                let _   = AdjSet.iter_inorder (fun x -> Stack.push x nxt) (AdjSet.diff out vis) in
                iter (AdjSet.union out  vis) nxt
        in iter visited stck
    ;;

    (* Get adjacency list of a node *)
    let adj_list_of node nodeMap =
        let (incoming, outgoing, _label) = NodeMap.find node nodeMap in
        AdjSet.fold (
            fun anode alist -> anode :: alist
        )  (AdjSet.union incoming outgoing) []
    ;;

    let transpose nodeMap =
        NodeMap.map (fun (inc, out, label) -> (out, inc, label)) nodeMap
    ;;

    (*************************************************************************
    *                    Strongly connected Components                       *
    **************************************************************************)

    type sccNode = { mutable link: int; node: elt };;

    let cmpscc {link=left;_} {link=right;_} = Int.compare left right
    ;;

    module SccNode = struct
        type t      = sccNode
        let compare = fun x y -> (Unique.compare x.node y.node)
        let equal   = fun x y -> (cmpscc x y) = 0
        let hash    = fun x   -> x.link
    end

    (* Hashtbl creation here is not deterministic when you iter or fold *)
    module SccTbl: Hashtbl.S with type key := sccNode = Hashtbl.Make(SccNode)
    module SccSet: TSet with type t := sccNode = TreeSet(SccNode)
    module SccMap: Map.S with type key := int = Map.Make(Int)

    (** Collect all members into a set *)
    let to_scc_set state =
        SccTbl.to_seq_keys state
        |> Seq.fold_left (fun acc el -> SccSet.add el acc) SccSet.empty
    ;;

    (* creates a Map of ints -> Graph.t where the int is the low-link value. -1
       is used to show Disconnected nodes from the Sccs 

       if the graph is just 'linear' then apparently it is its own SCC but
       that may not be capture by tarjan so we main a "disconnected" entry
       as -1 
    *)
    let to_induced_graphs nodeMap sccs = 
        SccTbl.fold (fun {link=lowlink;_} elt acc -> 
            SccMap.update lowlink (fun nodeEl -> match nodeEl with
                (* TODO: update internal edges *)
                (* TODO: intergraph edges ?? *)
                | Some v -> Some (NodeMap.add elt (NodeMap.find elt nodeMap) v)
                | None ->   Some (NodeMap.singleton elt (NodeMap.find elt nodeMap))
            )
        acc) sccs SccMap.empty
    ;;

    (* save some memory reserves when creating the SccState *)
    let buckets size =
        int_of_float (ceil (float_of_int(size) /. 2.))
    ;;

    (** Tarjans SCC algorithm 
        basically we should pop until we find our own
        low-link when we started - this shows that it
        is an SCC on backtrack - otherwise we keep pushing on to the stack
        The stack is there to maintain an invariance over the visited nodes
        during Depth first search

        In the end remaining elements in the Stack are their own SCCs this way
        All elements always belong to an SCC
    *)
    let tarjan nodeMap   =
        let invar        = Stack.create () in
        let sccs         = SccTbl.create (buckets (NodeMap.cardinal nodeMap)) in
        let lowlink      = ref 0 in
        let monotonic x  = let () = x := !x+1 in !x+1 in
        let contains n s = Seq.find (fun {node=m;_} -> equal n m) @@ (Stack.to_seq s) in
        let _            = NodeMap.iter (fun key _ -> let _ = dfs (fun x dfsvis ->
            if AdjSet.mem x dfsvis then
                match contains x invar with
                | Some {link=tslot;_} -> 
                        let _ = Seq.take_while (fun pp ->
                        let _ = (pp.link <- (min pp.link tslot)) in
                        let _ = SccTbl.add sccs pp pp.node in
                        not (equal x pp.node)
                    ) (Stack.to_seq invar) () in
                    true
                | None ->
                    false
            else
                let _ = Stack.push {node=x;link=(monotonic lowlink)} invar in
                false
        ) key nodeMap in ()) nodeMap in 
        let _ = Seq.iter (fun x -> SccTbl.add sccs x x.node) (Stack.to_seq invar) in
        sccs
    ;;

    (*************************************************************************
     *                           Clique Algos                                *
    **************************************************************************)
    

end;;
