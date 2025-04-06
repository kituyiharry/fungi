signature GraphElt = sig 
  type edge
  include OrderedType
  val toString: t -> string (* used for hashing *)
end

signature TGraph = sig
    type elt    (* Main graph node *)
    type edge   (* Type of edge *)

    structure AdjSet: TSet where type t = elt

    type weights = (elt, edge) HashTable.hash_table

    (** Vertex adjacency type  *)
    type adj = {
        inc: elt AdjSet.set, (* incoming set *)
        out: elt AdjSet.set, (* outgoing set *)
        lab: elt,            (* label *)
        edg: weights         (* Weights edges *)
    }

    structure NodeMap: MAP where type key = elt

    (** Vertex carries node information  *)
    structure Vertex: sig 
      include OrderedType  where type t = adj
 
      val empty  : elt -> adj
      val weights: elt -> adj NodeMap.map -> weights
      val edge   : elt -> elt -> adj NodeMap.map -> edge
      val edge2  : elt -> weights -> edge
      val update : elt -> weights -> edge -> unit
      val ensure : elt -> weights -> edge -> unit
    end
    
    val empty:       adj NodeMap.map
    val equal:       elt -> elt -> bool
    val add:         elt -> adj NodeMap.map -> adj NodeMap.map

end

functor MakeGraph(structure Unique: GraphElt) :> TGraph where type elt = Unique.t and type edge = Unique.edge = struct
    
    type elt     = Unique.t
    type edge    = Unique.edge
    type weights = (elt, edge) HashTable.hash_table
    exception NotFound

    structure AdjSet = TreeSet(Unique)

    type adj = {
        inc: elt AdjSet.set, (* incoming set *)
        out: elt AdjSet.set, (* outgoing set *)
        lab: elt,            (* label *)
        edg: weights         (* Weights edges *)
    }

    fun equal l r = Unique.compare l r = General.EQUAL

    structure NodeMap = RedBlackMap(Unique)

    (*  Incoming nodes  Outgoing nodes data *)
    structure Vertex = struct

        type t       = adj

        fun compare ({lab=x, ...}: adj) ({lab=y, ...}: adj) = 
          Unique.compare x y

        fun empty (lbl: elt) = 
          { 
            inc=AdjSet.empty, out=AdjSet.empty, 
            lab=lbl, edg=(HashTable.mkTable ((fn x => (HashString.hashString (Unique.toString x))), fn (l, r) => equal l r) (8, NotFound))
          }

        fun weights (n: elt) (g: adj NodeMap.map) = case (NodeMap.lookup (n, g)) of
          SOME {edg=h, ...} => h
          | NONE => raise NotFound

        fun edge  (f: elt) (t: elt) (g: adj NodeMap.map) =
          HashTable.lookup (weights f g) t

        fun edge2 (t: elt) (ht: weights) = 
          HashTable.lookup ht t

        fun update (t: elt) (ht: weights) (v: edge) = 
          HashTable.insert ht (t, v)

        fun ensure t ht v = if Option.isSome (HashTable.find ht t) then () else 
          HashTable.insert ht (t, v)

    end


    val empty = NodeMap.empty

    fun add nodekey nodeMap =
        NodeMap.insert (nodekey, (Vertex.empty nodekey), nodeMap)

    fun add_edge _nodeFrom _nodeTo _nodeMap =
        ()

end
