signature ORDERED_TYPE = sig
    type t
    val compare : t * t -> order
end

signature MAP = sig
    type key
    type 'a map
 
    val empty : 'a map
    val insert : key * 'a * 'a map -> 'a map
    val lookup : key * 'a map -> 'a option
    val remove : key * 'a map -> 'a map
    val isEmpty : 'a map -> bool
    val toList : 'a map -> (key * 'a) list
end

functor RedBlackMap(Ord: ORDERED_TYPE) : sig include MAP where type key = Ord.t end = struct
    type key = Ord.t
 
    datatype color = RED | BLACK

    datatype 'a tree = 
        EMPTY
      | NODE of color * (key * 'a) * 'a tree * 'a tree

    type 'a map = 'a tree

    (* Create an empty map *)
    val empty = EMPTY

    (* Helper to check if a node is red *)
    fun isRed EMPTY = false
      | isRed (NODE(RED, _, _, _)) = true
      | isRed _ = false

    (* Balance the tree after insertion *)
    fun balance BLACK (z, zv) (NODE(RED, (x, xv), a, NODE(RED, (y, yv), b, c))) d =
          NODE(RED, (y, yv), NODE(BLACK, (x, xv), a, b), NODE(BLACK, (z, zv), c, d))
      | balance BLACK (z, zv) (NODE(RED, (y, yv), NODE(RED, (x, xv), a, b), c)) d =
          NODE(RED, (y, yv), NODE(BLACK, (x, xv), a, b), NODE(BLACK, (z, zv), c, d))
      | balance BLACK (x, xv) a (NODE(RED, (y, yv), b, NODE(RED, (z, zv), c, d))) =
          NODE(RED, (y, yv), NODE(BLACK, (x, xv), a, b), NODE(BLACK, (z, zv), c, d))
      | balance BLACK (x, xv) a (NODE(RED, (z, zv), NODE(RED, (y, yv), b, c), d)) =
          NODE(RED, (y, yv), NODE(BLACK, (x, xv), a, b), NODE(BLACK, (z, zv), c, d))
      | balance color (x, xv) a b = NODE(color, (x, xv), a, b)

    (* Insert a key-value pair into the tree *)
    fun insert' (x, v, EMPTY) = NODE(RED, (x, v), EMPTY, EMPTY)
      | insert' (x, v, NODE(color, (y, yv), a, b)) =
        case Ord.compare(x, y) of
            LESS => balance color (y, yv) (insert'(x, v, a)) b
          | GREATER => balance color (y, yv) a (insert'(x, v, b))
          | EQUAL => NODE(color, (x, v), a, b)

    (* Public insert function *)
    fun insert (key, value, tree) =
        case insert'(key, value, tree) of
            NODE(RED, item, left, right) => NODE(BLACK, item, left, right)
          | tree => tree

    (* Lookup a value in the map *)
    fun lookup (key, EMPTY) = NONE
      | lookup (key, NODE(_, (k, v), left, right)) =
        case Ord.compare(key, k) of
            LESS => lookup(key, left)
          | GREATER => lookup(key, right)
          | EQUAL => SOME v

    (* Remove a key from the map *)
    fun remove (key, tree) =
        let
            fun removeMin EMPTY = raise Empty
              | removeMin (NODE(_, item, EMPTY, right)) = (item, right)
              | removeMin (NODE(color, item, left, right)) =
                let val (min, left') = removeMin left
                in (min, balance color item left' right)
                end

            fun remove' EMPTY = EMPTY
              | remove' (NODE(color, (k, v), left, right)) =
                case Ord.compare(key, k) of
                    LESS => balance color (k, v) (remove' left) right
                  | GREATER => balance color (k, v) left (remove' right)
                  | EQUAL =>
                    case (left, right) of
                        (EMPTY, _) => right
                      | (_, EMPTY) => left
                      | _ =>
                        let val (successor, right') = removeMin right
                        in balance color successor left right'
                        end
        in
            remove' tree
        end

    (* Check if map is empty *)
    fun isEmpty EMPTY = true
      | isEmpty _ = false

    (* Convert map to sorted list *)
    fun toList tree =
        let
            fun traverse EMPTY acc = acc
              | traverse (NODE(_, (k, v), left, right)) acc =
                traverse left ((k, v) :: traverse right acc)
        in
            traverse tree []
        end
end
