signature TSequence = sig
    type 'a seq
      (*unit -> 'a front*)
    and 'a front  
      (*Nil | Cons of 'a * 'a seq*)

    val empty : 'a seq
    val cons : 'a * 'a seq -> 'a seq
    val head : 'a seq -> 'a    (* raises Empty if sequence is empty *)
    val tail : 'a seq -> 'a seq (* raises Empty if sequence is empty *)

    (* Constructors *)
    val fromList : 'a list -> 'a seq
    val tabulate : int * (int -> 'a) -> 'a seq
    val iterate : ('a -> 'a) -> 'a -> 'a seq

    (* Basic operations *)
    val take : 'a seq * int -> 'a seq
    val drop : 'a seq * int -> 'a seq
    val map : ('a -> 'b) -> 'a seq -> 'b seq
    val filter : ('a -> bool) -> 'a seq -> 'a seq
    val append : 'a seq * 'a seq -> 'a seq
    val fold_left: ('a -> 'b -> 'a) -> 'a -> ('b seq) -> 'a

    (* Conversion *)
    val toList : 'a seq -> 'a list
end

structure Sequence :> TSequence = struct

  datatype 'a front = Nil | Cons of 'a * (unit -> 'a front)
  type 'a seq = unit -> 'a front

  fun empty () = Nil

  fun cons (x, s) () = Cons(x, s)

  fun head s = 
      case s () of
          Nil => raise Empty
        | Cons(x, _) => x

  fun tail s =
      case s () of
          Nil => raise Empty
        | Cons(_, s') => s'

  fun fromList [] = empty
    | fromList (x::xs) = cons(x, fromList xs)

  fun tabulate (n, f) =
      let fun tab i =
          if i >= n then empty
          else cons(f i, tab (i+1))
      in tab 0
      end

  fun iterate f x = 
      cons(x, iterate f (f x))

  fun take (s, n) =
      if n <= 0 then empty
      else
          fn () =>
              case s () of
                  Nil => Nil
                | Cons(x, s') => Cons(x, take(s', n-1))

  fun drop (s, n) =
      if n <= 0 then s
      else
          case s () of
              Nil => empty
            | Cons(_, s') => drop(s', n-1)

  fun map f s () =
      case s() of
          Nil => Nil
        | Cons(x, s') => Cons(f x, map f s')

  fun filter p s () =
      case s() of
          Nil => Nil
        | Cons(x, s') =>
              if p x then Cons(x, filter p s')
              else (filter p s')()

  fun append (s1, s2) () =
      case s1 () of
          Nil => s2 ()
        | Cons(x, s1') => Cons(x, append(s1', s2))

  fun toList s =
      case s () of
          Nil => []
        | Cons(x, s') => x :: toList s'

  fun concat ss () =
        case ss () of
            Nil => Nil
          | Cons(s, rest) =>
                case s () of
                    Nil => (concat rest) ()
                  | Cons(x, xs) => Cons(x, append(xs, concat rest))

  fun fold_left f acc seq = case seq () of
    Nil => acc
    | Cons (x, next) =>
        let val acc = f acc x in
        fold_left f acc next
      end

end
