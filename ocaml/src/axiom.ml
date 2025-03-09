
(** {1:axiom Axioms} 
   we use a wrap type to denote extremes, ideally this is just a float
   but the challenge is we don't know they final type ahead of time (int, float,
   ast) so we
   prefigure it as we need it for some algorithms e.g djikstra. We assume that

   - {i `Inf} for positive infinity
   - {i `NegInf} for negative infinity
   - {i `Val x} for x which is a realised value
   - {i `Nan} for not a number and a substitute for "null" or least element
*)
 
(** {3:space Space}
    this is an ast mainly used when doing path finding or flow algorithms in
    the graph. It allows you to bring in your own implementation of the edge.
    Example valid structure are `Float` or `Int`.*)
module type Space = sig

  type t
  include Set.OrderedType with type t := t

  val zero : t
  val one  : t

  val add  : t -> t -> t
  val sub  : t -> t -> t

end

(** wraps a value a for our internal use *)
type +!'a wrap = [`Inf | `NegInf | `Nan | `Val of 'a]

(** compare 2 values, if both are external values the we apply your provided
    compare function through f. example for a float value: 

    {@ocaml[
        let x = Axiom.wcompare (Float.compare) (`Val 0.) (`Val 0.) =  0;;
        let y = Axiom.wcompare (Float.compare) (`Inf)    (`Val 0.) =  1;;
        let z = Axiom.wcompare (Float.compare) (`Val 0.) (`Inf)    = -1;;
    ]}

*)
let wcompare f l r = match (l, r) with
    | (`Val l',`Val r')   ->  f l' r'
    | (`Inf,    `Inf)     ->  0
    | (`NegInf, `NegInf)  ->  0
    | (`Nan,    `Nan)     ->  0
    | (`NegInf, `Inf)     -> -1
    | (`NegInf, `Val _)   -> -1
    | (`Val _,  `Inf)     -> -1
    | (`Nan,    `Inf)     -> -1
    | (`Nan,    `NegInf)  -> -1
    | (`Nan,    `Val _)   -> -1
    | (`Inf,    `NegInf)  ->  1
    | (`Val _,  `NegInf)  ->  1
    | (`Inf,    `Val _)   ->  1
    | (`Inf,    `Nan)     ->  1
    | (`NegInf, `Nan)     ->  1
    | (`Val _,  `Nan)     ->  1
;;

(** Apply a function only to internal values, otherwise raise a not found
    exception 

    {@ocaml[
        let x = Axiom.wapply (Float.add) (`Val 1.) (`Val 1.) =  2.;;
        let y = Axiom.wapply (Float.add) (`Inf)    (`Val 0.) =  exception Not_found;;
        let z = Axiom.wapply (Float.add) (`Val 0.) (`Inf)    =  exception Not_found;;
    ]}
*)
let wapply f l = match l with
    | `Val x -> f x
    |  _     -> raise Not_found
;;

(** Similar to `Axiom.wapply` but returns a left or right value if no value is
    present 

    {@ocaml[
        let x = Axiom.wbind (Float.add) (`Val 1.) (`Val 1.) =  2.;;
        let y = Axiom.wbind (Float.add) (`Inf)    (`Val 0.) =  `Inf;;
        let z = Axiom.wbind (Float.add) (`Val 0.) (`Inf)    =  `Inf;;
    ]}
*)
let wbind f l r = match (l, r) with
    | (`Val l', `Val r') -> (`Val (f l' r'))
    | (x, `Val _) -> x
    | (`Val _, y) -> y
    | (x',      _y') -> x'
;;

(** convert to string after apply (f x) for `Val x if `Val is present *)
let string_of_wrap f v = match v with
    | `Inf     ->  "`Inf"
    | `NegInf  ->  "`NegInf"
    | `Nan     ->  "`Nan"
    | `Val x   ->  Format.sprintf ("`Val %s") (f x)
;;

(** minimum value with compare function f *)
let wmin f l r = if (wcompare f l r) = -1 then l else r
;;

(** maximum value with compare function f *)
let wmax f l r = if (wcompare f l r) =  1 then l else r
;;
