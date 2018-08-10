(***
 *** suite de Farey (fractions réduites entre 0 et 1 de dénominateur borné)
 ***
 *** https://en.wikipedia.org/wiki/Farey_sequence
 *** voir #073
 ***)

(* https://en.wikipedia.org/wiki/Farey_sequence *)

(* the Farey sequence of order dmax is the ordered sequence of irreducible
 * fractions between 0 and 1, included, whose denominator is at most dmax.
 * fractions are pairs (a,b) of coprime integers with 0 ⩽ a < b. *)

type t = int * int

(* the Farey sequence is symmetric (if the fraction (a, b) is irreducible, then
 * (b−a, b) too), so we can easily change the direction. *)
val sym : t -> t

(* THEOREM 1:
 * the fractions (a, b) and (c, d) are adjacent in the Farey sequence of order
 * dmax if and only if:
 *   (1)  b, d ⩽ dmax < b+d
 *   (2)  b×c − a×d = 1
 * at order b+d, the fractions (a, b), (a+c, b+d), (c, d) are adjacent.
 * the middle fraction is called the mediant (in this case, it is guaranteed to
 * be in irreducible form).
 * so the terms of the Farey sequence of some order, between two given fractions
 * which are adjacent at some lower order, are obtained by inserting mediants
 * (this is how the Stern-Brocot tree is constructed).
 *
 * theorem 1 allows us to compute in O(1) the successor at order dmax of some
 * fraction (a, b), provided we know the successor (c0, d0) at some lower order.
 *)

val assert_adjacent_fractions : int -> t -> t -> unit

(* THEOREM 2:
 * for any three fractions (a, b), (c, d), (e, f) which are adjacent at order
 * dmax, (c, d) is equal to the mediant (a+e, b+f) of (a, b) and (e, f)
 * (however, that mediant is not always in irreducible form).
 *
 * theorem 2 allows us to compute in O(1) the successor at order dmax of two
 * adjacent fractions (a, b), (c, d); hence, we can iterate in O(n) = O(dmax²)
 * on the Farey sequence of order dmax, where n is the length of the sequence
 * (the length of the sequence is the sum of the totient function φ, is is
 * asymptotically equivalent to 3∕π² dmax²).
 *)

(* [iter_farey dmax f] iterates [f] on every fraction of the Farey sequence of
 * order [dmax]. the optional arguments [starting1] and [starting2] are two
 * adjacent fractions in the sequence, which are taken as the starting point
 * (instead of (0,1) and (1,dmax)). *)
val iter_farey : int -> ?starting1:t -> ?starting2:t -> (t -> unit) -> unit

(* [next_farey dmax frac] returns the successor fraction of [frac] in the Farey
 * sequence of order [dmax]. *)
val next_farey : int -> t -> t

val prev_farey : int -> t -> t

val rev_iter_farey : int -> ?starting1:t -> ?starting2:t -> (t -> unit) -> unit

(* [list_farey dmax] returns the Farey sequence of order [dmax] as a list. *)
val list_farey : int -> t list
