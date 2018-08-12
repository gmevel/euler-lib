(** Commonly useful functions not related to arithmetic. *)

(******************************************************************************)

(** Generic fast exponentiation.
    [pow ~mult ~unit x n] is [unit] composed [n] times to the right with [x],
    provided that [n] is non‐negative. For example:
{[
    pow ~mult:(^) ~unit:"x" "y" 5
]}
    yields ["xyyyyy"].
    {b Complexity:} 𝒪(log([n])) calls to [mult].
    @param mult the composition operator; should be associative.
    @param unit the left‐most operand of the product (most often, we use a
    neutral element for [mult]). *)
val pow : mult:('a -> 'a -> 'a) -> unit:'a -> 'a -> int -> 'a

(** Memoizing fixpoint combinator.
    Example use:
{[
    let fib = memoized_fix\@\@fun fib n ->
      if n < 2 then
        1
      else
        fib (n-1) + fib (n-2)
]} *)
val memoized_fix : (('a -> 'b) -> ('a -> 'b)) -> 'a -> 'b
