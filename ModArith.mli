(** Modular arithmetic. *)

(**
    This module defines modular arithmetic operations, that is, operations on
    elements of the ring ℤ∕mℤ where m is a positive integer, called the modulus.
    All operations take m as a named parameter [~modulo]. Elements of ℤ∕mℤ are
    represented by their canonical representatives between 0 and m−1 (included),
    of type [int]. All functions may assume that the modulus is positive and
    that canonical representatives are used, and may raise [Assert_failure] if
    that is not the case.

    The modulus can also be set globally, and not repeated for each individual
    operation, by giving it as a parameter to the functor {!Make}. This provides
    unary and binary operators.
 *)

(******************************************************************************)

(** Modular opposite. [opp ~modulo:m a] is the unique element [a'] of ℤ∕[m]ℤ
    such that [a']+[a] = 0. *)
val opp : modulo:int -> int -> int

(** Modular inverse. [inv ~modulo:m a] is the unique element [a'] of ℤ∕[m]ℤ such
    that [a']×[a] = 1, if it exists.
    {b Complexity:} 𝒪(log([a])) = 𝒪(log([m])) ([a] being under canonical form).
    @raise Division_by_zero when [a] is not inversible. *)
val inv : modulo:int -> int -> int

(** Modular addition. *)
val add : modulo:int -> int -> int -> int

(** Modular substraction. *)
val sub : modulo:int -> int -> int -> int

(** Modular multiplication.
    {b Complexity:} 𝒪(log(min([a],[b]))) = 𝒪(log([m]))
    ([a] and [b] being under canonical forms). *)
val mul : modulo:int -> int -> int -> int

(** Modular division. [div ~modulo:m a b] is the unique element [c] of ℤ∕[m]ℤ
    such that [c]×[b] = [a], if it exists.
    {b Complexity:} 𝒪(log([b])) = 𝒪(log([m])) ([b] being under canonical form).
    @raise Division_by_zero when [b] is not inversible. *)
val div : modulo:int -> int -> int -> int

(** A more general modular division, which does not assume its right operand to
    be inversible. [div_nonunique ~modulo:m a b] is an element [c] such that
    [c]×[b] = [a], if there exists one. The result is defined modulo
    [m] ∕ gcd([m], [b]); it is unique only when [b] is inversible.
    For example, modulo 10, 3×4 = 8×4 = 2, so 2 divided by 4 may be 3 or 8
    (this example shows that the division 2 ∕ 4 cannot be simplified to 1 ∕ 2).
    {b Complexity:} 𝒪(log([b])) = 𝒪(log([m])) ([b] being under canonical form).
    @raise Division_by_zero when there is no such element. *)
val div_nonunique : modulo:int -> int -> int -> int

exception Factor_found of int

(** A version of the modular division specialized for factorization purposes.
    [div_factorize ~modulo:m a b] is similar to [div ~modulo:m a b] but handles
    more precisely the cases when [b] is not inversible.
    {b Complexity:} 𝒪(log([b])) = 𝒪(log([m])) ([b] being under canonical form).
    @raise Division_by_zero when [b] is zero.
    @raise Factor_found when [b] is non‐zero and not inversible; in this case,
    gcd([m],[b]) is a non‐trivial factor of [m], which is returned as the
    parameter of the exception [Factor_found].
*)
val div_factorize : modulo:int -> int -> int -> int

(** Modular exponentiation. When [n] is non‐negative, [pow ~modulo:m a n] is
    [a]{^[n]} in the ring ℤ∕[m]ℤ; when [n] is negative, it is [a']{^−[n]} where
    [a'] is the modular inverse of [a].
    {b Complexity:} 𝒪(log([m])×log(|[n]|)).
    @raise Division_by_zero when [n] is negative and [a] is not inversible. *)
val pow : modulo:int -> int -> int -> int

(** [rand ~modulo:m ()] draws a random element of the ring ℤ∕[m]ℤ, with the
    uniform distribution. *)
val rand : modulo:int -> unit -> int

(** The functor application [Make (M)] defines modular arithmetic operations
    with a fixed, non‐zero modulus [M.modulo]. Because the modulus needs not be
    repeated for each individual operation, meaningful unary and binary
    operators can be defined.
    Operations in the resulting module follow the same specifications as those
    in module {!ModArith}, with respect to return values, exceptions raised, and
    time costs. *)
module Make : (sig val modulo : int end) -> sig

  (** The (positive) modulus m. *)
  val modulo : int

  (** The type of elements of the ring ℤ∕mℤ. *)
  type t = private int

  val of_int : int -> t
  val to_int : t -> int
  (** Conversions to and from integers. *)

  (** Modular opposite. *)
  val opp : t -> t

  (** An infix alias for [opp]. *)
  val ( ~-: ) : t -> t

  (** Modular inverse. *)
  val inv : t -> t

  (** An infix alias for [inv]. *)
  val ( ~/: ) : t -> t

  (** Modular addition. *)
  val ( +: ) : t -> t -> t

  (** Modular substraction. *)
  val ( -: ) : t -> t -> t

  (** Modular multiplication. *)
  val ( *: ) : t -> t -> t

  (** Modular division. *)
  val ( /: ) : t -> t -> t

  (** This is {!ModArith.div_factorize ~modulo}. *)
  val ( //: ) : t -> t -> t

  (** Modular exponentiation. *)
  val pow : t -> int -> t

  (** Random generation with the uniform distribution. *)
  val rand : unit -> t
end
