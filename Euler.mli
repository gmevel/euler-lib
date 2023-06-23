(** This is a library for arithmetic algorithms, primarily developed to solve
    {{: https://projecteuler.net} Project Euler} problems.
 *)

(******************************************************************************)
(******************************************************************************)

(** {2 Toplevel values} *)

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
    let fib = memoized_fix@@fun fib n ->
      if n < 2 then
        1
      else
        fib (n-1) + fib (n-2)
]} *)
val memoized_fix : (('a -> 'b) -> ('a -> 'b)) -> 'a -> 'b

(******************************************************************************)
(******************************************************************************)

(** {2 Modules on OCaml integers} *)

module Arith : sig
  (** Arithmetic on overflowing integers.

      All operations defined here act on overflowing integers. An overflowing
      integer is any native integer (type [int]) except [Stdlib.min_int].
      Otherwise said, they are the integers whose absolute value is at most
      [max_int] = 2{^[int_size]−1} − 1. This makes the range of overflowing
      integers symmetrical, so that computing the opposite [( ~- )] or the
      absolute value [abs] never overflows (the presence of an additional value
      2{^[int_size]−1} being arbitrarily interpreted as a negative value fits
      the modulo semantics of integers, but is alien to an overflowing
      semantics). This allows to use [Stdlib.min_int] as a special value, for
      example to signal that an overflow occurred. Here, we rather raise an
      exception for that purpose. All functions in this library may fail when
      given [Stdlib.min_int] where an overflowing integer was expected.

      All operations defined here either are free of overflows, or raise
      [Overflow] when their {e result} would exceed the range of overflowing
      integers (and only in that case). Functions which can overflow are
      signaled explicitly in this documentation.

      Functions in this library may raise [Assert_failure], instead of the more
      traditional [Invalid_argument], when some precondition is not met. This is
      not necessarily signaled in this documentation, but all preconditions are
      stated in the English description of functions. However, we still treat
      division‐by‐zero differently than other preconditions; for that we raise
      [Division_by_zero], and signal it in this documentation.

      As much as possible, time and space complexities are indicated. If absent,
      constant time or constant space is implied.
  *)

  (****************************************************************************)

  (** The largest representable integer. This is [Stdlib.max_int]. *)
  val max_int : int

  (** The smallest representable integer. This is the opposite of [max_int],
      and differs from [Stdlib.min_int]. *)
  val min_int : int

  (** Raised when the integer result of an operation is not representable. *)
  exception Overflow

  (** Raised when an operation was expected to perform an exact division but the
      dividend was not a multiple of the divisor. *)
  exception Division_not_exact

  (****************************************************************************)

  (** {2 Base operations } *)

  (** [sign a] is +1 if [a] is positive, 0 if it is null, and −1 if it is
      negative. *)
  val sign : int -> int

  (** [mul_sign s n] is [n] if [s] is non-negative and −[n] otherwise. *)
  val mul_sign : int -> int -> int

  (** [mul_sign0 s n] is [n] if [s] is positive, 0 if it is null, and −[n] if it
      is negative.
      In other words, it is equivalent to [mul (sign s) a], but much faster. *)
  val mul_sign0 : int -> int -> int

  (** Absolute value. By contrast with [Stdlib.abs], it cannot overflow. *)
  val abs : int -> int

  (** Minimum of two integers. *)
  val min : int -> int -> int

  (** Maximum of two integers. *)
  val max : int -> int -> int

  (** [compare a b] returns [0] when [a] is equal to [b], a negative integer
      when [a] is smaller than [b], and a positive integer when [a] is greater
      than [b]. It is the same as [Stdlib.compare] but much faster. *)
  val compare : int -> int -> int

  (** Integer opposite. By contrast with [Stdlib.(~-)], it cannot overflow. *)
  val opp : int -> int

  (** Overflowing integer addition.
      @raise Overflow when the result overflows. *)
  val add : int -> int -> int

  (** Overflowing integer subtraction.
      @raise Overflow when the result overflows. *)
  val sub : int -> int -> int

  (** Overflowing integer summation. Unlike a naive iteration of {!add}, this
      succeeds as long as the result is representable, even when partial sums
      overflow.
      Beware that the input sequence is read twice. If that is undesirable, use
      [Seq.memoize] (OCaml 4.14).
      {b Complexity:} time 𝒪([n]), space 𝒪(1)
      where [n] is the length of the sequence.
      @raise Overflow when the result overflows. *)
  val sum_seq : int Seq.t -> int

  (** Same as {!sum_seq} but where the input sequence is a list.
      @raise Overflow when the result overflows. *)
  val sum : int list -> int

  (** Overflowing integer multiplication.
      @raise Overflow when the result overflows. *)
  val mul : int -> int -> int

  (** [mul2 a] is equivalent to [mul 2 a] but much faster.
      @raise Overflow when the result overflows. *)
  val mul2 : int -> int

  (** [mul_pow2 k a] is equivalent to [mul (pow2 k) a] but much faster.
      @raise Overflow when the result overflows. *)
  val mul_pow2 : int -> int -> int

  (** Overflowing n-ary multiplication. Unlike a naive iteration of {!mul}, this
      succeeds as long as the result is representable even when partial products
      overflow (this situation only happens when one of the operands is zero).
      Every operand is read at most once;
      when an operand is zero, following operands are not read.
      {b Complexity:} time 𝒪([n]), space 𝒪(1)
      where [n] is the length of the sequence.
      @raise Overflow when the result overflows. *)
  val prod_seq : int Seq.t -> int

  (** Same as {!prod_seq} but where the input sequence is a list.
      @raise Overflow when the result overflows. *)
  val prod : int list -> int

  (** Exact integer division. By contrast with [Stdlib.(/)], it cannot overflow.
      @raise Division_by_zero when the divisor is null.
      @raise Division_not_exact when the dividend is not a multiple of the
      divisor. *)
  val div_exact : int -> int -> int

  (** [ediv a b] is the Euclidean division of [a] by [b]; it returns [(q, r)]
      such that [a] = [b]×[q] + [r] and 0 ≤ [r] < [b]. By contrast with division
      from the standard library, the remainder is never negative.
      @raise Division_by_zero when [b] is null. *)
  val ediv : int -> int -> int * int

  (** [equo a b] is the quotient of the Euclidean division of [a] by [b].
      @raise Division_by_zero when [b] is null. *)
  val equo : int -> int -> int

  (** [erem a b] is the remainder of the Euclidean division of [a] by [b].
      @raise Division_by_zero when [b] is null. *)
  val erem : int -> int -> int

  (** Faster alternatives when the divisor is 2. *)

  val ediv2 : int -> int * int
  val equo2 : int -> int
  val erem2 : int -> int

  (** Faster alternatives when the divisor is a power of 2.
      [ediv_pow2 a k] is equivalent to [ediv a (pow2 k)].
      @raise Overflow when the remainder overflows
        (happens only when [a] < 0 and [pow2 k] overflows). *)

  val ediv_pow2 : int -> int -> int * int
  val equo_pow2 : int -> int -> int
  val erem_pow2 : int -> int -> int

  (** [mul_div_exact a b d] computes [a]×[b]∕[d] when [d] does divide [a]×[b].
      @raise Division_by_zero when [d] is null.
      @raise Division_not_exact when [d] does not divide [a]×[b].
      @raise Overflow when the result overflows. *)
  val mul_div_exact : int -> int -> int -> int

  (** [mul_quo a b d] tries to compute [a]×[b]÷[d]. It can overflow even if the
      final result fits in the range of overflowing integers. This case is
      guaranteed not to happen as long the denominator of the reduced fraction
      is less than √[max_int] (in particular, when [d] is less than √[max_int]).
      {e FIXME: This must be fixed, but I don’t know how.}
      @raise Division_by_zero when [d] is null.
      @raise Overflow as described. *)
  val mul_quo : int -> int -> int -> int

  (** Overflowing integer exponentiation. [pow a n] is [a] to the power [n],
      provided that [n] is non‐negative. Of course, 0{^ 0} = 1.
      {b Complexity:} 𝒪(log([n])) integer multiplications.
      @raise Overflow when the result overflows. *)
  val pow : int -> int -> int

  (** [pow2 n] is equivalent to [pow 2 n], but much faster.
      {b Complexity:} 𝒪(1).
      @raise Overflow when the result overflows. *)
  val pow2 : int -> int

  (** [powm1 n] is equivalent to [pow (-1) n], but much faster.
      {b Complexity:} 𝒪(1). *)
  val powm1 : int -> int

  (** [log ~base n] is the logarithm of [n] in base [base] rounded towards zero,
      provided that [base] is at least 2 and that [n] is non‐negative.
      In other words, it returns ⌊ln([n])∕ln([base])⌋,
      This is the unique integer [k] such that [base]{^[k]} ≤ [n] < [base]{^[k]+1}.
      The default base is 10.
      {b Complexity:} 𝒪(log(log([n]))) integer multiplications.
      @return −1 when [n] = 0. *)
  val log : ?base:int -> int -> int

  (** [log2 n] is equivalent to [log ~base:2 n] but faster.
      @return −1 when [n] = 0. *)
  val log2 : int -> int

  (** [logsup ~base n] is the number of digits of [n] in base [base], provided
      that [base] is at least 2 and that [n] is non‐negative.
      It is equal to ⌈ln([n]+1)∕ln([base])⌉
      and also (when [n] is not null) to ⌊ln([n])∕ln([base])⌋ + 1.
      This is the unique integer [k] such that [base]{^[k]−1} ≤ [n] < [base]{^[k]}.
      The default base is 10.
      {b Complexity:} 𝒪(log(log([n]))) integer multiplications.
      @return 0 when [n] = 0. *)
  val logsup : ?base:int -> int -> int

  (** [log2sup n] is equivalent to [logsup ~base:2 n] but faster.
      @return 0 when [n] = 0. *)
  val log2sup : int -> int

  (** [isqrt n] is the integer square root of [n], provided that [n] is
      non‐negative. In other words, it is the greatest integer [r] such that
      [r]² ≤ [n], that is, ⌊√[n]⌋. *)
  val isqrt : int -> int

  (** [icbrt n] is the integer cube root of [n], rounded towards zero.
      In other words, it is [sign n × r] where [r] is the greatest integer such
      that [r]³ ≤ |[n]|. *)
  val icbrt : int -> int

  (****************************************************************************)

  (** {2 Divisors and multiples } *)

  (** [gcd a b] is the {e positive} greatest common divisor of [a] and [b].
      {b Complexity:} 𝒪(log(min(|[a]|,|[b]|))) integer divisions.
      @return 0 only when [a] = [b] = 0. *)
  val gcd : int -> int -> int

  (** [gcdext a b] is the extended Euclidean algorithm; it returns [(d, u, v)]
      where [d] is the {e positive} greatest common divisor of [a] and [b], and
      [u] and [v] are Bézout’s coefficients, such that [u]×[a] + [v]×[b] = [d].
      {b Complexity:} 𝒪(log(min(|[a]|,|[b]|))) integer divisions.
      @return [d] = 0 only when [a] = [b] = 0.
      @raise Overflow when the computation of Bézout’s coefficients provokes
        an overflow, even if there exists a representable pair of coefficients.
        {e FIXME: This must be fixed, but I don’t know how.} *)
  val gcdext : int -> int -> int * int * int

  (** [lcm a b] is the lesser common multiple of [a] and [b]. Its sign is that
      of [a]×[b].
      {b Complexity:} 𝒪(log(min(|[a]|,|[b]|))) integer divisions.
      @raise Overflow the result overflows. *)
  val lcm : int -> int -> int

  (** [valuation ~factor:d n] returns [(k, m)] such that [n] = [d]{^[k]}×[m] and
      [m] is not divisible by [d]. This assumes that [n] is not null and that
      [d] is not ±1.
      {b Complexity:} 𝒪([k]) = 𝒪(log([n])) integer divisions.
      @raise Division_by_zero when [d] is null. *)
  val valuation : factor:int -> int -> int * int

  (** [valuation_of_2] is equivalent to [valuation ~factor:2], but much faster. *)
  val valuation_of_2 : int -> int * int

  (** [is_square ~root n] is true if and only if [n] is the square of [root].
      When [root] is omitted, [is_square n] says whether [n] is a perfect
      square. *)
  val is_square : ?root:int -> int -> bool

  (** [jacobi a n] is the Jacobi symbol ([a]|[n]), provided that [n] is odd and
      positive.
      {b Complexity:} 𝒪(log(min(|[a]|,[n]))) integer divisions. *)
  val jacobi : int -> int -> int

  (****************************************************************************)

  (** {2 Binomial coefficients } *)

  (** [binoms n] returns the [n]{^th} row of Pascal’s triangle, provided that
      [n] is a non-negative integer.
      {b Complexity:} time 𝒪([n]), space 𝒪([n]).
      @raise Overflow when the greatest value of the result overflows.
        For 64‐bit OCaml, this happens for [n] ≥ 66. *)
  val binoms : int -> int array

  (** [binom n p] is the [p]{^th} element of the [n]{^th} row of Pascal’s
      triangle, provided that 0 ≤ [p] ≤ [n].
      {b Complexity:} time 𝒪(min([p],[n]−[p])) = 𝒪([n]), space 𝒪(1).
      @raise Overflow when the result overflows. *)
  val binom : int -> int -> int

  (** [central_binom p] is the [p]{^th} element of the 2×[p]{^th} row of
      Pascal’s triangle, provided that 0 ≤ [p].
      {b Complexity:} time 𝒪([p]), space 𝒪(1).
      @raise Overflow when the result overflows.
        For 64‐bit OCaml, this happens for [p] ≥ 33. *)
  val central_binom : int -> int

  (****************************************************************************)

  (** {2 Bit manipulation } *)

  (** [number_of_bits_set n] is the number of non-zero bits in the binary
      writing of the integer [n] (assuming two’s complement for negative
      numbers).
      {b Complexity:} 𝒪([result]). *)
  val number_of_bits_set : int -> int

  (****************************************************************************)

  (** {2 Randomness } *)

  (** [rand ~min ~max ()] draws a random integer with the uniform distribution
      between [min] and [max] (inclusive). [max] must be greater than or equal
      to [min]. [min] defaults to 0, [max] defaults to [max_int]. *)
  val rand : ?min:int -> ?max:int -> unit -> int

  (** [rand_signed ~max ()] draws a random integer with the uniform distribution,
      with an absolute value at most [max]. [max] must be non-negative. *)
  val rand_signed : ?max:int -> unit -> int

  (****************************************************************************)

  (** {2 Sequences } *)

  (** [range' ~step ~from ~til ()] returns the sequence of integers
      between [from] (inclusive) and [til] (exclusive), by increments of [step].
      [step] must be non-zero, but it can be negative, in which case the
      sequence is decreasing. [step] defaults to 1, [from] defaults to 0;
      when [til] is not given, the default is to build the sequence of all
      representable integers starting from [from] with increment [step].
      The sequence is persistent (the unit argument is meaningless, it just
      erases optional arguments).
      {b Complexity:} 𝒪(1) time and space. *)
  val range' : ?step:int -> ?from:int -> ?til:int -> unit -> int Seq.t

  (** [range ~from ~til] are the integers from [from] up to [til]−1.
      In other words it is [range' ~step:1 ~from ~til ()]. *)
  val range : from:int -> til:int -> int Seq.t

  (** [range_down ~from ~til] are the integers from [from] {e down} to [til]+1.
      In other words it is [range' ~step:~-1 ~from ~til ()]. *)
  val range : from:int -> til:int -> int Seq.t

  (** [range0 n] are the [n] integers from 0 up to [n]−1.
      In other words, it is [range ~from:0 ~til:n]. *)
  val range0 : int -> int Seq.t

  (** [range0 n] are the [n] integers from 1 up to [n].
      In other words, it is [range ~from:1 ~til:(n+1)]
      (except that [n] is allowed to be [max_int]). *)
  val range1 : int -> int Seq.t

  (****************************************************************************)

  (** {2 Operators }

      We deliberately override the standard operators. This is to make sure we
      don’t write unsafe arithmetic by accident.
  *)

  (** Prefix notation for [opp]. *)
  val ( ~- ) : int -> int

  (** Infix notation for [add]. *)
  val ( + ) : int -> int -> int

  (** Infix notation for [sub]. *)
  val ( - ) : int -> int -> int

  (** Infix notation for [mul]. *)
  val ( * ) : int -> int -> int

  (** Infix notation for [div_exact]. Note that this is more restrictive than
      the usual division from the standard library; this forces us to realize
      when we are doing a non-exact division, for which we must write [//]. *)
  val ( / ) : int -> int -> int

  (** Infix notation for [equo]. Note that this is not the same as [Stdlib.(/)]
      when the dividend is negative. *)
  val ( // ) : int -> int -> int

  (** Infix notation for [erem]. Same remark as for [//]. We don’t use (%)
      because we likely want that symbol to be available for other things (e.g.
      for function composition). *)
  val ( /% ) : int -> int -> int

  (** Same. *)
  val ( mod ) : int -> int -> int

  (** Module [Unsafe] gives access to the old operations for when we know what
      we are doing (i.e. we know that a given operation cannot overflow) and we
      absolutely don’t want to pay for the overhead of the safe functions.
      Operators in that module are suffixed with a [!] so as to distinguish them
      clearly. *)
  module Unsafe : sig

    val ( +! ) : int -> int -> int
    val ( -! ) : int -> int -> int
    val ( *! ) : int -> int -> int

  end (* module Unsafe *)

end (* module Arith *)

(******************************************************************************)
(******************************************************************************)

module Modular : sig
  (** Modular arithmetic.

      This module defines modular arithmetic operations, that is, operations on
      elements of the ring ℤ∕mℤ where m is a positive integer, called the
      modulus. All operations take m as a named parameter [~modulo]. Elements of
      ℤ∕mℤ are represented by their canonical representatives between 0 and m−1
      (included), of type [int]. All functions may assume that the modulus is
      positive and that canonical representatives are used, and may raise
      [Assert_failure] if that is not the case.

      The modulus can also be set globally, and not repeated for each individual
      operation, by giving it as a parameter to the functor {!Make}. This
      provides unary and binary operators.
  *)

  (****************************************************************************)

  (** Modular opposite. [opp ~modulo:m a] is the unique element [a'] of ℤ∕[m]ℤ
      such that [a']+[a] = 0. *)
  val opp : modulo:int -> int -> int

  (** Modular inverse. [inv ~modulo:m a] is the unique element [a'] of ℤ∕[m]ℤ
      such that [a']×[a] = 1, if it exists.
      {b Complexity:} 𝒪(log([a])) = 𝒪(log([m])) ([a] being under canonical form).
      @raise Division_by_zero when [a] is not invertible. *)
  val inv : modulo:int -> int -> int

  (** Modular addition. *)
  val add : modulo:int -> int -> int -> int

  (** Modular subtraction. *)
  val sub : modulo:int -> int -> int -> int

  (** Modular multiplication.
      {b Complexity:} 𝒪(log(min([a],[b]))) = 𝒪(log([m]))
      ([a] and [b] being under canonical forms). *)
  val mul : modulo:int -> int -> int -> int

  (** Modular division. [div ~modulo:m a b] is the unique element [c] of ℤ∕[m]ℤ
      such that [c]×[b] = [a], if it exists.
      {b Complexity:} 𝒪(log([b])) = 𝒪(log([m])) ([b] being under canonical form).
      @raise Division_by_zero when [b] is not invertible. *)
  val div : modulo:int -> int -> int -> int

  (** A more general modular division, which does not assume its right operand
      to be invertible. [div_nonunique ~modulo:m a b] is an element [c] such
      that [c]×[b] = [a], if there exists one. There exists such an element iff
      [a] is a multiple of gcd([m], [b]), in which case the result is defined
      modulo [m] ∕ gcd([m], [b]); it is unique only when [b] is invertible.
      For example, modulo 10, 3×4 = 8×4 = 2, so 2 divided by 4 may be 3 or 8
      (this example shows that the division 2 ∕ 4 cannot be simplified to 1 ∕ 2).
      {b Complexity:} 𝒪(log([b])) = 𝒪(log([m])) ([b] being under canonical form).
      @raise Division_by_zero when there is no such element. *)
  val div_nonunique : modulo:int -> int -> int -> int

  exception Factor_found of int

  (** A version of the modular inverse specialized for factorization purposes.
      [inv_factorize ~modulo:m a] is similar to [inv ~modulo:m a] but handles
      more precisely the cases when [a] is not invertible.
      {b Complexity:} 𝒪(log([a])) = 𝒪(log([m])) ([a] being under canonical form).
      @raise Division_by_zero when [a] is zero.
      @raise Factor_found when [a] is non‐zero and not invertible; in this case,
      gcd([m],[a]) is a non‐trivial factor of [m], which is returned as the
      parameter of the exception [Factor_found].
  *)
  val inv_factorize : modulo:int -> int -> int

  (** Modular exponentiation. When [n] is non‐negative, [pow ~modulo:m a n] is
      [a]{^[n]} in the ring ℤ∕[m]ℤ; when [n] is negative, it is [a']{^−[n]} where
      [a'] is the modular inverse of [a].
      {b Complexity:} 𝒪(log([m])×log(|[n]|)).
      @raise Division_by_zero when [n] is negative and [a] is not invertible. *)
  val pow : modulo:int -> int -> int -> int

  (** [rand ~modulo:m ()] draws a random element of the ring ℤ∕[m]ℤ, with the
      uniform distribution. *)
  val rand : modulo:int -> unit -> int

  (****************************************************************************)

  (** {2 Functorial interface} *)

  (** The functor application [Make (M)] defines modular arithmetic operations
      with a fixed, non‐zero modulus [M.modulo]. Because the modulus needs not
      be repeated for each individual operation, meaningful unary and binary
      operators can be defined.
      Operations in the resulting module follow the same specifications as those
      in module {!Modular}, with respect to return values, exceptions raised,
      and time costs. *)
  module Make : (sig val modulo : int end) -> sig

    (** The (positive) modulus m. *)
    val modulo : int

    (** The type of an element of the ring ℤ∕mℤ. *)
    type t = private int

    val of_int : int -> t
    val to_int : t -> int
    (** Conversions to and from integers. *)

    (** A prefix alias for [of_int]. *)
    val ( !: ) : int -> t

    (** Modular opposite. *)
    val opp : t -> t

    (** A prefix alias for [opp]. *)
    val ( ~-: ) : t -> t

    (** Modular inverse. *)
    val inv : t -> t

    (** A prefix alias for [inv]. *)
    val ( ~/: ) : t -> t

    (** Modular addition. *)
    val ( +: ) : t -> t -> t

    (** Modular subtraction. *)
    val ( -: ) : t -> t -> t

    (** Modular multiplication. *)
    val ( *: ) : t -> t -> t

    (** Modular division. *)
    val ( /: ) : t -> t -> t

    (** This is {!Modular.div_nonunique}[ ~modulo]. “Divide, just divide.” *)
    val ( //: ) : t -> t -> t

    (** This is {!Modular.inv_factorize}[ ~modulo]. *)
    val inv_factorize : t -> t

    (** Modular exponentiation. *)
    val pow : t -> int -> t

    (** An infix alias for [pow]. *)
    val ( **: ) : t -> int -> t

    (** Random generation with the uniform distribution. *)
    val rand : unit -> t

    (** The following operators are shortcuts that spare us the need to
        write [of_int] conversions on their operands. The most useful ones
        are [( *.:)] and [(/:.)], for multiplicative literal constants. *)

    val ( ~-:. ) : int -> t
    val ( ~/:. ) : int -> t
    val ( +.: ) : int -> t -> t
    val ( +:. ) : t -> int -> t
    val ( +.. ) : int -> int -> t
    val ( -.: ) : int -> t -> t
    val ( -:. ) : t -> int -> t
    val ( -.. ) : int -> int -> t
    val ( *.: ) : int -> t -> t
    val ( *:. ) : t -> int -> t
    val ( *.. ) : int -> int -> t
    val ( /.: ) : int -> t -> t
    val ( /:. ) : t -> int -> t
    val ( /.. ) : int -> int -> t
    val ( //.: ) : int -> t -> t
    val ( //:. ) : t -> int -> t
    val ( //.. ) : int -> int -> t
    val ( **.: ) : int -> int -> t

  end (* module Modular.Make *)

end (* module Modular *)

(******************************************************************************)
(******************************************************************************)

module Diophantine : sig
  (** Solving diophantine equations, {i i.e.} equations on integers. *)

  (****************************************************************************)

  (** Raised when a system has no solution. *)
  exception No_solution

  (** [solve_congruences @@ List.to_seq [ (a1, b1, m1) ; … ; (ak, bk, mk) ]],
      provided that the {i m{_i}} are non-zero, solves the following linear
      congruence system of unknown {i x}:
      - {i a{_1} · x ≡{_m{_1}} b{_1} }
      - {i … }
      - {i a{_k} · x ≡{_m{_k}} b{_k} }
      @return a pair [(x, m)] where 0 ≤ {i x} < {i m}, which represents the set
        of solutions {i x} + {i m}ℤ,
      @raise No_solution if there are no solutions.
  *)
  val solve_congruences : (int * int * int) Seq.t -> int * int

end (* module Diophantine *)

(******************************************************************************)
(******************************************************************************)

module Primes : sig
  (** Prime numbers and integer factorization. *)

  (****************************************************************************)

  (** The type of the factorized form of an integer.
      The factorization of [n] is a list [\[ (p1, k1) ; … ; (pℓ, kℓ) \]] such
      that [n] = [p1]{^[k1]} × … × [pℓ]{^[kℓ]} and [p1] < … < [pℓ] are prime.
  *)
  type factorization = (int * int) list

  (****************************************************************************)

  (** {2 Prime number count}

      The number π(x) of prime numbers less than x is asymptotically equivalent
      to x ∕ ln(x). It is also equivalent to li(x), where li is the
      {{: https://en.wikipedia.org/wiki/Logarithmic_integral_function}
      logarithmic integral function}, which gives a much more precise
      estimation.
  *)

  (** The logarithmic integral function. It is defined only for numbers
      (strictly) greater than [1.0].
      @param precision The series summation stops as soon as the increment
        becomes smaller than [precision]. *)
  val li : ?precision:float -> float -> float

  (** [overestimate_number_of_primes nmax] gives a relatively precise upper
      bound on the number of prime numbers below [nmax]. *)
  val overestimate_number_of_primes : int -> int

  (****************************************************************************)

  (** {2 First prime numbers} *)

  (** The twenty‐five prime numbers less than 100, in ascending order. *)
  val primes_under_100 : int array

  (** The prime numbers less than 10 000, in ascending order. *)
  val primes_under_10_000 : int array

  (** [primes nmax ~do_prime:f] calls [f] on all prime numbers in ascending
      order from 2 to {e slightly more than} [nmax], as soon as they are found.
      This is useful to iterate on prime numbers and stop when some condition is
      met.
      {b Complexity:} time 𝒪([nmax]×log(log([nmax]))),
      space 𝒪(π(√[nmax])) = 𝒪(√[nmax] ∕ log([nmax])).
  *)
  val primes : int -> do_prime:(int -> unit) -> unit

  (** Extended prime sieve. [factorizing_sieve nmax ~do_factors:f] computes the
      factorization of all numbers up to [nmax] (included). The result is an
      array [s] such that [s.(n)] is the factorization of [n]. The function also
      calls [f] on the factorization of each number from 2 to [nmax], in order.
      This is useful to iterate on the factorized form of (small) numbers and
      stop when some condition is met.
      Note that this is costly both in time and in space, so [nmax] is bridled
      with an internal upper bound.
      {b Complexity:} time 𝒪([nmax]×log(log([nmax]))),
      space 𝒪([nmax]×log(log([nmax]))).
  *)
  (* Supporting material for the complexity claims:
   *   — The number of non‐distinct prime factors of n, Ω(n), is log (log n)
   *     in average, hence its sum up to nmax is about log (log nmax).
   *         http://mathworld.wolfram.com/PrimeFactor.html
   *   — The number of distinct prime factors of n is also log (log n)
   *     in average, hence its sum up to nmax is about log (log nmax).
   *         http://oeis.org/A001221
   *         http://mathworld.wolfram.com/DistinctPrimeFactors.html
   *)
  val factorizing_sieve :
    int -> do_factors:(factorization -> int -> unit) -> factorization array

  (****************************************************************************)

  (** {2 Primality testing} *)

  (** Primality test. [is_prime n] is true if and only if [n] is a prime number.
      Note that this is a deterministic test.
      {b Complexity:} 𝒪(fast). *)
  val is_prime : int -> bool

  (** {2 Integer factorization} *)

  (** Integer factorization.
      This uses Lenstra’s elliptic‐curve algorithm for finding factors (as of
      2018, it is the most efficient known algorithm for 64‐bit numbers).
      {b Complexity:} 𝒪(terrible).
      @param tries The number of elliptic curves to try before resigning.
      @param max_fact The “small exponents” tried by Lenstra’s algorithm are the
        factorial numbers up to the factorial of [max_fact].
      @return the prime factorization of the given number.
        It may contain non‐prime factors [d], if their factorization failed
        within the allowed time; this is signaled by negating their value, as in
        [(−d, 1)]. This is highly unlikely with default parameters.
      @raise Assert_failure when the number to factorize is not positive.
  *)
  val factors : ?tries:int -> ?max_fact:int -> int -> factorization

  (****************************************************************************)

  (** {2 Usual functions}

      Some functions defined in this section can be computed efficiently when
      the factorization of their argument is known. Hence they take an optional
      argument [?factors] which is expected to be the factorization of their
      main argument. If absent, those functions resort to computing the
      factorization themselves, or another inefficient algorithm.
  *)

  (** Euler’s totient function. [eulerphi n], often noted φ([n]), is the number
      of integers between 1 and [n] which are coprime with [n], provided that
      [n] is positive. *)
  val eulerphi : ?factors:factorization -> int -> int

  (** [eulerphi_from_file nmax] loads precomputed values of φ from a file on
      disk.
      @return an array [phi] such that [phi.(n)] = φ([n]) for all
        1 ≤ [n] ≤ [nmax]. *)
  val eulerphi_from_file : int -> int array

  (** [number_of_divisors n] is the number of divisors of [n] (including 1 and
      [n] itself), provided that [n] is positive. *)
  val number_of_divisors : ?factors:factorization -> int -> int

  (** [divisors n] is the list of all divisors of [n] (including 1 and [n]
      itself) in ascending order, provided that [n] is positive. *)
  val divisors : ?factors:factorization -> int -> int list

  (** [gen_divisor_pairs n] returns all pairs (d, [n]/d) where d divides [n] and
      1 ≤ d ≤ √[n], provided that [n] is positive. Pairs are presented in
      ascending order of d. When [n] is a perfect square, the pair (√[n], √[n])
      is presented only once. *)
  val gen_divisor_pairs : ?factors:factorization -> int -> (int * int) Seq.t

end (* module Primes *)

(******************************************************************************)
(******************************************************************************)

module Farey : module type of Farey
