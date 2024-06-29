(** This is a library for arithmetic algorithms, primarily developed to solve
    {{: https://projecteuler.net} Project Euler} problems, although it is of
    more general use.
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
      Otherwise said, it is an integer whose absolute value is at most
      {!max_int} = 2{^[int_size]−1} − 1, implying that the minimum value is
      {!min_int} = −{!max_int} = −2{^[int_size]−1} + 1 = [Stdlib.min_int]+1 (!).
      This makes the range of overflowing
      integers symmetrical, so that computing the opposite [( ~- )] or the
      absolute value [abs] never overflows (the presence of an additional value
      2{^[int_size]−1} being arbitrarily interpreted as a negative value fits
      the modulo semantics of integers, but is alien to an overflowing
      semantics).

      Incidentally, this allows to use [Stdlib.min_int] as a special value, for
      example to signal that an overflow occurred. However in this library we rather raise an
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

      As much as possible, time and space complexities are indicated (time
      complexities are termed as a count of machine-arithmetic operations).
      If absent, constant time or constant space is implied.

      By opening it, this module can mostly be used as a drop-in replacement for
      Stdlib arithmetic operations. This allows lightweight notations and avoids
      accidental uses of overflow-unsafe Stdlib operations.
      Beware of the following changes:

      - {!min_int} has a different value (as explained);
      - {!min}, {!max} and {!compare} are monomorphic functions for type [int],
        instead of polymorphic functions;
      - {!( ** )} is redefined as the {e integer} exponentation
        (the floating-point exponentiation is available as {!( **. )});
      - {!( / )} is restricted to an {e exact} division
        (a possibly-rounded division is available as {!( // )},
        which differs from [Stdlib.(/)] in that it does an Euclidean division);
      - {!(mod)} is redefined as the {e Euclidean} remainder.
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

  (** [equal a b] returns [true] when [a] is equal to [b], [false] otherwise. *)
  val equal : int -> int -> bool

  (** [pred n] is [n]−1.
      @raise Overflow when the result overflows. *)
  val pred : int -> int

  (** [succ n] is [n]+1.
      @raise Overflow when the result overflows. *)
  val succ : int -> int

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
      {b Complexity:} time 𝒪({i n}), space 𝒪(1)
      where {i n} is the length of the sequence.
      @raise Overflow when the result overflows. *)
  val sum_of_seq : int Seq.t -> int

  (** Same as {!sum_of_seq} but where the input sequence is a list.
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
      {b Complexity:} time 𝒪({i n}), space 𝒪(1)
      where {i n} is the length of the sequence.
      @raise Overflow when the result overflows. *)
  val prod_of_seq : int Seq.t -> int

  (** Same as {!prod_of_seq} but where the input sequence is a list.
      @raise Overflow when the result overflows. *)
  val prod : int list -> int

  (** Exact integer division. By contrast with [Stdlib.(/)], it cannot overflow.
      @raise Division_by_zero when the divisor is null.
      @raise Division_not_exact when the dividend is not a multiple of the
      divisor. *)
  val div_exact : int -> int -> int

  (** [sdiv a b] is the “signed” division of [a] by [b];
      it returns [(q, r)] such that [a] = [b]×[q] + [r] and |[r]| < |[a]|
      and [r] is of the same sign as [a].

      This is the standard library’s division. However, using [sdiv] is better
      than computing both [Stdlib.(a / b)] and [Stdlib.(a mod b)] separately,
      because [sdiv] spares one machine division, which is much more costly than
      a multiplication.

      [sdiv] is slightly faster than {!ediv}, so it is also useful when we don’t
      need the remainder to be positive or when we know that [a] ≥ 0.

      @raise Division_by_zero when [b] is null. *)
  val sdiv : int -> int -> int * int

  (** [ediv a b] is the Euclidean division of [a] by [b];
      it returns [(q, r)] such that [a] = [b]×[q] + [r] and 0 ≤ [r] < [b].
      By contrast with {!sdiv}, the remainder is never negative.
      @raise Division_by_zero when [b] is null. *)
  val ediv : int -> int -> int * int

  (** [equo a b] is the quotient of the Euclidean division of [a] by [b].
      @raise Division_by_zero when [b] is null. *)
  val equo : int -> int -> int

  (** [erem a b] is the remainder of the Euclidean division of [a] by [b].
      @raise Division_by_zero when [b] is null. *)
  val erem : int -> int -> int

  val ediv2 : int -> int * int
  val equo2 : int -> int
  val erem2 : int -> int

  (** Faster alternatives when the divisor is 2. *)

  val ediv_pow2 : int -> int -> int * int
  val equo_pow2 : int -> int -> int
  val erem_pow2 : int -> int -> int

  (** Faster alternatives when the divisor is a power of 2.
      [ediv_pow2 a k] is equivalent to [ediv a (pow2 k)].
      @raise Overflow when the remainder overflows
        (happens only when [a] < 0 and [pow2 k] overflows;
        [equo_pow2] is not affected). *)

  (** [mul_div_exact a b c] computes [a]×[b]∕[c] when [c] does divide [a]×[b].
      @raise Division_by_zero when [c] is null.
      @raise Division_not_exact when [c] does not divide [a]×[b].
      @raise Overflow when the result overflows. *)
  val mul_div_exact : int -> int -> int -> int

  (** [mul_ediv a b c] computes the Euclidean division of [a]×[b] by [c],
      even when the intermediate product would overflow.
      @raise Division_by_zero when [c] is null.
      @raise Overflow when the quotient overflows. *)
  val mul_ediv : int -> int -> int -> int * int

  (** [mul_equo a b c] is the quotient of the Euclidean division of [a]×[b] by [c].
      @raise Division_by_zero when [c] is null.
      @raise Overflow when the result overflows. *)
  val mul_equo : int -> int -> int -> int

  (** [mul_erem a b c] is the remainder of the Euclidean division of [a]×[b] by [c].
      It cannot overflow.

      If you are interested in modular arithmetic, see also {!Modular.mul}.

      @raise Division_by_zero when [c] is null. *)
  val mul_erem : int -> int -> int -> int

  (****************************************************************************)

  (** {2 Exponentiation and logarithms } *)

  (** Overflowing integer exponentiation. [pow a n] is [a] to the power [n],
      provided that [n] is non‐negative. Of course, 0{^ 0} = 1.
      {b Complexity:} 𝒪(log([n])) integer multiplications.
      @raise Overflow when the result overflows. *)
  val pow : int -> int -> int

  (** [pow2 n] is equivalent to [pow 2 n], but much faster.
      {b Complexity:} 𝒪(1).
      @raise Overflow when the result overflows. *)
  val pow2 : int -> int

  (** [powm1 n] is equivalent to [pow (-1) (abs n)], but much faster.
      [n] may be negative.
      {b Complexity:} 𝒪(1). *)
  val powm1 : int -> int

  (** [ilog ~base n] is the logarithm of [n] in base [base] rounded towards zero,
      provided that [base] is at least 2 and that [n] is non‐negative.
      In other words, it returns ⌊ln([n])∕ln([base])⌋,
      This is the unique integer [k] such that [base]{^[k]} ≤ [n] < [base]{^[k]+1}.
      This is a relatively slow operation in general,
      but it is specially optimized for bases 2, 16, 64, 10 and 60.
      The default base is 10.
      {b Complexity:} 𝒪(log(log([n]))) integer multiplications.
      @return −1 when [n] = 0. *)
  val ilog : ?base:int -> int -> int

  (** [ilog2 n] is equivalent to [ilog ~base:2 n] but faster.
      @return −1 when [n] = 0. *)
  val ilog2 : int -> int

  (** [ilogsup ~base n] is the number of digits of [n] in base [base], provided
      that [base] is at least 2 and that [n] is non‐negative.
      It is equal to ⌈ln([n]+1)∕ln([base])⌉
      and also (when [n] is not null) to ⌊ln([n])∕ln([base])⌋ + 1.
      This is the unique integer [k] such that [base]{^[k]−1} ≤ [n] < [base]{^[k]}.
      As for {!ilog}, this is relatively slow
      but it is fast for bases 2, 16, 64, 10 and 60.
      The default base is 10.
      {b Complexity:} 𝒪(log(log([n]))) integer multiplications.
      @return 0 when [n] = 0. *)
  val ilogsup : ?base:int -> int -> int

  (** [ilog2sup n] is equivalent to [ilogsup ~base:2 n] but faster.
      @return 0 when [n] = 0. *)
  val ilog2sup : int -> int

  (** [is_pow ~base ~exp n] is true if and only if [n] = [base]{^[exp]}.
      When [exp] is omitted, [is_pow ~base n] says whether [n] is some power
      of [base].
      When [exp] is provided, it is equivalent to {!is_kth_pow}[ ~k:exp ~root:base n].
      The default base is 10. *)
  val is_pow : ?base:int -> ?exp:int -> int -> bool

  (** [is_pow2 n] is equivalent to [is_pow ~base:2 n], but much faster. *)
  val is_pow2 : int -> bool

  (****************************************************************************)

  (** {2 Roots } *)

  (** [kth_root ~k n] is the integer [k]{^th} root of [n], rounded towards zero.
      In other words, it is [sign n × r] where [r] is the greatest integer such
      that [r]{^[k]} ≤ |[n]|.
      [k] must be positive. If [k] is even, [n] must be non-negative. *)
  val kth_root : k:int -> int -> int

  (** [isqrt n] is the integer square root of [n], provided that [n] is
      non‐negative. In other words, it is the greatest integer [r] such that
      [r]² ≤ [n], that is, ⌊√[n]⌋.
      It is equivalent to [kth_root ~k:2 n] but should be faster. *)
  val isqrt : int -> int

  (** [isqrt_if_square n] is the integer square root of [n]
      if [n] is a perfect square, or [None] otherwise.
      When [n] is not square, this is faster than combining
      {!isqrt} with {!is_square}. *)
  val isqrt_if_square : int -> int option

  (** [icbrt n] is the integer cube root of [n], rounded towards zero.
      In other words, it is [sign n × r] where [r] is the greatest integer such
      that [r]³ ≤ |[n]|.
      It is equivalent to [kth_root ~k:3 n] but may be faster. *)
  val icbrt : int -> int

  (** [is_kth_pow ~k ~root n] is true if and only if [n] = [root]{^[k]}.
      When [root] is omitted, [is_kth_pow n] says whether [n] is a [k]{^th} power.
      When [root] is provided, it is equivalent to {!is_pow}[ ~base:root ~exp:k n]. *)
  val is_kth_pow : k:int -> ?root:int -> int -> bool

  (** [is_square ~root n] is true if and only if [n] is the square of [root].
      When [root] is omitted, [is_square n] says whether [n] is a perfect square.
      It is equivalent to [is_kth_pow ~k:2 ~root n] but faster. *)
  val is_square : ?root:int -> int -> bool

  (****************************************************************************)

  (** {2 Divisors and multiples } *)

  (** [is_multiple ~of_:a b] is [true] iff [b] is a multiple of [a].
      This function never raises [Division_by_zero],
      but returns [true] when [a] = 0 and [b] = 0. *)
  val is_multiple : of_:int -> int -> bool

  (** [is_even a] is equivalent to [is_multiple ~of_:2 a] but faster. *)
  val is_even : int -> bool

  (** [is_odd a] is equivalent to [not (is_multiple ~of_:2 a)] but faster. *)
  val is_odd : int -> bool

  (** [gcd a b] is the {e positive} greatest common divisor of [a] and [b].
      {b Complexity:} 𝒪(log(min(|[a]|,|[b]|))) integer divisions.
      @return 0 only when [a] = [b] = 0. *)
  val gcd : int -> int -> int

  (** The positive greatest common divisor of a sequence of numbers.
      {b Complexity:} 𝒪({i n} × log({i m})) integer divisions
      where {i n} is the length of the sequence and {i m} is its first element. *)
  val gcd_of_seq : int Seq.t -> int

  (** [gcdext a b] is the extended Euclidean algorithm; it returns [(d, u, v)]
      where [d] is the {e positive} greatest common divisor of [a] and [b], and
      [u] and [v] are Bézout’s coefficients, such that [u]×[a] + [v]×[b] = [d].
      Bézout’s coefficients [(u, v)] are defined modulo [(b/d, −a/d)].

      If [a] ≠ 0, [b] ≠ 0 and |[a]| ≠ |[b]|,
      then this function returns the unique pair of coefficients
      whose magnitude is minimal; this pair is in the following range
      (in particular, the function never overflows):

      - |[u]| ≤ ½|[b/d]|
      - |[v]| ≤ ½|[a/d]|

      In the edge cases ([a] = 0 or [b] = 0 or |[a]| = |[b]|),
      it returns [(u, v)] = (0, 0) or (±1, 0) or (0, ±1).

      {b Complexity:} 𝒪(log(min(|[a]|,|[b]|))) integer divisions.
      @return [d] = 0 only when [a] = [b] = 0. *)
  val gcdext : int -> int -> int * int * int

  (** The positive greatest common divisor of a sequence of numbers, with
      Bézout coefficients.
      [gcdext_of_seq @@ List.to_seq [ a1 ; … ; an ]] returns a pair
      [(d, [ u1 ; … ; un ])] such that [d] is the positive greatest common
      divisor of {i a{_1}, …, a{_n}}, and the {i u{_i}} are coefficients such that
      {i a{_1}×u{_1} + … + a{_n}×u{_n} = d}.

      {b Complexity:} 𝒪({i n} × log({i m})) integer divisions
      where {i n} is the length of the sequence and {i m} is its first element.

      @raise Overflow when the computation of Bézout’s coefficients provokes
        an overflow, which may happen even if there exists a representable
        vector of coefficients. *)
  val gcdext_of_seq : int Seq.t -> int * int list

  (** [lcm a b] is the lesser common multiple of [a] and [b].
      Its sign is that of [a]×[b].
      {b Complexity:} 𝒪(log(min(|[a]|,|[b]|))) integer divisions.
      @raise Overflow when the result overflows. *)
  val lcm : int -> int -> int

  (** The lesser common multiple of a sequence of numbers.
      {b Complexity:} 𝒪({i n} × log({i m})) integer divisions
      where {i n} is the length of the sequence and {i m} is its first element.
      @raise Overflow when the result overflows. *)
  val lcm_of_seq : int Seq.t -> int

  (** [valuation ~factor:d n] returns [(k, m)] such that [n] = [d]{^[k]}×[m] and
      [m] is not divisible by [d]. This assumes that [n] is not null and that
      [d] is not ±1.
      {b Complexity:} 𝒪([k]) = 𝒪(log([n])) integer divisions.
      @raise Division_by_zero when [d] is null. *)
  val valuation : factor:int -> int -> int * int

  (** [valuation_of_2] is equivalent to [valuation ~factor:2], but much faster. *)
  val valuation_of_2 : int -> int * int

  (** [smallest_root n] returns [(r, k)] such that [n] = [r]{^[k]} and |[r]| is
      minimal (which also implies that [k] is maximal).
      [n] must be non-zero.
      @return (1, 0) for [n] = 1, and (-1, 1) for [n] = -1. *)
  val smallest_root : int -> int * int

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

  (** [central_binom p] is the [p]{^th} element of the (2×[p]){^th} row of
      Pascal’s triangle, provided that 0 ≤ [p].
      {b Complexity:} time 𝒪([p]), space 𝒪(1).
      @raise Overflow when the result overflows.
        For 64‐bit OCaml, this happens for [p] ≥ 33. *)
  val central_binom : int -> int

  (****************************************************************************)

  (** {2 Bit manipulation } *)

  (** Most standard bitwise functions are omitted, because it is not clear what
      to do with overflowing integers. One common usage, dividing or multiplying
      by powers of 2, is covered by other, specialized functions.

      Missing functions from the standard library:
        [(land)] / [Int.logand],
        [(lor)] / [Int.logor],
        [(lxor)] / [Int.logxor],
        [lnot] / [Int.lognot],
        [(lsl)] / [Int.shift_left],
        [(lsr)] / [Int.shift_right_logical],
        [(asr)] / [Int.shift_right].
  *)

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
  val range_down : from:int -> til:int -> int Seq.t

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

  (** Prefix notation for {!opp}. *)
  val ( ~- ) : int -> int

  (** Infix notation for {!add}. *)
  val ( + ) : int -> int -> int

  (** Infix notation for {!sub}. *)
  val ( - ) : int -> int -> int

  (** Infix notation for {!mul}. *)
  val ( * ) : int -> int -> int

  (** Infix notation for {!div_exact}. Note that this is more restrictive than
      the usual division from the standard library; this forces us to realize
      when we are doing a non-exact division, for which we must write {! (//)}. *)
  val ( / ) : int -> int -> int

  (** Infix notation for {!equo}. Note that this is not the same as [Stdlib.(/)]
      when the dividend is negative. *)
  val ( // ) : int -> int -> int

  (** Infix notation for {!erem}. Same remark as for {! (//)}. We don’t use [(%)]
      because we likely want that symbol to be available for other things (e.g.
      for function composition). *)
  val ( /% ) : int -> int -> int

  (** Same. *)
  val ( mod ) : int -> int -> int

  (** Infix notation for {!pow}. Note that this overrides the standard
      library’s notation for floating-point exponentiation.
      Thus we re-expose the latter with the notation {! ( **. )}. *)
  val ( ** ) : int -> int -> int

  (** New infix notation for [Stdlib.( ** )], the floating-point exponentiation. *)
  val ( **. ) : float -> float -> float

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
      elements of the ring ℤ∕{i m}ℤ where {i m} is a positive integer, called the
      modulus. All operations take {i m} as a named parameter [~modulo]. Elements of
      ℤ∕{i m}ℤ are represented by their canonical representatives between 0 and {i m}−1
      (included), of type [int]. All functions may assume that the modulus is
      positive and that canonical representatives are used, and may raise
      [Assert_failure] if that is not the case.

      The modulus can also be set globally, and not repeated for each individual
      operation, by giving it as a parameter to the functor {!Make}. This
      provides unary and binary operators.

      A related function, {!Primes.order},
      which computes the multiplicative order modulo {i m},
      is found in module [Primes],
      because it depends on computing integer factorizations.
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

    (** The (positive) modulus {i m}. *)
    val modulo : int

    (** The type of an element of the ring ℤ∕{i m}ℤ. *)
    type t = private int

    val of_int : int -> t
    val to_int : t -> int
    (** Conversions to and from integers. *)

    (** A prefix alias for {!of_int}. *)
    val ( !: ) : int -> t

    (** Modular opposite. *)
    val opp : t -> t

    (** A prefix alias for {!opp}. *)
    val ( ~-: ) : t -> t

    (** Modular inverse. *)
    val inv : t -> t

    (** A prefix alias for {!inv}. *)
    val ( ~/: ) : t -> t

    (** Modular addition. *)
    val ( +: ) : t -> t -> t

    (** Modular subtraction. *)
    val ( -: ) : t -> t -> t

    (** Modular multiplication. *)
    val ( *: ) : t -> t -> t

    (** Modular division. *)
    val ( /: ) : t -> t -> t

    (** This is {!Modular.div_nonunique}[ ~modulo]. *)
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
        write {!of_int} conversions on their operands. The most useful ones
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

      {b Complexity:} time 𝒪({i k}×log(max({i m{_1}}, …, {i m{_k}}))),
      space 𝒪(1).

      @return a pair [(x, m)] where 0 ≤ {i x} < {i m}, which represents the set
        of solutions {i x} + {i m}ℤ,
      @raise No_solution if there are no solutions.
      @raise Overflow when the result overflows, that is, when {i m} overflows.
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

      The number π({i x}) of prime numbers less than x is asymptotically equivalent
      to {i x} ∕ ln({i x}). It is also equivalent to li(x), where li is the
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

  (** [iter_primes nmax ~do_prime:f] calls [f] on all prime numbers in ascending
      order from 2 to {e slightly more than} [nmax], as soon as they are found.
      This is useful to iterate on prime numbers and stop when some condition is
      met.
      {b Complexity:} time 𝒪([nmax]×log(log([nmax]))),
      space 𝒪(π(√[nmax])) = 𝒪(√[nmax] ∕ log([nmax])).
  *)
  val iter_primes : int -> do_prime:(int -> unit) -> unit

  (** The sequence of prime numbers up to a specified bound.
      This is significantly slower than {!iter_primes}
      (about 50 times slower for [nmax = 1_000_000_000]),
      but has the advantage that advancing through the sequence
      is controlled by the consumer,
      This is a purely functional algorithm,
      hence the produced sequence is persistent.
      {b Complexity:} time 𝒪([nmax]×log([nmax])×log(log([nmax]))),
      space 𝒪(√[nmax] ∕ log([nmax])).
  *)
  val gen_primes : int -> int Seq.t

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

  (** [number_of_divisors n] is the number of divisors of [n] (including 1 and
      [n] itself), provided that [n] is positive. *)
  val number_of_divisors : ?factors:factorization -> int -> int

  (** {{: https://en.wikipedia.org/wiki/Divisor_function} Divisor sum}.
      [sum_of_divisors ~k n],
      often noted σ{_[k]}([n]),
      is the sum of the [k]-th powers of all the divisors of [n]
      (including 1 and [n] itself),
      provided that [k] is non-negative and [n] is positive.
      In particular, for [k] = 0 it gives the number of divisors,
      and for [k] = 1 it gives the sum of the divisors.
      The default value of [k] is 1.

      @raise Overflow when the result overflows. *)
  val sum_of_divisors : ?k:int -> ?factors:factorization -> int -> int

  (** [divisors n] is the list of all divisors of [n] (including 1 and [n]
      itself) in ascending order, provided that [n] is positive. *)
  val divisors : ?factors:factorization -> int -> int list

  (** [divisor_pairs n] is the list of all pairs ({i d}, [n]/{i d})
      where d divides [n] and 1 ≤ {i d} ≤ √[n],
      provided that [n] is positive.
      Pairs are presented in ascending order of {i d}.
      When [n] is a perfect square, the pair (√[n], √[n]) is presented once. *)
  val divisor_pairs : ?factors:factorization -> int -> (int * int) list

  (** Same as {!divisor_pairs} but returns a [Seq.t]. *)
  val gen_divisor_pairs : ?factors:factorization -> int -> (int * int) Seq.t

  (** {{: https://en.wikipedia.org/wiki/Euler%27s_totient_function}
      Euler’s totient function}.
      [eulerphi n],
      often noted φ([n]),
      is the number of integers between 1 and [n] which are coprime with [n],
      provided that [n] is positive. *)
  val eulerphi : ?factors:factorization -> int -> int

  (** [eulerphi_from_file nmax] loads precomputed values of φ from a file on
      disk.
      @return an array [phi] such that [phi.(n)] = φ([n]) for all
        1 ≤ [n] ≤ [nmax]. *)
  val eulerphi_from_file : int -> int array

  (** {{: https://en.wikipedia.org/wiki/Jordan%27s_totient_function}
      Jordan’s totient function}.
      [jordan ~k n],
      often noted J{_[k]}([n]),
      is the number of [k]-tuples ({i a{_1}, …, a}{_[k]}) such that
      every {i a{_i}} is between 1 and [n],
      and gcd({i a{_1}, …, a}{_[k]}, [n]) = 1
      (in other words, the tuple is setwise-coprime with [n],
      but not necessarily pairwise-coprime).
      This is a generalization of Euler’s totient, which is obtained with [k] = 1.
      It requires that [k] and [n] are positive.

      @raise Overflow when the result overflows. *)
  val jordan : k:int -> ?factors:factorization -> int -> int

  (** {{: https://en.wikipedia.org/wiki/Carmichael_function}
      Carmichael’s function}.
      [carmichael n],
      often noted λ([n]),
      is the smallest positive exponent {i k} such that,
      for all {i a} coprime with [n], we have {i a{^k}} ≡ 1 (mod [n]).
      In other words, it is the exponent of the multiplicative group of integers
      modulo [n], that is, the least common multiple of the orders of all the
      invertible integers modulo [n].
      It divides Euler’s totient but may be strictly smaller than it.
      This function requires that [n] is positive.
  *)
  val carmichael : ?factors:factorization -> int -> int

  (** {{: https://en.wikipedia.org/wiki/Möbius_function}
      Möbius’ function}.
      [mobius n],
      often noted μ([n]),
      is 0 if [n] has a square factor,
      −1 if [n] has an odd number of prime factors,
      or +1 if [n] has an even number of prime factors.
      This function requires that [n] is positive.
  *)
  val mobius : ?factors:factorization -> int -> int

  (** {{: https://en.wikipedia.org/wiki/Arithmetic_derivative}
      Arithmetic derivative} of an integer.
      [derivative n],
      often noted D([n]),
      is such that
      D(0) = 0,
      D(−1) = −1,
      D({i p}) = 1 for all primes {i p},
      and D({i m×n}) = D({i m})×{i n} + {i m}×D({i n}) for all integers {i m, n}.
      @raise Overflow when the result overflows. *)
  val derivative : ?factors:factorization -> int -> int

  (** [order ~modulo:m a],
      where [m] ≠ 0,
      is the multiplicative order of [a] modulo [m],
      This is the smallest positive exponent {i n} such that
      [a]{^{i n}} ≡ 1 (mod [m]).

      If given, [factors_mod] must be the factorization of [m].

      If given, [factors_pred_primes] must be the factorizations
      of all the [p]−1 where [p] are the prime factors of [m],
      sorted by [p].

      @raise Division_by_zero when [a] is not invertible modulo [m].
  *)
  val order :
    ?factors_pred_primes:factorization list -> ?factors_mod:factorization ->
    modulo:int -> int -> int

  (** [order_with_known_multiple ~phi ~modulo:m a]
      is the same as {!order}[ ~modulo:m a],
      but exploits the fact that the order is a divisor of [phi].
      Values suitable for [phi] always include
      λ([m]) ({!carmichael}[ m])
      and φ([m]) ({!eulerphi}[ m]);
      in some situations, a smaller value may be known.

      If given, [factors_phi] must be the factorization of [phi].

      @raise Division_by_zero when [a] is not invertible modulo [m].
  *)
  val order_with_known_multiple :
    ?factors_phi:factorization -> phi:int ->
    modulo:int -> int -> int

  (** [order_mod_prime_pow ~modulo:(p, k) a],
      where [p] is a prime and [k] > 0,
      is the multiplicative order
      of [a] modulo [p]{^[k]},
      It is assumed that [p]{^[k]} does not overflow.

      If given, [factors_pred_prime] must be the factorization of [p]−1.

      @raise Division_by_zero when [a] is not invertible modulo [m].
  *)
  val order_mod_prime_pow :
    ?factors_pred_prime:factorization ->
    modulo:(int * int) -> int -> int

end (* module Primes *)

(******************************************************************************)
(******************************************************************************)

module Farey : module type of Farey
