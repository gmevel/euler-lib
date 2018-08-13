(** Arithmetic on overflowing integers. *)

(**
    All operations defined here act on overflowing integers. An overflowing
    integer is any native integer (type [int]) except [min_int]. Otherwise said,
    they are the integers whose absolute value is at most
    [max_int] = 2{^[int_size]−1} − 1. This makes the range of overflowing
    integers symmetrical, so that computing the opposite [( ~- )] or the
    absolute value [abs] never overflows (the presence of an additional value
    2{^[int_size]−1} being arbitrarily interpreted as a negative value fits the
    modulo semantics of integers, but is alien to an overflowing semantics).
    This allows to use [min_int] as a special value, for example to signal that
    an overflow occured. Here, we rather raise an exception for that purpose.
    All functions in this library may fail when given [min_int] where an
    overflowing integer was expected.

    All operations defined here either are free of overflows, or raise
    [Overflow] when their {e result} would exceed the range of overflowing
    integers (and only in that case). Functions which can overflow are signaled
    explicitely in this documentation.

    Functions in this library may raise [Assert_failure], instead of the more
    traditional [Invalid_argument], when some precondition is not met. This is
    not necessarily signaled in this documentation, but all preconditions are
    stated in the English description of functions. However, we still treat
    division‐by‐zero differently than other preconditions; for that we raise
    [Division_by_zero], and signal it in this documentation.

    As much as possible, time and space complexities are indicated. If absent,
    constant time or constant space is implied.
 *)

(** Raised when the result of an operation exceeds the range of overflowing
    integers. *)
exception Overflow

(** Raised when an operation was expected to perform an exact division but the
    dividend was not a multiple of the divisor. *)
exception Division_not_exact

(******************************************************************************)

(** {2 Base operations } *)

(** [sign a] is +1 if [a] is positive, −1 if [a] is negative, or 0 if [a] is
    null. *)
val sign : int -> int

(** Integer opposite. This is the same as [( ~- )]. It cannot overflow. *)
val ( ~-? ) : int -> int

(** Overflowing integer addition.
    @raise Overflow when the result exceeds the range of overflowing integers. *)
val ( +? ) : int -> int -> int

(** Overflowing integer substraction.
    @raise Overflow when the result exceeds the range of overflowing integers. *)
val ( -? ) : int -> int -> int

(** Overflowing integer multiplication.
    @raise Overflow when the result exceeds the range of overflowing integers. *)
val ( *? ) : int -> int -> int

(** Overflowing integer exponentiation. [pow a n] is [a] to the power [n],
    provided that [n] is non‐negative.
    {b Complexity:} 𝒪(log([n])) integer multiplications.
    @raise Overflow when the result exceeds the range of overflowing integers. *)
val pow : int -> int -> int

(** [ediv a b] is the Euclidean division of [a] by [b]; it returns [(q, r)] such
    that [a] = [b]×[q] + [r] and 0 ≤ [r] < [b].
    @raise Division_by_zero when [b] is null. *)
val ediv : int -> int -> int * int

(** [equo a b] is the quotient of the Euclidean division of [a] by [b].
    @raise Division_by_zero when [b] is null. *)
val equo : int -> int -> int

(** [erem a b] is the remainder of the Euclidean division of [a] by [b].
    @raise Division_by_zero when [b] is null. *)
val erem : int -> int -> int

(** [mul_div_exact a b d] computes [a]×[b]∕[d] when [d] does divide [a]×[b].
    @raise Division_by_zero when [d] is null.
    @raise Division_not_exact when [d] does not divide [a]×[b].
    @raise Overflow when the result exceeds the range of overflowing integers. *)
val mul_div_exact : int -> int -> int -> int

(** [mul_quo a b d] tries to compute [a]×[b]÷[d]. It can overflow even if the
    final result fits in the range of overflowing integers. This case is
    guaranteed not to happen as long the denominator of the reduced fraction is
    less than √[max_int] (in particular, when [d] is less than √[max_int]).
    {e This should be fixed, but I don’t know how.}
    @raise Division_by_zero when [d] is null.
    @raise Overflow as described. *)
val mul_quo : int -> int -> int -> int

(******************************************************************************)

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
    @raise Overflow when the computation of the Bézout’s coefficients provokes
    an overflow, even if there exists a pair of Bézout coefficients which would
    fit in the range of our overflowing integers. {e This should be fixed, but I
    don’t know how.} *)
val gcdext : int -> int -> int * int * int

(** [lcm a b] is the lesser common multiple of [a] and [b]. Its sign is that of
    [a]×[b].
    {b Complexity:} 𝒪(log(min(|[a]|,|[b]|))) integer divisions.
    @raise Overflow when the result exceeds the range of overflowing integers. *)
val lcm : int -> int -> int

(** [valuation ~factor:d n] returns [(k, m)] such that [n] = [d]{^[k]}×[m] and
    [m] is not divisible by [d]. This assumes that [n] is not null and that [d]
    is not ±1.
    {b Complexity:} 𝒪([k]) = 𝒪(log([n])) integer divisions.
    @raise Division_by_zero when [d] is null. *)
val valuation : factor:int -> int -> int * int

(** [valuation_of_2] is equivalent to [valuation ~factor:2], but much faster. *)
val valuation_of_2 : int -> int * int

(** [is_square n] is true if and only if [n] is the square of an integer. *)
val is_square : int -> bool

(** [jacobi a n] is the Jacobi symbol ([a]|[n]), provided that [n] is odd and
    positive.
    {b Complexity:} 𝒪(log(min(|[a]|,[n]))) integer divisions. *)
val jacobi : int -> int -> int

(******************************************************************************)

(** {2 Binomial coefficients } *)

(** [binoms n] returns the [n]{^th} row of Pascal’s triangle, provided that [n]
    is a non-negative integer.
    {b Complexity:} time 𝒪([n]), space 𝒪([n]).
    @raise Overflow when the greatest value of the result exceeds the range of
    overflowing integers. For 64‐bit OCaml, this happens for [n] ≥ 66. *)
val binoms : int -> int array

(** [binom n p] is the [p]{^th} element of the [n]{^th} row of Pascal’s
    triangle, provided that 0 ≤ [p] ≤ [n].
    {b Complexity:} time 𝒪(min([p],[n]−[p])) = 𝒪([n]), space 𝒪(1).
    @raise Overflow when the result exceeds the range of overflowing integers. *)
val binom : int -> int -> int

(** [central_binom p] is the [p]{^th} element of the 2×[p]{^th} row of Pascal’s
    triangle, provided that 0 ≤ [p].
    {b Complexity:} time 𝒪([p]), space 𝒪(1).
    @raise Overflow when the result exceeds the range of overflowing integers.
    For 64‐bit OCaml, this happens for [p] ≥ 33. *)
val central_binom : int -> int
