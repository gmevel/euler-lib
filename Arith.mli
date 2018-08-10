(** Arithmetic on native integers. *)

(**
    All operations defined here act on native integers (type [int]). Either they
    are free of overflows, or they raise [Overflow] when their {e result} would
    exceed the capacity of native integers (and only in that case). Functions
    which can overflow are signaled explicitely in this documentation.

    Functions in this library may raise [Assert_failure], instead of the more
    traditional [Invalid_argument], when some precondition is not met. This is
    not necessarily signaled in this documentation, but all preconditions are
    stated in the English description of functions. However, we still treat
    division‐by‐zero differently than other preconditions; for that we raise
    [Division_by_zero], and signal it in this documentation.

    As much as possible, time and space complexities are indicated. If absent,
    constant time or constant space is implied.
 *)

(** Raised when the result of an operation exceeds the capacity of native
    integers. *)
exception Overflow


(******************************************************************************)

(** {2 Base operations } *)

(** [sign a] is +1 if [a] is positive, −1 if [a] is negative, or 0 if [a] is
    null. *)
val sign : int -> int

(** [ediv a b] is the Euclidean division of [a] by [b]; it returns [(q, r)] such
    that [a] = [b]×[q] + [r] and 0 ≤ [r] < [b].
    @raise Division_by_zero if [b] is null. *)
val ediv : int -> int -> int * int

(** [equo a b] is the quotient of the Euclidean division of [a] by [b].
    @raise Division_by_zero if [b] is null. *)
val equo : int -> int -> int

(** [erem a b] is the remainder of the Euclidean division of [a] by [b].
    @raise Division_by_zero if [b] is null. *)
val erem : int -> int -> int


(******************************************************************************)

(** {2 Divisors and multiples } *)

(** [gcd a b] is the {e positive} greatest common divisor of [a] and [b].
    {b Complexity:} 𝒪(log(min([a],[b]))) integer divisions.
    @return 0 only when [a] = [b] = 0. *)
val gcd : int -> int -> int

(** [gcdext a b] is the extended Euclidean algorithm; it returns [(d, u, v)]
    where [d] is the {e positive} greatest common divisor of [a] and [b], and
    [u] and [v] are Bézout integers, such that [u]×[a] + [v]×[b] = [d].
    {b Complexity:} 𝒪(log(min([a],[b]))) integer divisions.
    @return [d] = 0 only when [a] = [b] = 0. *)
val gcdext : int -> int -> int * int * int

(** [lcm a b] is the lesser common multiple of [a] and [b]. Its sign is that of
    [a]×[b].
    {b Complexity:} 𝒪(log(min([a],[b]))) integer divisions.
    @raise Overflow if the result exceeds the capacity of native integers. *)
val lcm : int -> int -> int

(** [valuation ~factor:d n] returns [(k, m)] such that [n] = [d]{^[k]}×[m] and
    [m] is not divisible by [d]. This assumes that [n] is not null and that [d]
    is not ±1.
    {b Complexity:} 𝒪([k]) = 𝒪(log([n])) integer divisions.
    @raise Division_by_zero if [d] is null. *)
val valuation : factor:int -> int -> int * int

(** [valuation_of_2] is the same as [valuation ~factor:2]. It may be faster. *)
val valuation_of_2 : int -> int * int

(** [is_square n] is true if and only if [n] is the square of an integer. *)
val is_square : int -> bool

(******************************************************************************)

(** {2 Binomial coefficients } *)

(** [mul_div a b d] computes [a]×[b]∕[d], provided that [d], [a] and [b] are
    non‐negative. It does not overflow as long as the expected result is less
    than [max_int] and that [d] is less than √[max_int].
    @raise Division_by_zero if [d] is null.
    @raise Overflow as explained. *)
val mul_div : int -> int -> int -> int

(** [binoms n] returns the [n]{^th} row of Pascal’s triangle, provided that [n]
    is a non-negative integer.
    {b Complexity:} time 𝒪([n]), space 𝒪([n]).
    @raise Overflow if the greatest value of the result exceeds the capacity of
    native integers. For a 64‐bit version of OCaml, this happens for [n] ≥ 66. *)
val binoms : int -> int array

(** [binom n p] is the [p]{^th} element of the [n]{^th} row of Pascal’s
    triangle, provided that 0 ≤ [p] ≤ [n].
    {b Complexity:} time 𝒪(min([p],[n]−[p])) = 𝒪([n]), space 𝒪(1).
    @raise Overflow if the result exceeds the capacity of native integers. *)
val binom : int -> int -> int

(** [central_binom p] is the [p]{^th} element of the 2×[p]{^th} row of Pascal’s
    triangle, provided that 0 ≤ [p].
    {b Complexity:} time 𝒪([p]), space 𝒪(1).
    @raise Overflow if the result exceeds the capacity of native integers.
    For a 64‐bit version of OCaml, this happens for [p] ≥ 33. *)
val central_binom : int -> int
