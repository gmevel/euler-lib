(** Prime numbers and integer factorization. *)

(******************************************************************************)

(** The type of the factorized form of an integer.
    The factorization of [n] is a list [\[ (p1, k1) ; … ; (pℓ, kℓ) \]] such that
    [n] = [p1]{^[k1]} × … × [pℓ]{^[kℓ]} and [p1] < … < [pℓ] are prime.
 *)
type factorization = (int * int) list

(******************************************************************************)

(** {2 First prime numbers} *)

(** The twenty‐five prime numbers less than 100, in increasing order. *)
val primes_under_100 : int array

(** The prime numbers less than 10 000, in increasing order. *)
val primes_under_10_000 : int array

(** Usual prime sieve. [prime_sieve nmax ~do_prime:f] computes the sieve of
    prime numbers up to [nmax] (included). The result is an array [s] such that
    [s.(n)] is true if and only if [n] is a prime number. The function also
    calls [f] on each prime number as soon as they are found. This is useful to
    iterate on (small) prime numbers and stop when some condition is met.
    Note that this is costly both in time and in space, so [nmax] is bridled
    with an internal upper bound.
    {b Complexity:} time 𝒪([nmax]×log(log([nmax]))), space 𝒪([nmax]).
*)
val prime_sieve : int -> do_prime:(int -> unit) -> bool array

(** Extended prime sieve. [factorizing_sieve nmax ~do_factors:f] computes the
    factorization of all numbers up to [nmax] (included). The result is an array
    [s] such that [s.(n)] is the factorization of [n]. The function also calls
    [f] on the factorization of each number from 2 to [nmax], in order. This is
    useful to iterate on the factorized form of (small) numbers and stop when
    some condition is met.
    Note that this is costly both in time and in space, so [nmax] is bridled
    with an internal upper bound.
    {b Complexity:} time 𝒪([nmax]×log(log([nmax]))),
    space 𝒪([nmax]×log(log([nmax]))).
*)
(* Supporting material for the complexity claims:
 *   — The number of non‐necessarily distinct prime factors of n, Ω(n), is
 *     log (log n) in average, hence its sum up to nmax is about log (log nmax).
 *         http://mathworld.wolfram.com/PrimeFactor.html
 *   — The number of distinct prime factors of n is also log (log n) in average,
 *     hence its sum up to nmax is about log (log nmax).
 *         http://oeis.org/A001221
 *         http://mathworld.wolfram.com/DistinctPrimeFactors.html
 *)
val factorizing_sieve : int -> do_factors:(factorization -> unit) -> factorization array

(** [primes nmax] returns all prime numbers which are at most equal to [nmax],
    in increasing order. The end of the array is padded with 0 values.
    {b Complexity:} time 𝒪([nmax]) primality tests (see {!is_prime} below),
    space 𝒪(π([nmax])) = 𝒪([nmax]∕log([nmax])).
 *)
val primes : int -> int array

(******************************************************************************)

(** {2 Primality testing} *)

(** Primality test. [is_prime n] is true if and only if [n] is a prime number.
    Note that this is a deterministic test.
    {b Complexity:} 𝒪(fast). *)
val is_prime : int -> bool

(** {2 Integer factorization} *)

(** Integer factorization.
    This uses Lenstra’s elliptic‐curve algorithm for finding factors (as of
    2018, it is the most efficient known algorithm for 64‐bit numbers).
    @param tries The number of elliptic curves to try before resigning.
    @param max_fact The “small exponents” tried by Lenstra’s algorithm are the
    factorial numbers up to the factorial of [max_fact].
    @return the prime factorization of the given number.
    It may contain non‐prime factors [d], if their factorization failed within
    the allowed time; this is signaled by negating their value, as in [(−d, 1)].
    This is highly unlikely with default parameters.
    {b Complexity:} 𝒪(terrible). *)
val factors : ?tries:int -> ?max_fact:int -> int -> factorization

(******************************************************************************)

(** {2 Prime number count} *)

(**
    The number π(x) of prime numbers less than x is asymptotically equivalent to
    x ∕ ln(x). It is also equivalent to li(x), where li is the
    {{: https://en.wikipedia.org/wiki/Logarithmic_integral_function} logarithmic
    integral function}, which gives a much more precise estimation.
 *)

(** The logarithmic integral function. It is defined only for numbers (stricly)
    greater than [1.0].
    @param precision The series summation stops as soon as the increment becomes
    smaller than [precision]. *)
val li : ?precision:float -> float -> float

(** [overestimate_number_of_primes nmax] gives a relatively precise upper bound
    on the number of prime numbers below [nmax]. *)
val overestimate_number_of_primes : int -> int
