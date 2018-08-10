(* the 25 prime numbers under 100: *)
val primes_under_100 : int list

(* strong Fermat primality test, aka Miller-Rabin probable primality test:
 *     https://en.wikipedia.org/wiki/Strong_pseudoprime
 *     https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test *)
val miller_rabin_test : bases:(int list) -> int -> bool

(* deterministic primality test for 64-bit integers: *)
val is_prime : int -> bool

(* Miller-Rabin probabilistic primality test: *)
val is_probably_prime : ?rounds:int -> int -> bool



(*** estimation du nombre de nombres premiers ***)

(* logarithme intégral li(x)
 *     https://en.wikipedia.org/wiki/Logarithmic_integral_function *)
val li : ?precision:float -> float -> float

(* borne supérieure sur le nombre de nombres premiers inférieurs à [nmax] *)
val overestimate_number_of_primes : int -> int

(*** calcul des nombres premiers avec un crible ***)

val primes_sieve : int -> bool array
val primes_list  : int -> int list

(*** calcul des nombres premiers par tests successifs ***)

val primes_array : int -> int array

(*** nombres premiers précalculés ***)

val primes_list_from_file : int -> int list

(*** crible généralisé (factorisation, calcul du nombre de diviseurs…) ***)

type sieve_cell =
  {
    mutable remaining_to_factor : int ;
    mutable factors : (int * int) list ;
    mutable nb_divisors : int ;
  }

val sieve : int -> sieve_cell array
