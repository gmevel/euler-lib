(***** PRIMALITY TESTS ****
 *
 * AKS:
 *     https://en.wikipedia.org/wiki/AKS_primality_test
 * deterministic
 * polynomial but slow: Õ((log n)⁶) (reducible to Õ((log n)³) assuming Agrawal’s conjecture, which is suspected to be false)
 * no certificates
 * not used in practice
 *
 * ECPP (Elliptic Curve Primality Proving):
 *     https://en.wikipedia.org/wiki/Elliptic_curve_primality
 * deterministic
 * not proven polynomial, but very fast, much faster than AKS, Miller, …
 * can produce certificates
 *
 * Solovay-Strassen:
 *     https://en.wikipedia.org/wiki/Solovay%E2%80%93Strassen_primality_test
 * probabilistic (probability of a false positive, knowing the number is composite: less than 2^{−rounds} (much less in practice))
 * polynomial: O((log n)³)
 * similar to Miller-Rabin, superseded by it (historical importance for RSA)
 * not used anymore
 *
 * Miller-Rabin:
 *     https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
 * probabilistic (probability of a false positive, knowing the number is composite: less than 4^{−rounds} (much less in practice))
 * polynomial: O((log n)³), improved to Õ((log n)²) with FFT-based multiplications
 *
 * Miller’s variant of Miller-Rabin:
 *     https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
 * deterministic
 * correction depends on the generalized Riemann hypothesis
 * polynomial: Õ((log n)⁴) using FFT
 * not used in practice
 *
 * Baillie-PSW:
 *     https://en.wikipedia.org/wiki/Baillie%E2%80%93PSW_primality_test
 * probabilistic
 * deterministic for 64-bit integers (more efficient than the test with the seven bases show below?)
 *
 * simpler tests, often used before a general algorithm to speed up the test:
 * — trial division: try small factors (say, prime numbers less than 100)
 * — Fermat test: check that a^{n−1} ={n}= 1 for some random 2 ≤ a ≤ n−2
 *)

(* TODO:
 * implement ECPP and Miller
 *)

(* TODO:
  * use hashing to reduce the number of bases necessary
  * see https://miller-rabin.appspot.com/
  *)

(* TODO:
 * use zarith
 *)

let primes_under_100 =
  [  2 ;  3 ;  5 ;  7 ; 11 ; 13 ; 17 ; 19 ; 23 ; 29 ; 31 ; 37 ; 41 ; 43 ; 47 ;
    53 ; 59 ; 61 ; 67 ; 71 ; 73 ; 79 ; 83 ; 89 ; 97 ]

exception Composite
exception Prime

let miller_rabin_test_exn ~bases n =
  let n = abs n in
  if n <= 1 then
    raise Composite ;
  (* these two tests are subsumed by the trial division below: *)
  (*if n = 2 then
    raise Prime ;
  if n mod 2 = 0 then
    raise Composite ;*)
  (* first, a trial division (not necessary, but overall speeds up the primality
   * test by eliminating many composite numbers): *)
  primes_under_100 |> List.iter begin fun p ->
    if n = p then
      raise Prime ;
    if n mod p = 0 then
      raise Composite ;
  end ;
  (* every composite number up to 10_200 has at least one prime factor less than
   * 100: *)
  if n <= 10_200 then
    raise Prime ;
  (* now the general Miller-Rabin test for odd numbers: *)
  (* write n = m × 2^k + 1 where m is odd: *)
  let (k, m) = Arith.valuation_of_2 (n - 1) in
  (* perform the test for each given base: *)
  bases |> List.iter begin fun b ->
    let b = b mod n in
    let x = ModArith.pow ~modulo:n b m in
    let exception Strong_probable_prime in
    begin try
      (* test whether b^m ={n}= ±1 : *)
      if x = 1 || x = n-1 then
        raise Strong_probable_prime ;
      (* test whether b^{m×2^i} ={n}= −1 for some 1 ≤ i < k : *)
      let x = ref x in
      for _ = 1 to pred k do
        x := ModArith.mul ~modulo:n !x !x ;
        (* in the following case, we know that n is composite and we can compute
         * factors of n: gcd(n, b^{m×2^{i−1}} − 1) and gcd(n, b^{m×2^{i−1}} + 1)
         * are non-trivial factors of n: *)
        (*if !x = 1 then
          raise Composite ;*)
        if !x = n-1 then
          raise Strong_probable_prime
      done ;
      raise Composite
    with Strong_probable_prime ->
      ()
    end
  end

let miller_rabin_test ~bases n =
  begin match miller_rabin_test_exn ~bases n with
  | ()                  -> true  (* strong probable prime *)
  | exception Prime     -> true  (* definitely prime *)
  | exception Composite -> false (* definitely composite *)
  end

let is_prime : int -> bool =
  assert (Sys.word_size = 64) ;
  (* these small base sets are guaranteed to give allways-correct result for
   * values of the input below the specified bound. the last one works for (at
   * least) all 64-bit integers. they are found here:
   *     https://miller-rabin.appspot.com/
   * commented are sets whose some base does not fit in 62-bit integers. *)
  (*let bases1 = [ 9345883071009581737 ] in
  let bound1 = 341531 in*)
  let bases1 = [ 126401071349994536 ] in
  let bound1 = 291831 in
  let bases2 = [ 336781006125 ; 9639812373923155 ] in
  let bound2 = 1050535501 in
  (*let bases3 = [ 4230279247111683200 ; 14694767155120705706 ; 16641139526367750375 ] in
  let bound3 = 350269456337 in*)
  let bases3 = [ 15 ; 7363882082 ; 992620450144556 ] in
  let bound3 = 273919523041 in
  (*let bases4 = [ 2 ; 141889084524735 ; 1199124725622454117 ; 11096072698276303650 ] in
  let bound4 = 55245642489451 in*)
  let bases4 = [ 2 ; 2570940 ; 211991001 ; 3749873356 ] in
  let bound4 = 47636622961201 in
  let bases5 = [ 2 ; 4130806001517 ; 149795463772692060 ; 186635894390467037 ; 3967304179347715805 ] in
  let bound5 = 7999252175582851 in
  let bases6 = [ 2 ; 123635709730000 ; 9233062284813009 ; 43835965440333360 ; 761179012939631437 ; 1263739024124850375 ] in
  let bound6 = 585226005592931977 in
  let bases7 = [ 2 ; 325 ; 9375 ; 28178 ; 450775 ; 9780504 ; 1795265022 ] in
fun n ->
  let n = abs n in
  if n < bound1 then
    miller_rabin_test ~bases:bases1 n
  else if n < bound2 then
    miller_rabin_test ~bases:bases2 n
  else if n < bound3 then
    miller_rabin_test ~bases:bases3 n
  else if n < bound4 then
    miller_rabin_test ~bases:bases4 n
  else if n < bound5 then
    miller_rabin_test ~bases:bases5 n
  else if n < bound6 then
    miller_rabin_test ~bases:bases6 n
  else
    miller_rabin_test ~bases:bases7 n

(* TODO: tweak the default number of rounds; see this paragraph from Wikipedia:
 *
 * In addition, for large values of n, on average the probability that a
 * composite number is declared probably prime is significantly smaller than
 * 4−k. Damgård, Landrock and Pomerance[7] compute some explicit bounds and
 * provide a method to make a reasonable selection for k for a desired error
 * bound. Such bounds can, for example, be used to generate probable primes;
 * however, they should not be used to verify primes with unknown origin, since
 * in cryptographic applications an adversary might try to send you a
 * pseudoprime in a place where a prime number is required. In such cases, only
 * the error bound of 4−k can be relied upon.
 *
 * However, though this may be a sound probabilistic argument using Bayes'
 * theorem, later refinements by Ronald J. Burthe, Jr., proved the conjecture in
 * the introduction of the paper [8] that the upper bound of 4−k is valid for all
 * k > 1. Burthe improved the estimates for 25 <= k <= 50 to satisfy the
 * conjecture. The exact values for 2 <= k <= 24 were evaluated numerically using
 * a result of Monier's. 
 *
 *
 * *)
let is_probably_prime ?(rounds=10) n =
  (* we pick random bases between 2 and n−2, inclusive: *)
  let n' = max 1 (abs n - 3) in
  let bases = List.init rounds (fun _ -> 2 + Random.int n') in
  miller_rabin_test ~bases n




(* TODO: From this point, the code has not been checked for overflows. *)

(* le nombre π(x) de nombres premiers inférieurs à x est équivalent à x ∕ ln(x).
 * il est aussi équivalent au logarithme intégral li(x), qui donne une
 * estimation beaucoup plus précise. *)

let li ?(precision=0.0) =
  (* constante d’Euler–Mascheroni *)
  let gamma = 0.57721_56649_01532_86061 in
fun x ->
  (* calcul avec un développement en série: *)
(*
  assert (x <> 1.0) ;
  let log_x = log x in
  let s = ref (gamma +. log (abs_float log_x)) in
  let term = ref 1.0 in
  let n = ref 1 in while
    term := !term *. log_x /. float !n ;
    s := !s +. !term /. float !n ;
    if not @@ (abs_float !term > precision) then Printf.printf "{%u}\n" !n ;
    abs_float !term > precision
  do incr n done ;
  !s
*)
  (* calcul avec une série qui converge un peu plus vite, due à Ramanujan: *)
  assert (x > 1.0) ;
  let log_x = log x in
  let s = ref (gamma +. log log_x) in
  let term = ref (~-. 2.0 *. sqrt x) in
  let sum_of_inverses = ref 0.0 in
  let n = ref 1 in while
    if !n mod 2 = 1 then sum_of_inverses := !sum_of_inverses +. 1. /. float !n ;
    term := !term *. log_x *. ~-. 0.5 /. float !n ;
    s := !s +. !term *. !sum_of_inverses ;
    abs_float !term > precision
  do incr n done ;
  !s

(* borne supérieure sur le nombre de nombres premiers inférieurs à [nmax] *)
let overestimate_number_of_primes nmax =
  let x = float nmax in
  let y =
    if nmax >= 60_184 then x /. (log x -. 1.1)  (* [Pierre Dusart, 2010] *)
    else if nmax >= 1_606 then x /. (log x -. 1.5) (* valable dès n >= 5 *)
    else if nmax >= 2 then 1.25506 *. x /. log x
    else 0.0
  in truncate y

(* meilleure borne supérieure sur le nombre de nombres premiers inférieurs à [nmax] *)
let overestimate_number_of_primes nmax =
  truncate (li (float nmax))

(*** calcul des nombres premiers avec un crible ***)

let primes_sieve nmax =
  let sieve = Array.init nmax (fun i -> i mod 2 <> 0) in
  sieve.(2) <- true ;
  let i = ref 1 in
  while i := !i+2; !i < nmax do
    if sieve.(!i) then
      let j = ref !i in
      while j := !j+ !i; !j < nmax do
        sieve.(!j) <- false
      done
  done;
  sieve
let primes_list nmax =
  let sieve = primes_sieve nmax in
  let rec aux i =
    if i >= nmax then []
    else if sieve.(i) then i :: aux (i+2)
    else                        aux (i+2)
  in
  2 :: aux 3

(*** calcul des nombres premiers par tests successifs ***)

let primes_array nmax =
  let primes = Array.make (overestimate_number_of_primes nmax) 0 in
  let rec is_prime ?(i = 0) n =
    let p = primes.(i) in
    if p = 0 || p * p > n then
      true
    else if n mod p = 0 then
      false
    else
      is_prime ~i:(succ i) n
  in
  primes.(0) <- 2 ;
  let count_primes = ref 1 in
  for n = 3 to nmax do
    if is_prime n then begin
      primes.(!count_primes) <- n ;
      incr count_primes
    end
  done ;
  primes

(*** nombres premiers précalculés ***)

let primes_list_from_file nmax =
  let li = ref [] in
  let file = Scanf.Scanning.open_in "data/primes-under-1_000_000.data" in
  let again = ref true in
  while !again do
    (* "%_1[\r]@\n" is a format trick that matches \n, \r\n and end-of-file. *)
    Scanf.bscanf file "%u%_1[\r]@\n" @@fun p ->
    if p <= nmax then
      li := p :: !li ;
    again := (p < nmax)
  done ;
  Scanf.Scanning.close_in file ;
  List.rev !li

(*** crible généralisé (factorisation, calcul du nombre de diviseurs…) ***)

type sieve_cell =
  {
    mutable remaining_to_factor : int ;
    mutable factors : (int * int) list ;
    mutable nb_divisors : int ;
  }

let sieve nmax =
  let s = Array.init (succ nmax) (fun n -> {
      remaining_to_factor = n ;
      factors = [] ;
      nb_divisors = 1 ;
    }) in
  for n = 2 to nmax do
    if s.(n).remaining_to_factor <> 1 then begin
      for k = 1 to nmax / n do
        let cell = s.(k*n) in
        let count = ref 0 in
        while cell.remaining_to_factor mod n = 0 do
          cell.remaining_to_factor <- cell.remaining_to_factor / n ;
          incr count
        done ;
        cell.factors <- (n, !count) :: cell.factors ;
        cell.nb_divisors <- cell.nb_divisors * (!count + 1) ;
      done
    end
  done ;
  s
