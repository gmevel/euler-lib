exception Overflow

let sign a =
  if a = 0      then   0
  else if a > 0 then ~+1
  else               ~-1

let ediv a b =
  let q = a / b
  and r = a mod b in
  if r >= 0 then
    (q, r)
  else begin
    let s = sign b in
    (q - s, r + s*b)
  end

let equo a b =
  let q = a / b in
  if a >= 0 || q*b = a then
    q
  else
    q - sign b

let erem a b =
  let r = a mod b in
  if r >= 0 then
    r
  else
    r + abs b

let rec gcd a b =
  if b = 0 then
    abs a
  else
    gcd b (a mod b)

(* TODO: detect overflows *)
let gcdext a0 b0 =
  let rec gcdext a b u v x y =
    assert (a = u*a0 + v*b0) ;
    assert (b = x*a0 + y*b0) ;
    if b = 0 then begin
      if a > 0 then
        (a, u, v)
      else
        (~-a, ~-u, ~-v)
    end else begin
      let q = a / b in
      gcdext b (a mod b) x y (u-q*x) (v-q*y)
    end
  in
  gcdext a0 b0 1 0 0 1

(* TODO: detect overflows *)
let lcm a b =
  if a = 0 || b = 0 then
    0
  else
    a / gcd a b * b

let rec valuation ~factor:d n =
  assert (abs d <> 1) ;
  assert (n <> 0) ;
  if n mod d <> 0 then
    (0, n)
  else
    let (k, n') = valuation ~factor:d (n / d) in
    (k+1, n')

let valuation_of_2 n =
  (*assert (n <> 0) ;
  let k = ref 0 in
  let m = ref n in
  while !m mod 2 = 0 do
    incr k ;
    m := !m / 2 ;
  done ;
  (!k, !m)*)
  assert (n <> 0) ;                  (*    n = 0b ???????10000 *)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = snd@@frexp (float bits) in (* we convert to float and get the exponent *)
  (k, n lsr k)

let is_square n =
  let r = truncate @@ sqrt @@ float n in
  r*r = n

(* TODO: detect overflows *)
let mul_div a b d =
  assert (0 <= d) ;
  assert (0 <= a && 0 <= b) ;
  let (qa, a') = (a / d, a mod d) in
  let (qb, b') = (b / d, b mod d) in
  qa*b + a'*qb + a'*b'/d

(* TODO: detect overflows *)
let binoms n =
  assert (0 <= n) ;
  (* Uses the formula: binom(n,p) = binom(n,p−1) × (n−p+1) ∕ p *)
  let b = Array.make (n+1) 0 in
  b.(0) <- 1 ;
  b.(n) <- 1 ;
  for k = 1 to n/2 do
    (* (1) Overflow‐free as long as the expected result is less than max_int ∕ k
     *     [ overall, this makes the algorithm overflow‐free as long as the
     *     central binomial coefficient is less than max_int ∕ (n∕2);
     *     for 64‐bit OCaml, this is valid for n∕2 ≤ 30 ]: *)
    (*b.(k) <- b.(k-1) * (n-k+1) ∕ k ;*)
    (* (2) Overflow‐free as long as the expected result is less than max_int,
     *     and that k < √max_int [ overall, this makes the algorithm
     *     overflow‐free as long as the result itself is less than max_int,
     *     because for n large enough we have that n∕2 < √ binom(n, n∕2);
     *     for 64‐bit OCaml, this is valid for n∕2 ≤ 32 ]: *)
    b.(k) <- mul_div b.(k-1) (n-k+1) k ;
    b.(n-k) <- b.(k)
  done ;
  b

(* TODO: detect overflows *)
let binom n p =
  assert (0 <= p && p <= n) ;
  (* Uses the formula: binom(n,p) = binom(n−1,p−1) × n ∕ p *)
  let p = min p (n - p) in
  let m = n - p in
  let c = ref 1 in
  for k = 1 to p do
    (* (1) Overflow‐free as long as the expected result is less than max_int ∕ k
     *     [ overall, this makes the algorithm overflow‐free as long as the
     *     result is less than max_int ∕ q, where q = min(p, n−p) ]: *)
    (*c := !c * (m + k) ∕ k*)
    (* (1b) Avoids a few more overflows: *)
    (*c := !c * m ∕ k + !c ;*)
    (* (2) Overflow‐free as long as the expected result is less than max_int,
     *     and that k < √max_int [ overall, this makes the algorithm
     *     overflow‐free as long as the result itself is less than max_int,
     *     because for n large enough we have that q < √ binom(n, q) ]: *)
    c := mul_div !c (m+k) k ;
  done ;
  !c

(* TODO: detect overflows *)
let central_binom p =
  assert (0 <= p) ;
  (* Uses the formula: binom(2p,p) = binom(2(p−1),p−1) × 2(2p−1) ∕ p *)
  let c = ref 1 in
  for k = 1 to p do
    (*c := !c * 2 * (2*k - 1) ∕ k*)
    (* (2) Avoids all overflows, except for the result itself [ overall, this
     *     makes the algorithm overflow‐free as long as the result itself is
     *     less than max_int; for 64‐bit OCaml, this is valid for p ≤ 32 ] *)
    c := !c * 4  -  !c * 2 / k
  done ;
  !c

(* Another algorithm to compute a binomial coefficient: by computing its prime
 * factorization, using Kummer’s theorem:
 *     http://www.luschny.de/math/factorial/FastBinomialFunction.html
 *     https://en.wikipedia.org/wiki/Kummer's_theorem
 * Complexity: O(log(n)) ?
 *     number of primes below n: n ∕ log(n)
 *     for each prime p, we do log_p(n) operations.
 *
 * Compute modulo a prime number:
 *     https://en.wikipedia.org/wiki/Lucas's_theorem
 * Compute modulo 2^N:
 *     https://www.hindawi.com/journals/tswj/2013/751358/
 * Compute modulo anything (Chinese remainder theorem + generalization of Lucas’
 * theorem by Andrew Granville):
 *     https://fishi.devtail.io/weblog/2015/06/25/computing-large-binomial-coefficients-modulo-prime-non-prime/
 *)
