exception Overflow

exception Division_not_exact

let sign a =
  if a = 0      then   0
  else if a > 0 then ~+1
  else               ~-1

let ( ~-? ) = ( ~- )

(* Number of bits of an unsigned integer (OCaml integers are one bit less than
 * machine words, and there is one sign bit). *)
let uint_size = Sys.word_size - 2

let add_nonneg =
  let highest_bit = 1 lsl (uint_size - 1) in
fun a b ->
  assert (0 <= a) ;
  assert (0 <= b) ;
  if a land b land highest_bit <> 0 then
    raise Overflow
  else if (a lor b) land highest_bit = 0 then
    a + b
  else begin
    let s = a + b in
    if s land highest_bit = 0 then
      raise Overflow
    else
      s
  end

let ( +? ) a b =
  assert (a <> min_int) ;
  assert (b <> min_int) ;
  if a > 0 && b > 0 then
    add_nonneg a b
  else if a < 0 && b < 0 then
    ~- (add_nonneg ~-a ~-b)
  else
    a + b

let ( -? ) a b =
  assert (a <> min_int) ;
  assert (b <> min_int) ;
  if a > 0 && b < 0 then
    add_nonneg a ~-b
  else if a < 0 && b > 0 then
    ~- (add_nonneg ~-a b)
  else
    a - b

let mul_nonneg =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a b ->
  assert (0 <= a) ;
  assert (0 <= b) ;
  let (ah, al) = (a lsr uint_half_size, a land lower_half)
  and (bh, bl) = (b lsr uint_half_size, b land lower_half) in
  if ah <> 0 && bh <> 0 then
    raise Overflow
  else begin
    let al_bl = al*bl in
    let (h, l) = (al_bl lsr uint_half_size, al_bl land lower_half) in
    (* This expression does not overflow (at most one of ah×bl and al×bh is
     * non‐null; and each variable is at most equal to lower_half = 2^{N∕2}−1
     * where N = uint_size, so that the total is at most equal to
     * lower_half² + lower_half, which is less than 2^N): *)
    let h' = ah*bl + al*bh + h in
    if h' > lower_half then
      raise Overflow
    else
      (h' lsl uint_half_size) lor l
  end

let ( *? ) a b =
  assert (a <> min_int) ;
  assert (b <> min_int) ;
  if a >= 0 && b >= 0 then
    mul_nonneg a b
  else if a <= 0 && b <= 0 then
    mul_nonneg ~-a ~-b
  else if a > 0 (* && b < 0 *) then
    ~- (mul_nonneg a ~-b)
  else (* a < 0 && b > 0 *)
    ~- (mul_nonneg ~-a b)
(* This is made shorter using a bitwise trick, but it may not be faster because
 * of calls to [abs]. TODO: Benchmark it. *)
(*
let ( *? ) a b =
  assert (a <> min_int) ;
  assert (b <> min_int) ;
  if a lxor b >= 0 then
    mul_nonneg (abs a) (abs b)
  else
    ~- (mul_nonneg (abs a) (abs b))
*)

let pow =
  Common.pow ~mult:( *? ) ~unit:1

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
      (* TODO: Avoid overflows in intermediate values for computing the Bézout
       * coefficients (use zarith? prove that somehow we compute the smallest
       * coefficients possible and that there are in fact no overflows?). *)
      gcdext b (a mod b) x y (u -? q *? x) (v -? q *? y)
    end
  in
  gcdext a0 b0 1 0 0 1

let lcm a b =
  assert (a <> min_int) ;
  assert (b <> min_int) ;
  if a = 0 || b = 0 then
    0
  else
    a / gcd a b *? b

let rec valuation ~factor:d n =
  assert (abs d <> 1) ;
  assert (n <> 0) ;
  if n mod d <> 0 then
    (0, n)
  else
    let (k, n') = valuation ~factor:d (n / d) in
    (k+1, n')

(*
let valuation_of_2 n =
  assert (n <> 0) ;
  let k = ref 0 in
  let m = ref n in
  while !m mod 2 = 0 do
    incr k ;
    m := !m / 2 ;
  done ;
  (!k, !m)
*)
(* Made potentially faster with a trick. TODO: Benchmark it. *)
let valuation_of_2 n =
  assert (n <> 0) ;                  (*    n = 0b ???????10000 *)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = snd@@frexp (float bits) in (* we convert to float and get the exponent *)
  (k, n lsr k)

let is_square n =
  let r = truncate @@ sqrt @@ float n in
  r*r = n

let mul_div_exact a b d =
  if d = 0 then
    raise Division_by_zero ;
  let g = gcd a d in
  let (a, d) = (a / g, d / g) in
  let g = gcd b d in
  let (b, d) = (b / g, d / g) in
  if abs d <> 1 then
    raise Division_not_exact
  else
    a *? b

let mul_quo a b d =
  (* This will be checked anyway by following native divisions: *)
  (*if d = 0 then
    raise Division_by_zero ;*)
  let s = sign a * sign b * sign d
  and a = abs a
  and b = abs b
  and d = abs d in
  let g = gcd a d in
  let (a, d) = (a / g, d / g) in
  let g = gcd b d in
  let (b, d) = (b / g, d / g) in
  let (qa, ra) = (a / d, a mod d) in
  let (qb, rb) = (b / d, b mod d) in
  s * ((qa *? b) +? (ra *? qb) +? (ra *? rb / d))
  (* TODO: Avoid overflow in the intermediate product (use zarith?). *)

let binoms n =
  assert (0 <= n) ;
  (* Uses the formula: binom(n,p) = binom(n,p−1) × (n−p+1) ∕ p *)
  let b = Array.make (n+1) 0 in
  b.(0) <- 1 ;
  b.(n) <- 1 ;
  for k = 1 to n/2 do
    b.(k) <- mul_div_exact b.(k-1) (n-k+1) k ;
    b.(n-k) <- b.(k)
  done ;
  b

let binom n p =
  assert (0 <= p && p <= n) ;
  (* Uses the formula: binom(n,p) = binom(n−1,p−1) × n ∕ p *)
  let p = min p (n - p) in
  let m = n - p in
  let c = ref 1 in
  for k = 1 to p do
    c := mul_div_exact !c (m+k) k ;
  done ;
  !c

let central_binom p =
  assert (0 <= p) ;
  (* Uses the formula: binom(2p,p) = binom(2(p−1),p−1) × 2(2p−1) ∕ p *)
  let c = ref 1 in
  for k = 1 to p do
    (*c := mul_div_exact !c (2 * (2*k - 1)) k*)
    (* This expression already avoids all overflows except for the result: *)
    c := !c *? 4  -  !c * 2 / k
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
