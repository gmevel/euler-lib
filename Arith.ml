exception Overflow

let sign a =
  if a = 0      then   0
  else if a > 0 then ~+1
  else               ~-1

let ediv a b =
  if a >= 0 then
    (a / b, a mod b)
  else begin
    let s = sign b in
    ((a / b) - s, (a mod b) + s*b)
  end

let equo a b =
  if a >= 0 then
    a / b
  else
    (a / b) - sign b

let erem a b =
  if a >= 0 then
    a mod b
  else
    (a mod b) + abs b

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
  a / gcd a b * b

let rec valuation ~factor:d n =
  assert (d <> 0) ;
  assert (n <> 0) ;
  if n mod d <> 0 then
    (0, n)
  else
    let (k, n') = valuation ~factor:d (n / d) in
    (k+1, n')

(* [valuation_of_2 n] returns [(k, m)] such that [n = 2^k × m] and [m] is odd: *)
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

(******************************************************************************)

(***
 *** coefficients binomiaux
 ***)

let mul_div a b d =
  assert (1 <= d) ;
  assert (0 <= a && 0 <= b) ;
  let (qa, a') = (a / d, a mod d) in
  let (qb, b') = (b / d, b mod d) in
  qa*b + a'*qb + a'*b'/d

let binoms n =
  assert (0 <= n) ;
  (* utilise la formule: binom(n,p) = binom(n,p−1) × (n−p+1) ∕ p *)
  let b = Array.make (n+1) 0 in
  b.(0) <- 1 ;
  b.(n) <- 1 ;
  for k = 1 to n/2 do
    (* (1) garanti sans overflow si le résultat attendu est < max_int ∕ k: *)
    (*b.(k) <- b.(k-1) * (n-k+1) ∕ k ;*)
    (* (2) garanti sans overflow si le résultat attendu est < max_int et que
     * k < sqrt(max_int): *)
    b.(k) <- mul_div b.(k-1) (n-k+1) k ;
    b.(n-k) <- b.(k)
  done ;
  b

let binom n p =
  assert (0 <= p && p <= n) ;
  (* utilise la formule: binom(n,p) = binom(n−1,p−1) × n ∕ p *)
  let p = min p (n - p) in
  let m = n - p in
  let c = ref 1 in
  for k = 1 to p do
    (* (1) garanti sans overflow si le résultat attendu est < max_int ∕ k: *)
    (*c := !c * (m + k) ∕ k*)
    (* évite davantage d’overflows: *)
    (*c := !c * m ∕ k + !c ;*)
    (* (2) garanti sans overflow si le résultat attendu est < max_int et que
     * k < sqrt(max_int): *)
    c := mul_div !c (m+k) k ;
  done ;
  !c

(* [central_binom p] retourne le nombre de combinaisons de [p] éléments parmi
 * [2p]. calcule en temps O(p) et en espace O(1), et ne cause d’overflow que si
 * le résultat attendu est lui-même strictement supérieur à à max_int.
 * pour des entiers signés de 63 bits, c’est correct pour p ⩽ 32. *)
let central_binom p =
  assert (0 <= p) ;
  (* utilise la formule: binom(2p,p) = binom(2(p−1),p−1) × 2(2p−1) ∕ p *)
  let c = ref 1 in
  for k = 1 to p do
    (*c := !c * 2 * (2*k - 1) ∕ k*)
    (* évite davantage d’overflows: *)
    c := !c * 4  -  !c * 2 / k
  done ;
  !c

(* autre algorithme pour calculer un coefficient binomial: en calculant sa
 * factorisation en nombres premiers, avec le théorème de Kummer:
 *     http://www.luschny.de/math/factorial/FastBinomialFunction.html
 *     https://en.wikipedia.org/wiki/Kummer's_theorem
 * complexité: en O(log(n)) ?
 * nombre de nombres premiers inférieurs à n : n ∕ log(n)
 * pour chaque nombre premier p, on fait log_p(n) opérations.
 *)

(* calculer modulo un nombre premier:
 *     https://en.wikipedia.org/wiki/Lucas's_theorem
 * calculer modulo 2^N:
 *     https://www.hindawi.com/journals/tswj/2013/751358/
 * calculer modulo n’importe quoi (théorème des restes chinois + généralisation
 * du théorème de Lucas par Andrew Granville):
 *     https://fishi.devtail.io/weblog/2015/06/25/computing-large-binomial-coefficients-modulo-prime-non-prime/
 *)
