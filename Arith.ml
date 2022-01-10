(* We treat this native integer as an invalid value. We give it a name in this
 * module so that we can refer to it even though we rebind the name min_int. *)
let nan = min_int

let max_int = max_int

let min_int = min_int + 1

exception Overflow

exception Division_not_exact

(*
let sign a =
  if a = 0      then   0
  else if a > 0 then ~+1
  else               ~-1
*)
(* Below is a branchless implementation, which is 3.5 times faster than the
 * naive implementation. *)
(*
let sign a =
  (a asr Sys.int_size) lor ((a lor (-a)) lsr (Sys.int_size - 1))
*)
(* Using the standard comparison function, specialized to type `int`, is even
 * faster. Although the doc does not guarantee that `compare` always return
 * −1, 0 or +1, it is enforced in all OCaml versions up to 4.09. This is about
 * 4 times faster than the naive implementation. *)
let sign a =
  compare a 0

(* This is a free generalization of [abs] and should be very fast (although not
 * benchmarked). *)
let mul_sign s n =
  let u = s asr Sys.int_size in
  n lxor u - u

let mul_sign0 s n =
  let u = s asr Sys.int_size in
  (n lxor u - u) land ((s lxor (-s)) asr Sys.int_size)

(*
let abs = abs
*)
(* Below is a branchless implementation, which is about 7.5 times faster than
 * the naive implementation. *)
let abs n =
  let u = n asr Sys.int_size in
  n lxor u - u

(* A function specialized for the type int is faster than the existing
 * polymorphic function. *)
(*
let min a b =
  if a <= b then a else b
*)
(* Below is a branchless implementation, which is about 1.5 times faster than
 * the naive implementation. Much shorter versions are presented in the wild,
 * but they ignore the fact that the subtraction can overflow. *)
let min a b =
  let d = b - a in (* overflow is correctly dealt with *)
  let s = a lxor b in
  let r = (s land b) lor (lnot s land d) in
  a + (d land (r asr Sys.int_size))

(* Same remark. *)
(*
let max a b =
  if a <= b then b else a
*)
let max a b =
  let d = b - a in (* overflow is correctly dealt with *)
  let s = a lxor b in
  let r = (s land b) lor (lnot s land d) in
  b - (d land (r asr Sys.int_size))

(* By specializing the standard `compare` to type `int`, it becomes much faster,
 * about 5.5 times faster. *)
let compare : int -> int -> int = compare
(* Below is a branchless implementation, but it is about 1.5 slower than a call
 * to the standard `compare` specialized to type `int` (and it does not
 * normalize its return value to −1/0/+1). *)
(*
let compare a b =
  let s = (a lxor b) asr Sys.int_size in
  (s land (a lor 1)) lor (lnot s land (a-b))
*)

let opp = ( ~- )

(* Number of bits of an unsigned integer (OCaml integers are one bit less than
 * machine words, and there is one sign bit). *)
let uint_size = Sys.int_size - 1

let add a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let s = a + b in
  if (a lxor b) lor (a lxor lnot s) < 0 then
    s
  else
    raise Overflow

let sub a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let d = a - b in
  if (a lxor b) land (a lxor d) >= 0 then
    d
  else
    raise Overflow

let mul =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  assert (a0 <> nan) ;
  assert (b0 <> nan) ;
  let a = abs a0 in
  if a <= lower_half then begin
    let b = abs b0 in
    if b <= lower_half then
      a0 * b0
    else begin
      let al = a land lower_half in
      let (bh, bl) = (b lsr uint_half_size, b land lower_half) in
      let al_bl = al*bl in
      let (h, l) = (al_bl lsr uint_half_size, al_bl land lower_half) in
      (* This expression does not overflow (each variable is at most equal to
       * lower_half = 2^{N∕2}−1 where N = uint_size, so that the total is
       * at most equal to lower_half² + lower_half, which is less than 2^N): *)
      let h' = al*bh + h in
      if h' <= lower_half then
        sign (a0 lxor b0) * ((h' lsl uint_half_size) lor l)
      else
        raise Overflow
    end
  end
  else begin
    let b = abs b0 in
    if b <= lower_half then begin
      let (ah, al) = (a lsr uint_half_size, a land lower_half) in
      let bl = (b land lower_half) in
      let al_bl = al*bl in
      let (h, l) = (al_bl lsr uint_half_size, al_bl land lower_half) in
      (* This expression does not overflow (same proof): *)
      let h' = ah*bl + h in
      if h' <= lower_half then
        sign (a0 lxor b0) * ((h' lsl uint_half_size) lor l)
      else
        raise Overflow
    end
    else
      raise Overflow
  end

(* We don’t override the standard unsafe operators right now because we still
 * want to use them in this file, but in some parts we’ll need the safe
 * operations, so here we define local notations for them. *)
let ( +? ) = add
let ( -? ) = sub
let ( *? ) = mul

let mul2 a =
  assert (a <> nan) ;
  (* This does not raise [Overflow] when [a = Stdlib.min_int/2] but rather it
   * returns [Stdlib.min_int]; oh well… *)
  if a lxor (a lsl 1) >= 0 then
    a lsl 1
  else
    raise Overflow

let mul_pow2 k a =
  assert (0 <= k) ;
  assert (a <> nan) ;
  if abs a lsr max 0 (uint_size - k) = 0 then
    a lsl k
  else
    raise Overflow

let div_exact a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  if a mod b = 0 then
    a / b
  else
    raise Division_not_exact

let ediv a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let q = a / b in
  let r = a - q * b in
  if r >= 0 then
    (q, r)
  else
    (q - sign b, r + abs b)
(* NOTE: The implementation below is branchless and thus faster for arbitrary
 * input numbers, but it is slower when a >= 0, which is the common scenario.
 * The same remark applies to equo and erem below. *)
(*
let ediv a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let q = a / b in
  let r = a - q * b in
  let u = r asr Sys.int_size in
  (* u is 0 if r >= 0 and −1 if r < 0 *)
  (q - (u land sign b), r + (u land abs b))
*)

let equo a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let q = a / b in
  if a >= 0 || q*b = a then
    q
  else
    q - sign b

let erem a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let r = a mod b in
  if r >= 0 then
    r
  else
    r + abs b

let ediv2 a =
  (a asr 1, a land 1)

let equo2 a =
  a asr 1

let erem2 a =
  a land 1

let ediv_pow2 a k =
  assert (a <> nan) ;
  assert (0 <= k) ;
  (* [a asr uint_size] gives 0 if [a] ≥ 0 and −1 if [a] < 0 *)
  if k <= uint_size then
    (a asr k, a land ((1 lsl k) - 1))
  else if a >= 0 then
    (0, a)
  else
    raise Overflow

let equo_pow2 a k =
  assert (a <> nan) ;
  assert (0 <= k) ;
  a asr (min k uint_size)

let erem_pow2 a k =
  assert (a <> nan) ;
  assert (0 <= k) ;
  if k <= uint_size then
    a land ((1 lsl k) - 1)
  else if a >= 0 then
    a
  else
    raise Overflow

let pow =
  Common.pow ~mult:mul ~unit:1

let pow2 k =
  assert (0 <= k) ;
  if k < uint_size then
    1 lsl k
  else
    raise Overflow

let powm1 k =
  1 - ((k land 1) lsl 1)

(* This implementation is only valid for systems where native unsigned integers
 * are at most 53 bits (including 32‐bit OCaml). See comments below. *)
let log2sup_53bit n =
  assert (0 <= n) ;
  snd@@frexp (float n)

(* This implementation is only valid for systems where native unsigned integers
 * are at least 53 bits and at most 62 bits (including 64‐bit OCaml). *)
let log2sup_62bit n =
  assert (0 <= n) ;
  (* As long as the integer can be represented exactly as a floating‐point
   * number (53 bits being the precision of floating‐point numbers), the fastest
   * solution is by far to convert the integer to a floating‐point number and to
   * read its exponent. It gives wrong results for larger numbers, because those
   * may be rounded up (for example it gives 62 instead of 61, for all numbers
   * between 2^61−128 and 2^61−1). *)
  if n <= (1 lsl 53) - 1 then
    snd@@frexp (float n)
  (* If we know that the number is at least 2^53, we can discard the 54 lowest
   * bits, compute the result for the remaining bits, and add 54. Here we could
   * use the floating‐point trick again, but since there are only 8 bits
   * remaining, it is faster to compute the exponent ourselves. *)
  else begin
    let n = n lsr 54 in
    (* Using a linear search to find the highest bit set. *)
(*
         if n >= 128 then 62
    else if n >=  64 then 61
    else if n >=  32 then 60
    else if n >=  16 then 59
    else if n >=   8 then 58
    else if n >=   4 then 57
    else if n >=   2 then 56
    else                  54 lor n
  end
*)
    (* Benchmarking suggests that, for random integers, a dichotomy is faster
     * than a linear search for finding the highest bit set. This looks odd, but
     * may be explained by a better branch prediction behavior. The code below
     * is twice as fast as the code above. *)
(*
    if n >= 16 then begin
      if n >= 64 then begin
        (*! if n >= 128 then 62 else 61 !*)
        61 + (n lsr 7)
      end else begin
        (*! if n >= 32 then 60 else 59 !*)
        59 + (n lsr 5)
      end
    end else begin
      if n >= 4 then begin
        (*! if n >= 8 then 58 else 57 !*)
        57 + (n lsr 3)
      end else begin
        (*! if n >= 2 then 56 else 54 lor n !*)
        54 + min 2 n
      end
    end
*)
    (* Better yet, we can simply use precomputed values (stored in a string to
     * save space). This code is 16 times faster than the linear search. *)
    let log2sup_8bit = "67889999::::::::;;;;;;;;;;;;;;;;<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<================================================================>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>" in
    Char.code @@ String.unsafe_get log2sup_8bit n
  end

let log2sup =
  if uint_size <= 53 then
    log2sup_53bit
  else if uint_size <= 62 then
    log2sup_62bit
  else
    assert false

(* The following implementation of the integer square root is guaranteed
 * correct, but is MUCH slower than a naive floating‐point computation. *)
(*
let isqrt n =
  assert (0 <= n) ;
  (* The result of computing with floating‐point numbers has been checked for
   * all integers below 2^30 (which is max_int+1 for 32‐bit OCaml), so this is a
   * safe shortcut. Anyway, we must rule out n = 0 from the general case, since
   * it would cause a division by zero.
   * Note: Contrary to what one might believe, this floating‐point computation
   * is not correct for all integers below 2^53 (53 bits being the precision of
   * floating‐point numbers). For example, with n = 7865574647205624 = r²−1
   * where r = 88688075, it incorrectly gives r (instead of r−1).
   * See below for an explanation (this shortcut is in fact correct for all
   * integers below 2^52). *)
  if n <= (1 lsl 30) - 1 then
    truncate@@sqrt@@float n
  (* In the general case, we use a variation of the Babylonian method for
   * integer values.
   *     https://en.wikipedia.org/wiki/Integer_square_root#Using_only_integer_division
   * Let r be the integer square root of n, and let x0 be an integer at least
   * equal to r. Then, the following integer sequence:
   *     x_{n+1}  =  (x + n÷x) ÷ 2
   * decreases until it reaches r. From that point:
   *   — if n+1 is a perfect square, then the sequence cycles between r and r+1;
   *   — otherwise, the sequence remains steady at r. *)
  else begin
    (* For the initial value, we compute a good over‐approximation of the square
     * root using the number of binary digits of n (practical tests seem to
     * indicate that, with that choice, the number of iterations of the
     * Babylonian method is at most 7 for 64‐bit OCaml): *)
    let rx = ref 0 in
    let ry = ref (let k = log2sup n in 1 lsl ((k+1)/2)) in
    while
      let x = !ry in
      (* This sum does not overflow because its value is approximately 2×√n, far
       * less than max_int: *)
      let y = (x + n/x) lsr 1 in
      rx := x ;
      ry := y ;
      y < x
    do () done ;
    (* This test accounts for the case where n+1 is a perfect square (if we knew
     * that it wasn’t, we could get rid of rx and always return !ry): *)
    let x = !rx in
    let y = !ry in
    if x <= y then
      x
    else
      y
  end
*)

(* The following implementation uses the existing floating-point operation. It
 * is much faster (about the speed of the floating-point sqrt itself) and its
 * validity has been checked for all inputs.
 *
 * IEEE 754 guarantees that the result of the floating-point sqrt operation is
 * always equal to the nearest floating-point approximation of the real result.
 * As a consequence, the sqrt operation is monotonic (as the composition of two
 * monotonic functions, the real function √· and the approximation function).
 *
 * This implies that, in order to check the result of the function sqrt on all
 * 62-bit integers, it is enough to check it before and after every square
 * number, ie. to check the result of sqrt(r²−1) and sqrt(r²) for every 31-bit
 * integer r, which is doable in reasonable time.
 *
 * The outcome of this verification is that the floating-point function gives
 * the exact result for all numbers up to r² − 1 where r = 2^26 + 1. Starting
 * with this number, it is possible to get one more than the expected result.
 *
 * However, we can also verify that no 62-bit integer has an error more than
 * one. Hence, if the floating-point operation computes a result x, then the
 * actual square root is either x or x − 1.
 *
 * Mathematical insight: by a linear approximation, we have:
 *     √(r² − 1)  =  r × ( 1 − 1/(2r²) + 𝒪(1/r⁴) )
 * where the expression has been factored by r, which determines the magnitude
 * of the number. So the second term has a relative magnitude of 1/(2r²). Yet,
 * floating-point numbers provide 53 bits of mantissa. So, when 1/(2r²) becomes
 * smaller than 2^(−53), this term is dropped and we are left with just r. This
 * happens as soon as r > 2^26. *)
let isqrt n =
  assert (0 <= n) ;
  let x = truncate@@sqrt@@float n in
  (* We test whether the result of the floating-point calculation is indeed the
   * square root, ie. whether x² ≤ n. For values of n close to max_int, x is one
   * more than the square root, and x² overflows, giving min_int. To keep the
   * test correct even if the presence of an overflow, we write x²−1 < n instead
   * of x² ≤ n.
   * The first test is a shortcut for the common case (see the explanation above
   * for the constant involved). *)
  if x <= (1 lsl 26) || x*x - 1 < n then
    x
  else
    x - 1

(* The below implementation of [icbrt] has been verified for all 63-bit integers
 * with the same method as for [isqrt], i.e. based on the fact that the function
 * [fun x -> x ** (1./.3.)] is monotonic (which seems reasonable, but I haven’t
 * checked). *)
let icbrt n =
  let s = sign n
  and n = abs n in
  let x = truncate (float n ** (1./.3.)) in
  let next_cube = (x+1)*(x+1)*(x+1) in
  (* [(x+1)³] overflows only when [x³] is the largest representable cube; then
   * [x³ < 2^(int_size−1) ≤ (x+1)³ < 2^int_size], because [((x+1)/x)³ < 2] as
   * soon as [x ≥ 4]. So, provided that we have wrapping integers with
   * [int_size ≥ 7], [(x+1)³] appears to be negative, and we can detect an
   * overflow just by looking at its sign.
   * By contrast with [isqrt], an off-by-one error happens pretty quick, as soon
   * as n = 4³ = 64, so there is no point in shortcutting the test. *)
  if n < next_cube || next_cube < 0 then
    s * x
  else
    s * (x+1)

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
  assert (a <> nan) ;
  assert (b <> nan) ;
  if a = 0 || b = 0 then
    0
  else
    a / gcd a b *? b

let valuation ~factor:d n =
  assert (abs d <> 1) ;
  assert (n <> 0) ;
  let k = ref 0 in
  let m = ref n in
  while !m mod d = 0 do
    incr k ;
    m := !m / d ;
  done ;
  (!k, !m)

(*
let valuation_of_2 n =
  assert (n <> 0) ;
  let k = ref 0 in
  let m = ref n in
  while !m land 1 = 0 do
    incr k ;
    m := !m asr 1 ;
  done ;
  (!k, !m)
*)
(* Reading by chunks of 8 bits and using precomputed values for the last 8 bits
 * provides a significant speed-up. This implementation is 3.3 times faster than
 * the naive one.
 * NOTE: If we want to avoid spending 128 bytes of space, we can instead read by
 * chunks of 4 bits, and store 8 precomputed values. That is almost as fast. *)
let valuation_of_2 =
  let values128 = "\007\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\006\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000" in
fun n ->
  assert (n <> 0) ;
  let k = ref 0 in
  let m = ref n in
  while !m land 255 = 0 do
    k := !k + 8 ;
    m := !m asr 8 ;
  done ;
  let k = !k + (Char.code @@ String.unsafe_get values128 (!m land 127)) in
  (k, n asr k)
(* The following implementation is constant‐time. However, benchmarking shows
 * that it only becomes faster than the naive implementation above when the
 * valuation is at least 14, which is very unlikely. With random integers, it is
 * about twice slower. *)
(*
let valuation_of_2 n =
  assert (n <> 0) ;                  (*    n = 0b ???????10000 *)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = (* we convert to float and get the exponent *)
    if bits land (1 lsl 53) = 0 then
      snd@@frexp (float bits)
    else
      54 + (snd@@frexp (float (bits lsr 54)))
  in
  (k, n asr k)
*)


(* NOTE: Another interesting thing to compute about perfect squares:
 *     W. D. Stangl, “Counting Squares in ℤn”, Mathematics Magazine, 1996:
 *     https://www.maa.org/sites/default/files/Walter_D22068._Stangl.pdf)
 * In summary, if squares(n) is the number of squares modulo n, then squares is
 * a multiplicative function, and (for any odd prime p):
 *     squares(2^k) = {  (2^k +  8) ∕ 6  if k is even ≥ 2
 *                    {  (2^k + 10) ∕ 6  if k is odd
 *     squares(p^k) = {  (p^(k+1) +  p + 2) ∕ (2(p+1))  if k is even
 *                    {  (p^(k+1) + 2p + 1) ∕ (2(p+1))  if k is odd
 * In particular:
 *     squares(1)  = 1
 *     squares(2)  = 2
 *     squares(4)  = 2
 *     squares(p)  = (p+1) ∕ 2
 *     squares(p²) = (p² − p + 2) ∕ 2
 *)

let is_square =
  (* To quickly filter out many non-squares, we test whether the number is
   * a square modulo word_size (32 or 64).
   *
   * The only squares modulo 32 are 0, 1, 4, 9, 16, 17 and 25, so checking
   * whether the number is a square modulo 32 rules out 78% of non-squares.
   * Likewise, the ratio of non-squares modulo 64 is 81%.
   *
   * We use a single native integer to store the lookup table. The same code
   * works for 32-bit and 64-bit OCaml, because of two facts:
   *   - [1 lsl n] is the same as [1 lsl (n mod word_size)];
   *   - a square modulo 64 is also a square modulo 32 and, conversely, every
   *     square modulo 32 is the residue of a square modulo 64.
   * The test can easily be extended to larger powers of 2, but squares modulo
   * 128 and larger are not included in the lookup table below.
   *
   * See also https://gmplib.org/manual/Perfect-Square-Algorithm.html *)
  assert (Sys.word_size <= 64) ;
  let squares_mod_wordsz =
    (1 lsl  0) lor (1 lsl  1) lor (1 lsl  4) lor (1 lsl  9) lor
    (1 lsl 16) lor (1 lsl 17) lor (1 lsl 25) lor (1 lsl 33) lor
    (1 lsl 36) lor (1 lsl 41) lor (1 lsl 49) lor (1 lsl 57)
  in
  let[@inline] is_square_mod_wordsz n =
    squares_mod_wordsz land (1 lsl n) <> 0
  in
  let sqrt_max_int = 1 lsl (uint_size / 2) - 1 in
fun ?root n ->
  begin match root with
  | None   ->  is_square_mod_wordsz n && 0 <= n && let r = isqrt n in r * r = n
  | Some r ->  is_square_mod_wordsz n && abs r <= sqrt_max_int && r * r = n
  end

let jacobi a n =
  assert (0 < n) ;
  assert (n land 1 <> 0) ;
  let ra = ref (erem a n) in
  let rn = ref n in
  let acc = ref 0 in
  (* We accumulate a sum such that the result will be (−1)^acc, hence it is
   * enough to compute it modulo 2; recall that in this condition, (lxor) is
   * addition and (land) is multiplication. *)
  while !ra <> 0 do
    let a = !ra
    and n = !rn in
(*     assert (0 < a && a < n) ; *)
(*     assert (n land 1 <> 0) ; *)
    let (k, a) = valuation_of_2 a in
    (* a' is (a−1)∕2: *)
    let a' = a lsr 1 in
    (* n' is (n−1)∕2: *)
    let n' = n lsr 1 in
    (* This is equivalent modulo 2 to n' × (n'+1) ∕ 2: *)
    let t = (n' lsr 1) lxor n' in
    (* Computing modulo 2, we add s×k + a'×n' to the sum; the first term
     * accounts for the factors 2 (because (2|n) = (−1)^t), and the second term
     * accounts for the law of quadratic reciprocity): *)
    acc := !acc lxor (t land k) lxor (a' land n') ;
    (* Now we swap a and n thanks to the law of quadratic reciprocity: *)
    ra := n mod a ;
    rn := a
  done ;
  if !rn = 1 then
    powm1 !acc
  else
    0

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

let rand ?(min=0) ?(max=max_int) () =
  assert (min <= max) ;
  let min = Int64.of_int min
  and max = Int64.of_int max in
  let r = Random.int64 (Int64.sub (Int64.succ max) min) in
  Int64.to_int @@ Int64.add min r

let rand_signed ?(max=max_int) () =
  assert (0 <= max) ;
  rand ~min:(~- max) ~max ()

let ( ~- ) = opp
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div_exact
let ( // ) = equo
let ( /% ) = erem
let ( mod ) = erem

module Unsafe = struct
  let ( +! ) = Stdlib.( + )
  let ( -! ) = Stdlib.( - )
  let ( *! ) = Stdlib.( * )
end



(* Tests. *)
let () =
  assert (jacobi 2 3 = ~-1) ;
  assert (jacobi 2 9 = 1) ;
  assert (jacobi 21 39 = 0) ;
  assert (jacobi 30 59 = ~-1) ;
  ()
