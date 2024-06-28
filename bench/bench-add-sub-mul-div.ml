(*! exception Overflow !*)
let nan = min_int
let uint_size = Sys.int_size - 1

let sign a =
  compare a 0

let[@inline] mul_sign s n =
  let u = s asr Sys.int_size in
  n lxor u - u

let[@inline] abs n =
  let u = n asr Sys.int_size in
  n lxor u - u

(* specialized to type int *)
let min (a : int) (b : int) =
  if a <= b then a else b
let max (a : int) (b : int) =
  if a <= b then b else a

let ilog2sup_8bit =
  "\000\001\002\002\003\003\003\003\004\004\004\004\004\004\004\004\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"

(* from bench-micro-opt.ml: the fastest implementation of ilog2sup *)
let ilog2sup62bit_3 n =
  (*! assert (0 <= n) ; !*)
  let k = ref 0 in
  let n = ref n in
  (* 31 instead of 32, because our integers have 31 or 63 bits: *)
  let y = !n lsr 31 in if y <> 0 then (k := !k + 31 ; n := y) ;
  let y = !n lsr 16 in if y <> 0 then (k := !k + 16 ; n := y) ;
  let y = !n lsr  8 in if y <> 0 then (k := !k +  8 ; n := y) ;
  !k + Char.code (String.unsafe_get ilog2sup_8bit !n)

(* from bench-micro-opt.ml: the 2nd fastest implementation of ilog2sup *)
let[@inline] hash_63bit ~magic n =
  (n * magic) lsr 57
let magic1 = 0x03f6eaf2cd271461
let magic1table_ilog2_of_pow2 =
  "\063\000\058\001\059\047\053\002\060\039\048\027\054\033\042\003\061\051\037\040\049\018\028\020\055\030\034\011\043\014\022\004\062\057\046\052\038\026\032\041\050\036\017\019\029\010\013\021\056\045\025\031\035\016\009\012\044\024\015\008\023\007\006\005"
let ilog2sup62bit_6 n =
  (*! assert (0 <= n) ; !*)
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let n = n lor (n lsr 4) in
  let n = n lor (n lsr 8) in
  let n = n lor (n lsr 16) in
  let n = n lor (n lsr 32) in
  let n = n + 1 in
  Char.code (String.unsafe_get magic1table_ilog2_of_pow2 (hash_63bit ~magic:magic1 n))

let ilog2sup = ilog2sup62bit_3

let ilog2 n =
  ilog2sup n - 1

(******************************************************************************)

(*
 * ADD
 *)

(* the initial implementation in this library: *)
let add1 =
  let add_nonneg =
    let highest_bit = 1 lsl (uint_size - 1) in
  fun a b ->
    if a land b land highest_bit <> 0 then
      (*! raise Overflow !*)
      nan
    else if (a lor b) land highest_bit = 0 then
      a + b
    else begin
      let s = a + b in
      if s land highest_bit = 0 then
        (*! raise Overflow !*)
        nan
      else
        s
    end
  in
fun a b ->
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  if a lxor b >= 0 then begin
    if a >= 0 then
      add_nonneg a b
    else
      ~- (add_nonneg ~-a ~-b)
  end else
    a + b

(* implementation from Batteries (BatInt.SafeInt): *)
(* 1.1 times faster than add1 for arbitrary integers *)
(* 1.2 times faster than add1 for integers that cannot overflow *)
let add2 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let s = a + b in
  if (a > 0 && b > 0 && s < 0) || (a < 0 && b < 0 && s > 0) then
    (*! raise Overflow !*)
    nan
  else
    s

(* with an optimized test: *)
(* 2 times faster than add1 for arbitrary integers *)
(* 3 times faster than add1 for integers that cannot overflow *)
let add3 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let s = a + b in
  if (a lxor b) lor (a lxor lnot s) < 0 then
    s
  else
    (*! raise Overflow !*)
    nan

(******************************************************************************)

(*
 * SUB
 *)

(* the initial implementation in this library: *)
let sub1 =
  let add_nonneg =
    let highest_bit = 1 lsl (uint_size - 1) in
  fun a b ->
    if a land b land highest_bit <> 0 then
      (*! raise Overflow !*)
      nan
    else if (a lor b) land highest_bit = 0 then
      a + b
    else begin
      let s = a + b in
      if s land highest_bit = 0 then
        (*! raise Overflow !*)
        nan
      else
        s
    end
  in
fun a b ->
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  if a lxor b >= 0 then
    a - b
  else begin
    if a >= 0 then
      add_nonneg a ~-b
    else
      ~- (add_nonneg ~-a b)
  end

(* implementation from Batteries (BatInt.SafeInt): *)
(* 1.1 times faster than sub1 for arbitrary integers *)
(* 1.2 times faster than sub1 for integers that cannot overflow *)
let sub2 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let d = a - b in
  if (a > 0 && b < 0 && d < 0) || (a < 0 && b > 0 && d > 0) then
    (*! raise Overflow !*)
    nan
  else
    d

(* with an optimized test: *)
(* 2 times faster than sub1 for arbitrary integers *)
(* 3 times faster than sub1 for integers that cannot overflow *)
let sub3 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let d = a - b in
  if (a lxor b) land (a lxor d) >= 0 then
    d
  else
    (*! raise Overflow !*)
    nan

(******************************************************************************)

(*
 * MUL
 *)

(* implementation from Batteries (BatInt.SafeInt): *)
let mul0 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let c = a * b in
  (*! if (a lor b) lsr 31 = 0 || b = 0 || c / b = a then !*)
  if (abs a lor abs b) <= 0x7FFFFFFF || b = 0 || c / b = a then
  (*! if b = 0 || c / b = a then !*)
    c
  else
    (*! raise Overflow !*)
    nan

(* the initial implementation in this library: *)
let mul_nonneg =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a b ->
  let (ah, al) = (a lsr uint_half_size, a land lower_half)
  and (bh, bl) = (b lsr uint_half_size, b land lower_half) in
  if ah <> 0 && bh <> 0 then
    (*! raise Overflow !*)
    nan
  else begin
    let al_bl = al*bl in
    let (h, l) = (al_bl lsr uint_half_size, al_bl land lower_half) in
    (* This expression does not overflow (at most one of ah×bl and al×bh is
     * non‐null; and each variable is at most equal to lower_half = 2^{N∕2}−1
     * where N = uint_size, so that the total is at most equal to
     * lower_half² + lower_half, which is less than 2^N): *)
    let h' = ah*bl + al*bh + h in
    if h' > lower_half then
      (*! raise Overflow !*)
      nan
    else
      (h' lsl uint_half_size) lor l
  end

let mul1 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  if a lxor b >= 0 then begin
    if a >= 0 then
      mul_nonneg a b
    else
      mul_nonneg ~-a ~-b
  end else begin
    if a >= 0 then
      ~- (mul_nonneg a ~-b)
    else
      ~- (mul_nonneg ~-a b)
  end

(* avoiding branches with `mul_sign` and `abs`, which are themselves branchless: *)
let mul2 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  mul_sign (a lxor b) (mul_nonneg (abs a) (abs b))

(* inlining `mul_nonneg` and delaying some operations: *)
let mul3 =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if a <= lower_half || b <= lower_half then begin
    let (ah, al) = (a lsr uint_half_size, a land lower_half) in
    let (bh, bl) = (b lsr uint_half_size, b land lower_half) in
    let al_bl = al*bl in
    let (h, l) = (al_bl lsr uint_half_size, al_bl land lower_half) in
    (* This expression does not overflow (at most one of ah×bl and al×bh is
     * non‐null; and each variable is at most equal to lower_half = 2^{N∕2}−1
     * where N = uint_size, so that the total is at most equal to
     * lower_half² + lower_half, which is less than 2^N): *)
    let h' = al*bh + ah*bl + h in
    if h' <= lower_half then
      mul_sign (a0 lxor b0) ((h' lsl uint_half_size) lor l)
    else
      (*! raise Overflow !*)
      nan
  end
  else
    (*! raise Overflow !*)
    nan

(* with a fast-path for small integers: *)
let mul4 =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if (a lor b) <= lower_half then
    a0 * b0
  else if a <= lower_half || b <= lower_half then begin
    let (ah, al) = (a lsr uint_half_size, a land lower_half) in
    let (bh, bl) = (b lsr uint_half_size, b land lower_half) in
    let al_bl = al*bl in
    let (h, l) = (al_bl lsr uint_half_size, al_bl land lower_half) in
    (* This expression does not overflow (at most one of ah×bl and al×bh is
     * non‐null; and each variable is at most equal to lower_half = 2^{N∕2}−1
     * where N = uint_size, so that the total is at most equal to
     * lower_half² + lower_half, which is less than 2^N): *)
    let h' = al*bh + ah*bl + h in
    if h' <= lower_half then
      mul_sign (a0 lxor b0) ((h' lsl uint_half_size) lor l)
    else
      (*! raise Overflow !*)
      nan
  end
  else
    (*! raise Overflow !*)
    nan

(* with a fast-path for small integers,
 * and micro-optimizing the OR-test: *)
let mul4b =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if (a lor b) <= lower_half then
    a0 * b0
  else if (min[@inlined]) a b <= lower_half then begin
    let (ah, al) = (a lsr uint_half_size, a land lower_half) in
    let (bh, bl) = (b lsr uint_half_size, b land lower_half) in
    let al_bl = al*bl in
    let (h, l) = (al_bl lsr uint_half_size, al_bl land lower_half) in
    (* This expression does not overflow (at most one of ah×bl and al×bh is
     * non‐null; and each variable is at most equal to lower_half = 2^{N∕2}−1
     * where N = uint_size, so that the total is at most equal to
     * lower_half² + lower_half, which is less than 2^N): *)
    let h' = al*bh + ah*bl + h in
    if h' <= lower_half then
      mul_sign (a0 lxor b0) ((h' lsl uint_half_size) lor l)
    else
      (*! raise Overflow !*)
      nan
  end
  else
    (*! raise Overflow !*)
    nan

(* with a fast-path for small integers and saving some operations: *)
let mul5 =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if (a lor b) <= lower_half then
    a0 * b0
  else if a <= lower_half then begin
    let (bh, bl) = (b lsr uint_half_size, b land lower_half) in
    let al_bl = a*bl in
    let (h, l) = (al_bl lsr uint_half_size, al_bl land lower_half) in
    (* This expression does not overflow (each variable is at most equal to
    * lower_half = 2^{N∕2}−1 where N = uint_size, so that the total is at most
    * equal to lower_half² + lower_half, which is less than 2^N): *)
    let h' = a*bh + h in
    if h' <= lower_half then
      mul_sign (a0 lxor b0) ((h' lsl uint_half_size) lor l)
    else
      (*! raise Overflow !*)
      nan
  end
  else if b <= lower_half then begin
    let (ah, al) = (a lsr uint_half_size, a land lower_half) in
    let al_bl = al*b in
    let (h, l) = (al_bl lsr uint_half_size, al_bl land lower_half) in
    (* This expression does not overflow (same proof): *)
    let h' = ah*b + h in
    if h' <= lower_half then
      mul_sign (a0 lxor b0) ((h' lsl uint_half_size) lor l)
    else
      (*! raise Overflow !*)
      nan
  end
  else
    (*! raise Overflow !*)
    nan

(* adapted from Hacker’s Delight (2nd ed, Fig 2-2),
 * with a fast-path for small integers: *)
let mul6 =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if (a lor b) <= lower_half then
    a0 * b0
  else begin
    (* there are 3 cases:
     *   - if [log2 a + log2 b ≤ uint_size - 2], then [a * b] cannot overflow;
     *   - if [log2 a + log2 b = uint_size - 1], then [a * b] may overflow but
     *     the mathematical result is less than 2^(uint_size+1), so we can
     *     detect an overflow by checking the sign of the machine result;
     *   - if [log2 a + log2 b ≥ uint_size], then [a * b] always overflows.
     *)
    let k = (ilog2sup62bit_3[@inlined]) a in
    let l = (ilog2sup62bit_3[@inlined]) b in
    if k + l <= uint_size + 1 then begin
      let m = a * b in
      if m >= 0 then
        mul_sign (a0 lxor b0) m
      else
        (*! raise Overflow !*)
        nan
    end
    else
      (*! raise Overflow !*)
      nan
  end

(* adapted from Hacker’s Delight (2nd ed, Fig 2-2),
 * with a fast-path for small integers
 * and a bit of micro-optimization: *)
let mul6b =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if (a lor b) <= lower_half then
    a0 * b0
  else begin
    let m = a0 * b0 in
    (* there are 3 cases:
     *   - if [log2 a + log2 b ≤ uint_size - 2], then [a * b] cannot overflow;
     *   - if [log2 a + log2 b = uint_size - 1], then [a * b] may overflow but
     *     the mathematical result is less than 2^(uint_size+1), so we can
     *     detect an overflow by checking the sign of the machine result;
     *   - if [log2 a + log2 b ≥ uint_size], then [a * b] always overflows.
     *)
    let k = (ilog2sup62bit_3[@inlined]) a in
    let l = (ilog2sup62bit_3[@inlined]) b in
    if k + l <= uint_size + 1 && (m lxor a0 lxor b0 >= 0 || m = 0) then
      m
    else
      (*! raise Overflow !*)
      nan
  end

(* adapted from Hacker’s Delight (2nd ed, Fig 2-2),
 * with a fast-path for small integers
 * and a bit of micro-optimization,
 * slightly rewritten: *)
let mul6c =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if (a lor b) <= lower_half then
    a0 * b0
  else begin
    let m = a0 * b0 in
    (* there are 3 cases:
     *   - if [log2 a + log2 b ≤ uint_size - 2], then [a * b] cannot overflow;
     *   - if [log2 a + log2 b = uint_size - 1], then [a * b] may overflow but
     *     the mathematical result is less than 2^(uint_size+1), so we can
     *     detect an overflow by checking the sign of the machine result;
     *   - if [log2 a + log2 b ≥ uint_size], then [a * b] always overflows.
     *)
    let kl= (ilog2sup62bit_3[@inlined]) a
          + (ilog2sup62bit_3[@inlined]) b in
    if kl    <= uint_size + 1 && (m lxor a0 lxor b0 >= 0 || m = 0) then
      m
    else
      (*! raise Overflow !*)
      nan
  end

(* adapted from Hacker’s Delight (2nd ed, Fig 2-2),
 * with a branchless implementation of ilog2sup: *)
let mul7 =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if (a lor b) <= lower_half then
    a0 * b0
  else begin
    (* there are 3 cases:
     *   - if [log2 a + log2 b ≤ uint_size - 2], then [a * b] cannot overflow;
     *   - if [log2 a + log2 b = uint_size - 1], then [a * b] may overflow but
     *     the mathematical result is less than 2^(uint_size+1), so we can
     *     detect an overflow by checking the sign of the machine result;
     *   - if [log2 a + log2 b ≥ uint_size], then [a * b] always overflows.
     *)
    let k = (ilog2sup62bit_6[@inlined]) a in
    let l = (ilog2sup62bit_6[@inlined]) b in
    if k + l <= uint_size + 1 then begin
      let m = a * b in
      if m >= 0 then
        mul_sign (a0 lxor b0) m
      else
        (*! raise Overflow !*)
        nan
    end
    else
      (*! raise Overflow !*)
      nan
  end

(* adapted from Hacker’s Delight (2nd ed, Fig 2-2),
 * with a fast-path for small integers
 * and a bit of micro-optimization,
 * with a branchless implementation of ilog2sup: *)
let mul7b =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if (a lor b) <= lower_half then
    a0 * b0
  else begin
    let m = a0 * b0 in
    (* there are 3 cases:
     *   - if [log2 a + log2 b ≤ uint_size - 2], then [a * b] cannot overflow;
     *   - if [log2 a + log2 b = uint_size - 1], then [a * b] may overflow but
     *     the mathematical result is less than 2^(uint_size+1), so we can
     *     detect an overflow by checking the sign of the machine result;
     *   - if [log2 a + log2 b ≥ uint_size], then [a * b] always overflows.
     *)
    let k = (ilog2sup62bit_6[@inlined]) a in
    let l = (ilog2sup62bit_6[@inlined]) b in
    if k + l <= uint_size + 1 && (m lxor a0 lxor b0 >= 0 || m = 0) then
      m
    else
      (*! raise Overflow !*)
      nan
  end

(* adapted from Hacker’s Delight (2nd ed, Fig 2-2),
 * with a fast-path for small integers
 * and a bit of micro-optimization,
 * slightly rewritten,
 * with a branchless implementation of ilog2sup: *)
let mul7c =
  let uint_half_size = uint_size / 2 in
  let lower_half = (1 lsl uint_half_size) - 1 in
fun a0 b0 ->
  (*! assert (a0 <> nan) ; !*)
  (*! assert (b0 <> nan) ; !*)
  let a = abs a0 in
  let b = abs b0 in
  if (a lor b) <= lower_half then
    a0 * b0
  else begin
    let m = a0 * b0 in
    let kl= (ilog2sup62bit_6[@inlined]) a
          + (ilog2sup62bit_6[@inlined]) b in
    if kl    <= uint_size + 1 && (m lxor a0 lxor b0 >= 0 || m = 0) then
      m
    else
      (*! raise Overflow !*)
      nan
  end

(******************************************************************************)

(*
 * EDIV / EQUO / EREM
 *)

(* the initial implementation in this library: *)
let ediv1 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let q = a / b
  and r = a mod b in
  if r >= 0 then
    (q, r)
  else begin
    let s = sign b in
    (q - s, r + s*b)
  end

(* avoiding a second division: *)
let ediv2 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let q = a / b in
  let r = a - q * b in
  if r >= 0 then
    (q, r)
  else begin
    let s = sign b in
    (q - s, r + s*b)
  end

(* avoiding a multiplication by ±1: *)
let ediv3 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let q = a / b in
  let r = a - q * b in
  if r >= 0 then
    (q, r)
  else
    (q - sign b, r + abs b)

(* branchless: *)
let ediv4 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let q = a / b in
  let r = a - q * b in
  let u = r asr Sys.int_size in
  (* u is 0 if r >= 0 and −1 if r < 0 *)
  (q - (u land sign b), r + (u land abs b))


(* the initial implementation in this library: *)
let equo1 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let q = a / b in
  if a >= 0 || q*b = a then
    q
  else
    q - sign b

(* simplifying the condition by computing the multiplication in all cases: *)
let equo2 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let q = a / b in
  let r = a - q * b in
  if r >= 0 then
    q
  else
    q - sign b

(* branchless *)
let equo3 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let q = a / b in
  let r = a - q * b in
  let u = r asr Sys.int_size in
  (* u is 0 if r >= 0 and −1 if r < 0 *)
  q - (u land sign b)


(* the initial implementation in this library: *)
let erem1 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let r = a mod b in
  if r >= 0 then
    r
  else
    r + abs b

(* branchless *)
let erem2 a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let r = a mod b in
  let u = r asr Sys.int_size in
  (* u is 0 if r >= 0 and −1 if r < 0 *)
  r + (u land abs b)

(******************************************************************************)

(*
 * TESTS
 *)

let timed ?msg f x =
  begin match msg with
  | None -> ()
  | Some msg -> Printf.printf "%s %!" msg
  end ;
  Gc.compact () ;
  let t0 = Sys.time () in
  let y = f x in
  let d = 100. *. (Sys.time () -. t0) in
  Printf.printf "--- %s%.3g s\n%!" (if d >= 10. then "" else " ") d ;
  y

let pp_binary out ?(len=1) n =
  assert (0 <= n) ;
  let s = ref "" in
  let n = ref n in
  while !n <> 0 do
    s := (if !n land 1 = 0 then "0" else "1") ^ !s ;
    n := !n lsr 1 ;
  done ;
  let s = !s in
  let pad = String.make (max 0 (len - String.length s)) '0' in
  Printf.fprintf out "%s" (pad ^ s)

let tests1 ~rounds ~min ~max =
  Gc.compact () ;
  let data = Array.init rounds (fun _ -> Euler.Arith.rand ~min ~max ()) in
  for i = 0 to rounds-2 do
    let x1 = data.(i) in
    let x2 = data.(i+1) in
    let y1 = add1 x1 x2 in
    let y2 = add2 x1 x2 in
    let y3 = add3 x1 x2 in
    assert (y1 = y2) ;
    assert (y1 = y3) ;
    let y1 = sub1 x1 x2 in
    let y2 = sub2 x1 x2 in
    let y3 = sub3 x1 x2 in
    assert (y1 = y2) ;
    assert (y1 = y3) ;
    let y0 = mul0 x1 x2 in
    let y1 = mul1 x1 x2 in
    let y2 = mul2 x1 x2 in
    let y3 = mul3 x1 x2 in
    let y4 = mul4 x1 x2 in
    let y4b = mul4b x1 x2 in
    let y5 = mul5 x1 x2 in
    let y6 = mul6 x1 x2 in
    let y6b = mul6b x1 x2 in
    let y6c = mul6c x1 x2 in
    let y7 = mul7 x1 x2 in
    let y7b = mul7b x1 x2 in
    let y7c = mul7c x1 x2 in
    assert (y1 = y0) ;
    assert (y1 = y2) ;
    assert (y1 = y3) ;
    assert (y1 = y4) ;
    assert (y1 = y4b) ;
    assert (y1 = y5) ;
    assert (y1 = y6 || let () = Printf.eprintf "%i × %i =?= %i or %i\n%!" x1 x2 y1 y6 in false) ;
    assert (y1 = y6b) ;
    assert (y1 = y6c) ;
    assert (y1 = y7) ;
    assert (y1 = y7b) ;
    assert (y1 = y7c) ;
  done ;
(*
  timed ~msg:"add1" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = add1 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"add2" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = add2 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"add3" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = add3 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"sub1" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = sub1 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"sub2" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = sub2 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"sub3" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = sub3 data.(i) data.(i+1) in ()
    done
  end () ;
*)
  timed ~msg:"mul0" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul0 data.(i) data.(i+1) in ()
    done
  end () ;
(*
  timed ~msg:"mul1" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul1 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul2" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul2 data.(i) data.(i+1) in ()
    done
  end () ;
*)
  timed ~msg:"mul3" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul3 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul4" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul4 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul4b" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul4b data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul5" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul5 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul6" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul6 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul6b" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul6b data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul6c" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul6c data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul7" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul7 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul7b" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul7b data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"mul7c" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = mul7c data.(i) data.(i+1) in ()
    done
  end () ;
  ()

let tests2 ~rounds ~min ~max =
  Gc.compact () ;
  let nonzero n = if n = 0 then 1 else n in
  let data = Array.init rounds (fun _ -> nonzero @@ Euler.Arith.rand ~min ~max ()) in
  for i = 0 to rounds-2 do
    let x1 = data.(i) in
    let x2 = data.(i+1) in
    let y1 = ediv1 x1 x2 in
    let y2 = ediv2 x1 x2 in
    let y3 = ediv3 x1 x2 in
    let y4 = ediv4 x1 x2 in
    assert (y1 = y2) ;
    assert (y1 = y3) ;
    assert (y1 = y4) ;
    let y1 = equo1 x1 x2 in
    let y2 = equo2 x1 x2 in
    let y3 = equo3 x1 x2 in
    assert (y1 = y2) ;
    assert (y1 = y3) ;
    let y1 = erem1 x1 x2 in
    let y2 = erem2 x1 x2 in
    assert (y1 = y2) ;
  done ;
  timed ~msg:"ediv1" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = ediv1 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"ediv2" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = ediv2 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"ediv3" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = ediv3 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"ediv4" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = ediv4 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"equo1" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = equo1 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"equo2" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = equo2 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"equo3" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = equo3 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"erem1" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = erem1 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"erem2" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = erem2 data.(i) data.(i+1) in ()
    done
  end () ;
  ()

let () =
  Random.self_init () ;
  assert (Sys.int_size = 63) ;
  Printf.printf "integers with 62-bit magnitude (can overflow):\n" ;
  tests1 ~rounds:(1 lsl 22) ~min:Euler.Arith.min_int ~max:max_int ;
  (*! Printf.printf "integers with 61-bit magnitude (+/− cannot overflow, × can):\n" ; !*)
  (*! tests1 ~rounds:(1 lsl 22) ~min:(min_int/2+1) ~max:(max_int/2) ; !*)
  (*! Printf.printf "integers with 32-bit magnitude (+/− cannot overflow, × might):\n" ; !*)
  (*! let max32bit = (1 lsl 32) - 1 in !*)
  (*! tests1 ~rounds:(1 lsl 22) ~min:(-max32bit) ~max:(max32bit) ; !*)
  (*! Printf.printf "integers with 31-bit magnitude (+/−/× cannot overflow):\n" ; !*)
  (*! let max31bit = (1 lsl 31) - 1 in !*)
  (*! tests1 ~rounds:(1 lsl 22) ~min:(-max31bit) ~max:(max31bit) ; !*)
  for b = 33 downto 30 do
    Printf.printf "integers with %u-bit magnitude:\n" b ;
    let maxnbit = (1 lsl b) - 1 in
    tests1 ~rounds:(1 lsl 24) ~min:(-maxnbit) ~max:maxnbit ;
  done ;
  (*! Printf.printf "integers with 15-bit magnitude (+/−/× cannot overflow):\n" ; !*)
  (*! let maxnbit = (1 lsl 15) - 1 in !*)
  (*! tests1 ~rounds:(1 lsl 24) ~min:(-maxnbit) ~max:maxnbit ; !*)
(*
  Printf.printf "division of integers with 62-bit magnitude (may need correction):\n" ;
  tests2 ~rounds:(1 lsl 22) ~min:Euler.Arith.min_int ~max:max_int ;
  Printf.printf "division of positive integers with 62-bit magnitude (does not need correction):\n" ;
  tests2 ~rounds:(1 lsl 22) ~min:1 ~max:max_int ;
*)
