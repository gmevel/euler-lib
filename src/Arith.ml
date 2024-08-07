(* We use some functions which appeared in the stdlib after 4.07 (specifically,
 * functions in Seq), so we use Stdcompat to get an up-to-date version of the
 * stdlib: *)
open! Stdcompat

(* Unchecked indexing, useful for precomputed values: *)
let ( .!( ) ) = Array.unsafe_get
let[@inline] ( .![ ] ) s i = (Char.code[@inlined]) (String.unsafe_get s i)

(* We treat this native integer as an invalid value. We give it a name in this
 * module so that we can refer to it even though we rebind the name min_int. *)
let nan = Stdlib.min_int

let max_int = Stdlib.max_int

let min_int = Stdlib.min_int + 1

(* Number of bits of an unsigned integer (OCaml integers are one bit less than
 * machine words, and there is one sign bit). *)
let uint_size = Sys.int_size - 1

let uint_half_size = uint_size / 2
let lower_half = (1 lsl uint_half_size) - 1
let sqrt_max_int = lower_half

exception Overflow

exception Division_not_exact

(* We will use this datatype internally for some intermediate results that would
 * not fit in one OCaml integer, but that fit in two. *)
type long_int = {
  lo : int ;
  hi : int ;
}

(*
let sign a =
  if a = 0      then   0
  else if a > 0 then ~+1
  else               ~-1
*)
(* Below is a branchless implementation, which is 3.5 times faster than the
 * naive implementation. *)
(*
let[@inline] sign a =
  (a asr (Sys.int_size - 1)) lor ((-a) lsr (Sys.int_size - 1))
*)
(* Using the standard comparison function, specialized to type `int`, is even
 * faster. Although the doc does not guarantee that `compare` always return
 * −1, 0 or +1, it is enforced in all OCaml versions up to 4.14. This is about
 * 4 times faster than the naive implementation. *)
let[@inline] sign a =
  (*! assert (a <> nan) ; !*)
  compare a 0

(* This is a free generalization of [abs] and should be very fast (although not
 * benchmarked). *)
let[@inline] mul_sign s n =
  (*! assert (s <> nan) ; !*)
  (*! assert (n <> nan) ; !*)
  let u = s asr Sys.int_size in
  n lxor u - u

let[@inline] mul_sign0 s n =
  (*! assert (s <> nan) ; !*)
  (*! assert (n <> nan) ; !*)
  let u = s asr Sys.int_size in
  (n lxor u - u) land ((s lxor (-s)) asr Sys.int_size)

(*
let abs = abs
*)
(* Below is a branchless implementation, which is about 7.5 times faster than
 * the naive implementation. *)
let[@inline] abs n =
  (*! assert (n <> nan) ; !*)
  let u = n asr Sys.int_size in
  n lxor u - u

(* A function specialized for the type int is faster than the existing
 * polymorphic function. *)
(*
let min a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  if a <= b then a else b
*)
(* Below is a branchless implementation, which is about 1.5 times faster than
 * the naive implementation. Much shorter versions are presented in the wild,
 * but they ignore the fact that the subtraction can overflow. *)
let min a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
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
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
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
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let s = (a lxor b) asr Sys.int_size in
  (s land (a lor 1)) lor (lnot s land (a-b))
*)

let equal : int -> int -> bool = (=)

let[@inline] pred a =
  assert (a <> nan) ;
  if a = min_int then
    raise Overflow
  else
    a - 1

let[@inline] succ a =
  assert (a <> nan) ;
  if a = max_int then
    raise Overflow
  else
    a + 1

let opp = ( ~- )

let[@inline] add a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let s = a + b in
  if (a lxor b) lor (a lxor lnot s) < 0 then
    s
  else
    raise Overflow

let[@inline] sub a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let d = a - b in
  if (a lxor b) land (a lxor d) >= 0 then
    d
  else
    raise Overflow

let unsigned_long_add (a : int) (b : int) : long_int =
  assert (a >= 0) ;
  assert (b >= 0) ;
  let s = a + b in
  if s >= 0 then
    { hi = 0 ; lo = s }
  else
    { hi = 1 ; lo = s - max_int - 1 }

(* This implementation of [sum_of_seq] uses linear space in the worst case, but
 * it consumes the input sequence only once.
 *
 * [add_cons x ys] adds [x] to the list of summands [ys] and then collapses as
 * many consecutive summands as possible without overflowing: *)
let rec add_cons x ys =
  assert (x <> nan) ;
  begin match ys with
  | [] ->
      [ x ]
  | y :: ys' ->
      let s = x + y in
      (* overflow test (see [add]): *)
      if (x lxor y) lor (x lxor lnot s) < 0 then
        add_cons s ys'
      else
        x :: ys
  end
let sum_of_seq_oneshot xs =
  let ys = Seq.fold_left (fun ys x -> add_cons x ys) [ 0 ] xs in
  (* Invariant: all summands in [ys] are of the same sign (because consecutive
   * summands of different signs could be summed without overflow). *)
  begin match ys with
  | [ y ] -> y
  | _     -> raise Overflow
      (* If there are several summands remaining, then they are of the same sign
       * and cannot be summed, so the result is an overflow. *)
  end

(* This implementation of [sum_of_seq] is in constant space, but it consumes the
 * input sequence twice. *)
let rec sum_of_seq_aux s pos neg =
  if s >= 0 then
    begin match neg () with
    | Seq.Nil            -> Seq.fold_left add s pos
    | Seq.Cons (n, neg') -> assert (n <> nan) ;
                            sum_of_seq_aux (s + n) pos neg'
    end
  else
    begin match pos () with
    | Seq.Nil            -> Seq.fold_left add s neg
    | Seq.Cons (p, pos') -> sum_of_seq_aux (s + p) pos' neg
    end
let sum_of_seq_twoshot xs =
  let (pos, neg) = Seq.partition (fun x -> x >= 0) xs in
  sum_of_seq_aux 0 pos neg

(* We prefer the constant-space version, because from it we can obtain the
 * behavior of the oneshot version by memoizing the input sequence. *)
let sum_of_seq = sum_of_seq_twoshot

(* Here is a similar algorithm for lists, but, by contrast with [Seq.t], it
 * needlessly uses linear space, because it allocates two lists whose cumulated
 * length is that of the input list: *)
(*
let rec sum_aux pos neg =
  begin match pos, neg with
  | [], xs
  | xs, [] ->
      List.fold_left add 0 xs
  | p::pos', n::neg' ->
      assert (n <> nan) ;
      let s = p + n in
      if s >= 0 then
        sum_aux (s::pos') neg'
      else
        sum_aux pos' (s::neg')
  end
let sum xs =
  let (pos, neg) = List.partition (fun x -> x >= 0) xs in
  sum_aux pos neg
*)

(* Since algorithms on [Seq.t] use less memory, we use them. *)
let sum xs =
  sum_of_seq_twoshot (List.to_seq xs)

let unsigned_long_mul (a : int) (b : int) : long_int =
  assert (a >= 0) ;
  assert (b >= 0) ;
  let (ah, al) = (a lsr uint_half_size, a land lower_half) in
  let (bh, bl) = (b lsr uint_half_size, b land lower_half) in
  let mid = unsigned_long_add (al * bh) (ah * bl) in
  let mid_lo = mid.lo land lower_half in
  let low = unsigned_long_add (mid_lo lsl uint_half_size) (al * bl) in
  let mid_hi = (mid.hi lsl uint_half_size) lor (mid.lo lsr uint_half_size) in
  let hi = (ah * bh) + mid_hi + low.hi in
  { lo = low.lo ; hi = hi }

let mul a0 b0 =
  assert (a0 <> nan) ;
  assert (b0 <> nan) ;
  let a = abs a0 in
  let b = abs b0 in
  (* fast-path for small integers: *)
  if (a lor b) <= lower_half then
    a0 * b0
  else if a <= lower_half || b <= lower_half then begin
    (* NOTE: There does not seem to be any speed benefit in sparing some
     * operations by distinguishing the two cases. Perhaps any benefit is
     * countered by the increase in code size and the additional branching. *)
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
      raise Overflow
  end
  else
    raise Overflow

(* The following implementation of overflowing multiplication (adapted from
 * Hacker’s Delight, 2nd ed, Fig 2-2) is appealing but, outside of the fast
 * path, it calls [ilog2sup]; even with our fastest branchfree version of
 * [ilog2sup], it is about twice slower than the implementation above. *)
(*
let mul a0 b0 =
  assert (a0 <> nan) ;
  assert (b0 <> nan) ;
  let a = abs a0 in
  let b = abs b0 in
  (* fast-path for small integers: *)
  if (a lor b) <= lower_half then
    a0 * b0
  else begin
    let m = a0 * b0 in
    (* There are 3 cases:
     *   - if [ilog2 a + ilog2 b ≤ uint_size - 2], then [a * b] cannot overflow;
     *   - if [ilog2 a + ilog2 b = uint_size - 1], then [a * b] may overflow but
     *     the mathematical result is less than 2^(uint_size+1), so we can
     *     detect an overflow by checking the sign of the machine result;
     *   - if [ilog2 a + ilog2 b ≥ uint_size], then [a * b] always overflows.
     *)
    let k = (ilog2sup_branchless[@inlined]) a in
    let l = (ilog2sup_branchless[@inlined]) b in
    if k + l <= uint_size + 1 && (m lxor a0 lxor b0 >= 0 || m = 0) then
      (* ^ the last condition "|| m = 0" handles the case when a0 = 0 || b0 = 0;
       * this case would fail the sign test that precedes. *)
      m
    else
      raise Overflow
  end
*)

(* We don’t override the standard unsafe operators right now because we still
 * want to use them in this file, but in some parts we’ll need the safe
 * operations, so here we define local notations for them. *)
let ( +? ) = add
let ( -? ) = sub
let ( *? ) = mul

let[@inline] mul2 a =
  assert (a <> nan) ;
  (* FIXME: This does not raise [Overflow] when [a = Stdlib.min_int/2] but
   * rather it returns [nan]; oh well… *)
  if a lxor (a lsl 1) >= 0 then
    a lsl 1
  else
    raise Overflow

let[@inline] mul_pow2 k a =
  assert (0 <= k) ;
  assert (a <> nan) ;
  if abs a lsr max 0 (uint_size - k) = 0 then
    a lsl k
  else
    raise Overflow

let prod_of_seq xs =
  let rec prod_aux p xs =
    begin match xs () with
    | Seq.Nil ->
        p
    | Seq.Cons (x, xs') ->
        if x = 0 then
          0
        else begin
          begin match mul p x with
          | p' ->
              prod_aux p' xs'
          | exception Overflow ->
              if Seq.exists (fun x -> x = 0) xs' then
                0
              else
                raise Overflow
          end
        end
    end
  in
  prod_aux 1 xs

let prod xs =
  prod_of_seq (List.to_seq xs)

let div_exact a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let q = a / b in
  if a = q * b then
    q
  else
    raise Division_not_exact

let sdiv a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let q = a / b in
  let r = a - q * b in
  (q, r)

let ediv a b =
  let (q, r) = sdiv a b in
  if r >= 0 then
    (q, r)
  else
    (q - sign b, r + abs b)
(* NOTE: The implementation below is branchless and thus faster for arbitrary
 * input numbers, but it is slower when a >= 0, which is the common scenario.
 * The same remark applies to equo and erem below. *)
(*
let ediv a b =
  let (q, r) = sdiv a b in
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

let[@inline] ediv2 a =
  (*! assert (a <> nan) ; !*)
  (a asr 1, a land 1)

let[@inline] equo2 a =
  (*! assert (a <> nan) ; !*)
  a asr 1

let[@inline] erem2 a =
  (*! assert (a <> nan) ; !*)
  a land 1

let[@inline] ediv_pow2 a k =
  assert (a <> nan) ;
  assert (0 <= k) ;
  (* [a asr uint_size] gives 0 if [a] ≥ 0 and −1 if [a] < 0 *)
  if k <= uint_size then
    (a asr k, a land ((1 lsl k) - 1))
  else if a >= 0 then
    (0, a)
  else
    raise Overflow

let[@inline] equo_pow2 a k =
  assert (a <> nan) ;
  assert (0 <= k) ;
  a asr (min k uint_size)

let[@inline] erem_pow2 a k =
  assert (a <> nan) ;
  assert (0 <= k) ;
  if k <= uint_size then
    a land ((1 lsl k) - 1)
  else if a >= 0 then
    a
  else
    raise Overflow

let pow_ =
  (*! assert (a <> nan) ; !*)
  (*! assert (k <> nan) ; !*)
  Common.pow ~mult:mul ~unit:1

let[@inline] pow2 k =
  assert (0 <= k) ;
  if k < uint_size then
    1 lsl k
  else
    raise Overflow

let[@inline] powm1 k =
  (*! assert (k <> nan) ; !*)
  1 - ((k land 1) lsl 1)

(* We precompute powers of small bases. This is useful for a fast logarithm.
 *
 * The helper function below builds a table which contains 0, followed by all
 * powers of [base] that do not overflow (if the provided [max_exp] is correct),
 * followed by [max_int]+1: *)
let make_table_prev_pow ~base ~max_exp =
  Array.init (max_exp + 3)
    (fun i ->
      if i = 0 then 0
      else if i-1 <= max_exp then pow_ base (i-1)
      else Stdlib.min_int)

(* NOTE: these max exponents are correct for Sys.int_size = 31, 32, 63, 64. *)
let table_prev_pow3 = make_table_prev_pow ~base:3 ~max_exp:(uint_size * 12/19)
let table_prev_pow5 = make_table_prev_pow ~base:5 ~max_exp:(uint_size * 3/7)
let table_prev_pow6 = make_table_prev_pow ~base:6 ~max_exp:(uint_size * 5/13)
let table_prev_pow7 = make_table_prev_pow ~base:7 ~max_exp:(uint_size * 4/11)
(*! let table_prev_pow9 = make_table_prev_pow ~base:9 ~max_exp:(uint_size * 4/13) !*)
let table_prev_pow10 = make_table_prev_pow ~base:10 ~max_exp:(uint_size * 3/10)
let table_prev_pow60 = make_table_prev_pow ~base:60 ~max_exp:(uint_size / 6)

(* Since we have precomputed powers of small bases, we also let [pow] take
 * advantage of it.
 *
 * TODO: benchmark this...
 *)
let rec pow a k =
  (*! assert (a <> nan) ; !*)
  assert (0 <= k) ;
  begin match a with
  | -1 -> powm1 k
  | 0 -> if k = 0 then 1 else 0
  | 1 -> 1
  | 2 -> pow2 k
  | 3 ->
      if k < Array.length table_prev_pow3 - 2
      then table_prev_pow3.!(k+1)
      else raise Overflow
  | 4 -> pow2 (mul2 k)
  | 5 ->
      if k < Array.length table_prev_pow5 - 2
      then table_prev_pow5.!(k+1)
      else raise Overflow
  | 6 ->
      if k < Array.length table_prev_pow6 - 2
      then table_prev_pow6.!(k+1)
      else raise Overflow
      (* No need to store the powers of 6, we can use the fact that 6 = 2×3: *)
      (*! if k < Array.length table_prev_pow3 - 2 !*)
      (*! then mul_pow2 k table_prev_pow3.!(k+1) !*)
      (*! else raise Overflow !*)
  | 7 ->
      if k < Array.length table_prev_pow7 - 2
      then table_prev_pow7.!(k+1)
      else raise Overflow
  | 8 -> pow2 (3 *? k)
  | 9 ->
      (*! if k < Array.length table_prev_pow9 - 2 !*)
      (*! then table_prev_pow9.!(k+1) !*)
      (*! else raise Overflow !*)
      (* No need to store the powers of 9, we can use the fact that 9 = 3²: *)
      if k < Array.length table_prev_pow3 - 2
      then let p3 = table_prev_pow3.!(k+1) in p3 *? p3
      else raise Overflow
  | 10 ->
      if k < Array.length table_prev_pow10 - 2
      then table_prev_pow10.!(k+1)
      else raise Overflow
  | 16 -> pow2 (mul_pow2 2 k)
  | 32 -> pow2 (5 *? k)
  | 64 -> pow2 (6 *? k)
  | 128 -> pow2 (7 *? k)
  | 256 -> pow2 (mul_pow2 3 k)
  | _ when a < 0 && -10 <= a ->
      mul_sign (k lsl uint_size) (pow (-a) k)
  | _ ->
      (* Whenever possible, we reduce a in order to take profit
       * from the special cases when a is small. *)
      (* [valuation_of_2] is not defined yet: *)
      (*! let (val2, a') = valuation_of_2 a in !*)
      (*! if abs a' <= 10 then !*)
      (*!   mul_pow2 (val2 *? k) (pow a' k) !*)
      (*! else !*)
      if a land 1 = 0 then
        if a land 0b11 = 0 then
          if a land 0b1111 = 0 then
            if a land 0b1111_1111 = 0 then
              mul_pow2 (mul_pow2 3 k) (pow (a asr 8) k)
            else mul_pow2 (mul_pow2 2 k) (pow (a asr 4) k)
          else mul_pow2 (mul2 k) (pow (a asr 2) k)
        else mul_pow2 k (pow (a asr 1) k)
      else
        begin match k with
        | 0 -> 1
        | 1_-> a
        | 2 -> a *? a
        | 3 -> a *? a *? a
        | _ -> pow_ a k
        end
  end

(* Here is how to compute [ilog2sup] using floating-point operations.
 * Don’t use it, it is much slower (about 5 times slower) than our best solution
 * based on integer operations. *)
(*
let ilog2sup =
  (* As long as the integer can be represented exactly as a floating‐point
   * number (53 bits being the precision of floating‐point numbers), we can
   * convert to [float] and read its exponent. It gives wrong results for larger
   * numbers, because those may be rounded up (for example it gives 62 instead
   * of 61, for all numbers between 2^61−128 and 2^61−1). *)
  if uint_size <= 53 then
    fun n ->
      assert (0 <= n) ;
      snd@@frexp (float n)
  else if uint_size <= 62 then
    fun n ->
      assert (0 <= n) ;
      if n <= (1 lsl 53) - 1 then
        snd@@frexp (float n)
      else
        54 + snd@@frexp (float (n lsr 54))
  else
    assert false
*)

(* This is the fastest implementation of [ilog2sup], according to benchmarks.
 * (see Hacker’s Delight, 2nd ed, Figure 5-12 and the text below) *)
let ilog2sup =
  (* We use precomputed values for the ilog2sup of 8-bit numbers: *)
  let table_ilog2sup_8bit =
    "\000\001\002\002\003\003\003\003\004\004\004\004\004\004\004\004\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
  in
fun n ->
  assert (0 <= n) ;
  let k = ref 0 in
  let n = ref n in
  (* 31 instead of 32, because our integers have 31 or 63 bits: *)
  let hi = !n lsr 31 in if hi <> 0 then (k := !k + 31 ; n := hi) ;
  let hi = !n lsr 16 in if hi <> 0 then (k := !k + 16 ; n := hi) ;
  let hi = !n lsr  8 in if hi <> 0 then (k := !k +  8 ; n := hi) ;
  !k + table_ilog2sup_8bit.![!n]

(* The following branchless implementation is almost as fast as the one above
 * when benchmarked in isolation (1.2 times slower), and it seems to fare better
 * with inlining (perhaps because it benefits from instruction parallelism?).
 * Besides, its precomputed table is also useful for [valuation_of_2]. *)
let rec ilog2sup n =
  assert (0 <= n) ;            (* n = 0b ???????10000 *)
  (* we leak the highest set bit to lower bits: *)
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let n = n lor (n lsr 4) in
  let n = n lor (n lsr 8) in
  let n = n lor (n lsr 16) in
  let n = n lor (n lsr 32) in  (* n = 0b 000000011111 *)
  let n = n + 1 in             (* n = 0b 000000100000 *)
  magic_table_ilog2_of_pow2.![magic_hash n]

(* For 64-bit OCaml: This function returns a 6-bit number, ie. between 0 and 63.
 * It associates a distinct hash:
 *   - to every 63-bit number of the form 2^i where 0 ≤ i ≤ 63;
 *   - to every 63-bit number of the form 2^i − 1 where 0 ≤ i ≤ 63,
 *     with one exception: 2^0 − 1 (= 0) collides with 2^58 − 1.
 * Note that the same magic constant would also work for 64-bit numbers:
 * the shift must be changed from 57 to 58, and the tables adjusted;
 * then we also get distinct hashes for the 2^i, and for the 2^i − 1,
 * where 0 ≤ i ≤ 63, excepted 2^0 − 1 (= 0) colliding with 2^1 − 1 (= 1).
 *
 * For 32-bit OCaml: same but
 *   - integers have 31 bits
 *   - the hash is a 5-bit number between 0 and 31
 *   - 0 ≤ i ≤ 31
 *   - the 31-bit collision is 2^0 − 1 vs 2^22 − 1
 *   - for 32-bit integers, adjust the shift from 26 to 27
 *   - the 32-bit collision is 2^0 − 1 vs 2^1 − 1.
 *
 * Explanation (for 64-bit OCaml):
 *
 * This works because in the binary string "magic00000", where "magic" is the
 * binary writing of the magic constant and "00000" are five 0s, all six-bit
 * sequences are distinct. Then, computing the magic hash of 2^i just returns
 * the i-th sequence of six bits in this string (recall that to multiply by 2^i
 * is to shift left by i bits).
 *
 * Why this also works for numbers of the form 2^i − 1 is a bit more mysterious.
 * Essentially, this is because bits 58--63 are 0s and bits 52--57 are 1s. Then,
 * computing the magic hash of 2^i − 1 returns the highest six bits of
 *     (magic lsl i) − magic
 * and, because the next six bits are 111111, the subtraction always propagate
 * a carry to the highest six bits… except when the left operand of the
 * subtraction also has 111111 in bits 52--57, which happens for i = 0.
 * Therefore, when i ≠ 0, the magic hash of 2^i − 1 is one less than the magic
 * hash of 2^i.
 *
 * http://graphics.stanford.edu/~seander/bithacks.html#IntegerLogDeBruijn
 * https://stackoverflow.com/questions/11376288/fast-computing-of-log2-for-64-bit-integers
 * https://en.wikipedia.org/wiki/De_Bruijn_sequence
 *)
and magic_hash =
  if Sys.int_size = 31 then
    fun[@inline] n -> (n * 0x07DCD629) lsr 26
  else if Sys.int_size = 63 then
    fun[@inline] n -> (n * 0x03F6EAF2CD271461) lsr 57
  else
    assert false

(* Precomputed hash table for the ilog2sup of powers of two.
 * It is such that if [h] is the magic_hash of 2^i where 0 ≤ i ≤ 63,
 * then the value of the table at index [h] is [i].
 *)
and magic_table_ilog2_of_pow2 =
  if Sys.int_size = 31 then
    "\031\000\022\001\028\023\013\002\029\026\024\017\019\014\009\003\030\021\027\012\025\016\018\008\020\011\015\007\010\006\005\004"
  else if Sys.int_size = 63 then
    "\063\000\058\001\059\047\053\002\060\039\048\027\054\033\042\003\061\051\037\040\049\018\028\020\055\030\034\011\043\014\022\004\062\057\046\052\038\026\032\041\050\036\017\019\029\010\013\021\056\045\025\031\035\016\009\012\044\024\015\008\023\007\006\005"
  else
    assert false

(* By picking a good initial estimation for the logarithm, our implementation is
 * likely much faster than the naive implementation (not checked thoroughly). *)
let ilogsup ?(base=10) n =
  begin match base with
  |  2 -> ilog2sup n
  |  3 ->
      let l = (81 * ilog2sup n + 127) (* / 128 *) lsr 7 in
      l + ((n - table_prev_pow3.!(l)) asr Sys.int_size)
  |  4 -> (ilog2sup n + 1) (* / 2 *) lsr 1
  |  5 ->
      let l = (7 * ilog2sup n + 15) (* / 16 *) lsr 4 in
      l + ((n - table_prev_pow5.!(l)) asr Sys.int_size)
  |  6 ->
      let l = (25 * ilog2sup n + 63) (* / 64 *) lsr 6 in
      l + ((n - table_prev_pow6.!(l)) asr Sys.int_size)
  |  7 ->
      let l = (23 * ilog2sup n + 63) (* / 64 *) lsr 6 in
      l + ((n - table_prev_pow7.!(l)) asr Sys.int_size)
  |  8 -> (ilog2sup n + 2) / 3
  |  9 ->
      (*! let l = (41 * ilog2sup n + 127) (* / 128 *) lsr 7 in !*)
      (*! l + ((n - table_prev_pow9.!(l)) asr Sys.int_size) !*)
      (* No need to store the powers of 9, just use the logarithm base 3: *)
      let l3 = (81 * ilog2sup n + 127) (* / 128 *) lsr 7 in
      (l3 + ((n - table_prev_pow3.!(l3)) asr Sys.int_size) + 1) (* / 2 *) lsr 1
  | 10 ->
      (* See Hacker’s Delight (2nd ed, Chapter 11, text below Figure 11-11)
       * for an explanation of the method. Here, since we are not computing
       * floor(log10(x)) but rather ilog10sup = ceil(log10(x+1)), we use an
       * over-approximation rather than an under-approximation:
       *     log10 (2) = 39/128 − ε  where ε > 0
       * so that:
       *     ilog10sup(x) = ceil(39/128 × log2sup(x)) − ε'  where ε' > 0.
       * The constant 39/128 is accurate enough that, for any x < 2^64,
       * the error commited ε' is at most 1. Then, we test whether an error
       * has been commited by using just a table lookup.
       *
       * General formula: to commit an error of at most 1 for all x < 2^N when
       * computing the logarithm in base B, the chosen constant c must satisfy:
       *     ilog_B(2) ≤ c < (1 + N×log_B(2)) / (1 + N)
       *)
      let l = (39 * ilog2sup n + 127) (* / 128 *) lsr 7 in
      l + ((n - table_prev_pow10.!(l)) asr Sys.int_size)
      (* ^ this hack avoids branching: [(x - y) asr Sys.int_size]
       *   returns 0 if [x - y >= 0] and -1 if [x - y < 0]. *)
  | 16 -> (ilog2sup n + 3) (* / 4 *) lsr 2
  | 32 -> (ilog2sup n + 4) / 5
  | 64 -> (ilog2sup n + 5) / 6
  | 128 -> (ilog2sup n + 6) / 7
  | 256 -> (ilog2sup n + 7) (* / 8 *) lsr 3
  | 60 ->
      let l = (11 * ilog2sup n + 63) (* / 64 *) lsr 6 in
      l + ((n - table_prev_pow60.!(l)) asr Sys.int_size)
  | _ ->
      assert (2 <= base) ;
      assert (0 <= n) ;
      if n <> 0 then begin
        (* Below is an excellent estimation, which also gives 0 for n=0, but:
         * (1) I’m not sure whether the estimation is always below the result;
         * (2) as we use float operations, it needs benchmarking. *)
        (*! let l_est = truncate (log (float (n+1)) /. log (float base)) in !*)
        (* Below is a more conservative under-estimation, proven correct;
         * I could also prove the associated over-estimation, (displayed below
         * in a comment), and:
         *     l_overest ≤ 2 × l_underest        for any base
         *     l_overest ≤ 3/2 × l_underest + 1  for base ≥ 4
         *     l_overest ≤ 4/3 × l_underest + 1  for base ≥ 8
         *     l_overest ≤ 5/4 × l_underest + 1  for base ≥ 16
         *     …
         * which is not satisfying because a priori we might do O(l_underest)
         * iterations from the initial estimation; however quick tests with
         * base=3 suggest that in practice, we iter much less than that(?). *)
        let l_underest = (ilog2sup n - 1) / ilog2sup (base-1) + 1 in
        (*! let l_overest = ilog2sup (n-1) / (ilog2sup base - 1) + 1 in !*)
        begin match pow_ base l_underest with
        | p ->
            (* Divisions are costly, we rather do repeated multiplications than
             * repeated divisions; we need one division for overflow control,
             * though: *)
            let stop = min n (max_int / base) in
            let p = ref p in
            let l = ref l_underest in
            (* invariant: p = base^l *)
            while !p <= stop do
              p := !p * base ; (* we carefully avoid overflowing here *)
              incr l ;
            done ;
            if !p > n then !l else !l + 1
        | exception Overflow -> l_underest
        end
      end
      else 0
  end

let[@inline] ilog2 n =
  ilog2sup n - 1

let[@inline] ilog ?base n =
  ilogsup ?base n - 1

let is_pow ?(base=10) ?exp n =
  assert (base <> nan) ;
  assert (exp <> Some nan) ;
  assert (n <> nan) ;
  begin match exp with
  | None ->
      if base = 0 then n = 1 || n = 0
      else if base = 1 then n = 1
      else if base = -1 then abs n = 1
      else n <> 0 && pow base (ilog ~base:(abs base) (abs n)) = n
  | Some exp ->
      (exp >= 0 || abs base = 1) && (try pow base (abs exp) = n with Overflow -> false)
  end

let[@inline] is_pow2 n =
  n land (n - 1) = 0 && n > 0

(* This function is placed here because it uses [ilog2sup]. *)
let unsigned_long_ediv (a : long_int) (b : int) : int * int =
  assert (a.hi >= 0 && a.lo >= 0) ;
  assert (b > 0) ;
  (* This internal function produces the quotient in a regular OCaml integer,
   * thus it can overflow. It wouldn’t be hard to extend it to produce the
   * quotient in a long_int, but we don’t need it. *)
  if a.hi >= b then
    raise Overflow
  else begin
    (* This is a long division where bits are grouped in chunks. We want chunks
     * to be as large as possible in order to minimize the number of operations,
     * but the chunk size k must satisfy (b−1) × 2^k ≤ max_int, because, at any
     * step of the algorithm, the current remainder may be as high as b−1, and
     * when shifting it by k bits we might get a value as high as (b−1) × 2^k,
     * and this value must not overflow. So we just take the largest such k.
     *
     * However, using a multi-bit base requires that we run a machine division
     * at each step, and machine divisions are very expensive. By contrast, the
     * simpler binary division (i.e. with base 2, reading bits one by one)
     * requires only a comparison and a subtraction, which is very cheap.
     *
     * So we seek a tradeoff: when chunk_size is large enough, we use our
     * chunked long division; when it is too small, we fallback to a binary
     * division. Even if it wasn’t for speed, we need our binary division
     * algorithm in order to deal with the case where chunk_size is zero
     * (happens when b > max_int/2), because in this case our chunked division
     * is unable to avoid overflows.
     *
     * Finding the best tradeoff would require proper benchmarking. The
     * threshold below is a very rough guess. Things to consider:
     *
     *   - by switching from the chunked division to the binary division, we
     *     replace one machine division by about chunk_size additions;
     *   - the relative latency of a machine division w.r.t addition varies
     *     considerably; with moderately recent processors, it can be as high as
     *     40--90 (Intel Skylake, 2015), or 30 (AMD Zen1, 2016); cutting-edge
     *     processors bring it down to 15 (Intel IceLake, 2020; AMD Zen4, 2022).
     *)
    let chunk_size = uint_size - ilog2sup (b-1) in (* threshold *)
    if chunk_size >= 20 then begin
      (* This is the chunked long division. *)
      let chunk_base = 1 lsl chunk_size in
      let chunk_mask = chunk_base - 1 in
      let q = ref 0 in
      let r = ref a.hi in
      let i = ref (uint_size - chunk_size) in
      while !i >= 0 do
        (*! assert (0 <= !r && !r < b) ; !*)
        let r' = (!r lsl chunk_size) lor ((a.lo lsr !i) land chunk_mask) in
        let (q1, r1) = sdiv r' b in
        (*! assert (0 <= q1 && q1 < chunk_base) ; !*)
        q := (!q lsl chunk_size) lor q1 ;
        r := r1 ;
        i := !i - chunk_size ;
      done ;
      let tail_size = !i + chunk_size in
      if tail_size > 0 then begin
        let tail_base = 1 lsl tail_size in
        let tail_mask = tail_base - 1 in
        (*! assert (0 <= !r && !r < b) ; !*)
        let r' = (!r lsl tail_size) lor (a.lo land tail_mask) in
        let (q1, r1) = sdiv r' b in
        (*! assert (0 <= q1 && q1 < tail_base) ; !*)
        q := (!q lsl tail_size) lor q1 ;
        r := r1 ;
      end ;
      (!q, !r)
    end
    else begin
      (* This is a variant of the binary long division. To avoid overflows when
       * shifting the remainder by one bit, we allow the remainder r to be
       * negative, and we keep its magnitude in the range 0...b/2 (by contrast,
       * the usual algorithm keeps r in the range 0 ≤ r < b). To achieve this,
       * we may either subtract b from, or add b to, the remainder, in which
       * case we accordingly add +1 or −1 to the quotient.
       *
       * An existing optimization of this is the “non-restoring division”, in
       * which we *always* add +1 or −1 to the quotient (i.e. the quotient’s new
       * digit is never 0), but I’m not sure whether it can be adapted to avoid
       * overflows:
       * https://en.wikipedia.org/wiki/Division_algorithm#Non-restoring_division
       *)
      let half_b = b lsr 1 in
      let q = ref 0 in
      let r = ref a.hi in
      if !r > half_b then begin
        q := 1 ;
        r := !r - b ;
      end ;
      for i = uint_size - 1 downto 0 do
        (*! assert (0 <= !q && (0 < !q || 0 <= !r)) ; !*)
        (*! assert (abs !r <= half_d) ; !*)
        q := (!q lsl 1) ;
        r := (!r lsl 1) lor ((a.lo lsr i) land 1) ;
        if abs !r > half_b then begin
          q := !q + sign !r ;
          r := !r - mul_sign !r b ;
        end ;
      done ;
      (* Lastly, restore a non-negative remainder: *)
      if !r < 0 then begin
        q := !q - 1 ;
        r := !r + b ;
      end ;
      (!q, !r)
    end
  end

let mul_ediv a b c =
  assert (a <> nan) ;
  assert (b <> nan) ;
  assert (c <> nan) ;
  (* checked by [ediv] anyway: *)
  (*if c = 0 then
    raise Division_by_zero ;*)
  begin try
    ediv (a *? b) c
  with Overflow ->
    let (q, r) = unsigned_long_ediv (unsigned_long_mul (abs a) (abs b)) (abs c) in
    if a lxor b >= 0 then
      (mul_sign c q, r)
    else if r <> 0 then
      (~- (mul_sign c (succ q)), abs c - r)
    else
      (~- (mul_sign c q), 0)
  end

let mul_equo a b c =
  fst (mul_ediv a b c)

(* Assuming that a and b are residues modulo m, returns the residue of a×b.
 * This is the basis of [Modular.mul], so we want it to be as fast as possible.
 * It is placed here, rather than in [Modular.ml], because we use it to
 * implement [mul_erem] and [gcdext]. *)
let _modular_mul ~modulo:m a b =
  (* This is an internal function with the following assumptions: *)
  (*! assert (m > 0) ; !*)
  (*! assert (0 <= a && a < m) ; !*)
  (*! assert (0 <= b && b < m) ; !*)
  if (a lor b) <= lower_half then
    (* Fast path: just use native arithmetic. *)
    (a * b) mod m
  else begin
    (* Slow path: there are several possible methods.
     * TODO: benchmark them and choose the best.
     *
     * METHOD (A): with a long division algorithm in base 2.
     *
     * We can use [mul_ediv] here because a and b are residues modulo m, so they
     * are non-negative and the quotient a×b÷m does not overflow.
     *)
(*
    snd (mul_ediv a b m)
*)
    (*
     * METHOD (B): with a multiplication algorithm in base 2.
     *
     * I suspect it to be faster than (A) because, by contrast with the chunked
     * long division, it does not perform any machine division; and by contrast
     * with the binary long division, it does not loop on all 62 bits of the
     * input, instead it loops over the bits of min(a,b) and skips its leading
     * zeros. But this needs benchmarking.
     *)
    let ra = ref a in
    let rb = ref b in
    if a < b then begin
      ra := b ;
      rb := a ;
    end ;
    let res = ref 0 in
    while !rb > 0 do
      (*
      if !rb mod 2 = 1 then
        res := _add ~modulo:m !res !ra ;
      ra := _add ~modulo:m !ra !ra ;
      rb := !rb / 2 ;
      *)
      (* Inlining this code manually gives considerable speedup: *)
      let a = !ra in
      let b = !rb in
      let m_a = m - a in
      if b land 1 <> 0 then
        res := (let r = !res in if r < m_a then r + a else r - m_a) ;
      ra := (if a < m_a then a + a else a - m_a) ;
      rb := b lsr 1 ;
    done ;
    !res
    (* Below are older, unfruitful ideas for optimizing the modular
     * multiplication.
     *
     * FIRST IDEA, I thought of computing the “low” and “high” parts of the
     * product a × b, by piecewise multiplication, such that:
     *
     *     a × b = low + (high × R),    where R := 2^w = max_int + 1
     *
     * Then we can reduce low, high and R modulo m, and we are left computing:
     *
     *     a ×: b = low' +: (high' ×: R')
     *
     * But then how to compute the product high' ×: R' without overflowing? Does
     * not seem simpler than the initial problem.
     *
     * A wild idea to further reduce high': if m is odd, then R is coprime with
     * m, so (assuming we can do that without first having implemented the
     * multiplication) we can pre-compute its modular inverse T := ((m+1)/2)^w
     * (because (m+1)/2 is the modular inverse of 2). Then, we can reduce high'
     * modulo T:
     *
     *     a ×: b = low' +: (high' // T) +: ((high' mod T) ×: R')
     *
     * But T or R' may not be small with respect to m.
     *
     * SECOND IDEA: take another base for the piecewise multiplication. Namely,
     * take s = r or s = r+1, where r := isqrt(m).
     *
     * (1) if r² ≤ m ≤ r(r+1), take s := r; then s² = m−p where p is in 0…r.
     * (2) if r(r+1) < m < (r+1)², take s := r+1; then s² = m+q where q is in 1…r.
     *
     * In both cases, all numbers x in the range 0…(m−1) can be decomposed as
     * x := (x0 + x1.s) where x0, x1 are in the range 0…r (x0 is even in the
     * range 0…(s−1)). Then:
     *
     *     a ×: b = (a0 + a1.s) ×: (b0 + b1.s)
     *            = a0.b0 +: (a0.b1 +: a1.b0) ×: s +: (a1.b1) ×: s²
     *
     * where the products a0.b0, a1.b0, etc. are in the range 0…r², thus they do
     * not overflow and are already reduced modulo m. Note that it is also the
     * case of products such as x0.s (because x0 is in the range 0…(s-1) and
     * (s−1)s ≤ m in both cases) and such as x1.s (because x1.s ≤ x < m).
     *
     * Let’s decompose the intermediate results:
     *
     *     (a0.b1 +: a1.b0) := A + B.s
     *              (a1.b1) := C + D.s
     *
     * Then:
     *     a ×: b = (a0 + a1.s) ×: (b0 + b1.s)
     *            = a0.b0 +: A.s +: B ×: s² +: C ×: s² +: D.s ×: s²
     *       if (1):
     *            = a0.b0 +: A.s -: B.p -: C.p -: D.s ×: p
     *       if (2):
     *            = a0.b0 +: A.s +: B.q +: C.q +: D.s ×: q
     *
     * But the problem is how to compute the last product, D.s ×: p or D.S ×: q.
     * It may be as large as r³, which may overflow. Besides, rewriting it as
     * D ×: s³ does not help, because (r³ mod m) is not necessarily small with
     * respect to m.
     *
     * What about r := icbrt(m) ?
     *)
  end

let mul_erem a b c =
  assert (a <> nan) ;
  assert (b <> nan) ;
  assert (c <> nan) ;
  _modular_mul (erem a c) (erem b c) (abs c)

(* The following implementation of the integer square root only uses integers,
 * but is MUCH slower than a naive floating‐point computation. *)
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
    let ry = ref (let k = ilog2sup n in 1 lsl ((k+1)/2)) in
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
  let r = truncate@@sqrt@@float n in
  (* We test whether the result of the floating-point calculation is indeed the
   * square root, ie. whether x² ≤ n. For values of n close to max_int, x is one
   * more than the square root, and x² overflows, giving min_int. To keep the
   * test correct even if the presence of an overflow, we write x²−1 < n instead
   * of x² ≤ n.
   * The first test is a shortcut for the common case (see the explanation above
   * for the constant involved). *)
  if r <= (1 lsl 26) || r*r - 1 < n then
    r
  else
    r - 1

(* The below implementation of [icbrt] has been verified for all 63-bit integers
 * with the same method as for [isqrt], i.e. based on the fact that the function
 * [fun x -> x ** (1./.3.)] is monotonic (which seems reasonable, but I haven’t
 * checked). *)
let icbrt n0 =
  assert (n0 <> nan) ;
  let n = abs n0 in
  let r = truncate (float n ** (1./.3.)) in
  let next_cube = (r+1)*(r+1)*(r+1) in
  (* [(x+1)³] overflows only when [x³] is the largest representable cube; then
   * [x³ < 2^(int_size−1) ≤ (x+1)³ < 2^int_size], because [((x+1)/x)³ < 2] as
   * soon as [x ≥ 4]. So, provided that we have wrapping integers with
   * [int_size ≥ 7], [(x+1)³] appears to be negative, and we can detect an
   * overflow just by looking at its sign.
   * By contrast with [isqrt], an off-by-one error happens pretty quick, as soon
   * as n = 4³ = 64, so there is no point in shortcutting the test. *)
  if n < next_cube || next_cube < 0 then
    mul_sign n0 r
  else
    mul_sign n0 (r+1)

(* The below implementation of [kth_root] again follows the same principle:
 * assuming that, for any [k] ≥ 2, the function [fun x -> x ** (1. /. float k)]
 * is monotonic (which seems reasonable but I haven’t proven it), I have checked
 * manually that, for all 63-bit numbers, the difference between the true result
 * and the result given by the floating-point function is always −1, 0 or +1. *)
let kth_root ~k x0 =
  assert (k >= 1) ;
  (* k = 1 is a special case because [truncate (float x)] might commit an error,
   * and also because (r+1) might overflow! *)
  if k = 1 then
    x0
  else begin
    assert (x0 <> nan) ;
    assert (x0 >= 0 || k land 1 <> 0) ;
    let x = abs x0 in
    let r = truncate (float x ** (1. /. float k)) in
    if (try pow r k > x with Overflow -> true) then
      mul_sign x0 (r-1)
    else if (try pow (r+1) k > x with Overflow -> true) then
      mul_sign x0 r
    else
      mul_sign x0 (r+1)
  end

(* Here is the code used for the aforementioned verification of the
 * floating-point computation of the integer k-th root. *)
(*
let () =
  let module S = Set.Make (Int) in
  let kth_root_float ~k x =
    truncate (float x ** (1. /. float k))
  in
  for k = 2 to 62 do (* this takes a long time for k=2 (it is fast for k ≥ 3) *)
    Printf.printf "k = %i\n%!" k ;
    let set = ref S.empty in
    let last_root = ref 0 in
    begin try for r = 1 to max_int do
      let p = pow r k in
      let root1 = kth_root_float ~k (p-1) in
      let root2 = kth_root_float ~k p in
      (* monotonicity safety check (not a full proof): *)
      assert (!last_root <= root1 && root1 <= root2) ;
      last_root := root2 ;
      set := S.add (root1 - (r-1)) !set ;
      set := S.add (root2 - r) !set ;
    done with Overflow -> () end ;
    !set |>
    S.iter begin fun d ->
      Printf.printf "  difference: %i\n" d
    end
  done
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

let is_square_mod_wordsz =
  (* To quickly filter out many non-squares, we test whether the number is
   * a square modulo word_size (32 or 64).
   *
   * The only squares modulo 32 are 0, 1, 4, 9, 16, 17 and 25, so checking
   * whether the number is a square modulo 32 rules out 78% of non-squares.
   * Likewise, the ratio of non-squares modulo 64 is 81%.
   *
   * We use a single native integer to store the lookup table. The same code
   * works for 32-bit and 64-bit OCaml, because of two facts:
   *   - [1 lsl n] is the same as [1 lsl (n mod word_size)] (the OCaml manual
   *     states it is unspecified, but this is what happens in practice);
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
fun[@inline] n ->
  squares_mod_wordsz land (1 lsl n) <> 0

let is_square ?root n =
  assert (n <> nan) ;
  assert (root <> Some nan) ;
  begin match root with
  | None   ->  is_square_mod_wordsz n && 0 <= n && let r = isqrt n in r * r = n
  | Some r ->  is_square_mod_wordsz n && abs r <= sqrt_max_int && r * r = n
  end

let isqrt_if_square n =
  assert (n <> nan) ;
  if is_square_mod_wordsz n && 0 <= n then
    let r = isqrt n in
    if r * r = n then Some r else None
  else
    None

(* TODO: we can do the same kind of optimization for cubes, and thus provide
 * [is_cube] and [icbrt_if_cube], because there are only 3 cubes modulo 9
 * (which are 0, 1, 8); and, again, this extends to higher powers of 3.
 * However, for it to be interesting, we need a fast division by 9 (or by 3). *)

let is_kth_pow ~k ?root n =
  assert (k <> nan) ;
  assert (root <> Some nan) ;
  assert (n <> nan) ;
  if k < 0 then
    ((root = None || root = Some 1) && n = 1)
    || ((root = None || root = Some (~-1)) && powm1 k = n)
  else
    begin match root with
    | None   -> (0 <= n || k land 1 <> 0) && pow (kth_root ~k n) k = n
    | Some r -> (try pow r k = n with Overflow -> false)
    end

let[@inline] is_multiple ~of_:a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  begin try
    b mod a = 0
  with Division_by_zero ->
    b = 0
  end

let[@inline] is_even a =
  (*! assert (a <> nan) ; !*)
  a land 1 = 0

let[@inline] is_odd a =
  (*! assert (a <> nan) ; !*)
  a land 1 <> 0

let rec gcd a b =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  if b = 0 then
    abs a
  else
    gcd b (a mod b)

(* This is like [Seq.fold_left gcd 0 xs], except that there is a short-circuit
 * for when the result becomes 1. *)
let gcd_of_seq xs =
  let rec gcd_aux d xs =
    begin match xs () with
    | Seq.Nil           -> d
    | Seq.Cons (x, xs') -> let d' = gcd d x in if d' = 1 then 1 else gcd_aux d' xs'
    end
  in
  gcd_aux 0 xs

(* This is a straightforward implementation of the extended Euclidean algorithm.
 * Unfortunately, computing the Bézout’s coefficients may overflow, which is
 * a shame because there always exists a pair of representable coefficients
 * within integer boundaries. Below, we will use a more convoluted version to
 * avoid overflows, and we will also minimize the returned coefficients. *)
let _overflowing_gcdext a0 b0 =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
  let rec gcdext a b u v x y =
    (*! assert (a = u*a0 + v*b0) ; !*)
    (*! assert (b = x*a0 + y*b0) ; !*)
    if b = 0 then begin
      if a > 0 then
        (a, u, v)
      else
        (~-a, ~-u, ~-v)
    end else begin
      let (q, r) = sdiv a b in
      gcdext b r x y (u -? q *? x) (v -? q *? y)
    end
  in
  gcdext a0 b0 1 0 0 1

(* Assuming that [a] and [b] are residues modulo [m], this is the modular
 * subtraction. We need it for the modular GCD algorithm just below. *)
let _modular_sub ~modulo:m a b =
  (* This is an internal function with the following assumptions: *)
  (*! assert (m > 0) ; !*)
  (*! assert (0 <= a && a < m) ; !*)
  (*! assert (0 <= b && b < m) ; !*)
  if a >= b then a - b else a + (m - b)

(* To avoid overflows in [gcdext], we rely on a modular variant of the extended
 * Euclidean algorithm. In this version, we compute a coefficient modulo [m],
 * thus there is no overflow.
 *
 * This is also useful for computing modular inverses (see [Modular.inv]).
 *
 * [_modular_gcdext ~modulo:m b] returns a pair [(d, v)] where [1 ≤ d ≤ m] is
 * the GCD of [m] and [b], and [0 ≤ v < m] is such that [d = v·b  (mod m)].
 * When [b = 0], it returns [d = m]. Such a [v] is defined modulo [m/d].
 *)
let _modular_gcdext ~modulo:m b0 =
  (* This is an internal function with the following assumptions: *)
  (*! assert (m > 0) ; !*)
  (*! assert (0 <= b0 && b0 < m) ; !*)
  (* This is an adaptation of the code of [overflowing_gcdext]. By contrast with
   * the latter, we do not return [u], so we need neither parameter [u] nor [x].
   *
   * Invariants:
   *   0 ≤ b < a ≤ m
   *   a = v·b0  (mod m)
   *   b = y·b0  (mod m)
   *   0 ≤ v < m
   *   0 ≤ y < m  unless m = 1
   *)
  let rec gcdext a b v y =
    if b >= 2 then
      let (q, r) = sdiv a b in
      (* Here [q < m] since [b ≥ 2], so we can avoid computing [q mod m]: *)
      gcdext b r y (_modular_sub ~modulo:m v (_modular_mul ~modulo:m q y))
    else if b = 1 then
      (1, y)
    else (* b = 0 *)
      (a, v)
  in
  gcdext m b0 0 1

(* Our complete extended Euclidean algorithm uses [_modular_gcdext] as follows.
 * Assuming [0 < b < a], [_modular_gcdext ~modulo:a b] gives us the GCD [d] and
 * a coefficient [0 ≤ v < a] such that:
 *     v·b = d − u·a  for some u
 * Then we can deduce the matching coefficient [u]:
 *     −u = (v·b − d) / a = (v·(b/d) − 1) / (a/d)
 * Since [0 ≤ b < a] and [0 ≤ v], we have:
 *     0 ≤ −u < v < a
 * Thus, since [a] fits in a native integer, then so do [u] and [v].
 *
 * Moreover, all possible Bézout pairs (u,v) are equal modulo (b/d, −a/d),
 * so we can take the unique [v] in the range [0 ≤ v < a/d]. In that case
 * (still assuming [0 < b < a]), we also get:
 *     0 ≤ −u < b/d
 * so this allows us to minimize both [u] and [v].
 *
 * To compute [u] effectively, we may try to use the formula [u = (d − v·b) / a],
 * but this expression may overflow ([mul_ediv] might help, though?)…
 * Alternatively, we can use [_modular_gcdext] again! We get a coefficient [u]
 * such that:
 *      u·a = d (mod b)
 * A priori, that [u] is decoupled from our [v], but we have enough info to
 * build a Bézout pair (u1,v1) from [u] and [v]:
 *   - [u] tells us the class modulo [b/d] of [u1];
 *   - [v] tells us the class modulo [a/d] of [v1];
 *   - from what we have said earlier, there is a unique Bézout pair (u1,v1)
 *     such that:
 *         −b/d < u1 ≤ 0
 *            0 ≤ v1 < a/d
 *)
let _gcdext_aux a b =
  (* This is an internal function with the following assumption: *)
  (*! assert (0 < b && b < a) ; !*)
  let (a', b', d, u1, v1) =
    (* We may start by trying the simpler but possibly-overflowing procedure;
     * however, because of overflow checks, it is unlikely to be faster than the
     * general procedure. Thus this part has been commented. *)
    begin (* try
      (* [_overflowing_gcdext] works for arbitrary [a] and [b], but the
       * subsequent minimization assumes [0 < b < a]. *)
      let (d, u0, v0) = _overflowing_gcdext a b in
      let a' = a / d in
      let b' = b / d in
      (* Reduce the Bézout pair to the following range:
       *     −b' < u1 ≤ 0
       *      0  ≤ v1 < a' *)
      let (q, v1) = sdiv v0 a' in
      let u1 = u0 + q * b' in
      (a', b', d, u1, v1)
    with Overflow ->
    *)
      (* The general procedure, with modular arithmetic. *)
      let (d, v0) = _modular_gcdext ~modulo:a b in
      let a' = a / d in
      let b' = b / d in
      (* Reduce [v], then compute the unique associated [u] such that:
       *     −b' < u1 ≤ 0
       *      0  ≤ v1 < a' *)
      let v1 = erem v0 a' in
      let u1 =
        begin try
          (1 - v1 *? b') / a'
          (* TODO: benchmark a solution that uses [unsigned_long_{mul,ediv}]. *)
        with Overflow ->
          (* Here it is required that we have reduced [v1] before computing [u1]
           * because, otherwise, we don’t have unicity to guarantee us that the
           * computed [u1] matches [v1]. *)
          let (_d', u) = _modular_gcdext ~modulo:b' (a' mod b') in
          (*! assert (_d' = 1) ; !*)
          if u = 0 then 0 else u - b'
        end
      in
      (a', b', d, u1, v1)
    end
  in
  (* The Bézout pair (u1, v1) is in this reduced range: *)
  (*! assert (-b' < u1 && u1 <= 0) ; !*)
  (*! assert ( 0  < v1 && v1 <  a') ; (* v1 ≠ 0 because b < a *) !*)
  (*! assert (-u1 < v1) ; !*)
  (*! assert (u1 <> 0 || v1 = 1) ; (* because b ≠ 0 *) !*)
  (*! assert (d = u1*a + v1*b) ; !*)
  (* We further reduce it to the following range:
   *     |u2| ≤ b'/2
   *     |v2| ≤ a'/2 *)
  let (u2, v2) = (if v1 <= a' lsr 1 then (u1, v1) else (u1 + b', v1 - a')) in
  (*! assert (abs u2 <= b'/2) ; !*)
  (*! assert (abs v2 <= a'/2) ; !*)
  (*! assert (d = u2*a + v2*b) ; !*)
  (d, u2, v2)

let gcdext a0 b0 =
  let a = abs a0
  and b = abs b0 in
  if a = b || b = 0 then
    (a, sign a0, 0)
  else if a = 0 then
    (b, 0, sign b0)
  else if a < b then
    let (d, v, u) = _gcdext_aux b a in
    (d, mul_sign a0 u, mul_sign b0 v)
  else
    let (d, u, v) = _gcdext_aux a b in
    (d, mul_sign a0 u, mul_sign b0 v)

let gcdext_of_seq xs =
  (* With a first [fold_left] we read the sequence from left to right,
   * and compute the gcd d_i and associated coefficients (u_i, v_i) like so:
   *
   *     a1×u2 + a2×v2
   *     \_____ _____/
   *           v
   *           d2
   *           ... ×u3 + a3×v3
   *           \______ ______/
   *                  v
   *                  d3
   *                  ... ×u4 + a4×v4
   *                  \______ ______/
   *                         v
   *                         d4
   *                           .
   *                            .
   *                             .
   *                                 ... ×un + an×vn
   *                                 \______ ______/
   *                                        v
   *                                        dn = d
   *
   * Expanding the d_i recursively in these equations, the last line
   * gives a linear combination of all the a_i with a total equal to d.
   * With a second [fold_left], we compute the associated coefficients
   * by collapsing the u_i and v_i from bottom-to-top (right-to-left):
   *   - the coefficient of an     is                           vn
   *   - the coefficient of a{n−1} is                  v{n-1} × un
   *   - the coefficient of a{n−2} is         v{n-2} × u{n−1} × un
   *   - …
   *   - the coefficient of a2     is      v2 × u3 × u4 × ... × un
   *   - the coefficient of a1     is v1 × u2 × u3 × u4 × ... × un
   *)
  let (d, coeff_pairs) =
    Seq.fold_left
      begin fun (d, coeff_pairs) x ->
        let (d', u, v) = gcdext d x in
        (d', (u, v)::coeff_pairs)
      end
      (0, [])
      xs
  in
  let (_, coeffs) =
    List.fold_left
      begin fun (w, coeffs) (u, v) ->
        (*! (u *? w, (v *? w) :: coeffs) !*)
        (* We delay throwing Overflow exceptions because we might recover from
         * it, in the event that the overflowing expression is later multiplied
         * by zero. We record that an overflow occurred in [w] by using the
         * special value [nan].
         *
         * We minimize the risk of an overflow by relying on the fact that
         * [gcdext] returns coefficients whose magnitude are minimal. *)
        if w <> nan then
          ((try u *? w with Overflow -> nan), (v *? w) :: coeffs)
        else if v = 0 then
          ((if u = 0 then 0 else nan), 0 :: coeffs)
        else
          raise Overflow
      end
      (1, [])
      coeff_pairs
  in
  (d, coeffs)

let lcm a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  if a = 0 || b = 0 then
    0
  else
    a / gcd a b *? b

(* This is like [Seq.fold_left lcm 1 xs], except that there is a short-circuit
 * for when the result becomes 0, and that this short-circuit may avoid
 * overflows. *)
let lcm_of_seq xs =
  let rec lcm_aux m xs =
    begin match xs () with
    | Seq.Nil           -> m
    | Seq.Cons (x, xs') ->
        if x = 0 then 0
        else
          begin match lcm m x with
          | m'                 -> lcm_aux m' xs'
          | exception Overflow -> if Seq.exists ((=) 0) xs' then 0 else raise Overflow
          end
    end
  in
  lcm_aux 1 xs

let valuation ~factor:d n =
  (*! assert (d <> nan) ; !*)
  (*! assert (n <> nan) ; !*)
  assert (abs d <> 1) ;
  assert (n <> 0) ;
  let k = ref 0 in
  let m = ref n in
  let m' = ref 0 in
  while
    let (q, r) = sdiv !m d in
    m' := q ;
    r = 0
  do
    incr k ;
    m := !m' ;
  done ;
  (!k, !m)

(*
let valuation_of_2 n =
  (*! assert (n <> nan) ; !*)
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
(*
let valuation_of_2 =
  let table_val2_7bit =
    "\007\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\006\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000"
  in
fun n ->
  (*! assert (n <> nan) ; !*)
  assert (n <> 0) ;
  let k = ref 0 in
  let m = ref n in
  while !m land 255 = 0 do
    k := !k + 8 ;
    m := !m asr 8 ;
  done ;
  let k = !k + table_val2_7bit.![!m land 127] in
  (k, n asr k)
*)

(* The following implementation is branchless and constant-time. It is slightly
 * faster than the version above and re-uses the precomputed values for ilog2. *)
let valuation_of_2 n =
  assert (n <> 0) ;          (*    n = 0b ???????10000 *)
  let hbit = n land (-n) in  (* hbit = 0b 000000010000 *)
  let k = magic_table_ilog2_of_pow2.![magic_hash hbit] in
  (k, n asr k)

let smallest_root =
  let odd_prime_exponents = [| 3; 5; 7; 11; 13; 17; 19 |] in
  let smallest_root_positive n =
    assert (n > 1) ;
    (* The valuation [k] of a root of [n] is a divisor of the valuation of each
     * prime factor of [n]. Thus, to eliminate many cases quickly and to avoid
     * computing some k-th roots, we start by looking at the small prime factors
     * 2, 3, 5.
     *
     * We start with the prime factor 2 because [valuation_of_2] is cheap: *)
    let (v2, m2) = valuation_of_2 n in
    if v2 = 1 then (* [n] is a multiple of 2 but not of 4, thus cannot be a power *)
      (n, 1)
    else if m2 = 1 then (* [n] is a power of 2 *)
      (2, v2)
    else
    (* Now with the prime factor 3: *)
    let (v3, m23) = valuation ~factor:3 m2 in
    let v23 = gcd v2 v3 in
    if v23 = 1 then (* [n] cannot be a power *)
      (n, 1)
    else if m23 = 1 then (* the only prime factors of [n] are 2 and 3 *)
      (pow 2 (v2/v23) * pow 3 (v3/v23), v23)
    else
    (* Now with the prime factor 5: *)
    let (v5, m235) = valuation ~factor:5 m23 in
    let v235 = gcd v23 v5 in
    if v235 = 1 then (* [n] cannot be a power *)
      (n, 1)
    else if m235 = 1 then (* the only prime factors of [n] are 2, 3, 5 *)
      (pow 2 (v2/v235) * pow 3 (v3/v235) * pow 5 (v5/v235), v235)
    (* Now the general case. We have ruled out the case where the only prime
     * factors of [n] are 2, 3, 5, so any root of [n] is at least equal to 7.
     * Then, the root’s valuation is at most ilog7(max_int) = 22 on 64‐bit OCaml.
     *
     * Instead of checking whether [n] is a [k]-th pow for every possible [k] in
     * decreasing order, we will reconstruct [k] from its prime factorization.
     * It is thus enough to consider the prime exponents less than 22.
     *
     * We also keep in mind that [k] must be a divisor of [v235]. *)
    else begin
      (* invariant:
       *   - [!r]^[!k] = [n]
       *   - thus, the root of [n] is the root of [!r],
       *     and the valuation of that root in [n] is [!k × k']
       *     where [k'] is the valuation of the said root in [!r]
       *   - [k'] is a divisor of [!v]. *)
      let v = ref v235 in
      let k = ref 1 in
      let r = ref n in
      let s = ref 0 in
      let exception Break in
      begin try
        (* prime exponent 2: *)
        while !v land 1 = 0 &&
              (s := isqrt !r ; is_square ~root:!s !r) do
          r := !s ;
          k := !k lsl 1 ;
          v := !v lsr 1 ;
        done ;
        (* other prime exponents: *)
        odd_prime_exponents |>
        Array.iter begin fun p ->
          if !v <> 0 && !v < p then
            raise Break ;
          let v' = ref 0 in
          while
            let (qv, rv) = sdiv !v p in
            v' := qv ;
            rv = 0 && (s := kth_root ~k:p !r ; is_kth_pow ~k:p ~root:!s !r)
          do
            r := !s ;
            k := !k * p ;
            v := !v' ;
          done ;
        end
      with Break -> () end ;
      (!r, !k)
    end
  in
fun n ->
  assert (n <> nan) ;
  assert (n <> 0) ;
  if n = 1 then
    (1, 0)
  else if n = -1 then
    (-1, 1)
  else begin
    let (r, k) = smallest_root_positive (abs n) in
    if n >= 0 || k land 1 <> 0 then
      (sign n * r, k)
    else
      let (v2, k') = valuation_of_2 k in
      (~- (pow r (1 lsl v2)), k')
  end

let jacobi a n =
  (*! assert (a <> nan) ; !*)
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
    (* Computing modulo 2, we add t×k + a'×n' to the sum; the first term
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

(* This function is placed here because it uses [gcd], and is used by [binoms]. *)
let mul_div_exact a b c =
  assert (a <> nan) ;
  assert (b <> nan) ;
  assert (c <> nan) ;
  (* This will be checked anyway by following native divisions: *)
  (*if d = 0 then
    raise Division_by_zero ;*)
  let d = gcd a c in
  let (a', c') = (a / d, c / d) in
  a' *? (div_exact b c')

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


(* The following implementation uses a bitwise trick to remove the lowest bit
 * set (see Hacker’s Delight, 2nd ed, Fig 5-3). It does one iteration per bit
 * set, which on average is quite slow. *)
(*
let number_of_bits_set n =
  let n = ref n in
  let count = ref 0 in
  while !n <> 0 do
    n := !n land (!n - 1) ;
    incr count ;
  done ;
  !count
*)

(* The following implementation is branchless, constant-time, and much faster in
 * practice (at least 4 times faster even when there is only 8 bits set). It is
 * a divide-and-conquer algorithm that takes profit of bitwise parallelism.
 * (see Hacker’s Delight, 2nd ed, Fig 5-2 plus a later comment) *)
let number_of_bits_set x =
  (* Here we use [Int64.to_int] so that this code also compiles on 32-bit OCaml
   * (if we had written 63-bit [int] literals, the 32-bit compiler would fail).
   * Besides, this same code works for both 32-bit and 64-bit OCaml, because
   * [Int64.to_int] truncates the number to the right integer size.
   * Let’s just hope the compiler optimizes these constant expressions… *)
  (* add the adjacent bits pairwise, in parallel: *)
  let x = x - ((x lsr 1) land Int64.to_int 0x55555555_55555555L) in
  (* now add the 2-bit fields pairwise into 4-bit fields, in parallel: *)
  let x = (x land Int64.to_int 0x33333333_33333333L)
            + ((x lsr 2) land Int64.to_int 0x33333333_33333333L) in
  (* now add the 4-bit fields pairwise into 8-bit fields, in parallel: *)
  let x = (x + (x lsr 4)) land Int64.to_int 0x0F0F0F0F_0F0F0F0FL in
  (* The rest of this function... *)
(*
  (* now add the 8-bit fields pairwise, in parallel: *)
  let x = x + (x lsr 8) in
  (* now add the 16-bit fields pairwise, in parallel: *)
  let x = x + (x lsr 16) in
  (* now add the 32-bit fields pairwise: *)
  let x = x + (x lsr 32) in (* this line is broken on 32-bit OCaml *)
  x land 0xFF
*)
  (* ... can be optimized like this: *)
  (* now use a multiplication to add together all the 8-bit fields at once: *)
  (x * Int64.to_int 0x01010101_01010101L) lsr (Sys.word_size - 8)


let rand ?(min=0) ?(max=max_int) () =
  assert (min <> nan) ;
  assert (min <= max) ;
  let min = Int64.of_int min
  and max = Int64.of_int max in
  let r = Random.int64 (Int64.sub (Int64.succ max) min) in
  Int64.to_int @@ Int64.add min r

let rand_signed ?(max=max_int) () =
  assert (0 <= max) ;
  rand ~min:(~- max) ~max ()

(* Implementation of [range'].
 *
 * We must be careful about overflows. The implementation below performs just
 * one overflow-avoiding test at initialization; absence of overflow is then
 * guaranteed by the loop invariant, so we avoid doing one test per step, and we
 * also reduce the number of branches in the loop (two cases instead of three).
 * In this variant of the code, the helper functions [nonempty_range_up] and
 * [nonempty_range_down] produce non-empty ranges, but we could easily hoist the
 * leading cons-cell outside of these functions if we wanted.
 *
 * The distinction between [nonempty_range_up] and [nonempty_range_down] is
 * there primarily so that we avoid testing the sign of [step] again and again
 * when in the loop. So in the end, we managed to reduce the tests done at each
 * step of the loop to just one comparison.
 *
 * [til = nan] is allowed, to mean “infinity” (i.e [max_int]+1 when [step] > 0,
 * or [min_int]−1 when [step] < 0). For this to work, we need the helper
 * function [nonempty_range_up] to have its upper bound inclusive (because with
 * exclusive bounds, the comparison [from < til] breaks).
 * However [nonempty_range_down] still needs an exclusive bound (the comparison
 * [from >= to_] would break when [to_ = max_int+1 = nan]).
 *)
let rec nonempty_range_up ~step ~from ~prev_to () =
  (*! assert (step > 0) ; !*)
  (* so [to_ := prev_to + step] does not overflow,
   * and neither does [next := from + step] when [from <= prev_to]: *)
  (*! assert (prev_to <= max_int - step) ; !*)
  (* i.e. [from <= to_]: *)
  (*! assert (from <= prev_to + step) ; !*)
  if from <= prev_to
  then Seq.Cons (from, nonempty_range_up ~step ~from:(from+step) ~prev_to)
  else Seq.Cons (from, Seq.empty)
let rec nonempty_range_down ~step ~from ~prev_til () =
  (*! assert (step < 0) ; !*)
  (* so [til := prev_til + step] does not overflow,
   * and neither does [next := from + step] when [from > prev_til]: *)
  (*! assert (prev_til >= nan - step) ; !*)
  (* i.e. [from > til]: *)
  (*! assert (from > prev_til + step) ; !*)
  if from > prev_til
  then Seq.Cons (from, nonempty_range_down ~step ~from:(from+step) ~prev_til)
  else Seq.Cons (from, Seq.empty)
let range' ?(step=1) ?(from=0) ?(til=nan) () =
  assert (step <> nan && step <> 0) ;
  assert (from <> nan) ;
  if step >= 0 && from <= til-1 then
    (* NOTE: in the `else` case, the requested sequence is just a singleton
     * (because [min_int ≤ from ≤ til-1 < min_int+step ≤ from+step]), so instead
     * of calling `nonempty_range_up` we could also return `Seq.return from`: *)
    let prev_to = if til-1 >= min_int + step then til-1 - step else nan in
    nonempty_range_up ~step ~from ~prev_to
  else if step < 0 && from > til then
    (* NOTE: same remark: *)
    let prev_til = if til <= max_int + step then til - step else max_int in
    nonempty_range_down ~step ~from ~prev_til
  else
    Seq.empty
let[@inline] range ~from ~til = range' ~step:1 ~from ~til ()
let[@inline] range_down ~from ~til = range' ~step:~-1 ~from ~til ()
let[@inline] range0 len = range' ~step:1 ~from:0 ~til:len ()
let[@inline] range1 len = range' ~step:1 ~from:1 ~til:(len+1) ()

let ( ~- ) = opp
let ( + ) = add
let ( - ) = sub
let ( * ) = mul
let ( / ) = div_exact
let ( // ) = equo
let ( /% ) = erem
let ( mod ) = erem
let ( ** ) = pow
let ( **. ) = Float.pow

module Unsafe = struct
  let ( +! ) = Stdlib.( + )
  let ( -! ) = Stdlib.( - )
  let ( *! ) = Stdlib.( * )
end



(* Tests. *)
(* FIXME: Use an actual tool for unit tests. *)

let does_overflow f =
  begin match f () with
  | exception Overflow -> true
  | _                  -> false
  end

let () =
  for _ = 1 to 100 do
    let a = rand ~max:(2 * lower_half + 1) () in
    let b = rand ~max:(2 * lower_half + 1) () in
    let ab = unsigned_long_mul a b in
    begin match mul a b with
    | c                  -> assert (ab = { hi = 0 ; lo = c })
    | exception Overflow -> assert (ab.hi <> 0)
    end ;
  done

let () =
  [
    (0, 0); (1, 1); (2, 2); (3, 2); (4, 3); (5, 3); (6, 3); (7, 3);
    (8, 4); (9, 4); (15, 4); (16, 5); (17, 5); (max_int, uint_size);
  ] |>
  List.iter begin fun (x, y) ->
    assert (ilog2sup x = y) ;
  end

let test_table_pow base table =
  assert (table.(Array.length table - 1) = Stdlib.min_int) ;
  assert (does_overflow (fun () -> base *? table.(Array.length table - 2)))

let test_ilog base =
  assert (ilog ~base 0 = -1) ;
  let again = ref true in
  let i = ref 0 in
  while !again do
    begin match pow base !i with
    | exception Overflow ->
        again := false
    | p ->
        assert (ilog ~base (p-1) = !i-1) ;
        assert (ilog ~base p = !i) ;
        assert (ilog ~base (p+1) = if (base,!i) = (2,0) then 1 else !i) ;
        incr i ;
    end
  done ;
  let l = ilog ~base max_int in
  let p = pow base l in
  assert (p <= max_int) ;
  assert (does_overflow (fun () -> p *? base))

let () =
  test_table_pow 3 table_prev_pow3 ;
  test_table_pow 5 table_prev_pow5 ;
  test_table_pow 6 table_prev_pow6 ;
  test_table_pow 7 table_prev_pow6 ;
  (*! test_table_pow 9 table_prev_pow9 ; !*)
  test_table_pow 10 table_prev_pow10 ;
  test_table_pow 60 table_prev_pow60 ;
  for base = 2 to 20 do
    test_ilog base
  done ;
  test_ilog 32 ;
  test_ilog 64 ;
  test_ilog 60 ;
  ()

let () =
  assert (jacobi 2 3 = ~-1) ;
  assert (jacobi 2 9 = 1) ;
  assert (jacobi 21 39 = 0) ;
  assert (jacobi 30 59 = ~-1) ;
  ()

let () =
  let min = min_int in
  let max = max_int in
  let test_range ~step ~from ~til = List.of_seq @@ range' ~step ~from ~til () in
  (* step = 1: *)
  assert (test_range ~step:1 ~from:10 ~til:20
      = [10; 11; 12; 13; 14; 15; 16; 17; 18; 19]) ;
  assert (test_range ~step:1 ~from:10 ~til:11 = [10]) ;
  assert (test_range ~step:1 ~from:10 ~til:10 = []) ;
  assert (test_range ~step:1 ~from:10 ~til:9  = []) ;
  assert (test_range ~step:1 ~from:10 ~til:min = []) ;
  (* step = 3: *)
  assert (test_range ~step:3 ~from:10 ~til:19 = [10; 13; 16]) ;
  assert (test_range ~step:3 ~from:10 ~til:20 = [10; 13; 16; 19]) ;
  assert (test_range ~step:3 ~from:10 ~til:21 = [10; 13; 16; 19]) ;
  assert (test_range ~step:3 ~from:(max-1) ~til:(max-1) = []) ;
  assert (test_range ~step:3 ~from:max ~til:max = []) ;
  assert (test_range ~step:3 ~from:(max-2) ~til:max = [max-2]) ;
  assert (test_range ~step:3 ~from:(max-3) ~til:nan = [max-3; max]) ;
  assert (test_range ~step:3 ~from:min ~til:min = []) ;
  assert (test_range ~step:3 ~from:min ~til:(min+2) = [min]) ;
  assert (test_range ~step:3 ~from:min ~til:(min+3) = [min]) ;
  assert (test_range ~step:3 ~from:min ~til:(min+4) = [min; min+3]) ;
  (* step = -1: *)
  assert (test_range ~step:~-1 ~from:20 ~til:10
      = [20; 19; 18; 17; 16; 15; 14; 13; 12; 11]) ;
  assert (test_range ~step:~-1 ~from:20 ~til:19 = [20]) ;
  assert (test_range ~step:~-1 ~from:20 ~til:20 = []) ;
  assert (test_range ~step:~-1 ~from:20 ~til:21 = []) ;
  assert (test_range ~step:~-1 ~from:20 ~til:max = []) ;
  (* step = -3: *)
  assert (test_range ~step:~-3 ~from:20 ~til:11 = [20; 17; 14]) ;
  assert (test_range ~step:~-3 ~from:20 ~til:10 = [20; 17; 14; 11]) ;
  assert (test_range ~step:~-3 ~from:20 ~til:9  = [20; 17; 14; 11]) ;
  assert (test_range ~step:~-3 ~from:(min+1) ~til:(min+1) = []) ;
  assert (test_range ~step:~-3 ~from:min ~til:min = []) ;
  assert (test_range ~step:~-3 ~from:(min+2) ~til:min = [min+2]) ;
  assert (test_range ~step:~-3 ~from:(min+3) ~til:nan = [min+3; min]) ;
  assert (test_range ~step:~-3 ~from:max ~til:max = []) ;
  assert (test_range ~step:~-3 ~from:max ~til:(max-2) = [max]) ;
  assert (test_range ~step:~-3 ~from:max ~til:(max-3) = [max]) ;
  assert (test_range ~step:~-3 ~from:max ~til:(max-4) = [max; max-3]) ;
  (* large steps: *)
  assert (test_range ~step:max ~from:min ~til:min = []) ;
  assert (test_range ~step:max ~from:min ~til:max = [min; 0]) ;
  assert (test_range ~step:max ~from:min ~til:nan = [min; 0; max]) ;
  assert (test_range ~step:max ~from:(min+1) ~til:nan = [min+1; 1]) ;
  assert (test_range ~step:min ~from:max ~til:max = []) ;
  assert (test_range ~step:min ~from:max ~til:min = [max; 0]) ;
  assert (test_range ~step:min ~from:max ~til:nan = [max; 0; min]) ;
  assert (test_range ~step:min ~from:(max-1) ~til:nan = [max-1; -1]) ;
  ()
