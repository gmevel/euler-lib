(* We use some functions which appeared in the stdlib after 4.07 (specifically,
 * functions in Seq), so we use Stdcompat to get an up-to-date version of the
 * stdlib: *)
open! Stdcompat

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
let sign a =
  (a asr Sys.int_size) lor ((a lor (-a)) lsr (Sys.int_size - 1))
*)
(* Using the standard comparison function, specialized to type `int`, is even
 * faster. Although the doc does not guarantee that `compare` always return
 * −1, 0 or +1, it is enforced in all OCaml versions up to 4.14. This is about
 * 4 times faster than the naive implementation. *)
let sign a =
  (*! assert (a <> nan) ; !*)
  compare a 0

(* This is a free generalization of [abs] and should be very fast (although not
 * benchmarked). *)
let mul_sign s n =
  (*! assert (s <> nan) ; !*)
  (*! assert (n <> nan) ; !*)
  let u = s asr Sys.int_size in
  n lxor u - u

let mul_sign0 s n =
  (*! assert (s <> nan) ; !*)
  (*! assert (n <> nan) ; !*)
  let u = s asr Sys.int_size in
  (n lxor u - u) land ((s lxor (-s)) asr Sys.int_size)

(*
let abs = abs
*)
(* Below is a branchless implementation, which is about 7.5 times faster than
 * the naive implementation. *)
let abs n =
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

let pred a =
  assert (a <> nan) ;
  if a = min_int then
    raise Overflow
  else
    a - 1

let succ a =
  assert (a <> nan) ;
  if a = max_int then
    raise Overflow
  else
    a + 1

let opp = ( ~- )

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

let unsigned_long_add (a : int) (b : int) : long_int =
  assert (a >= 0) ;
  assert (b >= 0) ;
  let s = a + b in
  if s >= 0 then
    { hi = 0 ; lo = s }
  else
    { hi = 1 ; lo = s - max_int - 1 }

(* This implementation of [sum_seq] uses linear space in the worst case, but it
 * consumes the input sequence only once.
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
let sum_seq_oneshot xs =
  let ys = Seq.fold_left (fun ys x -> add_cons x ys) [ 0 ] xs in
  (* Invariant: all summands in [ys] are of the same sign (because consecutive
   * summands of different signs could be summed without overflow). *)
  begin match ys with
  | [ y ] -> y
  | _     -> raise Overflow
      (* If there are several summands remaining, then they are of the same sign
       * and cannot be summed, so the result is an overflow. *)
  end

(* This implementation of [sum_seq] is in constant space, but it consumes the
 * input sequence twice. *)
let rec sum_seq_aux s pos neg =
  if s >= 0 then
    begin match neg () with
    | Seq.Nil            -> Seq.fold_left add s pos
    | Seq.Cons (n, neg') -> assert (n <> nan) ;
                            sum_seq_aux (s + n) pos neg'
    end
  else
    begin match pos () with
    | Seq.Nil            -> Seq.fold_left add s neg
    | Seq.Cons (p, pos') -> sum_seq_aux (s + p) pos' neg
    end
let sum_seq_twoshot xs =
  let (pos, neg) = Seq.partition (fun x -> x >= 0) xs in
  sum_seq_aux 0 pos neg

(* We prefer the constant-space version, because from it we can obtain the
 * behavior of the oneshot version by memoizing the input sequence. *)
let sum_seq = sum_seq_twoshot

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
  sum_seq_twoshot (List.to_seq xs)

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
  if a <= lower_half then begin
    if b <= lower_half then
      a0 * b0
    else begin
      let (bh, bl) = (b lsr uint_half_size, b land lower_half) in
      let a_bl = a*bl in
      let (h, l) = (a_bl lsr uint_half_size, a_bl land lower_half) in
      (* This expression does not overflow (each variable is at most equal to
       * lower_half = 2^{N∕2}−1 where N = uint_size, so that the total is
       * at most equal to lower_half² + lower_half, which is less than 2^N): *)
      let h' = a*bh + h in
      if h' <= lower_half then
        mul_sign (a0 lxor b0) ((h' lsl uint_half_size) lor l)
      else
        raise Overflow
    end
  end
  else begin
    if b <= lower_half then begin
      let (ah, al) = (a lsr uint_half_size, a land lower_half) in
      let al_b = al*b in
      let (h, l) = (al_b lsr uint_half_size, al_b land lower_half) in
      (* This expression does not overflow (same proof): *)
      let h' = ah*b + h in
      if h' <= lower_half then
        mul_sign (a0 lxor b0) ((h' lsl uint_half_size) lor l)
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
  (* FIXME: This does not raise [Overflow] when [a = Stdlib.min_int/2] but
   * rather it returns [nan]; oh well… *)
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

let prod_seq xs =
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
  prod_seq (List.to_seq xs)

let div_exact a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  let q = a / b in
  if a = q * b then
    q
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
  (*! assert (a <> nan) ; !*)
  (a asr 1, a land 1)

let equo2 a =
  (*! assert (a <> nan) ; !*)
  a asr 1

let erem2 a =
  (*! assert (a <> nan) ; !*)
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
  (*! assert (a <> nan) ; !*)
  (*! assert (k <> nan) ; !*)
  Common.pow ~mult:mul ~unit:1

let pow2 k =
  assert (0 <= k) ;
  if k < uint_size then
    1 lsl k
  else
    raise Overflow

let powm1 k =
  (*! assert (k <> nan) ; !*)
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

(* By picking a good initial estimation for the logarithm, our implementation is
 * likely much faster than the naive implementation (not checked thoroughly). *)
let logsup ?(base=10) n =
  assert (2 <= base) ;
  assert (0 <= n) ;
  if n <> 0 then begin
    (* Below is an excellent estimation, which also gives 0 for n=0, but:
    *  (1) I’m not sure whether the estimation is always below the result;
    *  (2) as we use float operations, it needs benchmarking. *)
    (*! let l_est = truncate (log (float (n+1)) /. log (float base)) in !*)
    (* Below is a more conservative under-estimation, proven correct; I could
     * also prove the associated over-estimation, shown below in a comment, and:
     *     l_overest ≤ 2 × l_underest        for any base
     *     l_overest ≤ 3/2 × l_underest + 1  for base ≥ 4
     *     l_overest ≤ 4/3 × l_underest + 1  for base ≥ 8
     *     l_overest ≤ 5/4 × l_underest + 1  for base ≥ 16
     *     …
     * which is not satisfying because a priori we might still do O(l_underest)
     * iterations from the initial estimation; however quick tests with base=3
     * seem to suggest that in practice, we iter much less than that(?). *)
    let l_underest = (log2sup n - 1) / log2sup (base-1) + 1 in
    (*! let l_overest = log2sup (n-1) / (log2sup base - 1) + 1 in !*)
    begin match pow base l_underest with
    | p ->
        (* Divisions are costly, we rather do repeated multiplications than
         * repeated divisions; we still need one division for overflow control,
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

let log2 n =
  log2sup n - 1

let log ?base n =
  logsup ?base n - 1

let is_pow ?(base=10) ?exp n =
  assert (base <> nan) ;
  assert (exp <> Some nan) ;
  assert (n <> nan) ;
  begin match exp with
  | None ->
      if base = 0 then n = 1 || n = 0
      else if base = 1 then n = 1
      else if base = -1 then abs n = 1
      else n <> 0 && pow base (log ~base:(abs base) (abs n)) = n
  | Some exp ->
      (exp >= 0 || abs base = 1) && (try pow base (abs exp) = n with Overflow -> false)
  end

let is_pow2 n =
  n land (n - 1) = 0 && n > 0

(* This function is placed here because it uses [log2sup]. *)
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
    let chunk_size = uint_size - log2sup (b-1) in (* threshold *)
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
        let (q1, r1) = ediv r' b in
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
        let (q1, r1) = ediv r' b in
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
  let[@inline] is_square_mod_wordsz n =
    squares_mod_wordsz land (1 lsl n) <> 0
  in
  let sqrt_max_int = 1 lsl (uint_size / 2) - 1 in
fun ?root n ->
  assert (n <> nan) ;
  assert (root <> Some nan) ;
  begin match root with
  | None   ->  is_square_mod_wordsz n && 0 <= n && let r = isqrt n in r * r = n
  | Some r ->  is_square_mod_wordsz n && abs r <= sqrt_max_int && r * r = n
  end

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

let is_multiple ~of_:a b =
  assert (a <> nan) ;
  assert (b <> nan) ;
  begin try
    b mod a = 0
  with Division_by_zero ->
    b = 0
  end

let is_even a =
  (*! assert (a <> nan) ; !*)
  a land 1 = 0

let is_odd a =
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
let gcd_seq xs =
  let rec gcd_aux d xs =
    begin match xs () with
    | Seq.Nil           -> d
    | Seq.Cons (x, xs') -> let d' = gcd d x in if d' = 1 then 1 else gcd_aux d' xs'
    end
  in
  gcd_aux 0 xs

(* TODO: always return minimal [(u,v)] ? *)
let gcdext a0 b0 =
  (*! assert (a <> nan) ; !*)
  (*! assert (b <> nan) ; !*)
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
      (* FIXME: Avoid overflows in intermediate values for computing the Bézout
       * coefficients (use zarith? prove that somehow we compute the smallest
       * coefficients possible and that there are in fact no overflows?).
       * IDEA: Use [Modular.gcdext]! Assuming [0 ≤ b < a], it gives us the GCD
       * [d] and a coefficient [0 ≤ v < a], defined modulo [a/d], such that:
       *     v·b = d + u·a  for some u
       * Then we can deduce the coefficient [u]:
       *     u = (v·b − d) / a = (v·(b/d) − 1) / (a/d)
       * Since [0 ≤ b < a] and [0 ≤ v], we have:
       *     0 ≤ u < v < a
       * Thus, since [a] fits in a native integer, then so do [u] and [v].
       * Since we know that all possible [v] are defined modulo [a/d], we can
       * even minimize [v], i.e. take the unique [v] in the range [0 ≤ v < a/d].
       * In that case, we get:
       *     0 ≤ u < b/d
       * so we have also minimized [u].
       * The remaining question is how to compute [(v·b − d) / a] without
       * overflowing. *)
      gcdext b (a mod b) x y (u -? q *? x) (v -? q *? y)
    end
  in
  gcdext a0 b0 1 0 0 1

let gcdext_seq xs =
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
         * special value [nan]. *)
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
let lcm_seq xs =
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
  while !m mod d = 0 do
    incr k ;
    m := !m / d ;
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
let valuation_of_2 =
  let values128 = "\007\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\006\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000" in
fun n ->
  (*! assert (n <> nan) ; !*)
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
  (*! assert (n <> nan) ; !*)
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
     * Then, the root’s valuation is at most log7(max_int) = 22 on 64‐bit OCaml.
     *
     * Instead of checking whether [n] is a [k]-th pow for every possible [k] in
     * decreasing order, we will reconstruct [k] from its prime factorization.
     * It is thus enough to consider the prime exponents less than 22.
     *
     * We also keep in mind that [k] must be a divisor of [v235].
     *
     *
     *)
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
          while !v mod p = 0 &&
                (s := kth_root ~k:p !r ; is_kth_pow ~k:p ~root:!s !r) do
            r := !s ;
            k := !k * p ;
            v := !v / p ;
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

let number_of_bits_set n =
  let n = ref n in
  let count = ref 0 in
  while !n <> 0 do
    n := !n land (!n - 1) ;
    incr count ;
  done ;
  !count

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
 * exclusive bounds, the comparison [from < til] breaks). However
 * [nonempty_range_down]
 * still needs an exclusive bound (the comparison [rom >= to_] would break when
 * [to_ = max_int+1 = nan]).
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

module Unsafe = struct
  let ( +! ) = Stdlib.( + )
  let ( -! ) = Stdlib.( - )
  let ( *! ) = Stdlib.( * )
end



(* Tests. *)

let () =
  for _ = 1 to 100 do
    let a = rand () in
    let b = rand () in
    let ab = unsigned_long_mul a b in
    begin match mul a b with
    | c                  -> assert (ab = { hi = 0 ; lo = c })
    | exception Overflow -> assert (ab.hi <> 0)
    end ;
  done

(* FIXME: Use an actual tool for unit tests. *)
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
