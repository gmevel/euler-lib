(******************************************************************************)

(*
 * ABS
 *)

(* current OCaml implementation *)
let abs1 n =
  if n >= 0 then n else -n

(* branchless bitwise *)
(* 7.5 times faster than abs1 *)
(* 10 times faster than abs1 *)
(* 6 times faster than abs1 *)
let abs2 n =
  let u = n asr Sys.int_size in
  n lxor u - u

(******************************************************************************)

(*
 * MIN
 *)

(* current OCaml implementation (polymorphic) *)
(* 2.5 times SLOWER than min1 *)
let min0 a b =
  if a <= b then a else b
let max0 a b =
  if a <= b then b else a

(* specialized to type int *)
let min1 (a : int) (b : int) =
  if a <= b then a else b
let max1 (a : int) (b : int) =
  if a <= b then b else a

(* branchless bitwise, but can overflow *)
(* 6 times faster than min1 *)
let min2 a b =
  let d = b - a in (* can overflow! *)
  a + (d land (d asr Sys.int_size))
let max2 a b =
  let d = b - a in (* can overflow! *)
  b - (d land (d asr Sys.int_size))

(* branchless bitwise, always correct *)
(* 1.7 times faster than min1 *)
(* 1.5 times faster than min1 *)
let min3 a b =
  let d = b - a in (* overflow is correctly dealt with *)
  let s = a lxor b in
  let r = (s land b) lor (lnot s land d) in
  a + (d land (r asr Sys.int_size))
let max3 a b =
  let d = b - a in (* overflow is correctly dealt with *)
  let s = a lxor b in
  let r = (s land b) lor (lnot s land d) in
  b - (d land (r asr Sys.int_size))

(******************************************************************************)

(*
 * SIGN
 *)

(* naive implementation *)
let sign1 a =
  if a = 0      then   0
  else if a > 0 then ~+1
  else               ~-1

(* branchless bitwise *)
(* 3.5 times faster than sign1 *)
let sign2 a =
  (a asr Sys.int_size) lor ((a lor (-a)) lsr (Sys.int_size - 1))

(* branchless bitwise, from Hacker’s Delight (2nd ed, Section 2-8) *)
(* slightly faster than sign1 *)
let sign2b a =
  (a asr (Sys.int_size - 1)) lor ((-a) lsr (Sys.int_size - 1))
  (*! (a asr 62) lor ((-a) lsr 62) !*)

(* using compare specialized to type int, but ill-defined although correct in
 * practice *)
(* 4.3 times faster than sign1 *)
(* 2 times faster than sign2 and sign2b *)
let sign3 a =
  compare a 0

(******************************************************************************)

(*
 * CMP
 *)

(* current OCaml implementation (polymorphic) *)
(* => 5.5 times SLOWER than cmp1 *)
let cmp0 =
  compare

(* specialized to type int *)
(* => the fastest *)
let cmp1 : int -> int -> int =
  compare

(* branchless bitwise *)
(* => 1.5 times SLOWER than cmp1 *)
let cmp2 a b =
  let s = (a lxor b) asr Sys.int_size in
  (s land (a lor 1)) lor (lnot s land (a-b))

(******************************************************************************)

(*
 *  NUMBER_OF_BITS_SET (aka POP_COUNT)
 *)

let impls_number_of_bits_set = ref []
let register (f, desc) =
  impls_number_of_bits_set := !impls_number_of_bits_set @ [ (f, desc) ]

(* [Hacker’s Delight (2nd ed, Fig 5-3); current Euler implementation:]
 * branching: using a loop and a bitwise trick to remove the lowest set bit *)
(* => 7.5 times SLOWER than number_of_bits_set3b for 63-bit integers
 * => 5   times SLOWER than number_of_bits_set3b for 32-bit integers
 * => 3.5 times SLOWER than number_of_bits_set3b for 16-bit integers *)
let number_of_bits_set1 n =
  let n = ref n in
  let count = ref 0 in
  while !n <> 0 do
    n := !n land (!n - 1) ;
    incr count ;
  done ;
  !count

let () = register (number_of_bits_set1, "canceling one bits")

(* [Hacker’s Delight (2nd ed, last variant presented after Fig 5.2)]
 * branchless: parallel divide-and-conquer starting with 4-bit fields *)
(* => 1.3 times SLOWER than number_of_bits_set3b *)
let number_of_bits_set2 x =
  (* FIXME: these constants are for 64-bit OCaml *)
  (* add together the 4 bits of each 4-bit field, in parallel: *)
  let y = (x lsr 1) land 0x7777777777777777 in
  let x = x - y in
  let y = (y lsr 1) land 0x7777777777777777 in
  let x = x - y in
  let y = (y lsr 1) land 0x7777777777777777 in
  let x = x - y in
  (* now add the 4-bit fields pairwise into 8-bit fields, in parallel: *)
  let x = (x + (x lsr 4)) land 0x0F0F0F0F0F0F0F0F in
  (* now add the 8-bit fields pairwise, in parallel: *)
  let x = x + (x lsr 8) in
  (* now add the 16-bit fields pairwise, in parallel: *)
  let x = x + (x lsr 16) in
  (* now add the 32-bit fields pairwise: *)
  let x = x + (x lsr 32) in
  x land 0xFF

let () = register (number_of_bits_set2, "parallel sum with 4-bit fields")

(* [Hacker’s Delight (2nd ed, last variant presented after Fig 5.2)]
 * branchless: parallel divide-and-conquer starting with 4-bit fields
 * + one multiplication trick to add together all the 8-bit fields at once *)
(* => 1.1 times SLOWER than number_of_bits_set3b *)
let number_of_bits_set2b x =
  (* FIXME: these constants are for 64-bit OCaml *)
  (* add together the 4 bits of each 4-bit field, in parallel: *)
  let m = (x lsr 1) land 0x7777777777777777 in
  let x = x - m in
  let m = (m lsr 1) land 0x7777777777777777 in
  let x = x - m in
  let m = (m lsr 1) land 0x7777777777777777 in
  let x = x - m in
  (* now add the 4-bit fields pairwise into 8-bit fields, in parallel: *)
  let x = (x + (x lsr 4)) land 0x0F0F0F0F0F0F0F0F in
  (* now use a multiplication to add together all the 8-bit fields at once: *)
  (x * 0x0101010101010101) lsr 56

let () = register (number_of_bits_set2b, "parallel sum with 4-bit fields + multiplication")

(* [Hacker’s Delight (2nd ed, Fig 5.2)]
 * branchless: parallel divide-and-conquer starting with 2-bit fields
 * + one multiplication trick to add together all the 8-bit fields at once *)
(* => 1.2 times SLOWER than number_of_bits_set3b *)
let number_of_bits_set3 x =
  (* FIXME: these constants are for 64-bit OCaml *)
  (* add the adjacent bits pairwise, in parallel: *)
  let x = x - ((x lsr 1) land 0x5555555555555555) in
  (* now add the 2-bit fields pairwise into 4-bit fields, in parallel: *)
  let x = (x land 0x3333333333333333) + ((x lsr 2) land 0x3333333333333333) in
  (* now add the 4-bit fields pairwise into 8-bit fields, in parallel: *)
  let x = (x + (x lsr 4)) land 0x0F0F0F0F0F0F0F0F in
  (* now add the 8-bit fields pairwise, in parallel: *)
  let x = x + (x lsr 8) in
  (* now add the 16-bit fields pairwise, in parallel: *)
  let x = x + (x lsr 16) in
  (* now add the 32-bit fields pairwise: *)
  let x = x + (x lsr 32) in
  x land 0xFF

let () = register (number_of_bits_set3, "parallel sum with 2-bit fields")

(* [Hacker’s Delight (2nd ed, Fig 5.2 + a later comment)]
 * branchless: parallel divide-and-conquer starting with 2-bit fields *)
(* => the fastest *)
let number_of_bits_set3b x =
  (* FIXME: these constants are for 64-bit OCaml *)
  (* add the adjacent bits pairwise, in parallel: *)
  let x = x - ((x lsr 1) land 0x55555555_55555555) in
  (* now add the 2-bit fields pairwise into 4-bit fields, in parallel: *)
  let x = (x land 0x33333333_33333333) + ((x lsr 2) land 0x33333333_33333333) in
  (* now add the 4-bit fields pairwise into 8-bit fields, in parallel: *)
  let x = (x + (x lsr 4)) land 0x0F0F0F0F_0F0F0F0F in
  (* now use a multiplication to add together all the 8-bit fields at once: *)
  (x * 0x01010101_01010101) lsr 56

let () = register (number_of_bits_set3b, "parallel sum with 2-bit fields + multiplication")

let number_of_bits_set3b_32bit x =
  (* add the adjacent bits pairwise, in parallel: *)
  let x = x - ((x lsr 1) land 0x55555555) in
  (* now add the 2-bit fields pairwise into 4-bit fields, in parallel: *)
  let x = (x land 0x33333333) + ((x lsr 2) land 0x33333333) in
  (* now add the 4-bit fields pairwise into 8-bit fields, in parallel: *)
  let x = (x + (x lsr 4)) land 0x0F0F0F0F in
  (* now use a multiplication to add together all the 8-bit fields at once: *)
  ((x * 0x01010101) lsr 24) land 63

(* just to verify functional correctness: *)
let impls_number_of_bits_set_32bit =
  !impls_number_of_bits_set @ [ (number_of_bits_set3b_32bit, "32-bit version") ]

(* the best according to benchmarks: *)
let number_of_bits_set = number_of_bits_set3b

(******************************************************************************)

(*
 * ILOG2SUP, 8 BITS
 *)

let impls_ilog2sup8bit = ref []
let register (f, desc) =
  impls_ilog2sup8bit := !impls_ilog2sup8bit @ [ (f, desc) ]

(* linear search for finding the highest bit set *)
let ilog2sup8bit_1 n =
  (*! assert (0 <= n && n < 256) ; !*)
       if n >= 128 then 8
  else if n >=  64 then 7
  else if n >=  32 then 6
  else if n >=  16 then 5
  else if n >=   8 then 4
  else if n >=   4 then 3
  else if n >=   2 then 2
  else                  n

let () = register (ilog2sup8bit_1, "linear search for highest bit")

(* dichotomy for finding the highest bit set (surprisingly, this is faster) *)
(* 1.18 times faster than ilog2sub_8bit1 *)
let ilog2sup8bit_2 n =
  (*! assert (0 <= n && n < 256) ; !*)
  if n >= 16 then begin
    if n >= 64 then begin
      if n >= 128 then 8 else 7
    end else begin
      if n >= 32 then 6 else 5
    end
  end else begin
    if n >= 4 then begin
      if n >= 8 then 4 else 3
    end else begin
      if n >= 2 then 2 else n
    end
  end

let () = register (ilog2sup8bit_2, "binary search for highest bit")

(* dichotomy for finding the highest bit set, with less branching *)
(* 2 times faster than ilog2sub_8bit1 *)
let ilog2sup8bit_2b n =
  (*! assert (0 <= n && n < 256) ; !*)
  if n >= 16 then begin
    if n >= 64 then begin
      (*! if n >= 128 then 8 else 7 !*)
      7 + (n lsr 7)
    end else begin
      (*! if n >= 32 then 6 else 5 !*)
      5 + (n lsr 5)
    end
  end else begin
    if n >= 4 then begin
      (*! if n >= 8 then 4 else 3 !*)
      3 + (n lsr 3)
    end else begin
      (*! if n >= 2 then 2 else n !*)
      min1 2 n
    end
  end

let () = register (ilog2sup8bit_2b, "binary search, less branching")

(* branchless bitwise *)
(* 3 times faster than ilog2sub_8bit1 *)
let ilog2sup8bit_3 =
  let ilog2sup_4bit =
    "\000\001\002\002\003\003\003\003\004\004\004\004\004\004\004\004"
  in
fun n ->
  (*! assert (0 <= n && n < 256) ; !*)
  let h = n lsr 4 and l = n land 15 in
  if h > 0 then
    4 + Char.code (String.unsafe_get ilog2sup_4bit h)
  else
    Char.code (String.unsafe_get ilog2sup_4bit l)

let () = register (ilog2sup8bit_3, "precomputed 4 bits")

(* branchless bitwise *)
(* 3 times faster than ilog2sub_8bit1 *)
let ilog2sup8bit_4 =
  let nb_bits_set_4bit =
    "\000\001\001\002\001\002\002\003\001\002\002\003\002\003\003\004"
  in
fun n ->
  (*! assert (0 <= n && n < 256) ; !*)
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let h = n lsr 4 and l = n land 15 in
  Char.code (String.unsafe_get nb_bits_set_4bit h)
  + Char.code (String.unsafe_get nb_bits_set_4bit (h lor l))

let () = register (ilog2sup8bit_4, "branchless + precomputed 4 bits")

(* precomputed *)
(* 16 times faster than ilog2sub_8bit1 *)
let ilog2sup8bit_5 =
  let ilog2sup8bit_table =
    "\000\001\002\002\003\003\003\003\004\004\004\004\004\004\004\004\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b"
  in
fun n ->
  (*! assert (0 <= n && n < 256) ; !*)
  Char.code @@ String.unsafe_get ilog2sup8bit_table n

let () = register (ilog2sup8bit_5, "precomputed")

(******************************************************************************)

(*
 * ILOG2SUP, 62 BITS
 *)

let impls_ilog2sup62bit = ref []
let register (f, desc) =
  impls_ilog2sup62bit := !impls_ilog2sup62bit @ [ (f, desc) ]

(* float *)
(* => 5 times SLOWER than ilog2sup62bit_3 *)
let ilog2sup62bit_1 n =
  (*! assert (0 <= n) ; !*)
  if n <= (1 lsl 53) - 1 then
    snd@@frexp (float n)
  else
    54 + (snd@@frexp (float (n lsr 54)))

let () = register (ilog2sup62bit_1, "float")

(* [Hacker’s Delight (2nd ed, Fig 5-12)]
 * branching: dichotomy to find the leading one *)
(* => 3 times SLOWER than ilog2sup62bit_3 *)
let ilog2sup62bit_2 n =
  (*! assert (0 <= n) ; !*)
  let k = ref 0 in
  let x = ref n in
  (* 31 instead of 32, because our integers have 31 or 63 bits: *)
  let y = !x lsr 31 in if y <> 0 then (k := !k + 31 ; x := y) ;
  let y = !x lsr 16 in if y <> 0 then (k := !k + 16 ; x := y) ;
  let y = !x lsr  8 in if y <> 0 then (k := !k +  8 ; x := y) ;
  let y = !x lsr  4 in if y <> 0 then (k := !k +  4 ; x := y) ;
  let y = !x lsr  2 in if y <> 0 then (k := !k +  2 ; x := y) ;
  (*! let y = !x lsr  1 in if y <> 0 then (k := !k +  1 ; x := y) ; !*)
  (*! !k + !x (* here x < 2 *) !*)
  (* the last two lines can be slightly optimized: *)
  if !x lsr 1 <> 0 then !k + 2 else !k + !x

let () = register (ilog2sup62bit_2, "binary search")

(* [Hacker’s Delight (2nd ed, Fig 5-12 + comment below)]
 * branching: dichotomy to find the leading one + precomputed for the highest 8 bits *)
(* => the fastest *)
let ilog2sup62bit_3 n =
  (*! assert (0 <= n) ; !*)
  let k = ref 0 in
  let n = ref n in
  (* 31 instead of 32, because our integers have 31 or 63 bits: *)
  let y = !n lsr 31 in if y <> 0 then (k := !k + 31 ; n := y) ;
  let y = !n lsr 16 in if y <> 0 then (k := !k + 16 ; n := y) ;
  let y = !n lsr  8 in if y <> 0 then (k := !k +  8 ; n := y) ;
  !k + (ilog2sup8bit_5[@inlined]) !n

let () = register (ilog2sup62bit_3, "binary search + precomputed highest 8 bits")

(* [Hacker’s Delight (2nd ed, Fig 5-16)]
 * branchless: using number_of_bits_set *)
(* => 2 times SLOWER than ilog2sup62bit_3 *)
let ilog2sup62bit_4 n =
  (*! assert (0 <= n) ; !*)
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let n = n lor (n lsr 4) in
  let n = n lor (n lsr 8) in
  let n = n lor (n lsr 16) in
  let n = n lor (n lsr 32) in
  (number_of_bits_set[@inlined]) n

let () = register (ilog2sup62bit_4, "branchless with number_of_bit_set")

(* [Hacker’s Delight (2nd ed, Fig 5-16)]
 * branchless: using hashing and precomputed values for powers of 2 *)
(* => 8 times SLOWER than ilog2sup62bit_3 *)
let hashtbl_ilog2sup_of_pow2m1 =
  let ht = Hashtbl.create ~random:false Sys.int_size in
  for i = 0 to Sys.int_size - 1 do
    let p2m1 = (1 lsl i) - 1 in
    Hashtbl.add ht p2m1 i
  done ;
  ht
let ilog2sup62bit_5 n =
  (*! assert (0 <= n) ; !*)
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let n = n lor (n lsr 4) in
  let n = n lor (n lsr 8) in
  let n = n lor (n lsr 16) in
  let n = n lor (n lsr 32) in
  Hashtbl.find hashtbl_ilog2sup_of_pow2m1 n

(* DISABLED *)
(*! let () = register (ilog2sup62bit_5, "branchless with precomputed pow2m1, hashtbl") !*)

(* https://stackoverflow.com/questions/11376288/fast-computing-of-log2-for-64-bit-integers
 * http://graphics.stanford.edu/~seander/bithacks.html#IntegerLogDeBruijn
 * https://en.wikipedia.org/wiki/De_Bruijn_sequence *)

let magic0 = 571347909858961602
let magic1 = 0x03f6eaf2cd271461

let[@inline] hash_64bit ~magic n =
  Int64.(to_int @@ shift_right_logical (mul n (of_int magic)) 58)
let[@inline] hash_63bit ~magic n =
  (n * magic) lsr 57
let[@inline] hash_32bit ~magic n =
  Int32.(to_int @@ shift_right_logical (mul n (of_int magic)) 27)
let[@inline] hash_31bit ~magic n =
  ((n * magic) lsr 26) land 31

let magic1table_ilog2_of_pow2 =
  "\063\000\058\001\059\047\053\002\060\039\048\027\054\033\042\003\061\051\037\040\049\018\028\020\055\030\034\011\043\014\022\004\062\057\046\052\038\026\032\041\050\036\017\019\029\010\013\021\056\045\025\031\035\016\009\012\044\024\015\008\023\007\006\005"

let magictable_ilog2sup_of_pow2m1 =
  (* the first value is either 0 or 58 *)
  "\058\001\059\047\053\002\060\039\048\027\054\033\042\003\061\051\037\040\049\018\028\020\055\030\034\011\043\014\022\004\062\057\046\052\038\026\032\041\050\036\017\019\029\010\013\021\056\045\025\031\035\016\009\012\044\024\015\008\023\007\006\005\063\255"

(* => 1.25 times SLOWER than ilog2sup62bit_3 (so almost as good) *)
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

let () = register (ilog2sup62bit_6, "branchless with precomputed pow2, magic table")

(* => 1.4 times SLOWER than ilog2sup62bit_3 *)
let ilog2sup62bit_6b n =
  (*! assert (0 <= n) ; !*)
  (* n = 0 is not supported by our magic hash (2^0-1 and 2^58-1 collide), so
   * there must be special treatment for it; we avoid branching with a trick. *)
  let n = n lor (n lsr 1) in
  let n = n lor (n lsr 2) in
  let n = n lor (n lsr 4) in
  let n = n lor (n lsr 8) in
  let n = n lor (n lsr 16) in
  let n = n lor (n lsr 32) in
  (*! Array.unsafe_get magic1table_ilog2sup_of_pow2m1 (hash_63bit ~magic:magic1 n) !*)
  Char.code (String.unsafe_get magictable_ilog2sup_of_pow2m1 (hash_63bit ~magic:magic1 n))
    land n (* returns 0 for n = 0, doesn’t change the result otherwise *)

let () = register (ilog2sup62bit_6b, "branchless with precomputed pow2m1, magic table")

(******************************************************************************)

(*
 *  looking for magic numbers...
 *)

let mymagic0 = 0b0000001000011000101000111001001011001101001111010101110110111111
let mymagic1 = 0b0000001111110111100111010111000110110100110010110000101010001001
let my63magic0 = 0b000000100001100010100011100100101100110100111101010111011011111
let my63magic1 = 0b000000111110111100111010111000110110100110010110000101010001001
  (* ^ took a long time to find that one *)
let mymagic32 = 0x07DCD629

(* this function checks whether the given "magic" number indeed produces:
 *  (1) a distinct hash for every 64-bit number of the form 2^i with 0 ≤ i ≤ 63;
 *  (2) a distinct hash for every 64-bit number of the form 2^i − 1 with 0 ≤ i ≤ 63.
 * It also checks the same conditions fo 63-bit numbers, with 0 ≤ i ≤ 63.
 *)
let test_magic64 ~desc ~magic =
  let table = Array.make 64 (-1) in
  (* 64-bit magic *)
  let tableA = Array.copy table in
  let tableB = Array.copy table in
  Printf.eprintf "testing magicality of 0x%016X (%s)\n%!" magic desc ;
  for i = 0 to 63 do
    let hA = hash_64bit ~magic Int64.(shift_left one i) in
    let hB = hash_64bit ~magic Int64.(sub (shift_left one i) one) in
    let yA = tableA.(hA) in
    let yB = tableB.(hB) in
    if yA < 0 then tableA.(hA) <- i
    else Printf.eprintf "magic = 0x%016X:  64b collision 2^%i | 2^%i -> hash = %i\n%!" magic yA i hA ;
    if yB < 0 then tableB.(hB) <- i
    else Printf.eprintf "magic = 0x%016X:  64b collision 2^%i - 1 | 2^%i - 1 -> hash = %i\n%!" magic yB i hB ;
  done ;
  (* 63-bit magic *)
  assert (Sys.int_size = 63) ;
  let tableA = Array.copy table in
  let tableB = Array.copy table in
  for i = 0 to 63 do
    let hA = hash_63bit ~magic (1 lsl i) in
    let hB = hash_63bit ~magic ((1 lsl i) - 1) in
    let yA = tableA.(hA) in
    let yB = tableB.(hB) in
    if yA < 0 then tableA.(hA) <- i
    else Printf.eprintf "magic = 0x%016X:  63b collision 2^%i | 2^%i -> hash = %i\n%!" magic yA i hA ;
    if yB < 0 then tableB.(hB) <- i
    else Printf.eprintf "magic = 0x%016X:  63b collision 2^%i - 1 | 2^%i - 1 -> hash = %i\n%!" magic yB i hB ;
  done

(* this function checks whether the given "magic" number indeed produces:
 *  (1) a distinct hash for every 32-bit number of the form 2^i with 0 ≤ i ≤ 31;
 *  (2) a distinct hash for every 32-bit number of the form 2^i − 1 with 0 ≤ i ≤ 31.
 * It also checks the same conditions for 31-bit numbers, with 0 ≤ i ≤ 31.
 *)
let test_magic32 ~desc ~magic =
  let table = Array.make 32 (-1) in
  (* 32-bit magic *)
  let tableA = Array.copy table in
  let tableB = Array.copy table in
  Printf.eprintf "testing magicality of 0x%08X (%s)\n%!" magic desc ;
  for i = 0 to 31 do
    let hA = hash_32bit ~magic Int32.(shift_left one i) in
    let hB = hash_32bit ~magic Int32.(sub (shift_left one i) one) in
    let yA = tableA.(hA) in
    let yB = tableB.(hB) in
    if yA < 0 then tableA.(hA) <- i
    else Printf.eprintf "magic = 0x%08X:  32b collision 2^%i | 2^%i -> hash = %i\n%!" magic yA i hA ;
    if yB < 0 then tableB.(hB) <- i
    else Printf.eprintf "magic = 0x%08X:  32b collision 2^%i - 1 | 2^%i - 1 -> hash = %i\n%!" magic yB i hB ;
  done ;
  (* 31-bit magic *)
  let tableA = Array.copy table in
  let tableB = Array.copy table in
  for i = 0 to 31 do
    let hA = hash_31bit ~magic (1 lsl i) in
    let hB = hash_31bit ~magic ((1 lsl i) - 1) in
    let yA = tableA.(hA) in
    let yB = tableB.(hB) in
    if yA < 0 then tableA.(hA) <- i
    else Printf.eprintf "magic = 0x%08X:  31b collision 2^%i | 2^%i -> hash = %i\n%!" magic yA i hA ;
    if yB < 0 then tableB.(hB) <- i
    else Printf.eprintf "magic = 0x%08X:  31b collision 2^%i - 1 | 2^%i - 1 -> hash = %i\n%!" magic yB i hB ;
  done

let nope () =
  test_magic64 ~desc:"SO 64-bit" ~magic:magic0 ;
  test_magic64 ~desc:"SO 64-bit, halved" ~magic:magic1 ;
  (*! is_magic ~desc:"my 64-bit magic, prefer 0" ~magic:mymagic0 ; !*)
  test_magic64 ~desc:"my 64-bit magic, prefer 1" ~magic:mymagic1 ;
  (*! is_magic ~desc:"my 63-bit magic, prefer 0" ~magic:my63magic0 ; !*)
  test_magic64 ~desc:"my 63-bit magic, prefer 1" ~magic:my63magic1 ;
  (*! test_magic32 ~desc:"SO 64-bit, halved" ~magic:magic1 ; !*)
  test_magic32 ~desc:"Bit Twiddling Hacks" ~magic:0x077CB531 ;
  test_magic32 ~desc:"my 32-bit magic, prefer 1" ~magic:mymagic32 ;
  ()

let generate_magic_table_63bit ~magic f =
  let table = Array.make 64 (-1) in
  assert (Sys.int_size = 63) ;
  for i = 0 to 63 do
    let h = hash_63bit ~magic (f i) in
    if table.(h) < 0 then
      table.(h) <- i
    else
      Printf.eprintf "WARNING: 63b collision %i | %i -> hash = %i\n%!" table.(h) i h ;
  done ;
  (* generate an OCaml array syntax: *)
  Printf.printf "  [|" ;
  for h = 0 to Array.length table - 1 do
    if h mod 8 = 0 then
      Printf.printf "\n   " ;
    Printf.printf " %2i;" table.(h) ;
  done ;
  Printf.printf "\n  |]\n%!" ;
  (* generate an OCaml string syntax: *)
  Printf.printf "  \"" ;
  for h = 0 to Array.length table - 1 do
    Printf.printf "\\%03i" (if table.(h) >= 0 then table.(h) else 255) ;
  done ;
  Printf.printf "\"\n%!" ;
  ()

let generate_magic_table_31bit ~magic f =
  let table = Array.make 32 (-1) in
  for i = 0 to 31 do
    let h = hash_31bit ~magic (f i) in
    if table.(h) < 0 then
      table.(h) <- i
    else
      Printf.eprintf "WARNING: 31b collision %i | %i -> hash = %i\n%!" table.(h) i h ;
  done ;
  (* generate an OCaml array syntax: *)
  Printf.printf "  [|" ;
  for h = 0 to Array.length table - 1 do
    if h mod 8 = 0 then
      Printf.printf "\n   " ;
    Printf.printf " %2i;" table.(h) ;
  done ;
  Printf.printf "\n  |]\n%!" ;
  (* generate an OCaml string syntax: *)
  Printf.printf "  \"" ;
  for h = 0 to Array.length table - 1 do
    Printf.printf "\\%03i" (if table.(h) >= 0 then table.(h) else 255) ;
  done ;
  Printf.printf "\"\n%!" ;
  ()

let nope () =
  generate_magic_table_63bit ~magic:magic1 (fun i -> (1 lsl i)) ;
  generate_magic_table_63bit ~magic:magic1 (fun i -> (1 lsl i) - 1) ;
  generate_magic_table_31bit ~magic:mymagic32 (fun i -> (1 lsl i)) ;
  ()

(******************************************************************************)

(*
 * VAL2
 *)

let impls_val2 = ref []
let register (f, desc) =
  impls_val2 := !impls_val2 @ [ (f, desc) ]

(* naive *)
let val2_1 n =
  (*! assert (n <> 0) ; !*)
  let k = ref 0 in
  let m = ref n in
  while !m land 1 = 0 do
    incr k ;
    m := !m asr 1 ;
  done ;
  (!k, !m)

(* DISABLED *)
(*! let () = register (val2_1, "naive") !*)

(* constant-time using conversion to float *)
(* 2 times SLOWER than val2_1 *)
(* The following implementation is constant‐time. However, benchmarking shows
 * that it only becomes faster than the implementation above when the valuation
 * is at least 14, which is very unlikely. With random integers, it is about
 * twice slower. *)
let val2_2 n =
  (*! assert (n <> 0) ;                  (*    n = 0b ???????10000 *) !*)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = (* we convert to float and get the exponent *)
    if bits land (1 lsl 53) = 0 then
      snd@@frexp (float bits)
    else
      54 + (snd@@frexp (float (bits lsr 54)))
  in
  (k, n asr k)

(* DISABLED *)
(*! let () = register (val2_2, "float") !*)

(* reading 4 bits by 4 bits *)
(* XXX times faster than val2_1 *)
let val2_3 =
  let values8 = "\003\000\001\000\002\000\001\000" in
fun n ->
  (*! assert (n <> 0) ; !*)
  let k = ref 0 in
  let m = ref n in
  while !m land 15 = 0 do
    k := !k + 4 ;
    m := !m asr 4 ;
  done ;
  let k = !k + (Char.code @@ String.unsafe_get values8 (!m land 7)) in
  (k, n asr k)

let () = register (val2_3, "4-bit chunks + precomputed highest 4 bits")

(* reading 8 bits by 8 bits *)
(* XXX times faster than val2_1 *)
let val2_3b =
  let values8 = "\003\000\001\000\002\000\001\000" in
fun n ->
  (*! assert (n <> 0) ; !*)
  let k = ref 0 in
  let m = ref n in
  while !m land 255 = 0 do
    k := !k + 8 ;
    m := !m asr 8 ;
  done ;
  if !m land 15 = 0 then begin
    k := !k + 4 ;
    m := !m asr 4 ;
  end ;
  let k = !k + (Char.code @@ String.unsafe_get values8 (!m land 7)) in
  (k, n asr k)

let () = register (val2_3b, "8-bit chunks + precomputed highest 4 bits")

(* reading 16 bits by 16 bits *)
(* XXX times faster than val2_1 *)
let val2_3c =
  let values8 = "\003\000\001\000\002\000\001\000" in
fun n ->
  (*! assert (n <> 0) ; !*)
  let k = ref 0 in
  let m = ref n in
  while !m land 65535 = 0 do
    k := !k + 16 ;
    m := !m asr 16 ;
  done ;
  if !m land 255 = 0 then begin
    k := !k + 8 ;
    m := !m asr 8 ;
  end ;
  if !m land 15 = 0 then begin
    k := !k + 4 ;
    m := !m asr 4 ;
  end ;
  let k = !k + (Char.code @@ String.unsafe_get values8 (!m land 7)) in
  (k, n asr k)

let () = register (val2_3c, "16-bit chunks + precomputed highest 4 bits")

(* reading 8 bits by 8 bits *)
(* 3.3 times faster than val2_1 *)
let val2_3d =
  let values128 = "\007\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\006\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000" in
fun n ->
  (*! assert (n <> 0) ; !*)
  let k = ref 0 in
  let m = ref n in
  while !m land 255 = 0 do
    k := !k + 8 ;
    m := !m asr 8 ;
  done ;
  let k = !k + (Char.code @@ String.unsafe_get values128 (!m land 127)) in
  (k, n asr k)

let () = register (val2_3d, "8-bit chunks + precomputed highest 8 bits")

let rec generate_dichotomy out ?(indent=0) len off =
  let make_indent indent =
    String.make (indent*2) ' '
  in
  assert (len >= 2 && len land 1 = 0) ;
  let len' = len / 2 in
  let off' = off + len' in
  if len = 2 then begin
    Printf.fprintf out "%s%u - ((n asr %u) land 1)\n" (make_indent indent) off' off ;
    assert false
  end
  else if len = 4 then begin
    Printf.fprintf out "%s%u + values8.((n asr %u) land 7)\n" (make_indent indent) off off ;
    assert false
  end
  else if len = 8 then begin
    Printf.fprintf out "%s%u + (Char.code @@ String.unsafe_get values128 ((n asr %u) land 127))\n" (make_indent indent) off off ;
  end
  else begin
    Printf.fprintf out "%sif n land %u <> 0 then\n" (make_indent indent) (1 lsl off' - 1) ;
    generate_dichotomy out ~indent:(indent+1) len' off ;
    Printf.fprintf out "%selse\n" (make_indent indent) ;
    generate_dichotomy out ~indent:(indent+1) len' off' ;
  end

(*! let () = generate_dichotomy stdout ~indent:2 64 0 !*)

(* dichotomy for finding the lowest bit set *)
(* 1.75 times faster than val2_1 *)
let val2_4 n =
  (*! assert (n <> 0) ; !*)
  let k =
    if n land 4294967295 <> 0 then
      if n land 65535 <> 0 then
        if n land 255 <> 0 then
          if n land 15 <> 0 then
            if n land 3 <> 0 then
              1 - (n land 1)
            else
              3 - ((n asr 2) land 1)
          else
            if n land 63 <> 0 then
              5 - ((n asr 4) land 1)
            else
              7 - ((n asr 6) land 1)
        else
          if n land 4095 <> 0 then
            if n land 1023 <> 0 then
              9 - ((n asr 8) land 1)
            else
              11 - ((n asr 10) land 1)
          else
            if n land 16383 <> 0 then
              13 - ((n asr 12) land 1)
            else
              15 - ((n asr 14) land 1)
      else
        if n land 16777215 <> 0 then
          if n land 1048575 <> 0 then
            if n land 262143 <> 0 then
              17 - ((n asr 16) land 1)
            else
              19 - ((n asr 18) land 1)
          else
            if n land 4194303 <> 0 then
              21 - ((n asr 20) land 1)
            else
              23 - ((n asr 22) land 1)
        else
          if n land 268435455 <> 0 then
            if n land 67108863 <> 0 then
              25 - ((n asr 24) land 1)
            else
              27 - ((n asr 26) land 1)
          else
            if n land 1073741823 <> 0 then
              29 - ((n asr 28) land 1)
            else
              31 - ((n asr 30) land 1)
    else
      if n land 281474976710655 <> 0 then
        if n land 1099511627775 <> 0 then
          if n land 68719476735 <> 0 then
            if n land 17179869183 <> 0 then
              33 - ((n asr 32) land 1)
            else
              35 - ((n asr 34) land 1)
          else
            if n land 274877906943 <> 0 then
              37 - ((n asr 36) land 1)
            else
              39 - ((n asr 38) land 1)
        else
          if n land 17592186044415 <> 0 then
            if n land 4398046511103 <> 0 then
              41 - ((n asr 40) land 1)
            else
              43 - ((n asr 42) land 1)
          else
            if n land 70368744177663 <> 0 then
              45 - ((n asr 44) land 1)
            else
              47 - ((n asr 46) land 1)
      else
        if n land 72057594037927935 <> 0 then
          if n land 4503599627370495 <> 0 then
            if n land 1125899906842623 <> 0 then
              49 - ((n asr 48) land 1)
            else
              51 - ((n asr 50) land 1)
          else
            if n land 18014398509481983 <> 0 then
              53 - ((n asr 52) land 1)
            else
              55 - ((n asr 54) land 1)
        else
          if n land 1152921504606846975 <> 0 then
            if n land 288230376151711743 <> 0 then
              57 - ((n asr 56) land 1)
            else
              59 - ((n asr 58) land 1)
          else
            if n land 4611686018427387903 <> 0 then
              61 - ((n asr 60) land 1)
            else
              63 - ((n asr 62) land 1)
  in
  (k, n asr k)

(* DISABLED *)
(*! let () = register (val2_4, "binary search for lowest bit") !*)

(* dichotomy for finding the lowest bit set, using precomputing to spare branches *)
(* 3 times faster than val2_1 *)
let val2_5 =
  let values128 = "\007\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\006\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000" in
fun n ->
  (*! assert (n <> 0) ; !*)
  let k =
    if n land 4294967295 <> 0 then
      if n land 65535 <> 0 then
        if n land 255 <> 0 then
          0 + (Char.code @@ String.unsafe_get values128 ((n asr 0) land 127))
        else
          8 + (Char.code @@ String.unsafe_get values128 ((n asr 8) land 127))
      else
        if n land 16777215 <> 0 then
          16 + (Char.code @@ String.unsafe_get values128 ((n asr 16) land 127))
        else
          24 + (Char.code @@ String.unsafe_get values128 ((n asr 24) land 127))
    else
      if n land 281474976710655 <> 0 then
        if n land 1099511627775 <> 0 then
          32 + (Char.code @@ String.unsafe_get values128 ((n asr 32) land 127))
        else
          40 + (Char.code @@ String.unsafe_get values128 ((n asr 40) land 127))
      else
        if n land 72057594037927935 <> 0 then
          48 + (Char.code @@ String.unsafe_get values128 ((n asr 48) land 127))
        else
          56 + (Char.code @@ String.unsafe_get values128 ((n asr 56) land 127))
  in
  (k, n asr k)

let () = register (val2_5, "binary search + precomputed highest 8 bits")

(* branchless, using number_of_bits_set *)
let val2_6 n =
  (*! assert (n <> 0) ;              (*    n = 0b ???????10000 *) !*)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = (number_of_bits_set[@inlined]) bits in
  (k, n asr k)

let () = register (val2_6, "branchless with number_of_bits_set")

(* branchless, using ilog2sup *)
let val2_7 n =
  (*! assert (n <> 0) ;              (*    n = 0b ???????10000 *) !*)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = (ilog2sup62bit_1[@inlined]) bits in
  (k, n asr k)

let () = register (val2_7, "branchless with ilog2sup (float)")

(* branchless, using ilog2sup *)
let val2_7b n =
  (*! assert (n <> 0) ;              (*    n = 0b ???????10000 *) !*)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = (ilog2sup62bit_3[@inlined]) bits in
  (k, n asr k)

let () = register (val2_7b, "with ilog2sup (binary search + precomputed)")

(* branchless, using ilog2sup *)
let val2_7c n =
  (*! assert (n <> 0) ;              (*    n = 0b ???????10000 *) !*)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = (ilog2sup62bit_6[@inlined]) bits in
  (k, n asr k)

let () = register (val2_7c, "branchless with ilog2sup (magic pow2)")

(* branchless, using hashing and precomputed values for powers of 2 minus 1 *)
let val2_8 n =
  (*! assert (n <> 0) ;              (*    n = 0b ???????10000 *) !*)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = Hashtbl.find hashtbl_ilog2sup_of_pow2m1 bits in
  (k, n asr k)

(* DISABLED *)
(*! let () = register (val2_8, "branchless with precomputed pow2m1, hashtbl") !*)

(* branchless, using magic number and precomputed values for powers of 2 *)
let val2_9 n =
  (*! assert (n <> 0) ;              (*    n = 0b ???????10000 *) !*)
  let hbit = n land (-n) in          (* hbit = 0b 000000010000 *)
  let k = Char.code (String.unsafe_get magic1table_ilog2_of_pow2 (hash_63bit ~magic:magic1 hbit)) in
  (k, n asr k)

let () = register (val2_9, "branchless with precomputed pow2, magic table")

(* branchless, using magic number and precomputed values for powers of 2 minus 1 *)
let val2_10 n =
  (*! assert (n <> 0) ;              (*    n = 0b ???????10000 *) !*)
  let bits = (n lxor (n-1)) lsr 1 in (* bits = 0b 000000001111 *)
  let k = Char.code (String.unsafe_get magictable_ilog2sup_of_pow2m1 (hash_63bit ~magic:magic1 bits)) land bits in
  (k, n asr k)

let () = register (val2_10, "branchless with precomputed pow2m1, magic table")

(******************************************************************************)
(******************************************************************************)
(******************************************************************************)

(*
 * TESTS
 *)

let timed ?(verbose=true) ?msg f x =
  if verbose then
    begin match msg with
    | None -> ()
    | Some msg -> Printf.printf "%s %!" msg
    end ;
  Gc.compact () ;
  let t0 = Sys.time () in
  let y = f x in
  let d = Sys.time () -. t0 in
  if verbose then
    Printf.printf "--- %.3g s\n%!" d ;
  (d, y)

let test name impls ?(expected=[]) ~rand rounds ~pp =
  Gc.compact () ;
  (* functional test: *)
  Printf.printf "%s: functional test...\n%!" name ;
  impls |> List.iter begin fun (f, _desc) ->
    expected |> List.iter begin fun (x, y0) ->
      let y = f x in
      assert (y0 = y
        || () = Printf.eprintf "%s [%s] %i = %a ≠ %a\n%!" name _desc x pp y pp y0)
    end
  end ;
  let data = Array.init rounds (fun _ -> rand ()) in
  let (reference_impl, _) = List.hd impls in
  for i = 0 to rounds-1 do
    let x = data.(i) in
    let y0 = reference_impl x in
    impls |> List.iter begin fun (f, _desc) ->
      let y = f x in
      assert (y0 = y
        || () = Printf.eprintf "%s [%s] %i = %a ≠ %a\n%!" name _desc x pp y pp y0)
    end
  done ;
  (* performance test: *)
  let timings =
    impls |> List.map begin fun (f, desc) ->
      timed ~verbose:false ~msg:(Printf.sprintf "%s [%-50s]" name desc) begin fun () ->
        for i = 0 to rounds-1 do
          let _y = f data.(i) in ()
        done
      end () |> fst
    end
  in
  let min_timing = List.fold_left min infinity timings in
  List.combine impls timings |>
  List.iter begin fun ((f, desc), timing) ->
    let f = timing /. min_timing in
    Printf.printf "%s [%-50s] --- rel = %.3g\n" name desc f ;
  end ;
  Printf.printf "%!"


let pp_int out x = Printf.fprintf out "%i" x
let pp_pair out (x,y) = Printf.fprintf out "(%i, %i)" x y

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

let subtraction_overflows b a =
  (b >= 0 && a < 0 && b-a < 0) || (b < 0 && a >= 0 && b-a >= 0)

let test_simple_functions rounds =
  Gc.compact () ;
  let data = Array.init rounds (fun _ -> Euler.Arith.rand ~min:Euler.Arith.min_int ~max:max_int ()) in
  for i = 0 to rounds-2 do
    let x1 = data.(i) in
    let x2 = data.(i+1) in
    let y1 = abs1 x1 in
    let y2 = abs2 x1 in
    assert (y1 = y2) ;
    let y0 = min0 x1 x2 in
    let y1 = min1 x1 x2 in
    let y2 = min2 x1 x2 in
    let y3 = min3 x1 x2 in
    assert (y1 = y0) ;
    assert (y1 = y2 || subtraction_overflows x2 x1) ;
    assert (y1 = y3) ;
    let y0 = max0 x1 x2 in
    let y1 = max1 x1 x2 in
    let y2 = max2 x1 x2 in
    let y3 = max3 x1 x2 in
    assert (y1 = y0) ;
    assert (y1 = y2 || subtraction_overflows x2 x1) ;
    assert (y1 = y3) ;
    let y1 = sign1 x1 in
    let y2 = sign2 x1 in
    let y2b = sign2b x1 in
    let y3 = sign3 x1 in
    assert (y1 = y2) ;
    assert (y1 = y2b) ;
    assert (y1 = y3) ;
    let y0 = cmp0 x1 x2 in
    let y1 = cmp1 x1 x2 in
    let y2 = cmp2 x1 x2 in
    assert (Euler.Arith.sign y1 = Euler.Arith.sign y0) ;
    assert (Euler.Arith.sign y1 = Euler.Arith.sign y2) ;
  done ;
  timed ~msg:"abs1" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = abs1 data.(i) in ()
    done
  end () |> ignore ;
  timed ~msg:"abs2" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = abs2 data.(i) in ()
    done
  end () |> ignore ;
  timed ~msg:"min0" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = min0 data.(i) data.(i+1) in ()
    done
  end () |> ignore ;
  timed ~msg:"min1" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = min1 data.(i) data.(i+1) in ()
    done
  end () |> ignore ;
  timed ~msg:"min2" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = min2 data.(i) data.(i+1) in ()
    done
  end () |> ignore ;
  timed ~msg:"min3" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = min3 data.(i) data.(i+1) in ()
    done
  end () |> ignore ;
  timed ~msg:"sign1" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = sign1 data.(i) in ()
    done
  end () |> ignore ;
  timed ~msg:"sign2" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = sign2 data.(i) in ()
    done
  end () |> ignore ;
  timed ~msg:"sign2b" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = sign2b data.(i) in ()
    done
  end () |> ignore ;
  timed ~msg:"sign3" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = sign3 data.(i) in ()
    done
  end () |> ignore ;
  timed ~msg:"cmp0" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = cmp0 data.(i) data.(i+1) in ()
    done
  end () |> ignore ;
  timed ~msg:"cmp1" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = cmp1 data.(i) data.(i+1) in ()
    done
  end () |> ignore ;
  timed ~msg:"cmp2" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = cmp2 data.(i) data.(i+1) in ()
    done
  end () |> ignore ;
  ()

let rounds = (1 lsl 22)

let () =
  Random.self_init () ;

  test_simple_functions rounds ;

  test "number_of_bits_set (arbitrary ints)" !impls_number_of_bits_set
    ~rand:(Euler.Arith.rand ~min:0 ~max:max_int) rounds
    ~pp:pp_int ;
  test "number_of_bits_set (32-bit ints)" impls_number_of_bits_set_32bit
    ~rand:(Euler.Arith.rand ~min:0 ~max:0xFFFF_FFFF) rounds
    ~pp:pp_int ;
  test "number_of_bits_set (16-bit ints)" impls_number_of_bits_set_32bit
    ~rand:(Euler.Arith.rand ~min:0 ~max:0xFFFF) rounds
    ~pp:pp_int ;

  test "ilog2sup8bit" !impls_ilog2sup8bit
    ~rand:(Euler.Arith.rand ~min:0 ~max:255) rounds
    ~expected:[
      (0, 0);
      (1, 1);
      (2, 2); (3, 2);
      (4, 3); (5, 3); (6, 3); (7, 3);
      (8, 4); (9, 4); (15, 4); (16, 5); (17, 5);
    ]
    ~pp:pp_int ;

  test "ilog2sup (arbitrary ints)" !impls_ilog2sup62bit
    ~expected:[
      (0, 0);
      (1, 1);
      (2, 2); (3, 2);
      (4, 3); (5, 3); (6, 3); (7, 3);
      (8, 4); (9, 4); (15, 4); (16, 5); (17, 5);
      (max_int, 62);
    ]
    ~rand:(Euler.Arith.rand ~min:0 ~max:max_int) rounds
    ~pp:pp_int ;
  test "ilog2sup (53-bit ints)" !impls_ilog2sup62bit
    ~rand:(Euler.Arith.rand ~min:0 ~max:0x003F_FFFF_FFFF_FFFF) rounds
    ~pp:pp_int ;
  test "ilog2sup (32-bit ints)" !impls_ilog2sup62bit
    ~rand:(Euler.Arith.rand ~min:0 ~max:0xFFFF_FFFF) rounds
    ~pp:pp_int ;
  test "ilog2sup (16-bit ints)" !impls_ilog2sup62bit
    ~rand:(Euler.Arith.rand ~min:0 ~max:0xFFFF) rounds
    ~pp:pp_int ;

  test "val2 (arbitrary ints)" !impls_val2
    ~rand:(Euler.Arith.rand ~min:1 ~max:max_int) rounds
    ~pp:pp_pair ;
  test "val2 (valuation >= 4)" !impls_val2
    ~rand:(fun () -> Euler.Arith.rand ~min:0x10 ~max:max_int () land -0x10) rounds
    ~pp:pp_pair ;
  test "val2 (valuation >= 8)" !impls_val2
    ~rand:(fun () -> Euler.Arith.rand ~min:0x100 ~max:max_int () land -0x100) rounds
    ~pp:pp_pair ;
  test "val2 (valuation >= 16)" !impls_val2
    ~rand:(fun () -> Euler.Arith.rand ~min:0x10000 ~max:max_int () land -0x10000) rounds
    ~pp:pp_pair ;
