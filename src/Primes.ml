(* We use some functions which appeared in the stdlib after 4.07 (specifically,
 * functions in Seq), so we use Stdcompat to get an up-to-date version of the
 * stdlib: *)
open! Stdcompat

type factorization = (int * int) list

(******************************************************************************)

let li ?(precision=0.0) =
  (* Euler–Mascheroni’s constant. *)
  let gamma = 0.57721_56649_01532_86061 in
fun x ->
  (* Computing with a series development. *)
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
  (* Computing with a series (by Ramanujan) which converges slightly faster. *)
  assert (x > 1.0) ;
  let log_x = log x in
  let s = ref (gamma +. log log_x) in
  let term = ref (~-. 2.0 *. sqrt x) in
  let sum_of_inverses = ref 0.0 in
  let n = ref 1 in while
    if !n land 1 <> 0 then sum_of_inverses := !sum_of_inverses +. 1. /. float !n ;
    term := !term *. log_x *. ~-. 0.5 /. float !n ;
    s := !s +. !term *. !sum_of_inverses ;
    abs_float !term > precision
  do incr n done ;
  !s

(* Over‐estimating with x ∕ ln(x). *)
(*
let overestimate_number_of_primes nmax =
  let x = float nmax in
  let y =
    if nmax >= 60_184 then x /. (log x -. 1.1)  (* [Pierre Dusart, 2010] *)
    else if nmax >= 1_606 then x /. (log x -. 1.5) (* valid as soon as n >= 5 *)
    else if nmax >= 2 then 1.25506 *. x /. log x
    else 0.0
  in truncate y
*)
(* Using the logarithmic integral function gives a much tighter upper bound. *)
let overestimate_number_of_primes nmax =
  assert (1 < nmax) ;
  truncate (li (float nmax))

(******************************************************************************)

let primes_under_100 =
  [|  2 ;  3 ;  5 ;  7 ; 11 ; 13 ; 17 ; 19 ; 23 ; 29 ; 31 ; 37 ; 41 ; 43 ; 47 ;
     53 ; 59 ; 61 ; 67 ; 71 ; 73 ; 79 ; 83 ; 89 ; 97 |]

let primes_under_10_000 =
  [|
        2 ;    3 ;    5 ;    7 ;   11 ;   13 ;   17 ;   19 ;   23 ;   29 ;
       31 ;   37 ;   41 ;   43 ;   47 ;   53 ;   59 ;   61 ;   67 ;   71 ;
       73 ;   79 ;   83 ;   89 ;   97 ;  101 ;  103 ;  107 ;  109 ;  113 ;
      127 ;  131 ;  137 ;  139 ;  149 ;  151 ;  157 ;  163 ;  167 ;  173 ;
      179 ;  181 ;  191 ;  193 ;  197 ;  199 ;  211 ;  223 ;  227 ;  229 ;
      233 ;  239 ;  241 ;  251 ;  257 ;  263 ;  269 ;  271 ;  277 ;  281 ;
      283 ;  293 ;  307 ;  311 ;  313 ;  317 ;  331 ;  337 ;  347 ;  349 ;
      353 ;  359 ;  367 ;  373 ;  379 ;  383 ;  389 ;  397 ;  401 ;  409 ;
      419 ;  421 ;  431 ;  433 ;  439 ;  443 ;  449 ;  457 ;  461 ;  463 ;
      467 ;  479 ;  487 ;  491 ;  499 ;  503 ;  509 ;  521 ;  523 ;  541 ;
      547 ;  557 ;  563 ;  569 ;  571 ;  577 ;  587 ;  593 ;  599 ;  601 ;
      607 ;  613 ;  617 ;  619 ;  631 ;  641 ;  643 ;  647 ;  653 ;  659 ;
      661 ;  673 ;  677 ;  683 ;  691 ;  701 ;  709 ;  719 ;  727 ;  733 ;
      739 ;  743 ;  751 ;  757 ;  761 ;  769 ;  773 ;  787 ;  797 ;  809 ;
      811 ;  821 ;  823 ;  827 ;  829 ;  839 ;  853 ;  857 ;  859 ;  863 ;
      877 ;  881 ;  883 ;  887 ;  907 ;  911 ;  919 ;  929 ;  937 ;  941 ;
      947 ;  953 ;  967 ;  971 ;  977 ;  983 ;  991 ;  997 ; 1009 ; 1013 ;
     1019 ; 1021 ; 1031 ; 1033 ; 1039 ; 1049 ; 1051 ; 1061 ; 1063 ; 1069 ;
     1087 ; 1091 ; 1093 ; 1097 ; 1103 ; 1109 ; 1117 ; 1123 ; 1129 ; 1151 ;
     1153 ; 1163 ; 1171 ; 1181 ; 1187 ; 1193 ; 1201 ; 1213 ; 1217 ; 1223 ;
     1229 ; 1231 ; 1237 ; 1249 ; 1259 ; 1277 ; 1279 ; 1283 ; 1289 ; 1291 ;
     1297 ; 1301 ; 1303 ; 1307 ; 1319 ; 1321 ; 1327 ; 1361 ; 1367 ; 1373 ;
     1381 ; 1399 ; 1409 ; 1423 ; 1427 ; 1429 ; 1433 ; 1439 ; 1447 ; 1451 ;
     1453 ; 1459 ; 1471 ; 1481 ; 1483 ; 1487 ; 1489 ; 1493 ; 1499 ; 1511 ;
     1523 ; 1531 ; 1543 ; 1549 ; 1553 ; 1559 ; 1567 ; 1571 ; 1579 ; 1583 ;
     1597 ; 1601 ; 1607 ; 1609 ; 1613 ; 1619 ; 1621 ; 1627 ; 1637 ; 1657 ;
     1663 ; 1667 ; 1669 ; 1693 ; 1697 ; 1699 ; 1709 ; 1721 ; 1723 ; 1733 ;
     1741 ; 1747 ; 1753 ; 1759 ; 1777 ; 1783 ; 1787 ; 1789 ; 1801 ; 1811 ;
     1823 ; 1831 ; 1847 ; 1861 ; 1867 ; 1871 ; 1873 ; 1877 ; 1879 ; 1889 ;
     1901 ; 1907 ; 1913 ; 1931 ; 1933 ; 1949 ; 1951 ; 1973 ; 1979 ; 1987 ;
     1993 ; 1997 ; 1999 ; 2003 ; 2011 ; 2017 ; 2027 ; 2029 ; 2039 ; 2053 ;
     2063 ; 2069 ; 2081 ; 2083 ; 2087 ; 2089 ; 2099 ; 2111 ; 2113 ; 2129 ;
     2131 ; 2137 ; 2141 ; 2143 ; 2153 ; 2161 ; 2179 ; 2203 ; 2207 ; 2213 ;
     2221 ; 2237 ; 2239 ; 2243 ; 2251 ; 2267 ; 2269 ; 2273 ; 2281 ; 2287 ;
     2293 ; 2297 ; 2309 ; 2311 ; 2333 ; 2339 ; 2341 ; 2347 ; 2351 ; 2357 ;
     2371 ; 2377 ; 2381 ; 2383 ; 2389 ; 2393 ; 2399 ; 2411 ; 2417 ; 2423 ;
     2437 ; 2441 ; 2447 ; 2459 ; 2467 ; 2473 ; 2477 ; 2503 ; 2521 ; 2531 ;
     2539 ; 2543 ; 2549 ; 2551 ; 2557 ; 2579 ; 2591 ; 2593 ; 2609 ; 2617 ;
     2621 ; 2633 ; 2647 ; 2657 ; 2659 ; 2663 ; 2671 ; 2677 ; 2683 ; 2687 ;
     2689 ; 2693 ; 2699 ; 2707 ; 2711 ; 2713 ; 2719 ; 2729 ; 2731 ; 2741 ;
     2749 ; 2753 ; 2767 ; 2777 ; 2789 ; 2791 ; 2797 ; 2801 ; 2803 ; 2819 ;
     2833 ; 2837 ; 2843 ; 2851 ; 2857 ; 2861 ; 2879 ; 2887 ; 2897 ; 2903 ;
     2909 ; 2917 ; 2927 ; 2939 ; 2953 ; 2957 ; 2963 ; 2969 ; 2971 ; 2999 ;
     3001 ; 3011 ; 3019 ; 3023 ; 3037 ; 3041 ; 3049 ; 3061 ; 3067 ; 3079 ;
     3083 ; 3089 ; 3109 ; 3119 ; 3121 ; 3137 ; 3163 ; 3167 ; 3169 ; 3181 ;
     3187 ; 3191 ; 3203 ; 3209 ; 3217 ; 3221 ; 3229 ; 3251 ; 3253 ; 3257 ;
     3259 ; 3271 ; 3299 ; 3301 ; 3307 ; 3313 ; 3319 ; 3323 ; 3329 ; 3331 ;
     3343 ; 3347 ; 3359 ; 3361 ; 3371 ; 3373 ; 3389 ; 3391 ; 3407 ; 3413 ;
     3433 ; 3449 ; 3457 ; 3461 ; 3463 ; 3467 ; 3469 ; 3491 ; 3499 ; 3511 ;
     3517 ; 3527 ; 3529 ; 3533 ; 3539 ; 3541 ; 3547 ; 3557 ; 3559 ; 3571 ;
     3581 ; 3583 ; 3593 ; 3607 ; 3613 ; 3617 ; 3623 ; 3631 ; 3637 ; 3643 ;
     3659 ; 3671 ; 3673 ; 3677 ; 3691 ; 3697 ; 3701 ; 3709 ; 3719 ; 3727 ;
     3733 ; 3739 ; 3761 ; 3767 ; 3769 ; 3779 ; 3793 ; 3797 ; 3803 ; 3821 ;
     3823 ; 3833 ; 3847 ; 3851 ; 3853 ; 3863 ; 3877 ; 3881 ; 3889 ; 3907 ;
     3911 ; 3917 ; 3919 ; 3923 ; 3929 ; 3931 ; 3943 ; 3947 ; 3967 ; 3989 ;
     4001 ; 4003 ; 4007 ; 4013 ; 4019 ; 4021 ; 4027 ; 4049 ; 4051 ; 4057 ;
     4073 ; 4079 ; 4091 ; 4093 ; 4099 ; 4111 ; 4127 ; 4129 ; 4133 ; 4139 ;
     4153 ; 4157 ; 4159 ; 4177 ; 4201 ; 4211 ; 4217 ; 4219 ; 4229 ; 4231 ;
     4241 ; 4243 ; 4253 ; 4259 ; 4261 ; 4271 ; 4273 ; 4283 ; 4289 ; 4297 ;
     4327 ; 4337 ; 4339 ; 4349 ; 4357 ; 4363 ; 4373 ; 4391 ; 4397 ; 4409 ;
     4421 ; 4423 ; 4441 ; 4447 ; 4451 ; 4457 ; 4463 ; 4481 ; 4483 ; 4493 ;
     4507 ; 4513 ; 4517 ; 4519 ; 4523 ; 4547 ; 4549 ; 4561 ; 4567 ; 4583 ;
     4591 ; 4597 ; 4603 ; 4621 ; 4637 ; 4639 ; 4643 ; 4649 ; 4651 ; 4657 ;
     4663 ; 4673 ; 4679 ; 4691 ; 4703 ; 4721 ; 4723 ; 4729 ; 4733 ; 4751 ;
     4759 ; 4783 ; 4787 ; 4789 ; 4793 ; 4799 ; 4801 ; 4813 ; 4817 ; 4831 ;
     4861 ; 4871 ; 4877 ; 4889 ; 4903 ; 4909 ; 4919 ; 4931 ; 4933 ; 4937 ;
     4943 ; 4951 ; 4957 ; 4967 ; 4969 ; 4973 ; 4987 ; 4993 ; 4999 ; 5003 ;
     5009 ; 5011 ; 5021 ; 5023 ; 5039 ; 5051 ; 5059 ; 5077 ; 5081 ; 5087 ;
     5099 ; 5101 ; 5107 ; 5113 ; 5119 ; 5147 ; 5153 ; 5167 ; 5171 ; 5179 ;
     5189 ; 5197 ; 5209 ; 5227 ; 5231 ; 5233 ; 5237 ; 5261 ; 5273 ; 5279 ;
     5281 ; 5297 ; 5303 ; 5309 ; 5323 ; 5333 ; 5347 ; 5351 ; 5381 ; 5387 ;
     5393 ; 5399 ; 5407 ; 5413 ; 5417 ; 5419 ; 5431 ; 5437 ; 5441 ; 5443 ;
     5449 ; 5471 ; 5477 ; 5479 ; 5483 ; 5501 ; 5503 ; 5507 ; 5519 ; 5521 ;
     5527 ; 5531 ; 5557 ; 5563 ; 5569 ; 5573 ; 5581 ; 5591 ; 5623 ; 5639 ;
     5641 ; 5647 ; 5651 ; 5653 ; 5657 ; 5659 ; 5669 ; 5683 ; 5689 ; 5693 ;
     5701 ; 5711 ; 5717 ; 5737 ; 5741 ; 5743 ; 5749 ; 5779 ; 5783 ; 5791 ;
     5801 ; 5807 ; 5813 ; 5821 ; 5827 ; 5839 ; 5843 ; 5849 ; 5851 ; 5857 ;
     5861 ; 5867 ; 5869 ; 5879 ; 5881 ; 5897 ; 5903 ; 5923 ; 5927 ; 5939 ;
     5953 ; 5981 ; 5987 ; 6007 ; 6011 ; 6029 ; 6037 ; 6043 ; 6047 ; 6053 ;
     6067 ; 6073 ; 6079 ; 6089 ; 6091 ; 6101 ; 6113 ; 6121 ; 6131 ; 6133 ;
     6143 ; 6151 ; 6163 ; 6173 ; 6197 ; 6199 ; 6203 ; 6211 ; 6217 ; 6221 ;
     6229 ; 6247 ; 6257 ; 6263 ; 6269 ; 6271 ; 6277 ; 6287 ; 6299 ; 6301 ;
     6311 ; 6317 ; 6323 ; 6329 ; 6337 ; 6343 ; 6353 ; 6359 ; 6361 ; 6367 ;
     6373 ; 6379 ; 6389 ; 6397 ; 6421 ; 6427 ; 6449 ; 6451 ; 6469 ; 6473 ;
     6481 ; 6491 ; 6521 ; 6529 ; 6547 ; 6551 ; 6553 ; 6563 ; 6569 ; 6571 ;
     6577 ; 6581 ; 6599 ; 6607 ; 6619 ; 6637 ; 6653 ; 6659 ; 6661 ; 6673 ;
     6679 ; 6689 ; 6691 ; 6701 ; 6703 ; 6709 ; 6719 ; 6733 ; 6737 ; 6761 ;
     6763 ; 6779 ; 6781 ; 6791 ; 6793 ; 6803 ; 6823 ; 6827 ; 6829 ; 6833 ;
     6841 ; 6857 ; 6863 ; 6869 ; 6871 ; 6883 ; 6899 ; 6907 ; 6911 ; 6917 ;
     6947 ; 6949 ; 6959 ; 6961 ; 6967 ; 6971 ; 6977 ; 6983 ; 6991 ; 6997 ;
     7001 ; 7013 ; 7019 ; 7027 ; 7039 ; 7043 ; 7057 ; 7069 ; 7079 ; 7103 ;
     7109 ; 7121 ; 7127 ; 7129 ; 7151 ; 7159 ; 7177 ; 7187 ; 7193 ; 7207 ;
     7211 ; 7213 ; 7219 ; 7229 ; 7237 ; 7243 ; 7247 ; 7253 ; 7283 ; 7297 ;
     7307 ; 7309 ; 7321 ; 7331 ; 7333 ; 7349 ; 7351 ; 7369 ; 7393 ; 7411 ;
     7417 ; 7433 ; 7451 ; 7457 ; 7459 ; 7477 ; 7481 ; 7487 ; 7489 ; 7499 ;
     7507 ; 7517 ; 7523 ; 7529 ; 7537 ; 7541 ; 7547 ; 7549 ; 7559 ; 7561 ;
     7573 ; 7577 ; 7583 ; 7589 ; 7591 ; 7603 ; 7607 ; 7621 ; 7639 ; 7643 ;
     7649 ; 7669 ; 7673 ; 7681 ; 7687 ; 7691 ; 7699 ; 7703 ; 7717 ; 7723 ;
     7727 ; 7741 ; 7753 ; 7757 ; 7759 ; 7789 ; 7793 ; 7817 ; 7823 ; 7829 ;
     7841 ; 7853 ; 7867 ; 7873 ; 7877 ; 7879 ; 7883 ; 7901 ; 7907 ; 7919 ;
     7927 ; 7933 ; 7937 ; 7949 ; 7951 ; 7963 ; 7993 ; 8009 ; 8011 ; 8017 ;
     8039 ; 8053 ; 8059 ; 8069 ; 8081 ; 8087 ; 8089 ; 8093 ; 8101 ; 8111 ;
     8117 ; 8123 ; 8147 ; 8161 ; 8167 ; 8171 ; 8179 ; 8191 ; 8209 ; 8219 ;
     8221 ; 8231 ; 8233 ; 8237 ; 8243 ; 8263 ; 8269 ; 8273 ; 8287 ; 8291 ;
     8293 ; 8297 ; 8311 ; 8317 ; 8329 ; 8353 ; 8363 ; 8369 ; 8377 ; 8387 ;
     8389 ; 8419 ; 8423 ; 8429 ; 8431 ; 8443 ; 8447 ; 8461 ; 8467 ; 8501 ;
     8513 ; 8521 ; 8527 ; 8537 ; 8539 ; 8543 ; 8563 ; 8573 ; 8581 ; 8597 ;
     8599 ; 8609 ; 8623 ; 8627 ; 8629 ; 8641 ; 8647 ; 8663 ; 8669 ; 8677 ;
     8681 ; 8689 ; 8693 ; 8699 ; 8707 ; 8713 ; 8719 ; 8731 ; 8737 ; 8741 ;
     8747 ; 8753 ; 8761 ; 8779 ; 8783 ; 8803 ; 8807 ; 8819 ; 8821 ; 8831 ;
     8837 ; 8839 ; 8849 ; 8861 ; 8863 ; 8867 ; 8887 ; 8893 ; 8923 ; 8929 ;
     8933 ; 8941 ; 8951 ; 8963 ; 8969 ; 8971 ; 8999 ; 9001 ; 9007 ; 9011 ;
     9013 ; 9029 ; 9041 ; 9043 ; 9049 ; 9059 ; 9067 ; 9091 ; 9103 ; 9109 ;
     9127 ; 9133 ; 9137 ; 9151 ; 9157 ; 9161 ; 9173 ; 9181 ; 9187 ; 9199 ;
     9203 ; 9209 ; 9221 ; 9227 ; 9239 ; 9241 ; 9257 ; 9277 ; 9281 ; 9283 ;
     9293 ; 9311 ; 9319 ; 9323 ; 9337 ; 9341 ; 9343 ; 9349 ; 9371 ; 9377 ;
     9391 ; 9397 ; 9403 ; 9413 ; 9419 ; 9421 ; 9431 ; 9433 ; 9437 ; 9439 ;
     9461 ; 9463 ; 9467 ; 9473 ; 9479 ; 9491 ; 9497 ; 9511 ; 9521 ; 9533 ;
     9539 ; 9547 ; 9551 ; 9587 ; 9601 ; 9613 ; 9619 ; 9623 ; 9629 ; 9631 ;
     9643 ; 9649 ; 9661 ; 9677 ; 9679 ; 9689 ; 9697 ; 9719 ; 9721 ; 9733 ;
     9739 ; 9743 ; 9749 ; 9767 ; 9769 ; 9781 ; 9787 ; 9791 ; 9803 ; 9811 ;
     9817 ; 9829 ; 9833 ; 9839 ; 9851 ; 9857 ; 9859 ; 9871 ; 9883 ; 9887 ;
     9901 ; 9907 ; 9923 ; 9929 ; 9931 ; 9941 ; 9949 ; 9967 ; 9973 ;
  |]

(******************************************************************************)

(* TODO: Use a better sieve, such as Pritchard’s or Atkin’s.
 *     https://en.wikipedia.org/wiki/Generating_primes
 *     https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes
 *     https://en.wikipedia.org/wiki/Wheel_factorization
 *     https://en.wikipedia.org/wiki/Sieve_of_Pritchard
 *     https://en.wikipedia.org/wiki/Sieve_of_Atkin
 *     https://github.com/kimwalisch/primesieve
 *)

(* Size of a bool, in bytes. *)
let bool_byte_size = Sys.word_size / 8
(* Size of an int, in bytes. *)
let int_byte_size = Sys.word_size / 8

(* Maximum size of a non‐segmented sieve, in bytes. We forbid larger sieves
 * because they consume too much memory and thus may provoke a crash, or at
 * least put the computer in distress. Use a segmented sieve in that case. *)
let max_sieve_byte_size = 1 lsl 28

(* Size of a sieve segment, in bytes. Compared to the non‐segmented algorithm,
 * the shorter the segment, the more memory is saved, but the greater the
 * constant factor in computing time is. The bottleneck is the processor cache:
 * when reducing the segment size, the time factor increases accordingly, but
 * there is a massive drop when the segment starts fitting in the cache. So the
 * best value is just under the cache size. *)
(* let segm_byte_size = 1 lsl 19 *)
(* This setting is made obsolete by our prime wheel (see below). Now the memory
 * footprint is adjusted through [Wheel.turns_per_segment] and the number of
 * pre‐culled primes. *)

(* When nmax is below that threshold, we use the non‐segmented sieve algorithm;
 * when nmax is at least equal to that threshold, we use the segmented one.
 * Tweak it with benchmarks. *)
let threshold_to_use_segmentation = 1 lsl 23

(* The “bitvector” is the datastructure we use for storing a large array of
 * boolean values. Bit packing induces a time penalty but divides space by 64,
 * so we can make our sieve segments 64 times larger with the same memory
 * footprint, which partially compensates for the loss in performance. In
 * practice, it is faster by a small factor, but only if using unsafe indexing…
 * Still, it divides space used by the non‐segmented sieve, and thus let us
 * avoid segmenting for larger values of [nmax]. *)
module type BITVECTOR
= sig
  type t
  val number_of_booleans_in_byte_size : int -> int
  val make : int -> t
  val get : t -> int -> bool
  val unset : t -> int -> unit
  val set_all : t -> unit
end

(* Implementation of bitvectors with a regular array of booleans. *)
module BitVector_array : BITVECTOR
= struct
  type t = bool array
  let number_of_booleans_in_byte_size byte_size = byte_size / bool_byte_size
  let[@inline] make n = Array.make n true
  let[@inline] get v i = v.(i)
  let[@inline] unset v i = v.(i) <- false
  (*let[@inline] get v i = Array.unsafe_get v i*)
  (*let[@inline] unset v i = Array.unsafe_set v i false*)
  let[@inline] set_all v = Array.fill v 0 (Array.length v) true
end

(* Implementation of bitvectors with bit packing. *)
module BitVector_bitpacking : BITVECTOR
= struct
  type t = bytes
  let number_of_booleans_in_byte_size byte_size = byte_size * 8
  let masks =
    [|
      0b00000001 ;
      0b00000010 ;
      0b00000100 ;
      0b00001000 ;
      0b00010000 ;
      0b00100000 ;
      0b01000000 ;
      0b10000000 ;
    |]
  let[@inline] make n =
    Bytes.make ((n + 7) lsr 3) '\x00'
  let[@inline] get v i =
    Char.code (Bytes.unsafe_get v (i lsr 3))
    land Array.unsafe_get masks (i land 7)
    = 0
  let[@inline] unset v i =
    let j = i lsr 3 in
    Bytes.unsafe_set v j
      (Char.unsafe_chr
         (Char.code (Bytes.unsafe_get v j)
          lor Array.unsafe_get masks (i land 7)))
  let[@inline] set_all v =
    Bytes.fill v 0 (Bytes.length v) '\x00'
end

module BitVector = BitVector_bitpacking

(* The non‐segmented variant of the sieve of Eratosthenes. *)
let eratosthenes_sieve =
  (* To save space, we only store odd numbers, so that the actual array only
   * stores C∕2 booleans, where C is the cardinal of the sieve.
   * Then, the “address” addr represents the number 2×addr + 1.
   * In the code below, addresses will be prefixed with ‘half_’. *)
  let max_sieve_bool_size = BitVector.number_of_booleans_in_byte_size max_sieve_byte_size in
  let max_nmax = max_sieve_bool_size * 2 - 1 in
fun nmax ~do_prime ->
  assert (3 <= nmax && nmax <= max_nmax) ;
  do_prime 2 ;
  let half_nmax = (nmax - 1) / 2 in
  let s = BitVector.make (half_nmax + 1) in
  let half_r = (Arith.isqrt nmax - 1) / 2 in
  for half_n = 1 to half_r do
    if BitVector.get s half_n then begin
      let p = (half_n lsl 1) lor 1 in
      do_prime p ;
      let addr_square_p = (p * p) lsr 1 in
      for i = 0 to (half_nmax - addr_square_p) / p do
        BitVector.unset s (addr_square_p + p * i)
      done
    end
  done ;
  for half_n = half_r + 1 to half_nmax do
    if BitVector.get s half_n then
      let p = (half_n lsl 1) lor 1 in
      do_prime p
  done

(* The segmented sieve algorithm is optimized with pre‐culling. Multiples of 2
 * are already ruled out, which divides by 2 how many numbers are inspected
 * for primality. We go further and rule out multiples of fixed small primes
 * p1, …, pk. In other words, we only consider numbers which are coprime with
 * all of these primes. Modulo Q = p1×…×pk, there are φ(Q) = (p1−1)×…×(pk−1)
 * such elements.
 *
 * For example, for the primes 2, 3, 5, we have that 2×3×5 = 30 and the only
 * numbers to consider are, modulo 30:
 *     1, 7, 11, 13, 17, 19, 23, 29
 * There are 1×2×4 = 8 of them, so we only consider an 8∕30‐th of all numbers,
 * which represents a ratio of 27%. Pre‐culling more primes reduces the ratio.
 *
 * To iterate on these numbers, we can use the differences between successive
 * elements:
 *     increments = [ 6, 4, 2, 4, 2, 4, 6 ]
 * We start with 1, then add 6 (to get 7), then add 2 (to get 11), and so on
 * until we reach 29; after that, we start over from 30+1. This fits naturally
 * into the segmented sieve algorithm, because we just have to set the cardinal
 * of the segments to 30, or a multiple or 30. We call each chunk of length 30
 * a “turn” of the wheel.
 *
 * The ratio {numbers considered for primality} ∕ {all numbers} is φ(Q) ∕ Q.
 * When pre‐culling all primes up to 17, it is about 18%.
 *
 * On the other hand, we have to store precomputed data made up of φ(Q) integers
 * (the wheel’s increments). The total memory footprint of the segmented sieve
 * with pre‐culling is:
 *   + φ(Q) integer values (the increments);
 *   + Q × {turns per segment} / 2 boolean values (the segment).
 * So how many primes are pre‐culled, as well as how many turns are done per
 * segment, should be chosen carefully. *)

module Wheel
: sig
  (* For the segmented prime sieve, the number of wheel turns per segment.
   * Segments are intervals of cardinal [diameter]×[turns_per_segment]. *)
  val turns_per_segment : int
  (* The cardinal of a turn. This is the product of all pre‐culled primes. *)
  val diameter : int
  (* The number of elements in a turn which are not pre‐culled, i.e. those which
   * are coprime with all pre‐culled primes. *)
  val number_of_coprimes : int
  (* The number of small primes that are pre‐culled. *)
  val number_of_primes : int
  (* [iter_half_coprimes ~turns f] iterates on all numbers [n] between 0 and
   * [diameter]×[turns] which are coprime with all pre‐culled primes.
   * More exactly, it iterates on their “half” ([n]−1)∕2 ([n] is always odd).
   * This is so because our sieve does not store even numbers. *)
  val iter_half_coprimes : turns:int -> (int -> unit) -> unit
  (* [increment i] is the increment from the (i−1)^th to the i^th wheel’s
   * coprime. The first increment is 2 in order to step from [diameter]−1 to
   * [diameter]+1 (recall that the ring of coprime residues is symmetric). *)
  val increment : int -> int
  (* [next_coprime_index i] is the successor of the wheel’s coprime’s index [i]
   * (these indexes range from 0 included to [number_of_coprimes] excluded). *)
  val next_coprime_index : int -> int
  (* The pre‐culled primes. *)
  val preculled_primes : int array
  (* The last pre‐culled prime. *)
  val last_preculled_prime : int
end
= struct

  let turns_per_segment = 4

  (* Values are pre‐computed with [gen-wheel.ml]. Adjust there the number of
   * primes to pre‐cull.
   *
   * TODO: Generally speaking, code generation could be done better. In this
   * case, we refer to a separate (generated) module, whereas what we really
   * want is having the pre‐computed values inserted back into the source code
   * (referring to another module induces a small penalty). Furthermore, we may
   * want to use other values of this module during pre‐computing (such as
   * [Primes.primes_under_100]), or numeric parameters. Having to set
   * parameters in two different places is inconvenient.
   * Things to consider:
   *   — ppx_blob, ocamlify, cppo (preprocessing tools which provide code inclusion)
   *   — MetaOCaml (fully‐fledged multi‐stage programming)
   *)

  let number_of_primes   : int = Primes__data_wheel.number_of_primes
  let number_of_coprimes : int = Primes__data_wheel.number_of_coprimes
  let diameter           : int = Primes__data_wheel.diameter
  let preculled_primes : int array = Primes__data_wheel.preculled_primes
  let last_preculled_prime : int = Primes__data_wheel.last_preculled_prime

  (* The wheel’s increments, divided by 2, stored in a string to save space. *)
  let half_increments : string = Primes__data_wheel.half_increments

  let[@inline] iter_half_coprimes ~turns f =
    let half_n = ref (~- 1) in
    for _ = 1 to turns do
      StringLabels.iter half_increments ~f:begin fun c ->
        let a = !half_n + Char.code c in
        half_n := a ;
        f a
      end
    done

  let[@inline] increment i =
    (*! assert (0 <= i && i < number_of_coprimes) ; !*)
    Char.code (String.unsafe_get half_increments i) lsl 1

  let[@inline] next_coprime_index i =
    (*! assert (0 <= i && i < number_of_coprimes) ; !*)
    if i + 1 < number_of_coprimes then i + 1 else 0

end (* module Wheel *)

(* The segmented variant of the sieve of Eratosthenes, with pre‐culling.
 *
 * TODO: Yet another optimization: by using a wheel, when advancing through the
 * sieve, we do skip numbers which are not coprime with all pre‐culled primes;
 * however, when crossing out the multiples of a found prime (other than
 * a pre‐culled prime), we cross out all the (odd) multiples of that prime,
 * including those that are NOT coprime with the pre-culled primes. This is
 * wasteful. Instead of enumerating odd multiples, we may enumerate just those
 * which are coprime with the pre‐culled primes. For that, we can use the wheel
 * again. This is done in [prime_seq], see below.
 *)
let segmented_eratosthenes_sieve =
  let segm_cardinal = Wheel.diameter * Wheel.turns_per_segment in
  (* To save space, we only store odd numbers, so that the actual array only
   * stores C∕2 booleans, where C is the cardinal of a segment.
   * Then, at step K, the “address” addr represents the number C×K + 2×addr + 1.
   * The segment represented is the set of numbers from C×K to C×(K+1) − 1, of
   * which we only store odd numbers.
   * In the code below, addresses will be prefixed with ‘addr_’ or ‘half_’. *)
  let half_segm_cardinal = segm_cardinal / 2 in
  let exception Break in
fun nmax ~do_prime ->
  assert (0 <= nmax) ;
  (* Compute the number of segments, and ceil [nmax] to the closest multiple of
   * the cardinal of a segment. *)
  let number_of_segments = nmax / segm_cardinal + 1 in
  assert (number_of_segments <= max_int / segm_cardinal) ;
  let ceiled_nmax = number_of_segments * segm_cardinal - 1 in
  (* Primes found so far are stored in this array. [count_primes] is their
   * number, [count_prime_squares] is the number of primes whose square is less
   * than the first value of the current segment (which means that the square
   * has already been eliminated).
   * We only need to store primes not greater than the square root of [nmax].
   * In fact, for Assertion A (below) to hold, we need to keep at least one
   * number greater than the square root. This is okay, we have room for it. *)
  let sqrt_nmax = Arith.isqrt ceiled_nmax in
  let primes = Array.make (overestimate_number_of_primes sqrt_nmax) 0 in
  let count_primes = ref 0 in
  let count_prime_squares = ref 0 in
  (* NOTE: As a micro‐optimization, [add_prime] is a reference to a closure, so
   * that we can avoid comparing primes against [sqrt_nmax] once the square root
   * has been reached. *)
  let rec add_prime = ref begin fun p ->
      do_prime p ;
      primes.(!count_primes) <- p ;
      incr count_primes ;
      if p > sqrt_nmax then
        add_prime := do_prime ;
    end
  in
  (* The current sieve segment is stored in this array. See the comment above
   * for how to translate from addresses to values and conversely. *)
  let s = BitVector.make half_segm_cardinal in
  (* [remove_multiples ~segm_first p m] marks as composite all elements of the
   * current segment which are multiple of [p]; [segm_first] is the first value
   * of the current segment, and [m] is the first multiple of [p] which is
   * at least equal to [segm_first] (we MUST have [segm_first] ≤ [m]). *)
  let[@inline] remove_multiples ~segm_first p first_multiple =
    let addr_first_multiple = (first_multiple - segm_first) lsr 1 in
    for i = 0 to (half_segm_cardinal - 1 - addr_first_multiple) / p do
      BitVector.unset s (addr_first_multiple + p * i)
    done
  in
  (* (0) Treat the prime 2 specially. *)
  !add_prime 2 ;
  incr count_prime_squares ;
  (* (1) Sieve the initial segment. This is regular sieving. *)
  begin
    let half_r = (Arith.isqrt (segm_cardinal - 1) - 1) / 2 in
    for half_n = 1 to half_r do
      if BitVector.get s half_n then begin
        let p = ((half_n lsl 1) lor 1) in
        !add_prime p ;
        remove_multiples ~segm_first:0 p (p * p)
      end
    done ;
    count_prime_squares := !count_primes ;
    for half_n = half_r + 1 to half_segm_cardinal - 1 do
      if BitVector.get s half_n then
        let p = ((half_n lsl 1) lor 1) in
        !add_prime p
    done ;
  end ;
  (* (2) Sieve following segments. *)
  for segm = 1 to number_of_segments - 1 do
    let segm_first = segm_cardinal * segm in
    let segm_last  = segm_first + segm_cardinal - 1 in
    (* Reset the sieve. *)
    BitVector.set_all s ;
    (* Rule out primes already found, and whose square is less than
     * [segm_first]. Pre‐culled primes do not need to be processed. *)
    for i = Wheel.number_of_primes to !count_prime_squares - 1 do
      let p = primes.(i) in
      (* Compute the first odd multiple of [p] at least equal to [segm_first]. *)
      let first_multiple = (((segm_first + p-1) / p) lor 1) * p in
      if first_multiple <= segm_last then
        remove_multiples ~segm_first p first_multiple
    done ;
    (* Rule out primes already found, and whose square is at least equal to
     * [segm_first]. *)
    begin try
      let r = Arith.isqrt segm_last in
      for i = !count_prime_squares to !count_primes - 1 do
        let p = primes.(i) in
        if p > r then begin
          count_prime_squares := i ;
          raise Break ;
        end ;
        remove_multiples ~segm_first p (p * p)
      done ;
      (* We can prove that there is always at least one prime left, ie. there
       * exists a prime p such that √((K+1)×C) ≤ p < K×C where K = [segm] is
       * the step and C = [segm_cardinal] is the cardinal of a segment. This can
       * be proven using Bertrand’s postulate. *)
      assert false (* Assertion A *)
    with Break -> () end ;
    (* Because there is still a prime whose square is greater than [segm_last],
     * we know that the new primes in this segment also have their squares
     * greater than [segm_last], so there is no need to sieve them out. *)
    Wheel.iter_half_coprimes ~turns:Wheel.turns_per_segment
    begin fun addr_n ->
      if BitVector.get s addr_n then
        let p = ((addr_n lsl 1) lor 1) + segm_first in
        !add_prime p
    end
  done

(* Euler’s sieve.
 *     https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes#Euler's_Sieve
 * By contrast with Eratosthenes’s sieve, Eulers’s sieve removes composite
 * numbers no more than once. It maintains a list of numbers still active, so
 * that removed composites are never visited, and the next prime is found in
 * constant time (as the first element of the list).
 * Hence, it has a better asymptotic time complexity than Eratosthenes’ sieve.
 *     Eratosthenes: time 𝒪([nmax]×log(log[nmax])), space 𝒪([nmax]).
 *     Euler:        time 𝒪([nmax]),                space 𝒪([nmax]).
 * However, log(log[nmax]) is not much and, in practice, Euler’s sieve is (very
 * slightly) slower than Eratosthenes’ with the same level of optimization.
 * Besides, it may require more space (since it stores a list of integers
 * instead of an array of booleans); and it cannot be segmented. So we prefer
 * Eratosthenes over Euler.
 * Still, I find this algorithm elegant, so I leave the code here. :-) *)
let euler_sieve =
  let max_sieve_int_size = max_sieve_byte_size / int_byte_size in
  let max_nmax = max_sieve_int_size * 2 - 1 in
fun nmax ~do_prime ->
  assert (3 <= nmax && nmax <= max_nmax) ;
  (* We store the elements of the sieve as a linked list embedded in an array.
   * If n is an element of the list, then next_elt.(n) gives the next (greater)
   * element of the list. The special value 0 means end‐of‐list. We store the
   * first element in next_elt.(0).
   *
   * In fact, to save space and time, we do not store even numbers. As a
   * consequence, the address a represents the odd number 2a+1.
   *
   * We need to mark elements for deletion. Of course we could use another array
   * storing boolean values, but in order to save space and time again, we make
   * it more compact. The convention we adopt is that bitwise‐negating the value
   * next_elt.(n) of an element n marks that element n for deletion. *)
  do_prime 2 ;
  let half_nmax = (nmax - 1) / 2 in
  let next_elt = Array.init (succ half_nmax) (fun n -> n + 1) in
(*   next_elt.(0) <- 1 ; *)
  next_elt.(half_nmax) <- 0 ;
  (* The loop invariant is that the elements of the list are the numbers which
   * are coprime with all the primes already identified.
   *
   * Each iteration consists in popping the first element p of the list, which
   * is prime, and removing all multiples of that prime which are still in the
   * list. Such multiples are of the form p×m where m is coprime with all
   * previous primes; in other words, m is itself an element of the list. So the
   * elements to remove are precisely the numbers p×m where m is an element of
   * the list and p×m ≤ nmax.
   *
   * Because we need to multiply p with all elements m of the list, elements
   * must not be removed immediately. Instead we mark them for deletion; they
   * are definitely removed when the cursor traverses them.
   *
   * No element is marked twice, so the sieve has a linear complexity. *)
  (* Stop as soon as the next prime exceeds √nmax. *)
  let r = (Arith.isqrt nmax - 1) / 2 in
  while next_elt.(0) <= r do
    (* Pop the first element of the list, which is a prime. *)
    let half_p = next_elt.(0) in
    let p = (half_p lsl 1) lor 1 in
    do_prime p ;
    next_elt.(half_p) <- lnot next_elt.(half_p) ; (* mark p for deletion *)
    (* Traverse the list, removing marked elements on‐the‐fly. For each element
     * m of the list, we mark the element p×m for deletion. We need to do so
     * only for m ≤ nmax ∕ p, hence we stop as soon as this bound is reached
     * (this always happen before the end of the list). A consequence is that
     * marked elements after this bound will not be deleted; this is not a
     * problem, because they will not be visited by subsequent traversals, since
     * each list traversal stops sooner than the previous one (because the bound
     * decreases as p increases). *)
    let previous = ref 0 in
    let current  = ref half_p in
    let bound = (nmax / p - 1) / 2 in
    while !current <= bound do
      let cur = !current in
      let next = next_elt.(cur) in
      (* If the current element is marked, we remove it from the linked list. *)
      if next < 0 then begin
        let next = lnot next in
        next_elt.(!previous) <- next ;
        current  := next ;
      (* Otherwise, we just step by one in the linked list. *)
      end else begin
        previous := cur ;
        current  := next ;
      end ;
      (* We mark p×m for deletion.
       * If p = 2p'+1 and m = 2m'+1, then p×m = 2(p×m' + p') + 1. *)
      let n = p * cur + half_p in
      assert (next_elt.(n) >= 0) ; (* elements are marked only once *)
      next_elt.(n) <- lnot next_elt.(n) ;
    done
  done ;
  (* All remaining elements are prime (they are the primes greater than √nmax).
   * Here, when traversing the list, we must make sure that we skip the elements
   * which were marked for deletion but not removed in previous steps. *)
  let current = ref next_elt.(0) in
  while !current <> 0 do
    let cur = !current in
    let next = next_elt.(cur) in
    if next < 0 then
      current := lnot next
    else begin
      current := next ;
      do_prime ((cur lsl 1) lor 1) ;
    end
  done

let primes nmax ~do_prime =
  assert (0 <= nmax) ;
  (* We are about to start a space‐consuming algorithm, so we’d better make room
   * for it. *)
  Gc.compact () ;
  (* We use precomputed primes. *)
  if nmax <= 10_000 then begin
    let i = ref 0 in
    let len = Array.length primes_under_10_000 in
    while !i < len && primes_under_10_000.(!i) <= nmax do
      do_prime primes_under_10_000.(!i) ;
      incr i ;
    done
  end
  (* We use the non‐segmented sieve of Eratosthenes. *)
  else if nmax < threshold_to_use_segmentation then
    eratosthenes_sieve nmax ~do_prime
  (* We use the segmented sieve of Eratosthenes. *)
  else
    segmented_eratosthenes_sieve nmax ~do_prime

(* TODO: Segmentation. *)
let factorizing_sieve =
  let max_sieve_bool_size = max_sieve_byte_size / bool_byte_size in
  let max_nmax = max_sieve_bool_size - 1 in
fun nmax ~do_factors ->
  assert (3 <= nmax && nmax <= max_nmax) ;
  let factors = Array.make (succ nmax) []
  and remaining_to_factor = Array.init (succ nmax) (fun n -> n) in
  for n = 2 to nmax do
    if remaining_to_factor.(n) <> 1 then begin
      for k = 1 to nmax / n do
        let m = k * n in
        (* TODO: Using another loop, all divisibility tests can be avoided. *)
        let (r', count) = Arith.valuation ~factor:n remaining_to_factor.(m) in
        remaining_to_factor.(m) <- r' ;
        factors.(m) <- (n, count) :: factors.(m) ;
      done
    end ;
    factors.(n) <- List.rev factors.(n) ;
    do_factors factors.(n) n
  done ;
  factors

(* Here is a purely functional version of the sieve of Eratosthenes, which is
 * able to produce a [Seq.t]. The idea is to remember, for each found prime,
 * what is the next multiple of that prime to be crossed. We keep them in
 * a priority queue, i.e. a heap. Then, as we advance through numbers, we
 * compare the current number to the smallest of the next multiples. As long as
 * we haven’t reached the smallest next multiples, the current number is prime.
 * When the current number reaches the smallest next multiples, we pop it from
 * the heap, and insert the subsequent multiples back into the heap.
 *
 * Adding a multiple to the heap amounts to crossing it out in the classical
 * sieve of Eratosthenes. Just as a given multiple may be crossed several times,
 * beware that a multiple may be present several times in the heap: once for
 * each prime factor smaller than its square root.
 *
 * (This allows to compute full factorizations if wanted.)
 *
 * This is significantly slower than the imperative sieve above. The heap adds
 * a logarithmic factor to the time complexity (more precisely, O(log(π(√n)))
 * = O(log n), because the heap stores π(√n) elements) and, in practice, most
 * time is spent operating it. I’ve benchmarked it to about 50 times slower than
 * the imperative sieve for nmax = 1_000_000_000.
 *
 * This implementation uses the wheel optimization to pre‐cull small primes.
 * This gives a more substantial speed-up than for the segmented imperative
 * sieve above (makes the sieve about 4 times faster for nmax = 1_000_000_000).
 * I suspect this is because it spares us many heap operations, and perhaps also
 * because the wheel optimization is not fully implemented in our imperative
 * sieve (see an earlier comment).
 *
 * Reference:
 *     "The Genuine Sieve of Eratosthenes", Melissa O’Neill
 *     https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf
 *)

type multiple_of_prime_in_wheel = {
  multiple : int ;
  prime : int ;
  idx : int ; (* the wheel’s coprime’s index of k such that multiple = k×prime *)
}

(* I’ve benchmarked several implementations of purely functional heaps (leftist,
 * pairing, binomial, skew binomial). The fastest appears to be the leftist heap
 * (on par with the pairing heap), provided by the Containers library. *)
module MultHeap =
  CCHeap.Make (struct
    type t = multiple_of_prime_in_wheel
    let leq = (<=)
  end)

let prime_seq nmax =
  (* If we start from a non-empty heap of multiples (which we make sure of by
   * initializing it with [mult_p1], below), then the heap never becomes empty
   * because, each time we pop a multiple, we re-insert a new one, except when
   * the new multiple would exceed [max_int]; but all of [max_int], [max_int]−2,
   * [max_int]−4 are composite, and the latter at least is not pre‐culled,
   * because its smallest prime factor is large (2969 on 32-bit OCaml, 34421 on
   * 64-bit OCaml). Hence, the largest non pre‐culled number is composite, and
   * so the heap contains it. *)
  assert (Wheel.last_preculled_prime < 2629) ;
  let sqrt_nmax = Arith.isqrt nmax in
  (* [idx] is the wheel’s coprime’s index of [n]. *)
  let rec seq_aux ~n ~idx next_mults () =
    (* End of the sequence (knowing that wheel’s increments are small, less than
     * 256, we can test for overflow on [n] simply by checking its sign): *)
    if n > nmax || n < 0 then
      Seq.Nil
    (* If [n] is composite: *)
    else if (MultHeap.find_min_exn next_mults).multiple <= n then begin
      let next_mults = ref next_mults in
      (* Pop all multiples that are equal to [n], insert back into the heap the
       * next multiple of the corresponding prime numbers: *)
      while
      (* "DO" *)
        let (next_mults', m) = MultHeap.take_exn !next_mults in
        let m'_idx = Wheel.next_coprime_index m.idx in
        let m' =
          { multiple = m.multiple + m.prime * Wheel.increment m'_idx ;
            prime = m.prime ;
            idx = m'_idx } in
        (* (same remark about overflows, knowing that [m.prime] ≤ √[max_int]) *)
        if m'.multiple >= 0 then
          next_mults := MultHeap.add next_mults' m'
        else
          next_mults := next_mults' ;
      (* "WHILE" *)
        (MultHeap.find_min_exn !next_mults).multiple <= n
      do () done ;
      let idx' = Wheel.next_coprime_index idx in
      let n' = n + Wheel.increment idx' in
      seq_aux ~n:n' ~idx:idx' !next_mults ()
    end
    (* If [n] is prime: *)
    else begin
      (* Insert the square of [n] as the first multiple of [n] to skip: *)
      let next_mults' =
        if n <= sqrt_nmax then
          MultHeap.add next_mults { multiple = n*n ; prime = n ; idx = idx }
        else
          next_mults
      in
      let idx' = Wheel.next_coprime_index idx in
      let n' = n + Wheel.increment idx' in
      Seq.Cons (n, seq_aux ~n:n' ~idx:idx' next_mults')
    end
  in (* /let seq_aux *)
  if nmax <= 10_000 then
    Seq.take_while (fun p -> p <= nmax) (Array.to_seq primes_under_10_000)
  else
    let p1 = 1  + Wheel.increment 1 in
    let p2 = p1 + Wheel.increment 2 in
    let mult_p1 = { multiple = p1*p1 ; prime = p1 ; idx = 1 } in
    let next_mults = MultHeap.add MultHeap.empty mult_p1 in
    Seq.append (Array.to_seq Wheel.preculled_primes) @@
    Seq.cons p1 @@
    seq_aux ~n:p2 ~idx:2 next_mults

(******************************************************************************)

(***** A QUICK REVIEW OF PRIMALITY TESTS ****
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
 * Solovay‐Strassen:
 *     https://en.wikipedia.org/wiki/Solovay%E2%80%93Strassen_primality_test
 * probabilistic (probability of a false positive, knowing the number is composite: less than 2^{−rounds} (much less in practice))
 * polynomial: O((log n)³)
 * similar to Miller‐Rabin, superseded by it (historical importance for RSA)
 * not used anymore
 *
 * Miller‐Rabin:
 *     https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
 * probabilistic (probability of a false positive, knowing the number is composite: less than 4^{−rounds} (much less in practice))
 * polynomial: O((log n)³), improved to Õ((log n)²) with FFT‐based multiplications
 *
 * Miller’s variant of Miller‐Rabin:
 *     https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
 * deterministic
 * correction depends on the generalized Riemann hypothesis
 * polynomial: Õ((log n)⁴) using FFT
 * not used in practice
 *
 * Baillie‐PSW:
 *     https://en.wikipedia.org/wiki/Baillie%E2%80%93PSW_primality_test
 * probabilistic
 * deterministic for 64‐bit integers (more efficient than the test with the seven bases show below?)
 *
 * simpler tests, often used before a general algorithm to speed up the test:
 * — trial divisions: try small factors (say, prime numbers less than 100)
 * — Fermat test: check that a^{n−1} ={n}= 1 for some random 2 ≤ a ≤ n−2
 *)

(* TODO:
 * Implement ECPP, Miller, Baillie‐PSW.
 *)

(* TODO:
  * Use hashing to reduce the number of bases necessary.
  * See https://miller-rabin.appspot.com/
  *)

exception Composite
exception Prime

(* Miller‐Rabin probable primality test (aka strong Fermat primality test):
 * [miller_rabin_test n] says whether [n] is strongly probably prime or not.
 * [n] must be odd and greater than 2.
 * {b Complexity:} 𝒪(k×log([n])³) where k is the number of bases.
 * @param bases The set of bases to try.
 * @return if [n] is strongly probably prime with respect to [bases]. If [n] is
 * in fact composite, the probability of a false positive is (much) less than
 * 4{^−k} where k is the number of bases.
 * @raise Prime if [n] is found to be definitely prime.
 * @raise Composite if [n] is found to be definitely composite. *)
let miller_rabin_test ~bases n =
  assert (3 <= n) ;
  assert (n land 1 <> 0) ;
  (* Write n = m × 2^k + 1 where m is odd. *)
  let (k, m) = Arith.valuation_of_2 (n - 1) in
  (* Perform the test for each given base. *)
  bases |> List.iter begin fun b ->
    let b = b mod n in
    let x = Modular.pow ~modulo:n b m in
    let exception Strong_probable_prime in
    begin try
      (* Test whether b^m ={n}= ±1. *)
      if x = 1 || x = n-1 then
        raise Strong_probable_prime ;
      (* Test whether b^{m×2^i} ={n}= −1 for some 1 ≤ i < k. *)
      let x = ref x in
      for _ = 1 to pred k do
        let y = Modular.mul ~modulo:n !x !x in
        (* When x² ={n}= 1, we know that n is composite and we can compute
         * factors of n: gcd(n, x − 1) and gcd(n, x + 1) are non‐trivial,
         * coprime factors whose product equals n.
         *
         * Likewise, when x² ={n}= −1, x is a square root of −1 modulo n. If
         * n is prime, then there can only be two such square roots, and there
         * are opposite to each other. We already found a square root r when
         * testing against the previous base, so we may compare x to r; if
         * x ≠ ±r, then n is composite, and gcd(n, x − r) and gcd(n, x + r) are
         * non-trivial, coprime factors whose product equals n.
         *
         * In practice, those additional tests are very seldom useful when
         * factorizing numbers, so they are commented out. *)
        (*! if y = 1 then !*)
        (*!   raise (Modular.Factor_found (Arith.gcd n (!x - 1))) ; !*)
        (*! if y = n-1 && !x <> !r && !x <> n - !r then !*)
        (*!   if !r = 0 then r := !x else !*)
        (*!     raise (Modular.Factor_found (Arith.gcd n (!x - !r))) ; !*)
        x := y ;
        if y = n-1 then
          raise Strong_probable_prime
      done ;
      raise Composite
    with Strong_probable_prime ->
      ()
    end
  end

(* Miller‐Rabin probabilistic primality test.
 * [is_probably_prime ~rounds:k n] is true when [n] is a strong probable prime
 * with respect to [k] randomly chosen bases. If [n] is in fact composite, the
 * probability of a false positive is (much) less than 4{^−[k]}. Thus, 10 is
 * reasonable value of [k].
 * {b Complexity:} 𝒪(k×log([n])³) where k is the number of bases.
 *)
(* TODO: tweak the default number of rounds; see this paragraph from Wikipedia:
 *
 *     In addition, for large values of n, on average the probability that a
 *     composite number is declared probably prime is significantly smaller than
 *     4−k. Damgård, Landrock and Pomerance[7] compute some explicit bounds and
 *     provide a method to make a reasonable selection for k for a desired error
 *     bound. Such bounds can, for example, be used to generate probable primes;
 *     however, they should not be used to verify primes with unknown origin,
 *     since in cryptographic applications an adversary might try to send you a
 *     pseudoprime in a place where a prime number is required. In such cases,
 *     only the error bound of 4−k can be relied upon.
 *
 *     However, though this may be a sound probabilistic argument using Bayes'
 *     theorem, later refinements by Ronald J. Burthe, Jr., proved the
 *     conjecture in the introduction of the paper [8] that the upper bound of
 *     4−k is valid for all k > 1. Burthe improved the estimates for 25 <= k <=
 *     50 to satisfy the conjecture. The exact values for 2 <= k <= 24 were
 *     evaluated numerically using a result of Monier's.
 *)
(*
let is_probably_prime ?(rounds=10) n =
  assert (0 <= rounds) ;
  let n = abs n in
  if n <= 3 then
    n = 2 || n = 3
  else if n land 1 = 0 then
    false
  else begin
    (* we pick random bases between 2 and n−2, inclusive: *)
    let bases = List.init rounds (fun _ -> Arith.rand ~min:2 ~max:(n-2) ()) in
    begin match miller_rabin_test ~bases n with
    | ()                  -> true  (* strong probable prime *)
    | exception Prime     -> true  (* definitely prime *)
    | exception Composite -> false (* definitely composite *)
    end
  end
*)

(* Deterministic primality test for 64‐bit numbers.
 * [is_prime_aux ~first_primes n] is true if and only if [n] is a prime number.
 * @param first_primes The set of prime factors to rule out with trial
 * divisions, before resorting to the Rabin‐Miller test. It must at least
 * contain 2, or [n] must be odd. *)
let is_prime_aux =
  (* These small base sets are guaranteed to give always‐correct result for
   * values of the input below the specified bound. the last one works for (at
   * least) all 64‐bit integers. They are found here:
   *     https://miller-rabin.appspot.com/
   * Each base set has a list ‘excl’ of counter-examples. These are the prime
   * factors of the bases which are below the specified bound. They need to be
   * checked only when they have not already been ruled out by a previous test,
   * i.e. only if they are greater than the bound of the previous base set.
   * Useful counter-examples are flagged with (*!*); there are so few of them
   * that these lists are not used by the program (they are only documentary),
   * instead the required checks are hardcoded.
   * These counter-examples come from the fact that the Miller-Rabin test
   * assumes that the number n being tested does not divide the base b. This
   * always holds in the probabilistic test, (where we try random bases between
   * 2 and n−2), but not in this deterministic variant (where we test n against
   * fixed bases). When n divides b, the test always report that n is definitely
   * composite, even when it prime.
   * NOTE: This test assumes a 64-bit version of OCaml. Some of the constants
   * below exceed 2^30, so it won’t even compile with 32-bit OCaml. Here are
   * constants that work for 32-bit OCaml: *)
  (*! let bases1 = [ 921211727 ] in !*)
  (*! let _excl1 = [ (*!*)331 ] in !*)
  (*! let bound1 = 49141 in !*)
  (*! let bases2 = [ 11000544 ; 31481107 ] in !*)
  (*! let _excl2 = [ 2 ; 3 ; 7 ; 19 ; 163 ; 241 ; 18661 ] in !*)
  (*! let bound2 = 316349281 in !*)
  (*! let bases3 = [ 2 ; 7 ; 61 ] in !*)
  (*! let _excl3 = [ 2 ; 7 ; 61 ] in !*)
  (* Conversely, this test is not known to be deterministic for numbers greater
   * than 2^64. *)
  assert (Sys.int_size = 63) ;
  (* 1 base — does not fit in 63-bit integers: *)
  (*! let bases1 = [ 9345883071009581737 ] in !*)
  (*! let _excl1 = [ 47 ; 98207 ] in !*)
  (*! let bound1 = 341531 in !*)
  (* 1 base: *)
  let bases1 = [ 126401071349994536 ] in
  let _excl1 = [ 2 ] in
  let bound1 = 291831 in
  (* 2 bases: *)
  let bases2 = [ 336781006125 ; 9639812373923155 ] in
  let _excl2 = [ 3 ; 5 ; 131 ; (*!*)6855593 ] in
  let bound2 = 1050535501 in
  (* 3 bases — does not fit in 63-bit integers: *)
  (*! let bases3 = [ 4230279247111683200 ; 14694767155120705706 ; 16641139526367750375 ] in !*)
  (*! let _excl3 = [ 2 ; 3 ; 5 ; 19 ; 29 ; 277 ; 991 ; 1931 ; 14347 ; 14683 ; 246557 ; (*!*)3709689913 ] in !*)
  (*! let bound3 = 350269456337 in !*)
  let bases3 = [ 15 ; 7363882082 ; 992620450144556 ] in
  let _excl3 = [ 2 ; 3 ; 5 ; 101 ; 60679 ] in
  let bound3 = 273919523041 in
  (* 4 bases — does not fit in 63-bit integers: *)
  (*! let bases4 = [ 2 ; 141889084524735 ; 1199124725622454117 ; 11096072698276303650 ] in !*)
  (*! let _excl4 = [ 2 ; 3 ; 5 ; 11 ; 23 ; 127 ; 56197 ; 3075593 ; 322232233 ; 3721305949 ] in !*)
  (*! let bound4 = 55245642489451 in !*)
  (* 4 bases: *)
  let bases4 = [ 2 ; 2570940 ; 211991001 ; 3749873356 ] in
  let _excl4 = [ 2 ; 3 ; 5 ; 23 ; 181 ; 390407 ; 40759493 ] in
  let bound4 = 47636622961201 in
  (* 5 bases: *)
  let bases5 = [ 2 ; 4130806001517 ; 149795463772692060 ; 186635894390467037 ; 3967304179347715805 ] in
  let _excl5 = [ 2 ; 3 ; 5 ; 13 ; 29 ; 59 ; 79 ; 167 ; 62633 ; 299197 ; 2422837 ; 332721269 ; 560937673 ] in
  let bound5 = 7999252175582851 in
  (* 6 bases: *)
  let bases6 = [ 2 ; 123635709730000 ; 9233062284813009 ; 43835965440333360 ; 761179012939631437 ; 1263739024124850375 ] in
  let _excl6 = [ 2 ; 3 ; 5 ; 7 ; 13 ; 41 ; 61 ; 179 ; 1381 ; 30839 ; 157321 ; 385417 ; 627838711 ; 1212379867 ; 7985344259 ] in
  let bound6 = 585226005592931977 in
  (* 7 bases: *)
  let bases7 = [ 2 ; 325 ; 9375 ; 28178 ; 450775 ; 9780504 ; 1795265022 ] in
  let _excl7 = [ 2 ; 3 ; 5 ; 7 ; 13 ; 19 ; 73 ; 193 ; 407521 ; 299210837 ] in
fun ~first_primes n ->
  let n = abs n in
  begin match
    if n <= 1 then
      raise Composite ;
    (* These two tests are subsumed by the trial divisions below, as long as
     * [first_primes] contain 2. *)
    (*if n = 2 then
      raise Prime ;
    if n land 1 = 0 then
      raise Composite ;*)
    (* First, trial divisions (not necessary, but overall speeds up the
     * primality test by eliminating many composite numbers). *)
    let r = Arith.isqrt n in
    first_primes |> Array.iter begin fun p ->
      if r < p then
        raise Prime ;
      if n mod p = 0 then
        raise Composite ;
    end ;
    assert (n land 1 <> 0) ;
    (* Now the general Miller‐Rabin test for odd numbers. *)
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
  with
  | ()                  -> true  (* strong probable prime *)
  | exception Prime     -> true  (* definitely prime *)
  | exception Composite ->       (* definitely composite, unless n divided one of the bases *)
      n = 6_855_593 (* hardcoded counter-example (see above) *)
  end

(* The end‐user primality test uses trial divisions with all prime numbers below
 * 100. *)
let is_prime = is_prime_aux ~first_primes:primes_under_100

(******************************************************************************)

(* TODO: Use twisted Edwards curves instead of Weierstrass curves?
 *     https://en.wikipedia.org/wiki/Lenstra_elliptic-curve_factorization#Twisted_Edwards_curves
 *)

(* This functor implements elliptic curves whose equation is under the form
 *     y² = x³ + ax + b
 * over the ring ℤ∕nℤ (here, n = [M.modulo]). *)
module Make_EllipticCurve (M : sig val modulo : int end) = struct

  (* The ring ℤ∕nℤ. *)
  module M = Modular.Make (M)

  (* The type of a point of an elliptic curve. *)
  type point =
    | Infinity
    | Finite of M.t * M.t

  (* The addition of two points of an elliptic curve.
   * It cannot raise [Division_by_zero]. It can raise [Factor_found d] where [d]
   * is a non‐trivial factor of [M.modulo].
   * Note: The coefficient [b] is only useful for checking assertions. *)
  let add ~a ~b p q =
    begin match p, q with
    | Infinity, r  |  r, Infinity ->
        r
    | Finite (xp, yp), Finite (xq, yq) ->
        assert M.(yp*:yp = xp*:xp*:xp +: a*:xp +: b) ;
        assert M.(yq*:yq = xq*:xq*:xq +: a*:xq +: b) ;
        if xp <> xq then begin
          assert (xp <> xq) ;
          (* Note: xq−xp is never zero, hence either the division succeeds or a
           * non‐trivial factor is found. *)
          let s = M.((yq -: yp) *: inv_factorize (xq -: xp)) in
          let t = M.(yp -: s*:xp) in
          let xr = M.(s*:s -: xp -: xq) in
          let yr = M.(~-: s*:xr -: t) in
          Finite (xr, yr)
        end else if yp = yq && (yp:>int) <> 0 then begin
          assert (xp = xq && yp = yq && (yp:>int) <> 0) ;
          (* Note: 2yp is never zero (provided that M.modulo is odd), hence
           * either the division succeeds or a non‐trivial factor is found. *)
          let xp2 = M.(xp*:xp) in
          let s = M.((xp2 +: xp2 +: xp2 +: a) *: inv_factorize (yp +: yp)) in
          let t = M.(yp -: s*:xp) in
          let xr = M.(s*:s -: xp -: xq) in
          let yr = M.(~-: s*:xr -: t) in
          Finite (xr, yr)
        end else begin
          assert (xp = xq && yp = M.opp yq) ;
          Infinity
        end
    end

  (* The multiplication of a point [n] times.
   * Note: The coefficient [b] is only useful for checking assertions. *)
  let mul ~a ~b p n =
    Common.pow ~mult:(add ~a ~b) ~unit:Infinity p n

  (* Draw a random elliptic curve of equation y² = x³ + ax + b, plus a point
   * (x₀, y₀) on it. The curve is not degenerate (its discriminant is not zero).
   * Note: The coefficient [b] is only useful for checking assertions. *)
  let rec rand () =
    let a = M.rand () in
    let x0 = M.rand () in
    let y0 = M.rand () in
    let b = M.(y0*:y0 -: x0*:x0*:x0 -: a*:x0) in
    if (M.(of_int 4*:a*:a*:a +: of_int 27*:b*:b) :> int) = 0 then
      rand ()
    else
      (a, b, x0, y0)

end (* module Make_EllipticCurve *)

let default_number_of_tries = max_int
let default_max_fact = 160

(* Lenstra’s elliptic‐curve algorithm for finding a factor of [n].
 * @return a non‐trivial factor of [n].
 * @raise Not_found when no factor was found within the allowed time bounds
 * (which is highly unlikely with the default parameters).
 * @param tries The number of elliptic curves to try before resigning.
 * @param max_fact The “small exponents” tried by the algorithm are the
 * factorial numbers up to the factorial of [max_fact]. *)
let lenstra_find_factor ~tries ~max_fact n =
  let module EC = Make_EllipticCurve (struct let modulo = n end) in
  begin try
    for _ = 1 to tries do
      let (a, b, x0, y0) = EC.rand () in
      let p = ref (EC.Finite (x0, y0)) in
      (* Note: Short‐circuiting the case when [p] becomes ∞ is not useful,
       * because it is very rare. *)
      for k = 2 to max_fact do
        p := EC.mul ~a ~b !p k
      done
    done ;
    raise Not_found
  with Modular.Factor_found d ->
    d
  end

(* Given the prime factorization of two integers, returns the factorization of
 * their product. *)
let rec merge_factors li1 li2 =
  begin match li1, li2 with
  | [], li  |  li, [] ->
      li
  | (p1, k1) :: li1', (p2, k2) :: li2' ->
      if p1 = p2 then
        (p1, k1+k2) :: merge_factors li1' li2'
      else if p1 < p2 then
        (p1, k1) :: merge_factors li1' li2
      else
        (p2, k2) :: merge_factors li1 li2'
  end

(* The primality test we use for factorization.
 * Our factorization process first performs trial divisions with all numbers
 * below 10 000, so subsequent primality tests need not perform trial divisions
 * again. Moreover, we know that all numbers whose square root is less than
 * 10 007 (the smallest prime number that we did not ruled out) are prime. *)
let lenstra_is_prime n =
  n < 100_140_049 (* = 10_007² *)
  || is_prime_aux ~first_primes:[||] n

(* Factorization algorithm based on Lenstra’s factor searching. *)
let rec lenstra_factors ~tries ~max_fact n =
  assert (1 < n) ;
  if lenstra_is_prime n then
    [ (n, 1) ]
  else begin
    begin match lenstra_find_factor ~tries ~max_fact n with
    | d ->
        let (q, _r) = Arith.sdiv n d in
        assert (_r = 0 && d <> 1 && d <> n) ;
        merge_factors
          (lenstra_factors ~tries ~max_fact d)
          (lenstra_factors ~tries ~max_fact q)
        (* Note: Very often, d is prime, but not always (for example,
         * n = 3577522661351062530 often yields a non‐prime factor). *)
    | exception Not_found ->
        [ (~-n, 1) ]
    end
  end

let factors ?(tries=default_number_of_tries) ?(max_fact=default_max_fact) n =
  assert (0 < n) ;
  (* (1) Trial divisions. *)
  let factored = ref [] in
  let n = ref n in
  let r = ref (Arith.isqrt !n) in
  begin try
    primes_under_10_000 |> Array.iter begin fun p ->
      if !r < p then
        raise Not_found ;
      let (k, n') = Arith.valuation ~factor:p !n in
      if k <> 0 then begin
        factored := (p, k) :: !factored ;
        n := n' ;
        r := Arith.isqrt n' ;
      end
    end
  with Not_found ->
    if !n <> 1 then begin
      factored := (!n, 1) :: !factored ;
      n := 1 ;
    end
  end ;
  let n = !n in
  (* (2) Factorization using Lenstra algorithm. *)
  if n = 1 then
    List.rev !factored
  else
    List.rev_append !factored (lenstra_factors ~tries ~max_fact n)

(******************************************************************************)

let with_factors (f : factorization -> int -> 'a) :
  ?factors:factorization -> int -> 'a
=
  fun ?factors:opt_factors n ->
    assert (0 < n) ;
    let factors =
      begin match opt_factors with
      | None         -> factors n
      | Some factors -> factors
      end
    in
    f factors n

let eulerphi =
  with_factors @@ fun factors n ->
    List.fold_left (fun m (p, _) -> m / p * (p-1)) n factors

let eulerphi_from_file nmax =
  assert (0 <= nmax && nmax <= 1_000_000) ;
  let phi = Array.make (nmax+1) 0 in
  let file = Scanf.Scanning.open_in "data/eulerphi-under-1_000_000.data" in
  for i' = 1 to nmax do
    (* "%_1[\r]@\n" is a format trick that matches \n, \r\n and end-of-file. *)
    Scanf.bscanf file "φ(%u) = %u%_1[\r]@\n" @@fun i phi_i ->
    assert (i = i') ;
    phi.(i) <- phi_i
  done ;
  Scanf.Scanning.close_in file ;
  phi

let number_of_divisors =
  with_factors @@ fun factors _ ->
    List.fold_left (fun m (_, k) -> m * (k+1)) 1 factors

let divisors =
  with_factors @@ fun factors _ ->
    let divisors = ref [] in
    let rec aux factors d =
      begin match factors with
      | [] ->
          divisors := d :: !divisors
      | (p, k) :: factors' ->
          let d = ref d in
          for _ = 0 to k do
            aux factors' !d ;
            d := !d * p ;
          done
      end
    in
    aux factors 1 ;
    List.sort (-) !divisors

type incremental_divisor = {
  divisor : int ;
  remaining_factors : factorization ;
}

module DivisorHeap =
  CCHeap.Make (struct
    type t = incremental_divisor
    let leq = (<=)
  end)

let gen_divisor_pairs =
  with_factors @@ fun factors n ->
    let r = Arith.isqrt n in
    let h = ref @@ DivisorHeap.add DivisorHeap.empty
                     { divisor = 1 ; remaining_factors = factors } in
    let rec augment_divisor_with_factors d factors =
      begin match factors with
      | [] ->
          ()
      | (p, k) :: factors' ->
          let d' = d * p in
          if d' <= r then begin
            let remaining_factors =
              if k = 1 then factors' else (p, k-1) :: factors' in
            h := DivisorHeap.add !h { divisor = d' ; remaining_factors } ;
            augment_divisor_with_factors d factors'
          end
      end
    in
    let rec gen () =
      begin match DivisorHeap.take !h with
      | None ->
          Seq.Nil
      | Some (h', x) ->
          h := h' ;
          if x.divisor = r then begin
            assert (x.divisor * x.divisor = n) ;
            Seq.Cons ((x.divisor, x.divisor), Seq.empty)
          end else begin
            assert (x.divisor < r) ;
            augment_divisor_with_factors x.divisor x.remaining_factors ;
            Seq.Cons ((x.divisor, n / x.divisor), gen)
          end
      end
    in
    gen

let divisor_pairs ?factors n =
  List.of_seq (gen_divisor_pairs ?factors n)
