(* Note: all functions here may assume that [m] is positive, and that arguments
 * [a] and [b] are under canonical form (that is, between 0 and [m] − 1).
 * They always return values under canonical form.
 *
 * TODO: check what happens for [min_int] (since [~-min_int = min_int]).
 * TODO: [pow ~modulo:1 a 0] currently returns 1 instead of 0.
 *
 * *)

(* let sqrt_max_int = truncate@@sqrt@@float max_int *)
let sqrt_max_int = 1 lsl ((Sys.word_size - 2) / 2)

(* overflow-free modular arithnetic
 *
 *     https://www.quora.com/How-can-I-execute-A-*-B-mod-C-without-overflow-if-A-and-B-are-lesser-than-C/answer/Dana-Jacobsen
 *     https://en.wikipedia.org/wiki/Modular_arithmetic#Example_implementations
 *)

let add ~modulo:m =
  assert (0 < m) ;
fun a b ->
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  let m_b = m - b in
  if a < m_b then
    a + b
  else
    a - m_b

let opp ~modulo:m =
  assert (0 < m) ;
fun a ->
  assert (0 <= a && a < m) ;
  if a = 0 then
    0
  else
    m - a

let sub ~modulo:m =
  assert (0 < m) ;
fun a b ->
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  if a >= b then
    a - b
  else
    a + (m - b)

let mul ~modulo:m =
  assert (0 < m) ;
fun a b ->
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  if (a lor b) < sqrt_max_int then
    (a * b) mod m
  else begin
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
        res := add ~modulo:m !res !ra ;
      ra := add ~modulo:m !ra !ra ;
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
  end

(* We compute the modular inverse using an extended Euclidean algorithm. We
 * reimplement the algorithm instead of using [Arith.gcdext] directly because
 * in the latter function Bézout’s coefficients can overflow, whereas we are
 * only interested in their class modulo [m]. *)
let inv ~modulo:m =
  let ( -: )  = sub ~modulo:m
  and ( *: )  = mul ~modulo:m in
  let rec gcdext a b u v x y =
    if b = 0 then begin
      if a <> 1 then
        raise Division_by_zero ;
      v
    end else begin
      let q = (a / b) mod m in
      gcdext b (a mod b) x y (u -: q *: x) (v -: q *: y)
    end
  in
fun b ->
  assert (0 <= b && b < m) ;
  gcdext m b 1 0 0 1

let div ~modulo:m =
  let mul = mul ~modulo:m
  and inv = inv ~modulo:m in
fun a b ->
  mul a (inv b)

(* Returns an element [x] such that [b]×[x] = [a], if there exists one.
 * It is unique only when [b] is inversible; in general, it is unique modulo [c]
 * where [c] = [m] ∕ gcd([m], [b]).
 * For example, modulo 10, 4×3 = 4×8 = 2, so 2 divided by 4 may be 3 or 8.
 * This example shows that the division 2 ∕ 4 cannot be simplified to 1 ∕ 2.
 * @raise Division_by_zero when there no such element. *)
let div_nonunique ~modulo:m =
  let ( -: )  = sub ~modulo:m
  and ( *: )  = mul ~modulo:m in
fun a b ->
  assert (0 <= b && b < m) ;
  assert (0 <= a && a < m) ;
  let dividend = a in
  let rec gcdext a b u v x y =
    if b = 0 then begin
      if dividend mod a <> 0 then
        raise Division_by_zero ;
      (dividend / a) *: v
    end else begin
      let q = (a / b) mod m in
      gcdext b (a mod b) x y (u -: q *: x) (v -: q *: y)
    end
  in
  gcdext m b 1 0 0 1

exception Factor_found of int

(* Like [div ~modulo:m a b], except that it raises Division_by_zero only when
 * [b] is zero. If [b] is not inversible but not zero, [d] = gcd([m], [b]) gives
 * a non‐trivial factor of [m]; in that case, this function raises
 * [Factor_found d]. *)
(*
let div_factorize ~modulo:m a b =
  begin try
    div ~modulo:m a b
  with Division_by_zero when b <> 0 ->
    let d = Arith.gcd m b in
    raise (Factor_found d)
  end
*)
let div_factorize ~modulo:m =
  let ( -: )  = sub ~modulo:m
  and ( *: )  = mul ~modulo:m in
  let rec gcdext a b u v x y =
    if b = 0 then begin
      if a <> 1 then
        raise (Factor_found a) ;
      v
    end else begin
      let q = (a / b) mod m in
      gcdext b (a mod b) x y (u -: q *: x) (v -: q *: y)
    end
  in
fun a b ->
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  if b = 0 then
    raise Division_by_zero
  else
    a *: gcdext m b 1 0 0 1

let pow ~modulo:m =
  let pow = Common.pow ~mult:(mul ~modulo:m) ~unit:1
  and inv = inv ~modulo:m in
fun a n ->
  if 0 <= n then
    pow a n
  else
    pow (inv a) ~-n

let rand ~modulo:m =
  assert (0 < m) ;
fun () ->
  Arith.rand m



module Make (M : sig val modulo : int end) = struct

  let modulo = abs M.modulo

  let () =
    assert (0 < modulo)

  type t = int

  let of_int a =
    Arith.erem a modulo

  let to_int a =
    a

  let opp = opp ~modulo
  let ( ~-: ) = opp

  let inv = inv ~modulo
  let ( ~/: ) = inv

  let ( +: ) = add ~modulo

  let ( -: ) = sub ~modulo

  let ( *: ) = mul ~modulo

  let ( /: ) = div ~modulo

  let ( //: ) = div_factorize ~modulo

  let pow = pow ~modulo

  let rand = rand ~modulo
end



(* tests *)
(* FIXME: Use an actual tool for unit tests. *)
let () =
  assert (mul ~modulo:max_int (max_int - 7) 2 = (max_int - 14)) ;
  assert (inv ~modulo:max_int (max_int-1) = (max_int-1)) ;
  assert (mul ~modulo:(max_int - 1) (max_int - 3) (max_int - 7) = 12) ;
  assert (inv ~modulo:42 37 = 25) ;
  ()
