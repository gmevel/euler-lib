(* Note: all functions here may assume that [m] is positive, and that arguments
 * [a] and [b] are under canonical form (that is, between 0 and [m] − 1).
 * They always return values under canonical form.
 *
 * TODO: not sure that [add_simple] and [sub_simple] are actually faster than
 *       [add] and [sub], make a benchmark.
 * TODO: check what happens for [min_int] (since [~-min_int = min_int]).
 * TODO: [pow ~modulo:1 a 0] currently returns 1 instead of 0.
 *
 * *)

let opp ~modulo:m =
  assert (0 < m) ;
fun a ->
  assert (0 <= a && a < m) ;
  if a = 0 then
    0
  else
    m - a

let inv ~modulo:m a =
  let (d, _, v) = Arith.gcdext m a in
  if d <> 1 then
    raise Division_by_zero
  else
    Arith.erem v m

(* let sqrt_max_int = truncate@@sqrt@@float max_int *)
let sqrt_max_int = 1 lsl ((Sys.word_size - 2) / 2)

let add_simple ~modulo:m =
  assert (0 < m && m - 1 <= max_int / 2) ;
fun a b ->
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  (a + b) mod m

let sub_simple ~modulo:m =
  assert (0 < m && m - 1 <= max_int / 2) ;
fun a b ->
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  (a - b) mod m

let mul_simple ~modulo:m =
  assert (0 < m && m - 1 <= sqrt_max_int) ;
fun a b ->
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  (a * b) mod m

let div_simple ~modulo:m =
  let mul = mul_simple ~modulo:m
  and inv = inv ~modulo:m in
fun a b ->
  mul a (inv b)

let pow_simple ~modulo:m =
  let pow = Common.pow ~mult:(mul_simple ~modulo:m) ~unit:1
  and inv = inv ~modulo:m in
fun a n ->
  if 0 <= n then
    pow a n
  else
    pow (inv a) ~-n

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
  if (a land b) < sqrt_max_int then
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
      if !rb mod 2 = 1 then
        res := add ~modulo:m !res !ra ;
      ra := add ~modulo:m !ra !ra ;
      rb := !rb / 2 ;
    done ;
    !res
  end

let div ~modulo:m =
  let mul = mul ~modulo:m
  and inv = inv ~modulo:m in
fun a b ->
  mul a (inv b)

let pow ~modulo:m =
  let pow = Common.pow ~mult:(mul ~modulo:m) ~unit:1
  and inv = inv ~modulo:m in
fun a n ->
  if 0 <= n then
    pow a n
  else
    pow (inv a) ~-n



module Make (M : sig val modulo : int end) = struct

  let m = abs M.modulo

  let () =
    assert (0 < m)

  type t = int

  let of_int a =
    Arith.erem a m

  let to_int a =
    a

  let opp = opp ~modulo:m
  let ( ~-: ) = opp

  let inv = inv ~modulo:m
  let ( ~/: ) = inv

  let ( +: ) =
    if m - 1 <= max_int / 2 then
      add_simple ~modulo:m
    else
      add ~modulo:m

  let ( -: ) =
    if m - 1 <= max_int / 2 then
      sub_simple ~modulo:m
    else
      sub ~modulo:m

  let ( *: ) =
    if m - 1 <= sqrt_max_int then
      mul_simple ~modulo:m
    else
      mul ~modulo:m

  let ( /: ) =
    if m - 1 <= sqrt_max_int then
      div_simple ~modulo:m
    else
      div ~modulo:m

  let pow =
    if m - 1 <= sqrt_max_int then
      pow_simple ~modulo:m
    else
      pow ~modulo:m

end



(* tests *)

let () =
  assert (mul ~modulo:(max_int - 1) (max_int - 3) (max_int - 7) = 12)
