(* Note: We must take care of avoiding overflows in the underlying integer
 * operations. For instance, the modular multiplication is not as simple as
 * [(a * b) mod m] (it is when [m]² < [max_int]).
 *     https://www.quora.com/How-can-I-execute-A-*-B-mod-C-without-overflow-if-A-and-B-are-lesser-than-C/answer/Dana-Jacobsen
 *     https://en.wikipedia.org/wiki/Modular_arithmetic#Example_implementations
 *)

let sqrt_max_int = 1 lsl ((Sys.int_size - 1) / 2)

(* User-facing functions perform domain checks before calling internal
 * functions. Internal functions are prefixed with an underscore. *)

let _add ~modulo:m a b =
  let m_b = m - b in
  if a < m_b then
    a + b
  else
    a - m_b
let[@inline] add ~modulo:m a b =
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  _add ~modulo:m a b

let _opp ~modulo:m a =
  if a = 0 then
    0
  else
    m - a
let[@inline] opp ~modulo:m a =
  assert (0 <= a && a < m) ;
  _opp ~modulo:m a

let _sub ~modulo:m a b =
  if a >= b then
    a - b
  else
    a + (m - b)
let[@inline] sub ~modulo:m a b =
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  _sub ~modulo:m a b

let[@inline] _mul ~modulo:m a b =
  Arith._modular_mul ~modulo:m a b
let[@inline] mul ~modulo:m a b =
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  _mul ~modulo:m a b

(* We compute the modular inverse using an extended Euclidean algorithm.
 * [_modular_gcdext ~modulo:m b] returns a pair [(d, v)] where [1 ≤ d ≤ m] is
 * the GCD of [m] and [b], and [0 ≤ v < m] is such that [d = v·b  (mod m)].
 * When [b = 0], it returns [d = m]. Such a [v] is defined modulo [m/d]. *)
let[@inline] _gcdext ~modulo:m b =
  Arith._modular_gcdext ~modulo:m b

let _inv ~modulo:m b =
  let (d, v) = _gcdext ~modulo:m b in
  if d = 1 then
    v
  else
    raise Division_by_zero
let[@inline] inv ~modulo:m b =
  assert (0 <= b && b < m) ;
  _inv ~modulo:m b

let _div ~modulo:m a b =
  _mul ~modulo:m a (_inv ~modulo:m b)
let[@inline] div ~modulo:m a b =
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  _div ~modulo:m a b

let _div_nonunique ~modulo:m a b =
  let (d, v) = _gcdext ~modulo:m b in
  let (a', r) = Arith.sdiv a d in
  if r = 0 then
    _mul ~modulo:m a' v
  else
    raise Division_by_zero
let[@inline] div_nonunique ~modulo:m a b =
  assert (0 <= a && a < m) ;
  assert (0 <= b && b < m) ;
  _div_nonunique ~modulo:m a b

exception Factor_found of int

(*
let inv_factorize ~modulo:m b =
  begin try
    inv ~modulo:m b
  with Division_by_zero when b <> 0 ->
    let d = Arith.gcd m b in
    raise (Factor_found d)
  end
*)
let _inv_factorize ~modulo:m b =
  let (d, v) = _gcdext ~modulo:m b in
  if d = 1 then
    v
  else if d = m then
    raise Division_by_zero
  else
    raise (Factor_found d)
let[@inline] inv_factorize ~modulo:m b =
  assert (0 <= b && b < m) ;
  _inv_factorize ~modulo:m b

let _pow ~modulo:m =
  (* For [m] = 1, [Common.pow] would not produce canonical values: *)
  if m = 1 then
    fun _a _n -> 0
  else
    fun a n ->
      if 0 <= n then
        Common.pow ~mult:(_mul ~modulo:m) ~unit:1 a n
      else
        Common.pow ~mult:(_mul ~modulo:m) ~unit:1 (_inv ~modulo:m a) ~-n
let[@inline] pow ~modulo:m a n =
  assert (0 <= a && a < m) ;
  assert (n <> Stdlib.min_int) ;
  _pow ~modulo:m a n

let _rand ~modulo:m () =
  Arith.rand ~max:(m-1) ()
let[@inline] rand ~modulo:m () =
  assert (0 < m) ;
  _rand ~modulo:m ()

(******************************************************************************)

module Make (M : sig val modulo : int end) = struct

  let () =
    assert (M.modulo <> 0) ;
    assert (M.modulo <> Stdlib.min_int)

  let modulo = abs M.modulo

  type t = int

  let of_int a =
    Arith.erem a modulo
  let ( !: ) = of_int

  let to_int a =
    a

  let opp = _opp ~modulo
  let ( ~-: ) = opp

  let inv = _inv ~modulo
  let ( ~/: ) = inv

  let ( +: ) = _add ~modulo

  let ( -: ) = _sub ~modulo

  let ( *: ) = _mul ~modulo

  let ( /: ) = _div ~modulo

  let ( //: ) = _div_nonunique ~modulo

  let inv_factorize = _inv_factorize ~modulo

  let pow = _pow ~modulo
  let ( **: ) = pow

  let rand = _rand ~modulo

  let ( ~-:. ) a = ~-: !:a
  let ( ~/:. ) a = ~/: !:a
  let ( +:. ) a b = a +: !:b
  let ( +.: ) a b = !:a +: b
  let ( +.. ) a b = !:a +: !:b
  let ( -:. ) a b = a -: !:b
  let ( -.: ) a b = !:a -: b
  let ( -.. ) a b = !:a -: !:b
  let ( *.: ) a b = !:a *: b
  let ( *:. ) a b = a *: !:b
  let ( *.. ) a b = !:a *: !:b
  let ( /.: ) a b = !:a /: b
  let ( /:. ) a b = a /: !:b
  let ( /.. ) a b = !:a /: !:b
  let ( //.: ) a b = !:a //: b
  let ( //:. ) a b = a //: !:b
  let ( //.. ) a b = !:a //: !:b
  let ( **.: ) a b = !:a **: b

end



(* tests *)
(* FIXME: Use an actual tool for unit tests. *)
let () =
  assert (mul ~modulo:max_int (max_int - 7) 2 = (max_int - 14)) ;
  assert (inv ~modulo:max_int (max_int-1) = (max_int-1)) ;
  assert (mul ~modulo:(max_int - 1) (max_int - 3) (max_int - 7) = 12) ;
  assert (inv ~modulo:42 37 = 25) ;
  ()
