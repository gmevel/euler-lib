(* TODO: Below is a solver for Pell equations i.e. integer equations of the form:
 *
 *     x² - d·y² = 1
 *
 * where [d] is a parameter and [x, y] are the unknown. It is implemented with
 * zarith. Adapt this code, clean it and integrate it into our euler lib.
 *
 * See e.g. https://github.com/danaj/Math-Prime-Util/issues/30 for example
 * implementations for a more general problem.
 *
 * NOTE: The Pell or Pell-Fermat equations would more accurately be named as the
 * Fermat-Brouncker-Lagrange equations (after European mathematicians) or the
 * Brahmagupta-Bhaskara equations (after the Indian mathematicians who studied
 * and solved the problem many centuries before the Europeans). For clarity and
 * brevity, this module uses the most conventional name.
 *
 *)

(*
 * Naive algorithm (for reference)
 *)

module Naive = struct

let solve d =
  if Z.perfect_square d then Z.(one, zero) else begin
    let y = ref Z.one in
    while not @@ Z.perfect_square Z.(d * pow !y 2 + one) do
      y := Z.succ !y
    done ;
    Z.sqrt Z.(d * pow !y 2 + one), !y
  end

end (* module Naive *)

(*
 * Chakravala method
 *
 *  https://fr.wikipedia.org/wiki/Équation_de_Pell-Fermat
 *  https://fr.wikipedia.org/wiki/Méthode_chakravala
 *)

module Chakravala = struct

(* retourne l’entier m qui vérifie le prédicat donné et minimise m² - n *)
let search_closest n predicate =
  let rec search r dr s ds =
    if Z.(sign r >= 0 && dr < ds) then begin
      if predicate r then r else
      let r = Z.pred r in
      (*let dr = Z.( n - r ** 2 ) in*)
      let dr = Z.( dr + ~$2 * r + one ) in
      search r dr s ds
    end
    else begin
      if predicate s then s else
      let s = Z.succ s in
      (*let ds = Z.( s ** 2 - n ) in*)
      let ds = Z.( ds + ~$2 * s - one ) in
      search r dr s ds
    end
  in
  let r, dr = Z.sqrt_rem n in
  let s = Z.succ r in
  (*let ds = Z.( s ** 2 - n ) in*)
  let ds = Z.( ~$2 * r + one - dr ) in
  search r dr s ds

let norm n (a, b) =
  Z.( a ** 2 - n * b ** 2 )

let mult n (a, b) (c, d) =
  Z.( a * c + b * d * n,  a * d + b * c )

let div n (a, b) q =
  Z.( a /| q,  b /| q )

let print n out (a, b) =
    Printf.printf "%a + %a √%a" Z.output a Z.output b Z.output n

let chakravala n =
  let ( *** ) = mult n
  and ( /// ) = div  n
  (*and pp      = print n*) in
  let rec step alpha k m0 =
    let m = search_closest n (fun m -> Z.( equal ((m0 + m) mod k) zero )) in
    (*let a', b' = Z.( (a * m + b * n) /| k,  (a + b * m) /| k ) in*)
    let alpha' = (m, Z.one) *** alpha /// k in
    (*Printf.printf "m = %a\n" Z.output m ;
    Printf.printf "α = %a\n" pp alpha' ;*)
    let sk' = norm n alpha' in
    let k' = Z.abs sk' in
    (* prend α de norme ±1, retourne α s’il est solution (cas +1),
     * α² sinon (cas −1) *)
    let square_if_needed alpha =
      if Z.sign sk' = 1 then alpha else alpha *** alpha
    in
    (* ±1: *)
    if Z.( equal k' one ) then
      square_if_needed alpha'
    (* ±2: *)
    else if Z.( equal k' ~$2 ) then
      alpha' *** alpha' /// Z.(~$2)
    (* ±4: *)
    else if Z.( equal k' ~$4 ) then begin
      if Z.is_even (fst alpha') && Z.is_even (snd alpha') then
        square_if_needed @@ alpha' /// Z.(~$2)
      else if Z.is_even n then
        alpha' *** alpha' /// Z.(~$4)
      else
        square_if_needed @@ alpha' *** alpha' *** alpha' /// Z.(~$8)
    end
    else
      step alpha' k' m
  in
  let alpha = Z.(one, zero) (* α_i = a_i + b_i √n *)
  and k = Z.one             (* k_i = |norme(α_i)| *)
  and m0 = Z.zero in        (* m_{i-1} (m_{-1} est débile) *)
  step alpha k m0

let solve d =
  if Z.perfect_square d then
    Z.(one, zero)
  else
    chakravala d

end (* module Chakravala *)
