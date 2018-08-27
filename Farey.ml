(***
 *** suite de Farey (fractions réduites entre 0 et 1 de dénominateur borné)
 ***
 *** https://en.wikipedia.org/wiki/Farey_sequence
 *** voir #073
 ***)

(* https://en.wikipedia.org/wiki/Farey_sequence *)

(* the Farey sequence of order dmax is the ordered sequence of irreducible
 * fractions between 0 and 1, included, whose denominator is at most dmax.
 * fractions are pairs (a,b) of coprime integers with 0 ⩽ a < b. *)

type t = int * int

(* the Farey sequence is symmetric (if the fraction (a, b) is irreducible, then
 * (b−a, b) too), so we can easily change the direction. *)
let sym (a, b) =
  (b-a, b)

(* THEOREM 1:
 * the fractions (a, b) and (c, d) are adjacent in the Farey sequence of order
 * dmax if and only if:
 *   (1)  b, d ⩽ dmax < b+d
 *   (2)  b×c − a×d = 1
 * at order b+d, the fractions (a, b), (a+c, b+d), (c, d) are adjacent.
 * the middle fraction is called the mediant (in this case, it is guaranteed to
 * be in irreducible form).
 * so the terms of the Farey sequence of some order, between two given fractions
 * which are adjacent at some lower order, are obtained by inserting mediants
 * (this is how the Stern-Brocot tree is constructed).
 *
 * theorem 1 allows us to compute in O(1) the successor at order dmax of some
 * fraction (a, b), provided we know the successor (c0, d0) at some lower order.
 *)

let assert_adjacent_fractions dmax (a, b) (c, d) =
  assert ((a,b) = (0,1)  ||  0 < a && a < b && b <= dmax (*&& gcd a b = 1*)) ;
  assert ((c,d) = (1,1)  ||  0 < c && c < d && d <= dmax (*&& gcd c d = 1*)) ;
  assert (b*c - a*d = 1) ; (* implies gcd a b = 1 && gcd c d = 1 *)
  assert (dmax < b + d)

(* THEOREM 2:
 * for any three fractions (a, b), (c, d), (e, f) which are adjacent at order
 * dmax, (c, d) is equal to the mediant (a+e, b+f) of (a, b) and (e, f)
 * (however, that mediant is not always in irreducible form).
 *
 * theorem 2 allows us to compute in O(1) the successor at order dmax of two
 * adjacent fractions (a, b), (c, d); hence, we can iterate in O(n) = O(dmax²)
 * on the Farey sequence of order dmax, where n is the length of the sequence
 * (the length of the sequence is the sum of the totient function φ, is is
 * asymptotically equivalent to 3∕π² dmax²).
 *)

(* [iter_farey dmax f] iterates [f] on every fraction of the Farey sequence of
 * order [dmax]. the optional arguments [starting1] and [starting2] are two
 * adjacent fractions in the sequence, which are taken as the starting point
 * (instead of (0,1) and (1,dmax)). *)
let rec iter_farey dmax ?starting1 ?starting2 f =
  assert (dmax > 0) ;
  let rec loop (a, b) (c, d) =
    assert_adjacent_fractions dmax (a, b) (c, d) ;
    f (c, d) ;
    if d <> 1 then begin
      let k = (dmax + b) / d in
      loop (c, d) (k*c-a, k*d-b)
    end
  in
  let frac1, frac2 =
    begin match starting1, starting2 with
    | None, None ->
        (0, 1), (1, dmax)
    | Some frac1, None ->
        frac1, next_farey dmax frac1
    | None, Some frac2 ->
        prev_farey dmax frac2, frac2
    | Some frac1, Some frac2 ->
        assert_adjacent_fractions dmax frac1 frac2 ;
        frac1, frac2
    end
  in
  f frac1 ;
  loop frac1 frac2

(* [next_farey dmax frac] returns the successor fraction of [frac] in the Farey
 * sequence of order [dmax]. *)
and next_farey dmax (a, b) =
  assert ((a, b) <> (1,1)) ;
  (* compute naively the successor (c₀, d₀) of (a, b) at order b. *)
  let (c0, d0) =
    let exception Found of (int * int) in
    let found = ref false in
    begin match
      iter_farey b begin fun frac' ->
        if !found then
          raise (Found frac')
        else
          found := (frac' = (a, b))
      end
    with
    | exception Found frac' -> frac'
    | () -> assert false
    end
  in
  (* deduce the successor (c, d) of (a, b) at order dmax.
   * explanation: if (a, b) and (c₀, d₀) are adjacent at order b,
   * then (a, b) and (a + c₀, b + d₀) are adjacent at order b + d₀,
   * thus (a, b) and (2×a + c₀, 2×b + d₀) are adjacent at order 2×b + d₀,
   * and so on,
   * so that (a, b) and (k×a + c₀, k×b + d₀) are adjacent at order k×b + d₀,
   * and we take the greatest k such that the denominator is less than dmax. *)
  let k = (dmax - d0) / b in
  let c = k*a + c0 in
  let d = k*b + d0 in
  assert_adjacent_fractions dmax (a, b) (c, d) ;
  (c, d)

and prev_farey dmax frac =
  sym @@ next_farey dmax @@ sym frac

let rev_iter_farey dmax ?starting1 ?starting2 f =
  let starting1 =
    begin match starting1 with
    | Some frac1 -> Some (sym frac1)
    | None       -> None
    end
  and starting2 =
    begin match starting2 with
    | Some frac2 -> Some (sym frac2)
    | None       -> None
    end
  in
  iter_farey dmax ?starting1 ?starting2 (fun frac -> f @@ sym frac)

(* [list_farey dmax] returns the Farey sequence of order [dmax] as a list. *)
let list_farey dmax =
  let li = ref [] in
  iter_farey dmax (fun frac -> li := frac :: !li) ;
  List.rev !li

let () =
  assert (next_farey 8 (1, 3) = (3, 8)) ;
  assert (prev_farey 8 (3, 7) = (2, 5)) ;
  assert ( list_farey 8 =
    [(0, 1); (1, 8); (1, 7); (1, 6); (1, 5); (1, 4); (2, 7); (1, 3); (3, 8);
     (2, 5); (3, 7); (1, 2); (4, 7); (3, 5); (5, 8); (2, 3); (5, 7); (3, 4);
     (4, 5); (5, 6); (6, 7); (7, 8); (1, 1)]
  )
