exception No_solution

(* Solves the equation a·x ={m}= b.
 * Only assumes m ≠ 0.
 * Returns a residue in canonical form. *)
let solve_congruence (a, b, m) =
  assert (m <> 0) ;
  let m = Arith.abs m in
  let (d, inv_a', _) = Arith.gcdext a m in
  let (b', r) = Arith.ediv b d in
  if r <> 0 then
    raise No_solution ;
  let m' = m / d in
  let inv_a' = Arith.erem inv_a' m' in
  let b' = Arith.erem b' m' in
  let c = Modular.mul ~modulo:m' inv_a' b' in
  (c, m')

(* Solves the system { x ={m}= a, x ={n}= b }.
 * Assumes that m, n > 0 and that the residues a and b are in canonical forms.
 * Returns a residue in canonical form. *)
let solve_2_congruences (a, m) (b, n) =
  let (d, u, _) = Arith.gcdext m n in
  (* the subtraction b−a cannot overflow because both a and b are non-negative
   * (residue modulo m and n, respectively): *)
  let (c, r) = Arith.ediv (b - a) d in
  if r <> 0 then
    raise No_solution ;
  let p = Arith.mul (m / d) n in
  (* a is already canonical modulo m, hence also modulo p: *)
  (*! let a = Arith.erem a p in !*)
  (* Since |c| < max(m, n) ≤ p, the canonical form of c modulo p is c if c ≥ 0,
   * otherwise it is c+p, hence we can avoid computing a division: *)
  (*! let c = Arith.erem c p in !*)
  let c = c + (p land (c asr Sys.int_size)) in
  let u = Arith.erem u p in
  (* since 0 ≤ m ≤ p, the canonical form of m modulo p is 0 if m = p, and
   * m otherwise, hence we can avoid computing a division: *)
  (*! let m = Arith.erem m p in !*)
  if m <> p then
    let x = Modular.(add ~modulo:p a (mul ~modulo:p c @@ mul ~modulo:p u m)) in
    (x, p)
  else
    (a, p)

let solve_congruences li =
  li
  |> List.map solve_congruence
  |> List.fold_left solve_2_congruences (0, 1)

(* tests *)
let () =
  assert ((23, 105) = solve_congruences [(1, 2, 3); (1, 3, 5); (1, 2, 7)]) ;
  assert ((785, 1122) = solve_congruences [(1, 3, 17); (1, 4, 11); (1, 5, 6)]) ;
  assert ((11, 12) = solve_congruences [(1, 3, 4); (1, 5, 6)]) ;
  assert ((11, 12) = solve_congruences [(1, -1, 4); (1, -1, 6)]) ;
  ()
