(* This helper program finds, for each given base, a rational coefficient that,
 * when multiplied by [uint_size], gives the maximal exponent to which the base
 * can be raised before overflowing.
 *
 * The coefficient works simultaneously for [uint_size] = 30, 31, 62 and 63.
 * The target maximal exponents are given as inputs to [find_coeff].
 *)

let find_coeff desc (a,b,c,d) =
  let exception Break in
  try
    let found0 = ref false in
    let found1 = ref false in
    for q = 1 to 256 do
      for p = 1 to 256 do
        if not !found0
        && 30 * p / q = a
        && 31 * p / q = b
        && 62 * p / q = c
        && 63 * p / q = d then begin
          Printf.printf "%s: (s−1) %i / %i\n%!" desc p q ;
          found0 := true
        end ;
        if not !found1
        && 30 * p / q - 1 = a
        && 31 * p / q - 1 = b
        && 62 * p / q - 1 = c
        && 63 * p / q - 1 = d then begin
          Printf.printf "%s: (s−1) %i / %i − 1\n%!" desc p q ;
          found1 := true
        end
      done
    done
  with Break -> ()

let () =
  find_coeff "for 3" (18,19,39,39) ;
  find_coeff "for 5" (12,13,26,27) ;
  find_coeff "for 6" (11,11,23,24) ;
  find_coeff "for 7" (10,11,22,22) ;
  find_coeff "for 9" (9,9,19,19) ;
  find_coeff "for 10" (9,9,18,18) ;
  find_coeff "for 60" (5,5,10,10) ;
  ()
