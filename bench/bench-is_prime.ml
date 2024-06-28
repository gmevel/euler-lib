(* compile and run with:
 *
 *     dune build
 *     dune install
 *     ocamlfind ocamlopt -package euler -linkpkg bench/bench-is_prime.ml && ./a.out
 *
 * or:
 *
 *     # add a rule in file `dune`, then:
 *     dune build bench && _build/default/bench/bench-is_prime.exe
 *
 *)

(*
 * TESTS
 *)

let timed ?msg f x =
  begin match msg with
  | None -> ()
  | Some msg -> Printf.printf "  · %s %!" msg
  end ;
  Gc.compact () ;
  let t0 = Sys.time () in
  let y = f x in
  Printf.printf "--- %.3g s\n%!" (Sys.time () -. t0) ;
  y

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

let rec rand_uniform ~max () =
  (* we draw numbers that are not multiple of 2 nor 3: *)
  let r = Euler.Arith.rand ~min:4 ~max () in
  if r land 1 = 0 || r mod 3 = 0
  then rand_uniform ~max ()
  else r

let rec rand_nosmallfactor100 ~max () =
  let r = Euler.Arith.rand ~min:101 ~max () in
  if Array.exists (fun p -> r mod p = 0) Euler.Primes.primes_under_100
  then rand_nosmallfactor100 ~max ()
  else r

let rec rand_nosmallfactor10000 ~max () =
  let r = Euler.Arith.rand ~min:10_007 ~max () in
  if Array.exists (fun p -> r mod p = 0) Euler.Primes.primes_under_10_000
  then rand_nosmallfactor10000 ~max ()
  else r

let test ~rounds ~rand =
  Gc.compact () ;
  let data = Array.init rounds (fun _ -> rand ()) in
  Printf.printf "  functional test...\n%!" ;
  for i = 0 to rounds-1 do
    let x1 = data.(i) in
    let y1 = Euler__Primes.is_prime x1 in
    let y2 = Euler__Primes.is_prime__using_bundles100_manual x1 in
    let y3 = Euler__Primes.is_prime__using_bundles100 x1 in
    let y4 = Euler__Primes.is_prime__using_bundles10000 x1 in
    assert (y1 = y2 || (Printf.printf "%i: %b <> %b\n%!" x1 y1 y2 ; false)) ;
    assert (y1 = y3 || (Printf.printf "%i: %b <> %b\n%!" x1 y1 y3 ; false)) ;
    assert (y1 = y4 || (Printf.printf "%i: %b <> %b\n%!" x1 y1 y4 ; false)) ;
  done ;
  Printf.printf "  performance test...\n%!" ;
  timed ~msg:"is_prime__trial100" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = Euler.Primes.is_prime data.(i) in ()
    done
  end () ;
  timed ~msg:"is_prime__using_bundles100_manual" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = Euler__Primes.is_prime__using_bundles100_manual data.(i) in ()
    done
  end () ;
  timed ~msg:"is_prime__using_bundles100" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = Euler__Primes.is_prime__using_bundles100 data.(i) in ()
    done
  end () ;
  timed ~msg:"is_prime__trial10000" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = Euler__Primes.is_prime__trial10000 data.(i) in ()
    done
  end () ;
  timed ~msg:"is_prime__using_bundles10000" begin fun () ->
    for i = 0 to rounds-1 do
      let _y = Euler__Primes.is_prime__using_bundles10000 data.(i) in ()
    done
  end () ;
  ()

let () =
  Random.self_init () ;
  Printf.printf "integers up to 100^2:\n%!" ;
  test ~rounds:(1 lsl 18) ~rand:(rand_uniform ~max:10_000) ;
  Printf.printf "integers up to 10_000^2:\n%!" ;
  test ~rounds:(1 lsl 18) ~rand:(rand_uniform ~max:100_000_000) ;
  Printf.printf "integers up to 10_000^2, no factor < 100:\n%!" ;
  test ~rounds:(1 lsl 18) ~rand:(rand_nosmallfactor100 ~max:100_000_000) ;
  Printf.printf "integers up to 10_000^2, no factor < 10_000:\n%!" ;
  test ~rounds:(1 lsl 18) ~rand:(rand_nosmallfactor10000 ~max:100_000_000) ;
  Printf.printf "integers up to max_int:\n%!" ;
  test ~rounds:(1 lsl 18) ~rand:(rand_uniform ~max:max_int) ;
  Printf.printf "integers up to max_int, no factor < 100:\n%!" ;
  test ~rounds:(1 lsl 18) ~rand:(rand_nosmallfactor100 ~max:max_int) ;
  Printf.printf "integers up to max_int, no factor < 10_000:\n%!" ;
  test ~rounds:(1 lsl 18) ~rand:(rand_nosmallfactor10000 ~max:max_int) ;
