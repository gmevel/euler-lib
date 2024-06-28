(* compile and run with:
 *
 *     dune build
 *     dune install
 *     ocamlfind ocamlopt -package euler -linkpkg bench/bench-sieves.ml && ./a.out
 *
 * or:
 *
 *     # add a rule in file `dune`, then:
 *     dune build bench && _build/default/bench/bench-sieves.exe
 *
 *)

(*
 * TESTS
 *)

let fun_test nmax =
  Printf.printf "functional test ...\n%!" ;
  let seq = ref @@ Euler.Primes.gen_primes nmax in
  Euler.Primes.iter_primes nmax ~do_prime:begin fun p ->
    begin match !seq () with
    | Seq.Nil ->
        assert (p > nmax)
    | Seq.Cons (p', seq') ->
        assert (p = p') ;
        seq := seq' ;
    end
  end ;
  Printf.printf "... success\n%!"

let timed ?msg f x =
  begin match msg with
  | None -> ()
  | Some msg -> Printf.printf "%s %!" msg
  end ;
  Gc.compact () ;
  let t0 = Sys.time () in
  let y = f x in
  Printf.printf "--- %.3g s\n%!" (Sys.time () -. t0) ;
  y

let perf_test nmax =
  Printf.printf "performance test ...\n%!" ;
  let sumI =
    timed ~msg:"segmented imperative sieve with wheel" begin fun nmax ->
      let sum = ref 0 in
      Euler.Primes.iter_primes nmax ~do_prime:begin fun p ->
        sum := !sum + p
      end ;
      !sum
    end nmax ;
  in
  let sumF =
    timed ~msg:("incremental functional sieve with wheel") begin fun nmax ->
      Seq.fold_left (+) 0 (Euler.Primes.gen_primes nmax)
    end nmax
  in
  Printf.printf "sumI = %i\n" sumI ;
  Printf.printf "sumF = %i\n" sumF ;
  ()

let () =
  (* when nmax is small: *)
  fun_test 99 ;
  (* when nmax >= Primes.threshold_to_use_segmentation = 2^23 ~ 8_000_000 : *)
  fun_test 10_000_000 ;
  perf_test 10_000_000 ;
  (*! perf_test 50_000_000 ; !*)
  (*! perf_test 1_000_000_000 ; !*)
