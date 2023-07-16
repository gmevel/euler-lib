(* This program pre‐computes values for pre‐culling small primes in a prime
 * sieve algorithm, and put them in an OCaml source file.
 * The result is used by [Primes.ml]. See there for more detail.
 * The target filename must be given as first argument to this program.
 *)

(* The largest prime to pre‐cull.
 * This is the only parameter to adjust. Everything else is deduced from it. *)
let pmax = 17

(******************************************************************************)

let first_primes : int array =
  [|  2 ;  3 ;  5 ;  7 ; 11 ; 13 ; 17 ; 19 ; 23 ; 29 ; 31 ; 37 ; 41 ; 43 ; 47 ;
     53 ; 59 ; 61 ; 67 ; 71 ; 73 ; 79 ; 83 ; 89 ; 97 |]

(* The primes to pre‐cull. *)
let preculled_primes : int array =
  let count = ref 0 in
  while first_primes.(!count) <= pmax do incr count done ;
  Array.sub first_primes 0 !count

(* Their count. *)
let number_of_primes : int = Array.length preculled_primes

(* Their product. *)
let diameter : int = Array.fold_left ( * ) 1 preculled_primes

(* Their Euler’s totient. *)
let number_of_coprimes : int = Array.fold_left (fun phi p -> phi*(p-1)) 1 preculled_primes

(* The numbers which are coprime with all pre‐culled primes. *)
let coprimes : int array =
  List.init diameter (fun n -> n)
  |> List.filter begin fun n ->
        Array.for_all (fun p -> n mod p <> 0) preculled_primes
      end
  |> Array.of_list

(* The differences between successive coprime numbers, divided by 2.
 * Increments are small primes, we store them in a string to save space.
 * The first increment is 2 in order to step from [diameter]−1 to [diameter]+1
 * (recall that the ring of coprime residues is symmetric). *)
let half_increments : string =
  String.init number_of_coprimes begin fun i ->
    let inc =
      if i = 0 then 2
      else coprimes.(i) - coprimes.(i-1)
    in
    Char.chr (inc lsr 1)
  end

let () =
  assert (number_of_primes = Array.length preculled_primes) ;
  assert (number_of_coprimes = Array.length coprimes) ;
  assert (number_of_coprimes = String.length half_increments) ;
  let out = open_out Sys.argv.(1) in
  Printf.fprintf out "let preculled_primes = [|" ;
    Array.iter (Printf.fprintf out " %u ;") preculled_primes ;
    Printf.fprintf out " |]\n\n" ;
  Printf.fprintf out "let last_preculled_prime = %u\n\n" preculled_primes.(number_of_primes-1) ;
  Printf.fprintf out "let number_of_primes = %u\n\n" number_of_primes ;
  Printf.fprintf out "let number_of_coprimes = %u\n\n" number_of_coprimes ;
  Printf.fprintf out "let diameter = %u\n\n" diameter ;
  Printf.fprintf out "let half_increments = %S\n\n" half_increments ;
  close_out out
