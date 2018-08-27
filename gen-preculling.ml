(* This program pre‐computes values for pre‐culling small primes in a prime
 * sieve algorithm, and put them in an OCaml source file.
 * The result is used by [Primality.ml]. See there for more detail.
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
let first_primes : int array =
  let count = ref 0 in
  while first_primes.(!count) <= pmax do incr count done ;
  Array.sub first_primes 0 !count

(* Their count. *)
let number_of_primes : int = Array.length first_primes

(* Their product. *)
let round_cardinal : int = Array.fold_left ( * ) 1 first_primes

(* Their Euler’s totient. *)
let phi : int = Array.fold_left (fun phi p -> phi*(p-1)) 1 first_primes

(* The numbers which are coprime with all pre‐culled primes. *)
let coprimes : int array =
  List.init round_cardinal (fun n -> n)
  |> List.filter begin fun n ->
        Array.for_all (fun p -> n mod p <> 0) first_primes
      end
  |> Array.of_list

(* The differences between successive coprime numbers, divided by 2.
 * Stored in a string to save space. *)
let half_increments : string =
  String.init phi begin fun i ->
    let inc =
      if i = 0 then 2
      else coprimes.(i) - coprimes.(i-1)
    in
    Char.chr (inc lsr 1)
  end

let () =
  let out = open_out Sys.argv.(1) in
  Printf.fprintf out "let number_of_primes = %u\n\n" number_of_primes ;
  Printf.fprintf out "let round_cardinal = %u\n\n" round_cardinal ;
  Printf.fprintf out "let half_increments = %S\n\n" half_increments ;
  close_out out
