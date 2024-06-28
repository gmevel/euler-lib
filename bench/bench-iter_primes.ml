(******************************************************************************)

exception E of int

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

let test nmax =
  (*! let () = timed (Euler.Primes.iter_primes ~do_prime:(fun p -> assert (Euler.Primes.is_prime p))) nmax in !*)
  let () = timed (Euler.Primes.iter_primes ~do_prime:(fun _ -> ())) nmax in
(*
  let exception E of int in
  let exception F of int in
  begin try
  for i = 0 to min (Array.length s) (Array.length t) - 1 do
    if s.(i) > nmax || s.(i) = 0 || t.(i) > nmax || t.(i) = 0 then
      raise (F i)
    else if s.(i) <> t.(i) then
      raise (E i)
  done
  with F i ->
    Printf.printf "number of primes = %u\n" i
  end
*)
  ()

let () =
  test ((1 lsl 36) - 1)
