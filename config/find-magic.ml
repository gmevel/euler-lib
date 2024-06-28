(* This program finds “magic” numbers, which yield perfect hashing functions
 * for powers of 2 (and optionally for predecessors of powers of 2). We use this
 * to implement the base-2 logarithm efficiently, see comments in [Arith.ml] for
 * an in-depth explanation. *)

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

exception Very_very_magic

let find_magic ~int_size ~hash_size =
  assert (0 < hash_size && hash_size <= int_size && int_size <= (1 lsl hash_size)) ;
  (* avoid overflows (+1 allows to search e.g. 64-bit magic numbers on 64-bit
   * OCaml, whose integers have only 63 bits; but then, the prefix must not
   * start with the bit 1 --- which I think is not a problem in practice): *)
  assert (int_size <= Sys.int_size + 1 && int_size <= 64) ;
  let mask = (1 lsl hash_size) - 1 in
  let shift_size = int_size - hash_size in
  let count_magic = ref 0 in
  let count_very_magic = ref 0 in
  let rec find_magic_number hashes_used ~h ~pref ~suff_size =
    if suff_size > 0 then begin
      (* build the magic number bit by bit, by using backtracking: *)
      let h0 = (h lsl 1) land mask in
      let h1 = h0 lor 1 in
      let pref0 = pref lsl 1 in
      let pref1 = pref0 lor 1 in
      (* we prefer 1 over 0: *)
      if not @@ List.mem h1 hashes_used then
        find_magic_number (h1 :: hashes_used) ~h:h1 ~pref:pref1 ~suff_size:(suff_size-1) ;
      if not @@ List.mem h0 hashes_used then
        find_magic_number (h0 :: hashes_used) ~h:h0 ~pref:pref0 ~suff_size:(suff_size-1) ;
    end
    else begin
      (* the number is built, it yields unique hashes for all the 2^i where
       *
       *     0 ≤ i ≤ int_size − hash_size
       *
       * now, check that the last few hashes it yields are also unique, i.e for
       *
       *     int_size − hash_size < i < int_size
       *)
      let exception Not_magic in
      begin try
        if h land 3 = 0 then raise Not_magic ;
        let h = ref h in
        for _ = 2 to hash_size do
          h := (!h lsl 1) land mask ;
          if List.mem !h hashes_used then
            raise Not_magic ;
        done ;
        (* the number is MAGIC, i.e. it yields unique hashes for all the 2^i *)
        let magic = pref in
        (*! Printf.printf "    %i-bit magic number: 0x%016X\n" int_size magic ; !*)
        incr count_magic ;
        (* test whether it is VERY MAGIC, i.e. it also yields unique hashes
         * for all the 2^i − 1 where 1 ≤ i < int_size *)
        let exception Not_very_magic in
        begin try
          let bitset = ref 0 in
          let magic64 = Int64.of_int magic in
          for i = 1 to int_size - 1 do
            (*! let h = ((magic lsl i) - magic) lsr shift_size in !*)  (* ← may overflow *)
            let h = Int64.(to_int @@ shift_right_logical (sub (shift_left magic64 i) magic64) shift_size) in
            let h = h land mask in
            let bit = (1 lsl h) in
            if !bitset land bit = 0 then
              bitset := !bitset lor bit
            else
              raise Not_very_magic ;
          done ;
          (* the number is very magic *)
          Printf.printf "    %i-bit very magic number: 0x%016X\n" int_size magic ;
          incr count_very_magic ;
          (* test whether it is VERY VERY MAGIC, i.e. it also yields a unique
           * hash for i = 0 (n = 2^0−1 = 0): *)
          if !bitset land 1 = 0 then begin
            (* the number is very very magic *)
            Printf.printf "    %i-bit very very magic number: 0x%016X\n%!" int_size magic ;
            (* we very happily stop if we ever find a very very magic number: *)
            raise Very_very_magic ;
          end
        with Not_very_magic -> () end ;
        (* report regular progress: *)
        if !count_magic land ((1 lsl 20) - 1) = 0 then
          Printf.printf "    I already found %i magic numbers, including %i very magic\n%!"
            !count_magic !count_very_magic ;
      with Not_magic -> () end ;
    end
  in
  for h = 0 to mask do
    Printf.printf "looking for magic with first %i bits: 0b%a...\n%!"
      hash_size (pp_binary ~len:hash_size) h ;
    find_magic_number [h] ~h:h ~pref:h ~suff_size:shift_size ;
  done

let () =
  find_magic ~int_size:32 ~hash_size:5 ;
  (*! find_magic ~int_size:64 ~hash_size:6 ; !*)
  ()
