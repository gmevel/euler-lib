(*
 *  Useful functions
 *)

let valuation_of_2 =
  let values128 = "\007\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\006\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\005\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000\004\000\001\000\002\000\001\000\003\000\001\000\002\000\001\000" in
fun n ->
  let k = ref 0 in
  let m = ref n in
  while !m land 255 = 0 do
    k := !k + 8 ;
    m := !m asr 8 ;
  done ;
  let k = !k + (Char.code @@ String.unsafe_get values128 (!m land 127)) in
  (k, n asr k)

let min a b =
  let d = b - a in (* overflow is correctly dealt with *)
  let s = a lxor b in
  let r = (s land b) lor (lnot s land d) in
  a + (d land (r asr Sys.int_size))

let max a b =
  let d = b - a in (* overflow is correctly dealt with *)
  let s = a lxor b in
  let r = (s land b) lor (lnot s land d) in
  b - (d land (r asr Sys.int_size))

let min_and_max a b =
  let d = b - a in (* overflow is correctly dealt with *)
  let s = a lxor b in
  let r = (s land b) lor (lnot s land d) in
  let d' = (d land (r asr Sys.int_size)) in
  (a + d', b - d')

let abs n =
  let u = n asr Sys.int_size in
  n lxor u - u

(*
 * GCD
 *)

let rec gcd1 a b =
  if b = 0 then
    abs a
  else
    gcd1 b (a mod b)

let gcd2f =
  let rec gcd_aux a b =
    let d = a - b in
    if d = 0 then
      a
    else begin
      let (_, c) = valuation_of_2 d in
      if c >= b then
        gcd_aux c b
      else
        gcd_aux b c
    end
  in
fun a b ->
  let (ka, a) = valuation_of_2 (abs a)
  and (kb, b) = valuation_of_2 (abs b) in
  let k = min ka kb in
  let (b, a) = min_and_max a b in
  (gcd_aux a b) lsl k

let gcd2i a b =
  let (ka, a) = valuation_of_2 (abs a)
  and (kb, b) = valuation_of_2 (abs b) in
  let k = min ka kb in
  let (b, a) = min_and_max a b in
  let ra = ref a
  and rb = ref b in
  (* I don’t know why, but this is SLOWER: *)
  (*! let ra = ref (max a b) !*)
  (*! and rb = ref (min a b) in !*)
  while !rb <> !ra do
    let (_, c) = valuation_of_2 (!ra - !rb) in
    if c >= !rb then
      ra := c
    else begin
      ra := !rb ;
      rb := c
    end
  done ;
  !ra lsl k

let gcd3f =
  let rec gcd_aux a b =
    let d = a - b in
    if d = 0 then
      a
    else begin
      let (_, c) = valuation_of_2 d in
      let (c, b) = min_and_max b c in
      gcd_aux b c
    end
  in
fun a b ->
  let (ka, a) = valuation_of_2 (abs a)
  and (kb, b) = valuation_of_2 (abs b) in
  let k = min ka kb in
  let (b, a) = min_and_max a b in
  (gcd_aux a b) lsl k

let gcd3i a b =
  let (ka, a) = valuation_of_2 (abs a)
  and (kb, b) = valuation_of_2 (abs b) in
  let k = min ka kb in
  let (b, a) = min_and_max a b in
  let ra = ref a
  and rb = ref b in
  while !rb <> !ra do
    let (_, c) = valuation_of_2 (!ra - !rb) in
    let (c, b) = min_and_max !rb c in
    ra := b ;
    rb := c ;
  done ;
  !ra lsl k

let gcd4f =
  let rec gcd_aux a b =
    let d = a - b in
    if d = 0 then
      a
    else begin
      let (_, c) = valuation_of_2 (abs d) in
      gcd_aux b c
    end
  in
fun a b ->
  let (ka, a) = valuation_of_2 (abs a)
  and (kb, b) = valuation_of_2 (abs b) in
  let k = min ka kb in
  (gcd_aux a b) lsl k

let gcd4i a b =
  let (ka, a) = valuation_of_2 (abs a)
  and (kb, b) = valuation_of_2 (abs b) in
  let k = min ka kb in
  let ra = ref a
  and rb = ref b in
  while !rb <> !ra do
    let (_, c) = valuation_of_2 (abs (!ra - !rb)) in
    ra := !rb ;
    rb := c ;
  done ;
  !ra lsl k

(*
 * TESTS
 *)

let timed ?msg f x =
  begin match msg with
  | None -> ()
  | Some msg -> Printf.printf "%s %!" msg
  end ;
  Gc.compact () ;
  let t0 = Sys.time () in
  let y = f x in
  Printf.printf "--- %.3g s\n%!" (100. *. (Sys.time () -. t0)) ;
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

let test ~rounds ~min ~max =
  Gc.compact () ;
  let data = Array.init rounds (fun _ -> Euler.Arith.rand ~min ~max ()) in
  for i = 0 to rounds-2 do
    let x1 = data.(i) in
    let x2 = data.(i+1) in
    let y1 = gcd1 x1 x2 in
    let y2f = gcd2f x1 x2 in
    let y2i = gcd2i x1 x2 in
    let y3f = gcd3f x1 x2 in
    let y3i = gcd3i x1 x2 in
    let y4f = gcd4f x1 x2 in
    let y4i = gcd4i x1 x2 in
    assert (y1 = y2f) ;
    assert (y1 = y2i) ;
    assert (y1 = y3f) ;
    assert (y1 = y3i) ;
    assert (y1 = y4f) ;
    assert (y1 = y4i) ;
  done ;
  timed ~msg:"gcd1" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = gcd1 data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"gcd2f" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = gcd2f data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"gcd2i" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = gcd2i data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"gcd3f" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = gcd3f data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"gcd3i" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = gcd3i data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"gcd4f" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = gcd4f data.(i) data.(i+1) in ()
    done
  end () ;
  timed ~msg:"gcd4i" begin fun () ->
    for i = 0 to rounds-2 do
      let _y = gcd4i data.(i) data.(i+1) in ()
    done
  end () ;
  ()

let () =
  Random.self_init () ;
  Printf.printf "integers with 62-bit magnitude:\n" ;
  test ~rounds:(1 lsl 22) ~min:Euler.Arith.min_int ~max:max_int ;
