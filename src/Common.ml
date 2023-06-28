(* This implementation avoids an unnecessary squaring (when used with
 * overflowing integers, this could provoke a spurious overflow). *)
let pow ~mult ~unit =
  let rec pow ~acc b n =
    if n land 1 = 0 then
      pow ~acc (mult b b) (n lsr 1)
    else if n = 1 then
      mult acc b
    else
      pow ~acc:(mult acc b) (mult b b) (n lsr 1)
  in
fun b n ->
  assert (n >= 0) ;
  if n = 0 then
    unit
  else
    pow ~acc:unit b n

let memoized_fix f =
  let mem = Hashtbl.create 1 in
  let rec fx n =
    try Hashtbl.find mem n with Not_found ->
      let r = f fx n in
      Hashtbl.add mem n r ;
      r
  in
  fx
