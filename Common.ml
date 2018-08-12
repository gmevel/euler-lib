let rec pow ~mult ~unit b n =
  assert (n >= 0) ;
  if n = 0 then
    unit
  else if n mod 2 = 0 then
    pow ~mult ~unit (mult b b) (n / 2)
  else
    pow ~mult ~unit:(mult unit b) (mult b b) (n / 2)

let rec memoized_fix f =
  let mem = Hashtbl.create 1 in
  let rec fx n =
    try Hashtbl.find mem n with Not_found ->
      let r = f fx n in
      Hashtbl.add mem n r ;
      r
  in
  fx
