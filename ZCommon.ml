let for_z from til ?(by=Z.one) f =
  let i = ref from in
  while Z.leq !i til do
    f !i ;
    i := Z.add !i by
  done
