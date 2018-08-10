(* exponentiation rapide générique *)
val pow : mult:('a -> 'a -> 'a) -> unit:'a -> 'a -> int -> 'a

(* combinateur de point fixe avec mémoïsation *)
val memoized_fix : (('a -> 'b) -> ('a -> 'b)) -> 'a -> 'b
(* exemple d’utilisation:
      let fib = memoized_fix@@fun fib n ->
        if n < 2 then
          1
        else
          fib (n-1) + fib (n-2)
*)
