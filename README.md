# Euler

`euler` is a library for doing integer arithmetic with OCaml’s standard integers (31 or 63 bits). 

# What’s in it?

`euler` provides:

- Drop-in, overflow-detecting base arithmetic:
  if you are paranoid about vicious bugs sneaking in silently, this library detects overflows and signal them by throwing an exception; the module can be used as a drop-in replacement for the standard library (beware that Euler.Arith.min_int differs from Stdlib.min_int, the latter being a forbidden value). There are also a few additional functions such as integer logarithms and square roots.
- More advanced arithmetic:
  for the weird folks (like myself) who are interested in advanced arithmetic but do not care about integers larger than 262, and thus do not want the burden of using an arbitrary-precision library (zarith of GMP), there you are. The library provides some classic functions such as
  - the GCD,
  - the Jacobi symbol,
  - primality testing (fast and deterministic for all 63-bit integers!),
  - integer factorization (implementing Lenstra’s elliptic curve factorization, which was apparently one of the best known algorithms back when I wrote that code, but obviously it is still very slow! — and I must say I understand very little about it…),
  - a prime sieve (heavily optimized) and a factorization sieve,
  - Euler’s totient function (slow too, of course),
  - and so on.
- Solvers for some forms of integer equations (so-called “Diophantine equations”):
  - linear congruence systems (the Chinese remainder theorem),
  - Pell-Fermat’s equations (the Chakravala method) — preliminary code that just needs some packaging effort).
- Modular arithmetic:
    including finding modular inverses (and pseudo-inverses). A nice functorial interface provides convenient notations and uses a private type to enforce that values are always normalized in the range 0…m−1 where m is the modulus. Example use:
    ```{ocaml}
    module M = Euler.Modular.Make (struct let modulo = 42 end)
    let () = assert (M.( !:1 /: (!:33 +: !:4) = !:5 **:(-4) ))
    (* said otherwise, modulo 42, the inverse of (33 + 4) is equal to 5^(−4) *)
    ```

# But why?

Writing this library was fun and educative for me, and allowed me to solidify my math training in code. In fact, as the name suggests, the initial incentive was playing with Project Euler (hence the focus on integers that fit in a machine word) while sparing me the boredom of implementing a prime sieve for the hundredth time.

Nevertheless, I believe euler might prove actually useful outside of the playground. Overflow detection is an actual need in some software, and implementing it is not trivial, even less so after some amount of micro-optimization (see code). Modular arithmetic is not trivial either (e.g. multiplication is not as simple as (a * b) mod m because this computation might overflow). And well, even integer logarithms and square roots are handy at times, and again they not trivial to implement (as using their floating-point counterpart gives incorrect results for large integers).

The library is documented, with a focus on algorithmic complexities, and implementation code has a lot of comments too. You can find the [docs here](https://perso.crans.org/mevel/odoc/euler/Euler/).
