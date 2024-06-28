# Euler

Euler is a library for doing arithmetic with OCaml’s native integers (31 or 63 bits).

**[Documentation][doc]**. Algorithmic complexities are documented. Besides, implementation code has a lot of comments.

## What’s in it?

Euler provides:

- **Overflow-detecting base arithmetic:**
  to avoid vicious bugs sneaking in silently, this library detects integer overflows and signals them by throwing an exception; the module `Euler.Arith` can be used to shadow the standard library’s arithmetic functions (beware that `Euler.Arith.min_int` differs from `Stdlib.min_int`, the latter being a forbidden value).
  There are also additional elementary functions on integers, such as logarithms and square roots.
- **Advanced arithmetic:**
  for the weird folks (like myself) who are interested in advanced arithmetic but do not care about integers larger than 2<sup>62</sup>, and thus do not want the burden of using an arbitrary-precision library (like zarith or GMP), there you are. The library provides many classic functions such as
  - the GCD,
  - the Jacobi symbol,
  - primality testing (fast and deterministic for all 63-bit integers),
  - factorization (implementing Lenstra’s elliptic curve factorization, allegedly one of the best algorithms known as of 2018, but obviously still very slow — and I understand very little about it),
  - a prime sieve (heavily optimized Eratosthenes sieve),
  - a factorization sieve,
  - Euler’s totient function (slow, obviously),
  - and so on.
- **Solvers for some forms of integer equations (so-called “Diophantine equations”):**
  - linear congruence systems (the Chinese remainder theorem),
  - Pell-Fermat’s equations (the Chakravala method — preliminary code that needs some packaging effort).
- **Modular arithmetic:**
    including finding modular inverses, pseudo-inverses and multiplicative orders.
    A nice functorial interface provides convenient notations and uses a private type to enforce that values are always normalized in the range 0…m−1 where m is the modulus. Example use:
    ```ocaml
    module M = Euler.Modular.Make (struct let modulo = 42 end)
    let () = assert (M.( !:1 /: (!:33 +: !:4) = !:5 **:(-4) ))
    (* modulo 42, the inverse of (33 + 4) is equal to 5^(−4) *)
    ```

## Why to use it?

Euler may prove actually useful outside of the playground.

* Some software need overflow detection, and implementing it is not trivial, even less so after some amount of micro-optimization.
* Modular arithmetic is not trivial either (e.g. multiplication is not as simple as `(a * b) mod m` because this computation might overflow).
* Even integer logarithms and square roots are handy at times, and again they not trivial to implement (as using their floating-point counterpart gives incorrect results for large integers).

However the functions related to prime numbers, for instance, are more of an educational or recreational interest, because of their limitation to native integers. Actual applications, such as cryptography, need arbitrary-precision integers.

## Why has it been written?

Writing this library was fun and educative for me, and allowed me to solidify my math training in code. In fact, as the name suggests, the initial incentive was to play with [Project Euler][project-euler] (hence the focus on integers that fit in a machine word) while sparing me the boredom of re-implementing a prime sieve for the hundredth time.

[doc]: https://gmevel.github.io/euler-lib/index.html/euler/Euler/
[project-euler]: https://projecteuler.net/
