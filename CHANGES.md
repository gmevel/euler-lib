# 0.3

- optimize several functions in Arith:
  + `mul`
  + `pow` for small bases
  + `log2` and `log2sup` (5x faster)
  + `log` and `logsup` for bases 2, 16, 64, 10, 60
  + `valuation_of_2`
  + `number_of_bits_set` (7x times faster)
- BREAKING: add notation `Arith.( ** )` for integer exponentiation
- add `Arith.sdiv`
- fix overflow in `Arith.mul_quo`
- BREAKING: rename `Arith.mul_quo` to `mul_equo`
- add `Arith.mul_{ediv,erem}`
- fix overflow in `Arith.gcdext`
- let `Arith.gcdext` return minimal coefficients
- add `Arith.{gcd,gcdext,lcm}_of_seq`
- add functions related to integer powers and roots:
  + `Arith.is_pow`
  + `Arith.is_pow2`
  + `Arith.kth_root`
  + `Arith.is_kth_pow`
  + `Arith.smallest_root`
- fix `Arith.range_down` not being exposed in the interface
- add some classical arithmetic functions whose computation uses factorization:
  + `Primes.divisor_pairs`
  + `Primes.sum_of_divisors`
  + `Primes.jordan`
  + `Primes.carmichael`
  + `Primes.mobius`
  + `Primes.derivative`
- add `Primes.order`, `Primes.order_with_known_multiple`, `Primes.order_mod_prime_pow`
- BREAKING: rename sequence-related functions:
  + `Arith.sum_seq` -> `sum_of_seq`
  + `Arith.prod_seq` -> `prod_of_seq`
  + `Primes.prime_seq` -> `gen_primes`
  + `Primes.primes` -> `iter_primes`

# 0.2

- add `Primes.prime_seq`
- lower the minimal supported version of OCaml to 4.07

# 0.1

- First release
