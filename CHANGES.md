# 0.3

- add notation `Arith.( ** )` for integer exponentiation
- add `Arith.sdiv`
- rename `Arith.mul_quo` to `mul_equo` and fix overflow
- add `Arith.mul_{ediv,erem}`
- let `Arith.gcdext` return minimal coefficients, fix overflow
- add `Arith.{gcd,gcdext,lcm}_seq`
- add functions related to integer powers and roots: `Arith.is_pow`,
  `Arith.is_pow2`, `Arith.kth_root`, `Arith.is_kth_pow`, `Arith.smallest_root`
- fix `Arith.range_down` not being exposed in the interface
- add `Primes.divisor_pairs`

# 0.2

- add `Primes.prime_seq`
- lower the minimal supported version of OCaml to 4.07

# 0.1

- First release
