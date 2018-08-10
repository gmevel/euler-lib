exception Overflow

val sign : int -> int

val ediv : int -> int -> int * int
val equo : int -> int -> int
val erem : int -> int -> int

val gcd : int -> int -> int
val gcdext : int -> int -> int * int * int
val lcm : int -> int -> int

val valuation      : factor:int -> int -> int * int
val valuation_of_2 :               int -> int * int

val is_square : int -> bool

(******************************************************************************)

(***
 *** coefficients binomiaux
 ***)

(* [mul_div a b d] calcule a×b/d sans overflow si le résultat attendu est
 * inférieur à max_int et que d est inférieur à sqrt(max_int). *)
val mul_div : int -> int -> int -> int

(* [binoms n] retourne tous les coefficients binomiaux d’ordre [n].
 * calcule en temps O(n) et en espace O(n), et ne cause pas d’overflow si:
 *     — version 1: si les résultats attendus sont inférieurs à max_int ∕ (n∕2);
 *     — version 2: si les résultats attendus sont inférieurs à max_int
 *       [ce qui implique que n∕2 < sqrt(max_int)].
 * pour des entiers signés de 63 bits, la version 1 est correcte pour n∕2 ⩽ 30;
 * la version 2 est correcte pour n∕2 ⩽ 32. *)
val binoms : int -> int array

(* [binom n p] retourne le nombre de combinaisons de [p] éléments parmi [n].
 * calcule en temps O(q) et en espace O(1), et ne cause pas d’overflow si:
 *     — version 1: si le résultat attendu est inférieur à max_int ∕ q;
 *     — version 2: si le résultat attendu est inférieur à max_int
 *       [ce qui implique que q < sqrt(max_int)];
 * où q = min(p, n−p).
 * pour des entiers signés de 63 bits, la version 1 est correcte pour n∕2 ⩽ 30;
 * la version 2 est correcte pour n∕2 ⩽ 32. *)
val binom : int -> int -> int

(* [central_binom p] retourne le nombre de combinaisons de [p] éléments parmi
 * [2p]. calcule en temps O(p) et en espace O(1), et ne cause d’overflow que si
 * le résultat attendu est lui-même strictement supérieur à à max_int.
 * pour des entiers signés de 63 bits, c’est correct pour p ⩽ 32. *)
val central_binom : int -> int
