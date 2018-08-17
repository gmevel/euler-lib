val opp : modulo:int -> int -> int
val inv : modulo:int -> int -> int

val add : modulo:int -> int -> int -> int
val sub : modulo:int -> int -> int -> int
val mul : modulo:int -> int -> int -> int
val div : modulo:int -> int -> int -> int
val div_nonunique : modulo:int -> int -> int -> int
val div_factorize : modulo:int -> int -> int -> int
val pow : modulo:int -> int -> int -> int

val rand : modulo:int -> unit -> int

exception Factor_found of int

module Make : (sig val modulo : int end) -> sig
  val modulo : int
  type t = private int
  val of_int : int -> t
  val to_int : t -> int
  val opp : t -> t
  val inv : t -> t
  val ( ~-: ) : t -> t
  val ( ~/: ) : t -> t
  val ( +: ) : t -> t -> t
  val ( -: ) : t -> t -> t
  val ( *: ) : t -> t -> t
  val ( /: ) : t -> t -> t
  val ( //: ) : t -> t -> t
  val pow : t -> int -> t
  val rand : unit -> t
end
