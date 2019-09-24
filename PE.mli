(* FIXME: When an explicit interface is given, odoc does not generate the
 * documentation for submodules (including functors) such as PE.ModArith.Make.
 * Is this a bug in odoc? *)

(** {2 Toplevel values} *)

module Common : module type of Common

(** {2 Modules on OCaml integers} *)

module Arith : module type of Arith

module ModArith : module type of ModArith

module Primality : module type of Primality

module Farey : module type of Farey

(** {2 Modules on Zarith integers} *)

module ZCommon : module type of ZCommon
