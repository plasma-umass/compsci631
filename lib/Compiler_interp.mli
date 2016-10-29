open Compiler_util

type value =
  | VInt of int
  | VBool of bool
  | VArray of value array
  | VClosure of (value list -> value)
  [@@deriving show]

val eval : exp -> value
