open Compiler_util

(** Indicates that the program terminated with a normal exception. *)
exception Aborted of string
            
type value =
  | VInt of int
  | VBool of bool
  | VArray of value array
  | VClosure of (value list -> value)
  [@@deriving show]

val eval : exp -> value
