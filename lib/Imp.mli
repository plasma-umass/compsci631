(** The IMP language. *)

(** Operators to compare integers. *)
type cmp = Imp_syntax.cmp =
  | Lt  (** less than *)
  | Gt  (** greater than *)
  | Eq  (** equal to *)
  | Lte  (** less than or equal to *)
  | Gte (** greater than or equal to *)

(* Operators on integers. *)
type aop = Imp_syntax.aop =
  | Add | Sub | Mul

type aexp = Imp_syntax.aexp =
  | AConst of int
  | AVar of string
  | AOp of aop * aexp * aexp

type bexp = Imp_syntax.bexp =
  | BConst of bool
  | BAnd of bexp * bexp
  | BOr of bexp * bexp
  | BNot of bexp
  | BCmp of cmp * aexp * aexp

type cmd = Imp_syntax.cmd =
  | CSkip
  | CAbort
  | CAssign of string * aexp
  | CIf of bexp * cmd * cmd
  | CWhile of bexp * bexp * cmd
  | CSeq of cmd * cmd

(** [from_string string] produces [(pre, cmd, post)], where the
    first and last assertions in the program are interpreted as pre- and
    post-conditions respectively. *)
val from_string : string -> bexp * cmd * bexp

(** Reads the contents of a file. See [from_string] for more information. *)
val from_file : string -> bexp * cmd * bexp

(** Prints a string that represents a boolean expression. *)
val bexp_to_string : bexp -> string